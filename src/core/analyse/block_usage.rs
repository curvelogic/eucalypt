//! Analyse block usage patterns in a core expression tree.
//!
//! Walks the expression tree and classifies every `DefaultBlockLet`
//! binding as having:
//! - **static-only** access (all references are `Lookup(Var(Bound(bv)), key, ...)`)
//! - **dynamic** access (at least one bare `Var(Bound(bv))` reference)
//!
//! Reports member counts, accessed member counts, and eliminated
//! member counts so we can quantify the DCE opportunity.

use crate::core::expr::*;
use moniker::{BoundVar, Embed, Var};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

/// Summary of access patterns for a single block binding.
#[derive(Debug, Clone)]
pub struct BlockUsageSummary {
    /// Pretty name of the binding (from the binder)
    pub name: String,
    /// Total number of members in the block body
    pub total_members: usize,
    /// Members accessed via static Lookup
    pub static_accesses: HashSet<String>,
    /// Whether the block escapes dynamically (bare var reference)
    pub escapes: bool,
    /// Sites where the block escapes (pretty names of the bound var references)
    pub escape_sites: Vec<String>,
    /// Members that could be eliminated (total - accessed) if static-only
    pub eliminable: usize,
}

/// Overall report for a core expression tree.
#[derive(Debug, Default)]
pub struct BlockUsageReport {
    pub blocks: Vec<BlockUsageSummary>,
}

impl BlockUsageReport {
    pub fn total_members(&self) -> usize {
        self.blocks.iter().map(|b| b.total_members).sum()
    }

    pub fn total_static_only(&self) -> usize {
        self.blocks.iter().filter(|b| !b.escapes).count()
    }

    pub fn total_escaping(&self) -> usize {
        self.blocks.iter().filter(|b| b.escapes).count()
    }

    pub fn total_eliminable(&self) -> usize {
        self.blocks.iter().map(|b| b.eliminable).sum()
    }

    pub fn print_report(&self) {
        eprintln!("=== Block Usage Report ===");
        eprintln!("Total DefaultBlockLet bindings: {}", self.blocks.len());
        eprintln!("  Static-only: {}", self.total_static_only());
        eprintln!("  Escaping (dynamic): {}", self.total_escaping());
        eprintln!(
            "  Total members across all blocks: {}",
            self.total_members()
        );
        eprintln!("  Total eliminable members: {}", self.total_eliminable());
        eprintln!();
        for b in &self.blocks {
            let status = if b.escapes { "ESCAPES" } else { "static-only" };
            eprintln!(
                "  {}: {} members, {} accessed, {} eliminable [{}]",
                b.name,
                b.total_members,
                b.static_accesses.len(),
                b.eliminable,
                status
            );
            if b.escapes && !b.escape_sites.is_empty() {
                let sites: Vec<_> = b.escape_sites.iter().take(5).collect();
                eprintln!("    escape sites: {sites:?}");
            }
        }
    }
}

/// Analyse block usage patterns in a core expression.
pub fn analyse_block_usage(expr: &RcExpr) -> BlockUsageReport {
    let mut analyser = BlockUsageAnalyser::default();
    analyser.analyse(expr);
    analyser.report
}

type BindingId = *const Expr<RcExpr>;

#[derive(Default)]
struct BlockUsageAnalyser {
    scopes: VecDeque<RcExpr>,
    /// DefaultBlockLet bindings we're tracking
    candidates: HashMap<BindingId, BlockInfo>,
    report: BlockUsageReport,
}

struct BlockInfo {
    name: String,
    total_members: usize,
    static_accesses: HashSet<String>,
    escapes: bool,
    escape_sites: Vec<String>,
}

impl BlockUsageAnalyser {
    fn analyse(&mut self, expr: &RcExpr) {
        self.walk(expr);
        self.finalise();
    }

    fn walk(&mut self, expr: &RcExpr) {
        match &*expr.inner {
            Expr::Let(_, scope, _) => {
                // Register DefaultBlockLet bindings
                for (ref binder, Embed(ref value)) in &scope.unsafe_pattern.unsafe_pattern {
                    if value.inner.is_default_let() {
                        let id: BindingId = Rc::as_ptr(&value.inner);
                        let name = binder
                            .0
                            .pretty_name
                            .clone()
                            .unwrap_or_else(|| "<anon>".to_string());
                        let total = self.count_block_members(value);
                        self.candidates.insert(
                            id,
                            BlockInfo {
                                name,
                                total_members: total,
                                static_accesses: HashSet::new(),
                                escapes: false,
                                escape_sites: Vec::new(),
                            },
                        );
                    }
                }
                self.scopes.push_front(expr.clone());
                for (_, Embed(ref value)) in &scope.unsafe_pattern.unsafe_pattern {
                    self.walk(value);
                }
                self.walk(&scope.unsafe_body);
                self.scopes.pop_front();
            }
            Expr::Lam(_, _, scope) => {
                self.scopes.push_front(expr.clone());
                self.walk(&scope.unsafe_body);
                self.scopes.pop_front();
            }
            Expr::Lookup(_, e, member, fb) => {
                // Check for static access: Lookup(Var(Bound(bv)), member, ...)
                if let Expr::Var(_, Var::Bound(bound_var)) = &*e.inner {
                    self.record_static_access(bound_var, member);
                } else {
                    self.walk(e);
                }
                if let Some(fallback) = fb {
                    self.walk(fallback);
                }
            }
            Expr::Var(_, Var::Bound(bound_var)) => {
                self.record_escape(bound_var);
            }
            Expr::App(_, f, args) => {
                self.walk(f);
                for a in args {
                    self.walk(a);
                }
            }
            Expr::List(_, xs) => {
                for x in xs {
                    self.walk(x);
                }
            }
            Expr::Block(_, bm) => {
                for (_, v) in bm.iter() {
                    self.walk(v);
                }
            }
            Expr::Meta(_, e, m) => {
                self.walk(e);
                self.walk(m);
            }
            Expr::ArgTuple(_, xs) => {
                for x in xs {
                    self.walk(x);
                }
            }
            Expr::Soup(_, xs) => {
                for x in xs {
                    self.walk(x);
                }
            }
            Expr::Operator(_, _, _, e) => {
                self.walk(e);
            }
            _ => {}
        }
    }

    fn record_static_access(&mut self, bv: &BoundVar<String>, member: &str) {
        if let Some(scope_expr) = self.scopes.get(bv.scope.0 as usize) {
            if let Expr::Let(_, scope, _) = &*scope_expr.inner {
                if bv.binder.to_usize() < scope.unsafe_pattern.unsafe_pattern.len() {
                    let (_, Embed(ref value)) =
                        &scope.unsafe_pattern.unsafe_pattern[bv.binder.to_usize()];
                    let id: BindingId = Rc::as_ptr(&value.inner);
                    if let Some(info) = self.candidates.get_mut(&id) {
                        info.static_accesses.insert(member.to_owned());
                    }
                }
            }
        }
    }

    fn record_escape(&mut self, bv: &BoundVar<String>) {
        if let Some(scope_expr) = self.scopes.get(bv.scope.0 as usize) {
            if let Expr::Let(_, scope, _) = &*scope_expr.inner {
                if bv.binder.to_usize() < scope.unsafe_pattern.unsafe_pattern.len() {
                    let (_, Embed(ref value)) =
                        &scope.unsafe_pattern.unsafe_pattern[bv.binder.to_usize()];
                    let id: BindingId = Rc::as_ptr(&value.inner);
                    if let Some(info) = self.candidates.get_mut(&id) {
                        info.escapes = true;
                        info.escape_sites.push(
                            bv.pretty_name
                                .clone()
                                .unwrap_or_else(|| format!("?{}", bv.binder.to_usize())),
                        );
                    }
                }
            }
        }
    }

    fn count_block_members(&self, expr: &RcExpr) -> usize {
        match &*expr.inner {
            Expr::Let(_, scope, LetType::DefaultBlockLet) => match &*scope.unsafe_body.inner {
                Expr::Block(_, bm) => bm.len(),
                _ => 0,
            },
            Expr::Meta(_, e, _) => self.count_block_members(e),
            _ => 0,
        }
    }

    fn finalise(&mut self) {
        for (_, info) in self.candidates.drain() {
            let eliminable = if info.escapes {
                0
            } else {
                info.total_members
                    .saturating_sub(info.static_accesses.len())
            };
            self.report.blocks.push(BlockUsageSummary {
                name: info.name,
                total_members: info.total_members,
                static_accesses: info.static_accesses,
                escapes: info.escapes,
                escape_sites: info.escape_sites,
                eliminable,
            });
        }
    }
}
