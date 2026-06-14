//! `UnitInterface` — cross-unit compilation boundary.
//!
//! A dependency makes several source-level contributions to a dependent
//! unit's compilation.  Previously these were handled by four separate,
//! ad-hoc mechanisms scattered across the pipeline:
//!
//! 1. Bracket content-modes — per-file `BracketRegistry` (removed; colon
//!    heuristic now makes this self-contained)
//! 2. Monad-namespace registry — drain/seed pattern in the desugarer
//! 3. Operator table — rediscovered from the merged tree at cook time
//! 4. Type schemes — `PreludeSummary` / `with_seed` in the type checker
//!
//! `UnitInterface` unifies the last three into a single struct that is
//! built incrementally as each pipeline phase completes and seeded into
//! the next compilation unit.
//!
//! ## Build order
//!
//! | Phase completed | Fields populated |
//! |---|---|
//! | translate / desugar | `monad_specs`, `monad_type_hints` |
//! | cook | `operators` |
//! | typecheck | `type_summary` |

use std::collections::HashMap;

use crate::{
    common::sourcemap::Smid,
    core::{
        demand::Demand,
        desugar::desugarer::MonadSpec,
        expr::{has_internal_export, Expr, Fixity, Precedence, Primitive, RcExpr},
        typecheck::check::PreludeSummary,
    },
};

// ── OperatorInfo ─────────────────────────────────────────────────────────────

/// Fixity, precedence, and optional type annotation for an operator binding.
///
/// Extracted from the pre-cook expression (where the `Meta` wrappers are still
/// present) so that both cook and the type checker can consume the information
/// from a single extraction pass.
#[derive(Debug, Clone)]
pub struct OperatorInfo {
    /// Source location of the operator definition.
    pub smid: Smid,
    /// Fixity (infix-left, infix-right, prefix, postfix, nullary).
    pub fixity: Fixity,
    /// Numeric precedence.
    pub precedence: Precedence,
    /// Raw `type:` annotation string, if present in the definition metadata.
    ///
    /// Used by the type checker to build operator overload constraints.
    /// `None` if the operator definition carries no `type:` annotation.
    pub type_annotation: Option<String>,
}

// ── Visibility ───────────────────────────────────────────────────────────────

/// Declaration visibility at the unit boundary.
///
/// All declarations default to `Public`.  Declarations annotated with
/// `export: :internal` (W3 / eu-u2vn.3) are `Internal` and must not be
/// re-exported to dependents.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Public,
    Internal,
}

// ── UnitInterface ─────────────────────────────────────────────────────────────

/// The complete cross-unit interface exported by a compiled eucalypt unit.
///
/// Carried by `SourceLoader` and seeded into the next compilation unit so
/// that the four cross-unit mechanisms share a single data structure.
///
/// Fields that are not yet populated for a given phase are left at their
/// `Default` values (empty maps / default `PreludeSummary`).
#[derive(Debug, Clone, Default)]
pub struct UnitInterface {
    /// Monad namespace specs (bind/return shapes), keyed by namespace name
    /// (e.g. `"io"`, `"list"`).
    ///
    /// Populated after translate/desugar.  Seeded into the next desugarer
    /// via `seed_monad_namespace_registry`.
    pub monad_specs: HashMap<String, MonadSpec>,

    /// Monad wrapper type hints for LSP element-type display, keyed by
    /// monad name (e.g. `"io" → "IO(a)"`).
    ///
    /// Populated after translate/desugar.
    pub monad_type_hints: HashMap<String, String>,

    /// Operator metadata extracted from the pre-cook expression, keyed by
    /// operator name (e.g. `"+"`, `"<"`).
    ///
    /// Populated after the translate/desugar phase, before cook strips `Meta`
    /// wrappers.  Consumed by the type checker for operator constraint
    /// discharge.
    pub operators: HashMap<String, OperatorInfo>,

    /// Type schemes, aliases, and branch shapes from the prelude (or a
    /// compiled dependency unit).
    ///
    /// Populated after typecheck.  Seeded into the next `Checker` via
    /// `Checker::with_seed`.
    pub type_summary: PreludeSummary,

    /// Declaration visibility, keyed by binding name.
    ///
    /// Populated after typecheck (eu-u2vn.3).  All declarations default to
    /// `Visibility::Public`; `export: :internal` annotations set
    /// `Visibility::Internal`.
    pub visibility: HashMap<String, Visibility>,

    /// Demand (strictness) annotations for exported bindings, keyed by
    /// binding name.
    ///
    /// Populated after STG compilation (W9).  Entries carry the demand the
    /// compiler computed for each top-level binding; absent entries are
    /// equivalent to `Demand::default()` (all `Unknown`).
    ///
    /// Consumed by W6's pre-compiled prelude blob and by cross-unit
    /// compiler passes (W11).  A missing entry costs only optimisation,
    /// never correctness.
    pub demands: HashMap<String, Demand>,
}

impl UnitInterface {
    /// Seed a new `UnitInterface` with monad registries from `self`.
    ///
    /// Used by `SourceLoader::translate` to carry monad namespace
    /// registrations forward from the prelude into user files.
    pub fn seed_desugarer_monad_registries(
        &self,
        desugarer: &mut crate::core::desugar::desugarer::Desugarer,
    ) {
        desugarer.seed_monad_namespace_registry(&self.monad_specs);
        desugarer.seed_monad_type_registry(&self.monad_type_hints);
    }

    /// Drain monad registries from a finished desugarer into `self`.
    ///
    /// Called after `desugarer.translate_unit` so that newly registered
    /// monad namespaces are persisted for subsequent translation units.
    pub fn drain_desugarer_monad_registries(
        &mut self,
        desugarer: &mut crate::core::desugar::desugarer::Desugarer,
    ) {
        self.monad_specs
            .extend(desugarer.drain_monad_namespace_registry());
        self.monad_type_hints
            .extend(desugarer.drain_monad_type_registry());
    }

    /// Populate `self.operators` by walking `expr` before cook strips `Meta` wrappers.
    ///
    /// Cook's `distribute_fixities` pass removes `Meta` nodes from operator
    /// definitions to move fixity/precedence to call sites, erasing `type:`
    /// annotations in the process.  Calling this method on the merged
    /// pre-cook expression captures both the operator shape (fixity, precedence,
    /// source location) and any `type:` annotation string before they are lost.
    ///
    /// Replaces the separate `extract_operator_type_strings` call in `check.rs`,
    /// consolidating operator discovery into a single extraction pass.
    pub fn extract_operators_from_expr(&mut self, expr: &RcExpr) {
        collect_operator_info(expr, &mut self.operators);
    }

    /// Walk `expr` and record each binding's visibility in `self.visibility`.
    ///
    /// Bindings with `export: :internal` metadata are recorded as
    /// `Visibility::Internal`; all others default to `Visibility::Public`.
    ///
    /// Should be called on the pre-cook merged expression (at the same time as
    /// `extract_operators_from_expr`) so that the `Meta` wrappers are still present.
    pub fn extract_visibility_from_expr(&mut self, expr: &RcExpr) {
        collect_visibility(expr, &mut self.visibility);
    }

    /// Walk `expr` and register each exported binding in `self.demands` with
    /// a conservative `Demand::default()` annotation.
    ///
    /// This establishes the demand signature slot for every exported binding.
    /// The slot starts conservative (all `Unknown`); future analysis passes
    /// (W11 strictness analysis) populate it with richer information.
    ///
    /// Call at the same pipeline stage as `extract_visibility_from_expr`.
    pub fn extract_demands_from_expr(&mut self, expr: &RcExpr) {
        collect_demands(expr, &mut self.demands);
    }

    /// Build a `HashMap<String, String>` of operator name → raw `type:` annotation
    /// from `self.operators`, suitable for `parse_operator_overloads`.
    pub fn operator_type_strings(&self) -> HashMap<String, String> {
        self.operators
            .iter()
            .filter_map(|(name, info)| {
                info.type_annotation
                    .as_ref()
                    .map(|s| (name.clone(), s.clone()))
            })
            .collect()
    }
}

// ── Operator extraction helpers ──────────────────────────────────────────────

/// Recursively collect `OperatorInfo` for all operator bindings in `expr`.
///
/// Mirrors the traversal in `fixity::extract_operator_type_strings` but also
/// captures fixity, precedence, and source location from the `Operator` node.
fn collect_operator_info(expr: &RcExpr, out: &mut HashMap<String, OperatorInfo>) {
    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            for (name, value) in &scope.pattern {
                if let Some(info) = extract_operator_info_from_value(value) {
                    out.insert(name.clone(), info);
                }
            }
            collect_operator_info(&scope.body, out);
        }
        Expr::Meta(_, inner, _) => collect_operator_info(inner, out),
        _ => {}
    }
}

/// Extract `OperatorInfo` from a binding value, peeling through `Meta` layers.
///
/// Returns `Some` only if the innermost expression (through zero or more `Meta`
/// wrappers) is an `Operator` node.
fn extract_operator_info_from_value(value: &RcExpr) -> Option<OperatorInfo> {
    match &*value.inner {
        Expr::Meta(_, inner, meta_block) => {
            if let Some(mut info) = extract_operator_info_from_value(inner) {
                // If this Meta layer has a `type:` annotation and we haven't
                // already found one, capture it.
                if info.type_annotation.is_none() {
                    info.type_annotation = extract_type_str_from_block(meta_block);
                }
                Some(info)
            } else {
                None
            }
        }
        Expr::Operator(smid, fixity, precedence, _) => Some(OperatorInfo {
            smid: *smid,
            fixity: *fixity,
            precedence: *precedence,
            type_annotation: None,
        }),
        _ => None,
    }
}

/// Extract the string value of the `type:` key from a block expression.
fn extract_type_str_from_block(block_expr: &RcExpr) -> Option<String> {
    let block = match &*block_expr.inner {
        Expr::Block(_, b) => b,
        _ => return None,
    };
    let type_expr = block.get("type")?;
    extract_str_literal(type_expr)
}

fn extract_str_literal(expr: &RcExpr) -> Option<String> {
    match &*expr.inner {
        Expr::Literal(_, Primitive::Str(s)) => Some(s.clone()),
        // s"..." type-data literals are accepted as type annotation strings.
        Expr::Literal(_, Primitive::TypeData(s)) => Some(s.clone()),
        _ => None,
    }
}

// ── Visibility extraction helpers ────────────────────────────────────────────

/// Recursively collect `Visibility` for all bindings in `expr`.
fn collect_visibility(expr: &RcExpr, out: &mut HashMap<String, Visibility>) {
    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            for (name, value) in &scope.pattern {
                let vis = if has_internal_export(value) {
                    Visibility::Internal
                } else {
                    Visibility::Public
                };
                out.insert(name.clone(), vis);
            }
            collect_visibility(&scope.body, out);
        }
        Expr::Meta(_, inner, _) => collect_visibility(inner, out),
        _ => {}
    }
}

/// Recursively register all exported bindings in `expr` with a conservative
/// `Demand::default()` annotation.
///
/// Internal bindings (marked `export: :internal`) are excluded — they are not
/// part of the cross-unit interface.
fn collect_demands(expr: &RcExpr, out: &mut HashMap<String, Demand>) {
    match &*expr.inner {
        Expr::Let(_, scope, _) => {
            for (name, value) in &scope.pattern {
                if !has_internal_export(value) {
                    out.entry(name.clone()).or_default();
                }
            }
            collect_demands(&scope.body, out);
        }
        Expr::Meta(_, inner, _) => collect_demands(inner, out),
        _ => {}
    }
}
