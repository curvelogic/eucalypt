# Block Duality DCE Investigation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Investigate whether the dual nature of blocks (namespace vs data structure) prevents effective dead code elimination, quantify the impact, explore approaches to unlock intra-block DCE, and decide whether to pursue a solution for 0.4.0.

**Architecture:** Eucalypt blocks serve two roles: (1) **namespace/module** — accessed via static dot notation (`ns.member`), compiled to `Expr::Lookup` with a known key string; (2) **data structure** — passed to functions, merged, iterated via `ELEMENTS`/`KEYS`/`VALUES`/`MERGE`, accessed dynamically via `LOOKUP`. The existing block-level DCE in `prune.rs` (using `BlockAccessTracker`) already handles the easy case: `DefaultBlockLet` bindings accessed *only* via static `Lookup` patterns get their block body filtered. The investigation needs to determine exactly where and why the existing DCE fails — the primary suspect is the `dynamise` transform (`src/core/transform/dynamise.rs`) which converts free variables into `Lookup(target, key, Some(Var(Bound(original))))` patterns. The fallback `Some(Var(Bound(original)))` is traversed during the prune mark phase, hitting the bare `Var(Bound(bv))` case which calls `encounter()` and disqualifies the binding from block-level DCE, even though the fallback is never reached when the lookup succeeds. This needs empirical verification before any fix is attempted.

**Tech Stack:** Rust, existing core expression and STG infrastructure

---

### Task 1: Build a static analysis pass to quantify block access patterns

**Files:**
- Create: `src/core/analyse/block_usage.rs`
- Modify: `src/core/analyse/mod.rs` (add `pub mod block_usage;`)

**Step 1: Read existing analysis infrastructure**

Read `src/core/analyse/mod.rs` and `src/core/analyse/testplan.rs` to understand the existing analysis module pattern.

**Step 2: Create block_usage.rs analysis pass**

Create `src/core/analyse/block_usage.rs`:

```rust
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
        eprintln!("  Total members across all blocks: {}", self.total_members());
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
                for (ref binder, Embed(ref value)) in
                    &scope.unsafe_pattern.unsafe_pattern
                {
                    if value.inner.is_default_let() {
                        let id: BindingId = std::rc::Rc::as_ptr(&value.inner);
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
                let (_, Embed(ref value)) =
                    &scope.unsafe_pattern.unsafe_pattern[bv.binder.to_usize()];
                let id: BindingId = std::rc::Rc::as_ptr(&value.inner);
                if let Some(info) = self.candidates.get_mut(&id) {
                    info.static_accesses.insert(member.to_owned());
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
                    let id: BindingId = std::rc::Rc::as_ptr(&value.inner);
                    if let Some(info) = self.candidates.get_mut(&id) {
                        info.escapes = true;
                    }
                }
            }
        }
    }

    fn count_block_members(&self, expr: &RcExpr) -> usize {
        match &*expr.inner {
            Expr::Let(_, scope, LetType::DefaultBlockLet) => {
                match &*scope.unsafe_body.inner {
                    Expr::Block(_, bm) => bm.len(),
                    _ => 0,
                }
            }
            Expr::Meta(_, e, _) => self.count_block_members(e),
            _ => 0,
        }
    }

    fn finalise(&mut self) {
        for (_, info) in self.candidates.drain() {
            let eliminable = if info.escapes {
                0
            } else {
                info.total_members.saturating_sub(info.static_accesses.len())
            };
            self.report.blocks.push(BlockUsageSummary {
                name: info.name,
                total_members: info.total_members,
                static_accesses: info.static_accesses,
                escapes: info.escapes,
                eliminable,
            });
        }
    }
}
```

**Step 3: Register the module**

Add to `src/core/analyse/mod.rs`:

```rust
pub mod block_usage;
```

**Step 4: Verify compilation**

```bash
cargo check
```

**Step 5: Commit**

```bash
git add src/core/analyse/block_usage.rs src/core/analyse/mod.rs
git commit -m "feat: add block usage analysis pass for DCE investigation"
```

---

### Task 2: Add --debug-block-usage CLI flag to dump the report

**Files:**
- Modify: `src/driver/options.rs` (add flag)
- Modify: `src/driver/prepare.rs` (invoke analysis and print report)

**Step 1: Read options.rs to find where flags are defined**

Read `src/driver/options.rs` and locate the `no_dce` flag definition (around line 168-170) to add a sibling flag.

**Step 2: Add the flag to options.rs**

Add after the `no_dce` field:

```rust
    /// Dump block usage analysis (static vs dynamic access patterns)
    #[arg(long = "debug-block-usage")]
    pub debug_block_usage: bool,
```

Add an accessor method in the impl block:

```rust
    pub fn debug_block_usage(&self) -> bool {
        self.debug_block_usage
    }
```

**Step 3: Invoke analysis in prepare.rs**

After the first `eliminate` call (around line 156 in `prepare.rs`), add:

```rust
    // Dump block usage analysis if requested
    if opt.debug_block_usage() {
        use crate::core::analyse::block_usage;
        let report = block_usage::analyse_block_usage(&loader.core().expr);
        report.print_report();
    }
```

**Step 4: Verify compilation**

```bash
cargo check
```

**Step 5: Commit**

```bash
git add src/driver/options.rs src/driver/prepare.rs
git commit -m "feat: add --debug-block-usage flag for DCE analysis"
```

---

### Task 3: Run analysis against the harness test suite

**Files:**
- No files modified (analysis only)

**Step 1: Build the project**

```bash
cargo build
```

**Step 2: Run against a simple test to verify the flag works**

```bash
cargo run -- --debug-block-usage harness/test/001_hello.eu 2>&1 | head -50
```

Examine the output. There should be block usage information on stderr.

**Step 3: Run against a more complex test that uses prelude functions**

```bash
cargo run -- --debug-block-usage harness/test/006_str.eu 2>&1 | head -80
```

This test uses `str.join`, `str.upper`, etc. -- look for which blocks escape vs which are static-only.

**Step 4: Run against a test with dynamic block access**

```bash
cargo run -- --debug-block-usage harness/test/007_lookup.eu 2>&1 | head -80
```

**Step 5: Document findings**

Record in a scratchpad:
- How many total `DefaultBlockLet` bindings exist in a typical programme
- How many are static-only vs escaping
- How many members could theoretically be eliminated
- Which prelude blocks escape and why

This is a **decision checkpoint**: if the analysis shows that the existing block-level DCE in `prune.rs` already eliminates most dead block members, the remaining effort may not be worth pursuing. If significant waste remains due to escaping blocks, proceed to Task 4.

---

### Task 4: Trace escape paths for prelude blocks

**Files:**
- Modify: `src/core/analyse/block_usage.rs` (add escape-reason tracking)

**Step 1: Extend BlockInfo to record escape reasons**

Add a field to `BlockInfo`:

```rust
    escape_sites: Vec<String>,
```

When `record_escape` is called, also record a description of the escape site. Include the pretty name of the bound var and context:

```rust
fn record_escape(&mut self, bv: &BoundVar<String>) {
    // ... existing code ...
    if let Some(info) = self.candidates.get_mut(&id) {
        info.escapes = true;
        info.escape_sites.push(
            bv.pretty_name.clone().unwrap_or_else(|| format!("?{}", bv.binder.to_usize()))
        );
    }
}
```

**Step 2: Update report printing to show escape sites**

For escaping blocks, print the first few escape sites:

```rust
if b.escapes {
    let sites: Vec<_> = b.escape_sites.iter().take(5).collect();
    eprintln!("    escape sites: {:?}", sites);
}
```

(Add `escape_sites` to `BlockUsageSummary` and populate it in `finalise`.)

**Step 3: Verify and run**

```bash
cargo check && cargo run -- --debug-block-usage harness/test/006_str.eu 2>&1
```

**Step 4: Commit**

```bash
git add src/core/analyse/block_usage.rs
git commit -m "feat: track escape reasons in block usage analysis"
```

---

### Task 5: Investigate the dynamise transform's role in block escape

**Files:**
- No files modified (investigation only)

**Step 1: Read the dynamise transform**

Read `src/core/transform/dynamise.rs` carefully. Understand when and why it wraps variables in `Lookup` expressions with fallbacks.

**Step 2: Trace how dynamise interacts with prelude blocks**

The `dynamise` transform converts free variables into runtime lookups against an implicit parameter. This is used for generalised functions (e.g. when a block's value is a function that references outer scope variables). After dynamise, a reference like `str` in a generalised function body becomes `Lookup(implicit_param, "str", Some(var(str)))`.

The key question: does the fallback `Some(var(str))` count as a bare `Var` reference that causes the block to escape?

**Step 3: Check the prune.rs handling of Lookup fallbacks**

Look at `ScopeTracker::traverse` in `prune.rs` for the `Expr::Lookup` case. The fallback is traversed -- if it contains a `Var(Bound(bv))`, does `encounter` get called (which disqualifies)?

Read lines 327-336 of `prune.rs`. The Lookup case calls `encounter_lookup` for the primary target and then traverses the fallback. If the fallback is `Var(Bound(bv))`, the fallback traversal hits the `Expr::Var(_, Var::Bound(bv))` case which calls `self.encounter(bv)` -- which **disqualifies** the binding from block-level DCE.

**Step 4: Document the finding**

The dynamise transform creates `Lookup(target, key, Some(Var(Bound(original))))`. The `Some(Var(Bound(original)))` fallback causes `encounter()` to be called during prune traversal, which disqualifies the binding from block-level DCE. This is a major source of unnecessary escape: the fallback is never actually used if the lookup succeeds, but the *presence* of the bare variable reference forces retention of all members.

This is a concrete, fixable inefficiency.

---

### Task 6: Prototype fallback-aware DCE in prune.rs

**Files:**
- Modify: `src/core/simplify/prune.rs` (change fallback handling)

**Step 1: Read the existing Lookup handling**

The current code (around line 327):

```rust
Expr::Lookup(_, e, member, fb) => {
    if let Expr::Var(_, Var::Bound(bound_var)) = &*e.inner {
        self.encounter_lookup(bound_var, member);
    } else {
        self.traverse(e);
    }
    if let Some(fallback) = fb {
        self.traverse(fallback);
    }
}
```

The problem: `self.traverse(fallback)` for a `Var(Bound(bv))` fallback calls `encounter(bv)` which disqualifies the target binding from block-level DCE.

**Step 2: Modify fallback traversal to use encounter_lookup**

When the fallback of a Lookup is a bare `Var(Bound(bv))` and the primary target is *also* `Var(Bound(bv))` pointing at the same binding, the fallback should be treated as a lookup-related reference rather than an escape. The key insight: in a dynamised expression `Lookup(target, key, Some(Var(Bound(same_target))))`, the fallback refers to the same binding and is never reached if the key is found.

Change the handling:

```rust
Expr::Lookup(_, e, member, fb) => {
    if let Expr::Var(_, Var::Bound(bound_var)) = &*e.inner {
        self.encounter_lookup(bound_var, member);
        // For fallbacks that reference the SAME binding as the target,
        // treat as a static lookup rather than an escape. This handles
        // dynamise-generated patterns like:
        //   Lookup(target, key, Some(Var(Bound(target))))
        if let Some(fallback) = fb {
            if let Expr::Var(_, Var::Bound(fb_var)) = &*fallback.inner {
                if fb_var.scope == bound_var.scope
                    && fb_var.binder == bound_var.binder
                {
                    // Same binding -- not an escape, just a lookup fallback
                    self.encounter_lookup(fb_var, member);
                } else {
                    self.traverse(fallback);
                }
            } else {
                self.traverse(fallback);
            }
        }
    } else {
        self.traverse(e);
        if let Some(fallback) = fb {
            self.traverse(fallback);
        }
    }
}
```

**Step 3: Verify compilation and run tests**

```bash
cargo check && cargo test --lib
```

**Step 4: Run the full test suite to check for regressions**

```bash
cargo test
```

All harness tests should pass. This change is conservative: it only changes the escape classification for the specific pattern where the fallback refers to the same binding as the target.

**Step 5: Run analysis to measure improvement**

```bash
cargo run -- --debug-block-usage harness/test/006_str.eu 2>&1
```

Compare the number of escaping blocks and eliminable members before and after.

**Step 6: Commit**

```bash
git add src/core/simplify/prune.rs
git commit -m "fix: treat dynamise fallbacks as static access in block-level DCE"
```

---

### Task 7: Measure the compiled output size impact

**Files:**
- No files modified (measurement only)

**Step 1: Measure compiled STG size before and after**

Use `--dump-stg` to compare output sizes:

```bash
# With the fix
cargo run -- --dump-stg harness/test/006_str.eu 2>/dev/null | wc -c

# Without the fix (revert temporarily or use --no-dce)
cargo run -- --dump-stg --no-dce harness/test/006_str.eu 2>/dev/null | wc -c
```

**Step 2: Measure allocation counts**

Use `--statistics` to compare allocation counts and ticks:

```bash
cargo run -- --statistics harness/test/006_str.eu 2>&1 >/dev/null
```

**Step 3: Run benchmarks**

```bash
cargo bench
```

**Step 4: Document the results**

Record:
- STG output size with and without DCE improvement
- Allocation count difference
- Benchmark timing difference
- Assessment of whether the improvement is meaningful

---

### Task 8: Investigate remaining escape paths after dynamise fix

**Files:**
- No files modified (investigation only)

**Step 1: Run the block usage analysis after the Task 6 fix**

```bash
cargo run -- --debug-block-usage harness/test/006_str.eu 2>&1
cargo run -- --debug-block-usage harness/test/010_prelude.eu 2>&1
```

Examine which blocks still escape and what their escape sites are.

**Step 2: Trace each remaining escape to its source**

For each escaping block, determine the root cause:
- Is it a genuine dynamic use (block passed to merge, elements, etc.)?
- Is it another transform introducing bare `Var` references?
- Is it a catenation pattern? (Check what `str.join(",")` actually
  compiles to — it likely desugars to `Lookup` not bare `Var`)

Use `--dump-cooked` or `--dump-stg` to inspect specific expressions:

```bash
cargo run -- --dump-cooked -e 'str.join(",")' 2>/dev/null | head -30
```

**Step 3: Document findings and decide**

At this point we should have enough data to decide:
- **Option A: Sufficient** — The dynamise fallback fix (Task 6) recovers most DCE opportunities. Ship it.
- **Option B: Moderate** — Additional specific patterns need handling. File follow-up tasks with concrete root causes.
- **Option C: Fundamental** — Block duality is deeply entrenched. Would require language-level changes (module system, import qualifiers). Not worth pursuing for 0.4.0.

---

### Task 9: Write unit tests for the improved DCE

**Files:**
- Modify: `src/core/simplify/prune.rs` (add test cases)

**Step 1: Add test for dynamise fallback pattern**

Add a test to the existing `tests` module in `prune.rs`:

```rust
#[test]
pub fn test_dynamise_fallback_allows_block_dce() {
    // Simulates the dynamise pattern:
    //   ns = DefaultBlockLet { used: 42, unused: 99 }
    //   result = Lookup(Var(ns), "used", Some(Var(ns)))
    //
    // The fallback Var(ns) should NOT prevent block-level DCE
    // because it refers to the same binding as the lookup target.
    let used = free("used");
    let unused = free("unused");
    let ns = free("ns");
    let result = free("result");

    let ns_var = || acore::var(ns.clone());

    let expr = let_(
        vec![
            (
                ns.clone(),
                default_let(vec![
                    (used.clone(), num(42)),
                    (unused.clone(), num(99)),
                ]),
            ),
            (
                result.clone(),
                lookup(ns_var(), "used", Some(ns_var())),
            ),
        ],
        var(result.clone()),
    );

    let pruned = prune(&expr);

    // Verify the block body is filtered (only "used" member)
    match &*pruned.inner {
        Expr::Let(_, scope, _) => {
            let (_, Embed(ref ns_val)) = scope.unsafe_pattern.unsafe_pattern[0];
            match &*ns_val.inner {
                Expr::Let(_, inner_scope, LetType::DefaultBlockLet) => {
                    match &*inner_scope.unsafe_body.inner {
                        Expr::Block(_, block_map) => {
                            assert_eq!(block_map.len(), 1);
                            assert!(block_map.get("used").is_some());
                            assert!(block_map.get("unused").is_none());
                        }
                        other => panic!("expected Block body, got: {other:?}"),
                    }
                }
                other => panic!("expected DefaultBlockLet, got: {other:?}"),
            }
        }
        other => panic!("expected outer Let, got: {other:?}"),
    }
}
```

**Step 2: Add test that different-binding fallback still preserves all**

```rust
#[test]
pub fn test_different_binding_fallback_preserves_all() {
    // Lookup(Var(ns), "used", Some(Var(other_ns)))
    // Different fallback binding should still cause escape
    let used = free("used");
    let unused = free("unused");
    let ns = free("ns");
    let other = free("other");
    let result = free("result");

    let expr = let_(
        vec![
            (
                ns.clone(),
                default_let(vec![
                    (used.clone(), num(42)),
                    (unused.clone(), num(99)),
                ]),
            ),
            (other.clone(), num(0)),
            (
                result.clone(),
                lookup(acore::var(ns.clone()), "used", Some(acore::var(other.clone()))),
            ),
        ],
        var(result.clone()),
    );

    let pruned = prune(&expr);

    // ns should still have all members since fallback is a different binding
    // (the fallback doesn't cause escape of ns, but ns itself is accessed
    // only via static lookup so it should still be filtered)
    match &*pruned.inner {
        Expr::Let(_, scope, _) => {
            let (_, Embed(ref ns_val)) = scope.unsafe_pattern.unsafe_pattern[0];
            match &*ns_val.inner {
                Expr::Let(_, inner_scope, LetType::DefaultBlockLet) => {
                    match &*inner_scope.unsafe_body.inner {
                        Expr::Block(_, block_map) => {
                            assert_eq!(block_map.len(), 1);
                            assert!(block_map.get("used").is_some());
                        }
                        other => panic!("expected Block body, got: {other:?}"),
                    }
                }
                other => panic!("expected DefaultBlockLet, got: {other:?}"),
            }
        }
        other => panic!("expected outer Let, got: {other:?}"),
    }
}
```

**Step 3: Run the tests**

```bash
cargo test --lib prune::tests
```

**Step 4: Commit**

```bash
git add src/core/simplify/prune.rs
git commit -m "test: add unit tests for dynamise-aware block-level DCE"
```

---

### Task 10: Clean up and write summary findings

**Files:**
- No source files modified

**Step 1: Run full test suite**

```bash
cargo test
```

**Step 2: Run clippy**

```bash
cargo clippy --all-targets -- -D warnings
```

Fix any warnings.

**Step 3: Summarise findings**

Write a summary for the task ticket documenting:

1. **Quantitative findings**: How many block members are eliminated by the existing DCE, how many more are eliminated by the dynamise-fallback fix, and how many remain uneliminated due to genuine dynamic access.

2. **Root causes of remaining waste** (to be determined empirically):
   - Dynamise fallback patterns (confirmed suspect)
   - Blocks used as merge targets — genuine dynamic access
   - Blocks passed as function arguments — genuine escape
   - Any other transform-introduced bare `Var` references

3. **Recommendation**: Whether to:
   - Ship the dynamise-fallback fix as a quick win
   - Pursue further DCE improvements (catenation specialisation)
   - Defer deeper structural changes (module system) to a future version

**Step 4: Commit analysis module**

If the analysis module is useful for future investigation, keep it. Otherwise mark it as `cfg(debug_assertions)` or behind a feature flag.

```bash
cargo fmt --all
git add -A
git commit -m "chore: finalise block duality DCE investigation"
```
