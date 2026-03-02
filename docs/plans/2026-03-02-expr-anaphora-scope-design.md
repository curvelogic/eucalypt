# Expression Anaphora Scope Boundary Fix Design

**Bead:** eu-mi8d
**Status:** Design approved, implementation plan follows

## Overview

Expression anaphora (`_0`, `_1`, `_`) fail when they appear inside a
parenthesised sub-expression that is then used as an operand of an
outer operator. The anaphora-to-lambda transformation treats paren
groups as scope boundaries, resolving the anaphora into a lambda at
the paren boundary instead of propagating outward through the full
expression tree.

## Root Cause

Parenthesised expressions desugar to nested `Soup` nodes. For
`(_0 + _1) / 2`, the core expression becomes:

```
Soup([Soup([ExprAnaphor(0), +, ExprAnaphor(1)]), /, 2])
```

In `cook_soup()`, anaphora detection uses `fill_gaps()` which only
checks **top-level** elements of the Soup for `naked_anaphora`. The
inner `Soup` is opaque to this scan — so `naked_anaphora` is empty at
the outer level. The inner Soup finds the anaphora first, wraps them
as a lambda, producing `(fn(a,b): a+b) / 2`.

The `in_expr_anaphor_scope` infection flag works correctly when at
least one explicit anaphor is at the outer level (e.g. `_0 * (_1 + 2)`),
but fails when **all** explicit anaphora are nested inside paren groups.

## Fix

Add a recursive pre-scan for explicit `ExprAnaphor` nodes in nested
`Soup` sub-expressions, run **before** `fill_gaps`. If found, the
outer Soup claims the anaphora and sets the infection flag.

```rust
fn contains_expr_anaphora(expr: &RcExpr) -> bool {
    match &*expr.inner {
        Expr::ExprAnaphor(_, _) => true,
        Expr::Soup(_, xs) => xs.iter().any(Self::contains_expr_anaphora),
        _ => false,
    }
}
```

Only `Soup` nodes (from paren groups) are transparent. ArgTuples
(commas), Lets (declarations), Lists, and Blocks remain scope
boundaries — the `_ => false` arm ensures this.

The pre-scan runs on the **pre-fill** expressions, so implicit section
anaphora inserted by `fill_gaps` (e.g. `(+ 1)`) are not detected.
Sections remain correctly contained by parentheses.

In `cook_soup`, the wrap decision becomes:

```rust
let has_deep_anaphora = !self.in_expr_anaphor_scope
    && exprs.iter().any(Self::contains_expr_anaphora);

let (filled, naked_anaphora) = self.insert_anaphora(exprs);

let wrap_lambda = !self.in_expr_anaphor_scope
    && (!naked_anaphora.is_empty() || has_deep_anaphora);
```

## Performance

Negligible overhead. The pre-scan only runs when
`in_expr_anaphor_scope` is false (outermost Soup level). It walks raw
expression tree nodes with no allocation — just enum pattern matching.
Once the outer Soup sets the infection flag, the scan is skipped for
all inner Soups during cooking.

## Edge Cases

| Expression | Behaviour |
|------------|-----------|
| `(_0 + _1) / 2` | Outer Soup claims both — `fn(a,b): (a+b) / 2` |
| `_0 * (_1 + 2)` | Works as before — `_0` at outer level triggers infection |
| `(+ 1) / 2` | Section contained by parens — `(fn(a): a+1) / 2` |
| `((_0 + _1)) / 2` | Recursive scan through nested Soups — works |
| `f((_0 + _1) / 2, x)` | ArgTuple boundary scopes correctly |
