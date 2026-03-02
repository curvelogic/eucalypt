# Block Anaphor Closure Bug Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the bug where block anaphora (`{k: •}`) with outer-scope variables in infix expressions cause type errors at runtime. For example, `{k: •}.(k + n)` where `n` is bound in an outer scope fails with "type mismatch: expected number, found list", while `_ + n` (expression anaphor) and named functions work correctly.

**Architecture:** The eucalypt pipeline flows: parse -> desugar -> cook -> verify -> simplify -> transform -> inline -> STG compile -> evaluate. The bug involves the interaction between the cooker (`src/core/cook/mod.rs`) which wraps block-anaphoric expressions in lambdas, and the STG compiler (`src/eval/stg/compiler.rs`) which translates core expressions to STG machine code using a linked-list context for de Bruijn index resolution. The core expression produced by the cooker is correct, but the STG compiler misresolves outer-scope variable references when an inner `let` binding (from the block anaphor pattern) sits inside the lambda.

**Tech Stack:** Rust, moniker (de Bruijn variable binding), LALRPOP parser

**Bug ID:** eu-dobu

---

## Root Cause Analysis

### The Pipeline for `{k: •}.(k + n)` with `n: 10`

1. **Desugar** produces (via `rebody` in static dot-lookup):
   ```
   let k = • in (k + n)
   ```
   The dot operator triggers `PendingLookup::Static` which calls `dlet.rebody(expr)` replacing the block's body with the dot's RHS.

2. **Cook** sees the `BlockAnaphor` `•` inside the let, records it, and calls `process_block_anaphora` which:
   - Calls `succ::succ(&expr)` to increment all bound-var scope offsets (accounting for the new lambda scope)
   - Wraps the result in `lam(binders, succed_expr)`
   - Produces: `λ(_b_a). let k = _b_a in ADD(k, n)` where `n` has scope offset 2 (lambda=0, let=1, outer=2)

3. **STG compile** processes this lambda+let+application:
   - `ProtoLambda` creates a context with `var_refs = [L(0)]` (the lambda parameter) and `size = 1`
   - Inside the lambda body, `ProtoLet` creates another context with `var_refs = [...]` for `k` and `size = 1`
   - When resolving `n` (scope offset 2), the lookup traverses: let-context -> lambda-context -> outer-context, bumping by each context's size
   - **The bug**: The let-context for `let k = _b_a` is a recursive let (`for_scope`), which means it has `size = 1`. This extra bump causes the outer variable `n` to resolve to the wrong STG local reference (offset by 1 too many), landing on a list pair instead of the number 10.

### Why `{x: •}.(x * x)` works

When both operands reference the same scope (the let-bound variable `x`), the let-context resolves them directly at scope 0 without needing to traverse outward. The bug only manifests when operands span different scopes.

### The Fix Strategy

**Approach: Eliminate the redundant inner let at the core level.**

The `process_block_anaphora` method currently takes the entire let expression (including its bindings) and wraps it in a lambda. This means the lambda parameter and the let binding are redundant -- they both bind the same value. By collapsing the let into the lambda (making `k` directly the lambda parameter name), we eliminate the extra scope level that confuses the STG compiler.

Before fix:
```
λ(_b_a). let k = _b_a in ADD(k, n)    -- 3 scopes: lambda, let, outer
```

After fix:
```
λ(k). ADD(k, n)                         -- 2 scopes: lambda, outer
```

This is the cleaner approach because:
1. The inner let is genuinely redundant (it just aliases the lambda parameter)
2. It simplifies the generated code
3. It avoids needing to change the STG compiler's context chain logic
4. The expression anaphor path (`process_expr_anaphora`) does not have this issue because it wraps a soup expression, not a let

---

## Task 1: Add Failing Harness Test for the Bug

**Files:**
- Modify: `harness/test/031_block_anaphora.eu`
- Modify: `tests/harness_test.rs` (no change needed -- test 031 already exists)

**Steps:**

1. Open `harness/test/031_block_anaphora.eu` and add test cases that exercise block anaphora with outer-scope variable references in infix expressions. Add them after the existing tests:

```eucalypt
# Block anaphor with outer scope variable in infix
n: 10
γ: [1, 2, 3] map({k: •}.(k + n)) //= [11, 12, 13]

# Block anaphor with outer scope variable - subtraction
δ: [5, 10, 15] map({k: •}.(k - n)) //= [-5, 0, 5]

# Block anaphor with multiple outer scope references
m: 100
ε: [1, 2, 3] map({k: •}.(k + n + m)) //= [111, 112, 113]

# Block anaphor with outer variable on left of infix
ζ: [1, 2, 3] map({k: •}.(n + k)) //= [11, 12, 13]

# Block anaphor with outer variable in multiplication
η: [2, 3, 4] map({k: •}.(k * n)) //= [20, 30, 40]
```

2. Verify the test fails:
```bash
cargo test test_harness_031
```

Expected: test fails with "type mismatch" or similar runtime error.

---

## Task 2: Understand the Core Expression Structure

**Files:**
- Read: `src/core/cook/mod.rs` (lines 127-155)
- Read: `src/core/anaphora.rs`
- Read: `src/core/expr.rs` (lines 580-637)

**Steps:**

1. Trace the current flow for `{k: •}.(k + n)`:
   - Desugaring produces: `Let(k = BlockAnaphor) in Soup(k, +, n)` (via `rebody`)
   - `cook_let` walks the let, encounters the `BlockAnaphor`, calls `cook_block_anaphor` which records it and substitutes a free var `_b_a`
   - After walking, `pending_block_anaphora` is non-empty, so `process_block_anaphora` is called
   - `process_block_anaphora` takes the cooked let `Let(k = _b_a) in ADD(k, n)` and wraps: `Lam([_b_a], succ(Let(k = _b_a) in ADD(k, n)))`

2. Identify the redundancy: after `succ` and lambda wrapping, the inner let binding `k = _b_a` just aliases the lambda parameter `_b_a`. The let is unnecessary.

---

## Task 3: Implement the Core Fix in `process_block_anaphora`

**Files:**
- Modify: `src/core/cook/mod.rs`

**Steps:**

1. Modify `process_block_anaphora` to detect when the wrapped expression is a `Let` whose bindings are all block-anaphor aliases (i.e., each binding's value is a `Var` referencing one of the pending block anaphora free vars). In the common case (single block anaphor), collapse the let by:
   - Using the let-bound variable's name as the lambda parameter name
   - Using the let's body (with the bound variable re-targeted to the lambda scope) as the lambda body
   - This avoids the redundant inner let scope entirely

2. The modified `process_block_anaphora` should:

```rust
/// Wrap a lambda around a block-anaphoric expression,
/// collapsing redundant inner lets where possible.
fn process_block_anaphora(&mut self, expr: RcExpr) -> Result<RcExpr, CoreError> {
    let binders = anaphora::to_binding_pattern(&self.pending_block_anaphora)?;
    let anaphora_vars: HashSet<String> = self
        .pending_block_anaphora
        .values()
        .filter_map(|v| v.pretty_name.clone())
        .collect();
    self.pending_block_anaphora.clear();

    // Try to collapse: if the expression is a Let where every
    // binding's value is one of our anaphor vars, we can eliminate
    // the let and bind the let-bound names directly as lambda
    // parameters.
    if let Expr::Let(_, scope, _) = &*expr.inner {
        let bindings = &scope.unsafe_pattern.unsafe_pattern;
        let body = &scope.unsafe_body;

        // Check that each binding's value is just a reference to
        // an anaphor variable
        let all_alias = bindings.iter().all(|(_, Embed(val))| {
            if let Expr::Var(_, Var::Free(fv)) = &*val.inner {
                fv.pretty_name
                    .as_ref()
                    .is_some_and(|n| anaphora_vars.contains(n))
            } else {
                false
            }
        });

        if all_alias && bindings.len() == binders.len() {
            // Use the let-bound names as lambda binders directly
            // and the let body as lambda body (with succ to
            // account for the new lambda scope, but NOT the
            // eliminated let scope)
            return Ok(core::lam(expr.smid(), binders, succ::succ(body)?));
        }
    }

    // Fallback: original behaviour
    Ok(core::lam(expr.smid(), binders, succ::succ(&expr)?))
}
```

3. Add necessary imports at the top of the file if not already present:
   - `use std::collections::HashSet;`
   - Ensure `Var`, `Embed` are accessible from the moniker imports

4. Verify the fix compiles:
```bash
cargo build 2>&1 | head -20
```

---

## Task 4: Verify the Harness Test Passes

**Steps:**

1. Run the specific failing test:
```bash
cargo test test_harness_031
```

Expected: test passes (exit code 0).

2. Run a broader set of tests to check for regressions:
```bash
cargo test --test harness_test
```

Expected: all harness tests pass.

---

## Task 5: Add Unit Test for `process_block_anaphora` Collapse

**Files:**
- Modify: `src/core/cook/mod.rs` (in the `#[cfg(test)]` module)

**Steps:**

1. Add a unit test that verifies the collapse optimisation works correctly. The test should:
   - Construct a core expression equivalent to `{k: •}.(k + n)` with an outer binding
   - Cook it
   - Verify the result is `Lam(_, body)` where body does NOT contain a `Let`

2. The test should also verify the non-collapsible case still works (e.g., a block anaphor where the let binding has a non-trivial value).

3. Run the unit tests:
```bash
cargo test --lib cook
```

---

## Task 6: Add Edge Case Harness Tests

**Files:**
- Modify: `harness/test/031_block_anaphora.eu`

**Steps:**

1. Add additional edge case tests:

```eucalypt
# Nested block anaphora with outer scope
outer: 5
θ: [1, 2, 3] map({x: •}.(x + outer)) //= [6, 7, 8]

# Block anaphor in a let binding context
ι: { v: 42 }
  [1, 2] map({k: •}.(k + v)) //= [43, 44]

# Block anaphor combined with other list operations
κ: [1, 2, 3] map({k: •}.(k + n)) filter({k: •}.(k > 11)) //= [12, 13]
```

2. Run the tests:
```bash
cargo test test_harness_031
```

---

## Task 7: Run Full Test Suite and Clippy

**Steps:**

1. Run the full test suite:
```bash
cargo test
```

2. Run clippy with all targets:
```bash
cargo clippy --all-targets -- -D warnings
```

3. Run rustfmt:
```bash
cargo fmt --all
```

4. Fix any issues found.

---

## Task 8: Verify with Manual Reproduction

**Steps:**

1. Create a temporary test file and run it:
```bash
echo 'n: 10  t: [1, 2, 3] map({k: •}.(k + n))' > /tmp/eu_test_anaphor.eu
cargo run -- /tmp/eu_test_anaphor.eu -t
```

Expected output should include `t` evaluating to `[11, 12, 13]`.

2. Also verify the expression anaphor equivalent still works:
```bash
echo 'n: 10  t: [1, 2, 3] map(_ + n)' > /tmp/eu_test_expr.eu
cargo run -- /tmp/eu_test_expr.eu -t
```

3. Clean up temporary files.

---

## Summary of Changes

| File | Change |
|------|--------|
| `src/core/cook/mod.rs` | Modify `process_block_anaphora` to collapse redundant inner lets |
| `harness/test/031_block_anaphora.eu` | Add test cases for outer-scope variables in block anaphora |

## Risk Assessment

- **Low risk**: The fix only affects block anaphora expressions, which is a well-scoped feature
- **The fallback path preserves existing behaviour**: If the let cannot be collapsed (non-trivial bindings), the original wrapping logic applies
- **Comprehensive existing tests**: The 031 test file already covers block anaphora basics, and the full harness suite covers the broader pipeline
- **Alternative approach if needed**: If the core-level fix proves insufficient for complex cases, an STG compiler fix in `Context::lookup` or `LetBinder::for_scope` could be investigated as a secondary option
