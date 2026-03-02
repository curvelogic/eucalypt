# Expression Anaphora Scope Boundary Fix Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix expression anaphora (`_0`, `_1`) failing when inside parenthesised sub-expressions used as operands of outer operators.

**Architecture:** The cooker (`src/core/cook/mod.rs`) resolves operator precedence and handles anaphora-to-lambda transformation. The bug is in `cook_soup()` where anaphora detection only checks top-level Soup elements. The fix adds a recursive pre-scan for explicit `ExprAnaphor` nodes in nested `Soup` sub-expressions before `fill_gaps`.

**Tech Stack:** Rust, moniker (variable binding)

**Bug ID:** eu-mi8d

---

## Task 1: Add Failing Harness Test

**Files:**
- Create: `harness/test/086_expr_anaphora_parens.eu`
- Modify: `tests/harness_test.rs` (add test entry)

**Step 1: Write the test file**

Create `harness/test/086_expr_anaphora_parens.eu`:

```eucalypt
# Expression anaphora inside parenthesised sub-expressions
# Bug eu-mi8d: parens truncate anaphora infection

# Basic case: binary op after paren group
α: zip-with((_0 + _1) / 2, [2, 4, 6], [4, 8, 12]) //= [3, 6, 9]

# Multiplication after paren group
β: zip-with((_0 + _1) * 2, [1, 2, 3], [4, 5, 6]) //= [10, 14, 18]

# Subtraction after paren group
γ: zip-with((_0 + _1) - 1, [1, 2, 3], [4, 5, 6]) //= [4, 6, 8]

# Single anaphor in parens with outer operator
δ: [10, 20, 30] map((_0) + 1) //= [11, 21, 31]

# Deeply nested parens
ε: zip-with(((_0 + _1)) / 2, [2, 4], [4, 8]) //= [3, 6]

# Outer-scope variable with paren anaphor
c: 2
ζ: zip-with((_0 + _1) / c, [2, 4, 6], [4, 8, 12]) //= [3, 6, 9]

# Mixed: one anaphor at outer level, one in parens (already works, regression check)
η: zip-with(_0 * (_1 + 2), [1, 2, 3], [4, 5, 6]) //= [6, 14, 24]
```

**Step 2: Add the test entry to the harness**

In `tests/harness_test.rs`, find the test list and add:

```rust
harness_test!(test_harness_086, "086_expr_anaphora_parens");
```

Add this near the other numbered test entries, following the existing pattern.

**Step 3: Run the test to verify it fails**

Run: `cargo test test_harness_086 -- --nocapture`

Expected: FAIL — the paren-scoped anaphora tests should produce runtime type errors.

**Step 4: Commit the failing test**

```bash
git add harness/test/086_expr_anaphora_parens.eu tests/harness_test.rs
git commit -m "test: add failing test for expression anaphora paren scope (eu-mi8d)"
```

---

## Task 2: Add `contains_expr_anaphora` Helper

**Files:**
- Modify: `src/core/cook/mod.rs`

**Step 1: Add the helper method to `Cooker`**

Add this method to the `impl Cooker` block, before `cook_soup`:

```rust
/// Check whether an expression tree contains explicit ExprAnaphor
/// nodes, recursing only through Soup nodes (from paren groups).
/// Other constructs (ArgTuple, Let, List, Block) remain scope
/// boundaries and are not traversed.
fn contains_expr_anaphora(expr: &RcExpr) -> bool {
    match &*expr.inner {
        Expr::ExprAnaphor(_, _) => true,
        Expr::Soup(_, xs) => xs.iter().any(Self::contains_expr_anaphora),
        _ => false,
    }
}
```

**Step 2: Verify it compiles**

Run: `cargo build 2>&1 | head -20`

Expected: compiles without errors.

**Step 3: Commit**

```bash
git add src/core/cook/mod.rs
git commit -m "feat: add contains_expr_anaphora pre-scan helper"
```

---

## Task 3: Modify `cook_soup` to Use the Pre-Scan

**Files:**
- Modify: `src/core/cook/mod.rs`

**Step 1: Update `cook_soup`**

Replace the current `cook_soup` method (lines 97-125 of `src/core/cook/mod.rs`) with:

```rust
/// Resolve precedence and handle expression anaphora
fn cook_soup(&mut self, exprs: &[RcExpr]) -> Result<RcExpr, CoreError> {
    // Pre-scan for explicit anaphora in nested sub-expressions
    // BEFORE fill_gaps, so implicit section anaphora are not detected.
    let has_deep_anaphora = !self.in_expr_anaphor_scope
        && exprs.iter().any(Self::contains_expr_anaphora);

    let (filled, naked_anaphora) = self.insert_anaphora(exprs);

    let wrap_lambda = !self.in_expr_anaphor_scope
        && (!naked_anaphora.is_empty() || has_deep_anaphora);

    if wrap_lambda {
        self.in_expr_anaphor_scope = true;
    }

    let atoms: Vec<_> = {
        let old_in_expr_anaphor_scope = self.in_expr_anaphor_scope;
        let subcooked = filled
            .iter()
            .map(|e| self.cook_(e.clone()))
            .collect::<Result<Vec<RcExpr>, CoreError>>();
        self.in_expr_anaphor_scope = old_in_expr_anaphor_scope;
        subcooked?
    };

    let cooked = shunt::shunt(atoms)?;

    if wrap_lambda {
        self.in_expr_anaphor_scope = false;

        self.process_expr_anaphora(cooked)
    } else {
        Ok(cooked)
    }
}
```

The only change is the addition of `has_deep_anaphora` and its inclusion
in the `wrap_lambda` condition. All other logic is unchanged.

**Step 2: Run the failing test**

Run: `cargo test test_harness_086 -- --nocapture`

Expected: PASS — all paren-scoped anaphora tests should now work.

**Step 3: Commit**

```bash
git add src/core/cook/mod.rs
git commit -m "fix: propagate expression anaphora through paren groups (eu-mi8d)"
```

---

## Task 4: Add Unit Test for the Pre-Scan

**Files:**
- Modify: `src/core/cook/mod.rs` (in `#[cfg(test)]` module)

**Step 1: Add unit test**

Add to the existing `tests` module at the bottom of `src/core/cook/mod.rs`:

```rust
#[test]
pub fn test_anaphora_through_parens() {
    // Simulates (_0 + _1) / 2 where parens create a nested Soup
    let l50 = core::infixl(Smid::fake(1), 50, bif("ADD"));
    let l60 = core::infixl(Smid::fake(2), 60, bif("DIV"));
    let ana0 = free("_e_n0");
    let ana1 = free("_e_n1");

    // Inner soup: _0 + _1 (from parens)
    let inner = soup(vec![
        core::expr_anaphor(Smid::fake(3), Some(0)),
        l50.clone(),
        core::expr_anaphor(Smid::fake(4), Some(1)),
    ]);

    // Outer soup: (inner) / 2
    let outer = soup(vec![inner, l60.clone(), num(2)]);

    // Should produce: lam([_0, _1], DIV(ADD(_0, _1), 2))
    let result = cook(outer).unwrap();
    assert_term_eq!(
        result,
        lam(
            vec![ana0.clone(), ana1.clone()],
            app(
                bif("DIV"),
                vec![app(bif("ADD"), vec![var(ana0), var(ana1)]), num(2)]
            )
        )
    );
}
```

**Step 2: Run the unit test**

Run: `cargo test --lib cook::tests::test_anaphora_through_parens`

Expected: PASS.

**Step 3: Commit**

```bash
git add src/core/cook/mod.rs
git commit -m "test: add unit test for anaphora propagation through parens"
```

---

## Task 5: Verify Sections Still Contained by Parens

**Files:**
- Modify: `src/core/cook/mod.rs` (in `#[cfg(test)]` module)

**Step 1: Add regression test for sections**

```rust
#[test]
pub fn test_section_contained_by_parens() {
    // Simulates (+ 1) / 2 — section should stay inside parens
    let l50 = core::infixl(Smid::fake(1), 50, bif("ADD"));
    let l60 = core::infixl(Smid::fake(2), 60, bif("DIV"));
    let ana0 = free("_e_i_l0");

    // Inner soup: + 1 (section — fill_gaps adds implicit anaphor)
    let inner = soup(vec![l50.clone(), num(1)]);

    // Outer soup: (inner) / 2
    let outer = soup(vec![inner, l60.clone(), num(2)]);

    // Should produce: DIV(lam([_0], ADD(_0, 1)), 2)
    // The section lambda stays inside — NOT lam([_0], DIV(ADD(_0, 1), 2))
    let result = cook(outer).unwrap();
    assert_term_eq!(
        result,
        app(
            bif("DIV"),
            vec![
                lam(vec![ana0.clone()], app(bif("ADD"), vec![var(ana0), num(1)])),
                num(2)
            ]
        )
    );
}
```

**Step 2: Run the test**

Run: `cargo test --lib cook::tests::test_section_contained_by_parens`

Expected: PASS — sections are not affected by the fix.

**Step 3: Commit**

```bash
git add src/core/cook/mod.rs
git commit -m "test: verify sections remain contained by parens after fix"
```

---

## Task 6: Run Full Test Suite and Clippy

**Step 1: Full test suite**

Run: `cargo test`

Expected: all tests pass.

**Step 2: Clippy**

Run: `cargo clippy --all-targets -- -D warnings`

Expected: no warnings.

**Step 3: Format**

Run: `cargo fmt --all`

**Step 4: Fix any issues, then commit if needed**

```bash
git add -A
git commit -m "chore: clippy and format fixes"
```

---

## Task 7: Close Bead

**Step 1: Close the bead**

```bash
bd close eu-mi8d
bd sync
```

---

## Summary of Changes

| File | Change |
|------|--------|
| `src/core/cook/mod.rs` | Add `contains_expr_anaphora` helper; modify `cook_soup` to pre-scan nested Soups for explicit anaphora |
| `harness/test/086_expr_anaphora_parens.eu` | New harness test for paren-scoped anaphora |
| `tests/harness_test.rs` | Add test entry for 086 |
