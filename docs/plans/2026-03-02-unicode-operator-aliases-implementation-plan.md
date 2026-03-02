# Unicode Operator Aliases Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add missing Unicode operator aliases to the prelude: `≤` for `<=`, `≥` for `>=`, and `≠` for `!=`.

**Architecture:** The prelude (`lib/prelude.eu`) already defines Unicode aliases for the boolean operators (`∧` for `&&`, `∨` for `||`, `¬` for `!`). Each alias is a user-defined operator with metadata specifying its precedence, associativity, and export suppression. The new aliases follow exactly the same pattern, delegating to the existing ASCII operators. No Rust code changes are needed -- the lexer already accepts these characters as operator identifiers because `≤` (U+2264), `≥` (U+2265), and `≠` (U+2260) are all in the Unicode `Sm` (MathSymbol) category, which `is_symbol_category()` in both `src/syntax/lex.rs` and `src/syntax/rowan/lex.rs` recognises as valid operator characters.

**Tech Stack:** Eucalypt prelude (`.eu` file), Rust harness tests

---

## Task 1: Add Unicode Operator Aliases to Prelude

Add `≤`, `≥`, and `≠` operator definitions to `lib/prelude.eu`, following the exact pattern of the existing `∧`, `∨`, and `¬` aliases.

**Files:**
- Modify: `lib/prelude.eu`

**Step 1: Add `≠` alias after the `!=` definition**

In `lib/prelude.eu`, immediately after the `(l != r)` definition (line 368), add:

```eucalypt
` { doc: "`l ≠ r` - `true` if and only if value `l` is not equal to value `r`. Unicode alias for `!=`."
    export: :suppress
    associates: :left
    precedence: :eq }
(l ≠ r): l != r
```

**Step 2: Add `≤` and `≥` aliases after the `>=` definition**

In `lib/prelude.eu`, immediately after the `(l >= r)` definition (line 438), add:

```eucalypt
` { doc: "`l ≤ r` - `true` if `l` is less than or equal to `r`. Unicode alias for `<=`."
    export: :suppress
    associates: :left
    precedence: :cmp }
(l ≤ r): l <= r

` { doc: "`l ≥ r` - `true` if `l` is greater than or equal to `r`. Unicode alias for `>=`."
    export: :suppress
    associates: :left
    precedence: :cmp }
(l ≥ r): l >= r
```

**Step 3: Verify the build**

Run: `cargo build`
Expected: PASS (no Rust changes, but ensures prelude parses correctly on next test run)

**Step 4: Commit**

```bash
git add lib/prelude.eu
git commit -m "feat: add Unicode operator aliases ≤, ≥, ≠ to prelude"
```

---

## Task 2: Add Harness Tests for Unicode Comparison Aliases

Add tests to the existing prelude harness test (`harness/test/010_prelude.eu`) to validate the new Unicode aliases work correctly.

**Files:**
- Modify: `harness/test/010_prelude.eu`

**Step 1: Add Unicode comparison tests to the equality section**

In `harness/test/010_prelude.eu`, in the `equality` block (around line 84), add a test for `≠`:

After `t7: not(null != null)` (line 91), add:
```eucalypt
    t8: 1 ≠ 2
    t9: not(1 ≠ 1)
```

Update the `pass` line for equality to include `t8, t9`:
```eucalypt
    pass: [t1, t2, t3, t4, t5, t6, t7, t8, t9] all-true?
```

**Step 2: Add Unicode comparison tests to the arithmetic section**

In `harness/test/010_prelude.eu`, in the `arithmetic` block (around line 96), add tests for `≤` and `≥`:

After `t8: 0 zero?` (line 104), add:
```eucalypt
    t9: 1 ≤ 2
    t10: 2 ≤ 2
    t11: 2 ≥ 1
    t12: 2 ≥ 2
    t13: not(3 ≤ 2)
    t14: not(1 ≥ 2)
```

Update the `pass` line for arithmetic to include the new tests:
```eucalypt
    pass: [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14] all-true?
```

**Step 3: Run the prelude harness test**

Run: `cargo test test_harness_010`
Expected: PASS

**Step 4: Run the arithmetic harness test to ensure no regressions**

Run: `cargo test test_harness_012`
Expected: PASS

**Step 5: Commit**

```bash
git add harness/test/010_prelude.eu
git commit -m "test: add harness tests for Unicode comparison operators ≤, ≥, ≠"
```

---

## Task 3: Verify No Regressions

Run the full test suite to ensure nothing is broken.

**Files:**
- None (verification only)

**Step 1: Run all harness tests**

Run: `cargo test --test harness_test`
Expected: PASS (all tests)

**Step 2: Run clippy**

Run: `cargo clippy --all-targets -- -D warnings`
Expected: PASS (no warnings)

**Step 3: Run formatting check**

Run: `cargo fmt --all -- --check`
Expected: PASS

---

## Notes

- **No Rust code changes required.** The lexer already handles `≤`, `≥`, and `≠` as operator characters because they are Unicode `MathSymbol` (`Sm`) category characters. The `is_oper_start()` and `is_oper_continuation()` functions in both `src/syntax/lex.rs` (line 114-131) and `src/syntax/rowan/lex.rs` (line 126-131) accept all `MathSymbol` characters via the `is_symbol_category()` helper.

- **Precedence and associativity.** The `≠` alias uses `:eq` precedence (same as `=` and `!=`). The `≤` and `≥` aliases use `:cmp` precedence (same as `<`, `>`, `<=`, `>=`). All are left-associative.

- **Export suppression.** All three aliases have `export: :suppress` metadata, matching the pattern of the existing ASCII operators and the `∧`/`∨`/`¬` aliases. This prevents them from appearing in rendered output as block keys.

- **Delegation pattern.** Each alias delegates to its ASCII counterpart rather than calling the intrinsic directly. This matches the pattern used by `∧` (which calls `l && r`) and `∨` (which calls `l || r`), rather than the direct-intrinsic pattern used by the ASCII operators themselves. This keeps the aliases as thin wrappers.
