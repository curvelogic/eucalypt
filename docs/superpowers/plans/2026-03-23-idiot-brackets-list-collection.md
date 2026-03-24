# Idiot Brackets: Collect Soup Items as List

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **MANDATORY**: Before writing ANY eucalypt (.eu) code, read: `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`, `docs/appendices/cheat-sheet.md`

**Goal:** Fix idiot bracket usage so that bracket content items are collected into a list, not catenated. `⟦ 1 2 3 ⟧` should pass `[1, 2, 3]` to the bracket function. Support destructuring patterns in bracket parameter definitions.

**Architecture:** Three changes: (1) `Element::BracketExpr` desugaring converts `Expr::Soup` items to `Expr::List`. (2) `classify_bracket_direct` accepts list/block patterns as bracket parameters, not just simple identifiers. (3) `BracketPair` desugaring uses `desugar_declaration_body_with_patterns` for destructuring support.

**Tech Stack:** Rust (parser AST in `src/syntax/rowan/ast.rs`, desugarer in `src/core/desugar/rowan_ast.rs`)

---

## Three changes required

### Change 1: BracketExpr desugaring — collect items as list

**Where:** `src/core/desugar/rowan_ast.rs:1359-1388`, the `Element::BracketExpr` arm in `impl Desugarable for Element`.

**Current:** `soup.desugar(desugarer)` produces a single expression. The cook phase later inserts catenation between adjacent values. So `⟦ 1 2 3 ⟧` becomes `⟦⟧(1 cat 2 cat 3)`.

**Fix:** After desugaring, a multi-item soup returns `Expr::Soup(smid, items)` where each item is a self-contained expression (call syntax already grouped, sub-expressions within `[...]` and `(...)` already resolved). Convert `Expr::Soup(items)` → `Expr::List(items)`. Single expressions → `Expr::List([item])`.

Both `Expr::Soup` and `core::list(smid, items)` exist in `src/core/expr.rs`.

No parser changes, no cook phase changes needed for this part. Monadic brackets (`BracketBlock`) use a completely separate code path and are unaffected.

### Change 2: Accept destructuring patterns in bracket definitions

**Where:** `src/syntax/rowan/ast.rs:686-702`, function `classify_bracket_direct`. Also `classify_bracket_paren` at lines ~730-750.

**Current:** Line 692 requires the single element inside the bracket to be a `NormalIdentifier`. A `List` element (e.g. `[f: lists]`) falls through to `MalformedHead`.

**Fix:** Change `DeclarationKind::BracketPair` to store the bracket's inner `Soup` instead of a `NormalIdentifier`. This lets the desugarer handle any pattern (simple name, list destructuring, block destructuring) using the existing `parse_param_pattern` machinery.

The variant changes from:
```rust
BracketPair(Option<ParenExpr>, BracketExpr, NormalIdentifier)
```
to:
```rust
BracketPair(Option<ParenExpr>, BracketExpr, Soup)
```

Classification logic: if the inner element is NOT a `Block` (which signals monadic mode), it's an idiot bracket. Pass the whole `Soup` through.

### Change 3: BracketPair desugaring with patterns

**Where:** `src/core/desugar/rowan_ast.rs:1661-1680`, the `DeclarationKind::BracketPair` arm.

**Current:** `param.text()` extracts a simple name from the `NormalIdentifier`. Calls `desugar_declaration_body` with a single string arg.

**Fix:** Parse the `Soup` as a `ParamPattern` via `parse_param_pattern`. Then call `desugar_declaration_body_with_patterns` (which already handles simple names, list destructuring, and block destructuring).

There are four sites that handle `BracketPair` (lines 1661, 2264, 2511, 2651). The primary desugaring is at 1661. Lines 2264/2511/2651 just extract the pair name — they access `bracket_expr.bracket_pair_name()` and don't use the param, so they need no change.

### Test file issues

The test file `097_idiot_brackets.eu` has additional issues beyond the bracket semantics:

1. `not-nil?` doesn't exist in the prelude — use `_ != null` or similar
2. `(as head)(fs head)` in `zapp` — catenation precedence makes this try to call a number. The intent is to apply the function from `fs` to the value from `as`. Needs rewriting as `(fs head)(as head)`.
3. `foldl(zapp, [f] map(repeat), lists)` — `[f] map(repeat)` wraps `f` in a list then maps `repeat` over it, giving `[repeat(f)]` = `[[f, f, f, ...]]`. This needs to be `[f] map(repeat) head` or just `repeat(f)`.

---

## Task 1: Update test file

**Files:**
- Modify: `tests/harness/097_idiot_brackets.eu`

- [ ] **Step 1: Rewrite test file**

Fix the test file with correct eucalypt idioms. The test file must exercise:
- Simple identity brackets: `⟦ 1 2 3 ⟧` = `[1, 2, 3]`
- Single item: `⟦ 42 ⟧` = `[42]`
- Symbols: `⟦ :a :b :c ⟧` = `[:a, :b, :c]`
- Parenthesised expressions as items: `⟦ (1 + 2) (3 * 4) ⟧` = `[3, 12]`
- Sub-expression catenation preserved: `⟦ [1, 2, 3] ⟧` head is `[1, 2, 3]`
- Angle brackets extracting single item: `⟨ :hello ⟩` = `:hello`
- Lookup brackets: `block ‹ :a :b :e ›` = `[1, 2, 5]`
- Coalescing: `⌊ null null 3 null ⌋` = `3`
- Idiom brackets with destructuring param: `⌈ [f: lists] ⌉: ...`

- [ ] **Step 2: Confirm test fails**

```bash
cargo test test_harness_097 2>&1 | tail -5
```

- [ ] **Step 3: Commit**

```bash
git add tests/harness/097_idiot_brackets.eu
git commit -m "test: rewrite idiot bracket tests for list-collection semantics"
```

---

## Task 2: Change BracketExpr desugaring to collect items as list

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs:1359-1388`

- [ ] **Step 1: Replace the `Element::BracketExpr` arm**

```rust
Element::BracketExpr(bracket) => {
    let span = text_range_to_span(bracket.syntax().text_range());
    let smid = desugarer.new_smid(span);

    let pair_name = bracket.bracket_pair_name().ok_or_else(|| {
        CoreError::InvalidEmbedding(
            "bracket expression missing bracket characters".to_string(),
            smid,
        )
    })?;

    // Idiot brackets: collect soup items as a list.
    //
    // Desugar the inner soup (groups call syntax like f(x) into
    // applications).  Convert the Soup items to a List — each
    // top-level soup element becomes one list item.  Catenation
    // within sub-expressions ([...], (...)) is preserved.
    let inner = if let Some(soup) = bracket.soup() {
        let desugared = soup.desugar(desugarer)?;
        let items = match &*desugared.inner {
            Expr::Soup(_, ref elems) => elems.clone(),
            _ => vec![desugared.clone()],
        };
        let varified_items: Vec<RcExpr> = items
            .into_iter()
            .map(|item| desugarer.varify(item))
            .collect();
        core::list(smid, varified_items)
    } else {
        return Err(CoreError::InvalidEmbedding(
            "empty bracket expression".to_string(),
            smid,
        ));
    };

    let bracket_fn_name = RcExpr::from(Expr::Name(smid, pair_name));
    let bracket_fn = desugarer.varify(bracket_fn_name);
    Ok(RcExpr::from(Expr::App(smid, bracket_fn, vec![inner])))
}
```

- [ ] **Step 2: Build and verify basic cases work**

```bash
cargo build 2>&1 | tail -3
```

Test a basic case in a file:
```bash
cat > /tmp/basic_bracket.eu << 'EOF'
⟦ x ⟧: x
t1: ⟦ 42 ⟧
t2: ⟦ 1 2 3 ⟧
EOF
eu /tmp/basic_bracket.eu
```

Expected: `t1: [42]`, `t2: [1, 2, 3]`

- [ ] **Step 3: Commit**

```bash
git add src/core/desugar/rowan_ast.rs
git commit -m "fix: idiot brackets collect soup items as list, not catenation"
```

---

## Task 3: Support destructuring patterns in bracket definitions

**Files:**
- Modify: `src/syntax/rowan/ast.rs` — `DeclarationKind::BracketPair`, `classify_bracket_direct`, `classify_bracket_paren`
- Modify: `src/core/desugar/rowan_ast.rs:1661-1680` — `BracketPair` desugaring

- [ ] **Step 1: Change `DeclarationKind::BracketPair` to store `Soup`**

In `src/syntax/rowan/ast.rs`, change:
```rust
BracketPair(Option<ParenExpr>, BracketExpr, NormalIdentifier),
```
to:
```rust
BracketPair(Option<ParenExpr>, BracketExpr, Soup),
```

- [ ] **Step 2: Update `classify_bracket_direct`**

Change the classification at line 691 from requiring `NormalIdentifier` to accepting any non-Block element:

```rust
fn classify_bracket_direct(bracket: BracketExpr, head_range: TextRange) -> DeclarationKind {
    let soup = bracket.soup();
    let inner_elements: Vec<_> = soup
        .as_ref()
        .map(|s| s.elements().collect())
        .unwrap_or_default();

    if inner_elements.len() == 1 {
        if let Element::Block(_) = &inner_elements[0] {
            // Block-mode bracket pair definition: ⟦{}⟧: ...
            DeclarationKind::BracketBlockDef(None, bracket)
        } else if let Some(soup) = soup {
            // Idiot bracket with parameter (simple name or destructuring pattern)
            DeclarationKind::BracketPair(None, bracket, soup)
        } else {
            DeclarationKind::MalformedHead(vec![ParseError::MalformedDeclarationHead {
                range: head_range,
            }])
        }
    } else if inner_elements.is_empty() {
        // ... existing empty case
    } else {
        // ... existing too-many-elements case
    }
}
```

**IMPORTANT:** Check that `bracket.soup()` returns an owned `Soup` or can be cloned. If it returns `Option<Soup>`, store it. The `Soup` must outlive the classification.

- [ ] **Step 3: Update `classify_bracket_paren` similarly**

The paren path at lines ~730-750 has the same `NormalIdentifier` check inside a `BracketExpr`. Apply the same change.

- [ ] **Step 4: Fix all compilation errors from the type change**

The `BracketPair` variant is used at lines 1661, 2264, 2511, 2651 in `rowan_ast.rs`. Lines 2264/2511/2651 don't use the param — they destructure as `BracketPair(_, bracket_expr, _)` and only access the bracket name. These just need the pattern updated.

Line 1661 is the primary desugaring:

```rust
rowan_ast::DeclarationKind::BracketPair(_, bracket_expr, param_soup) => {
    let pair_name = bracket_expr.bracket_pair_name().ok_or_else(|| {
        CoreError::InvalidEmbedding(
            "bracket pair declaration has no bracket pair name".to_string(),
            desugarer.new_smid(span),
        )
    })?;

    // Parse the bracket parameter as a pattern (supports simple names,
    // list destructuring [f: rest], block destructuring {x y})
    let pattern = parse_param_pattern(&param_soup)
        .ok_or_else(|| CoreError::InvalidEmbedding(
            "invalid bracket parameter pattern".to_string(),
            desugarer.new_smid(span),
        ))?;

    let (body, _, lambda_param_names, lambda_param_vars) =
        desugar_declaration_body_with_patterns(decl, desugarer, &[pattern], span)?;

    Ok(RowanDeclarationComponents {
        span,
        metadata,
        name: pair_name,
        args: lambda_param_names,
        body,
        arg_vars: lambda_param_vars,
        is_operator: false,
        fixity: None,
    })
}
```

**IMPORTANT:** Check the return type of `desugar_declaration_body_with_patterns`. It returns a `PatternBodyResult` struct — verify its fields match what `RowanDeclarationComponents` needs.

- [ ] **Step 5: Build**

```bash
cargo build 2>&1 | tail -5
```

- [ ] **Step 6: Commit**

```bash
git add src/syntax/rowan/ast.rs src/core/desugar/rowan_ast.rs
git commit -m "feat: support destructuring patterns in idiot bracket definitions"
```

---

## Task 4: Final verification

- [ ] **Step 1: Run the idiot bracket test**

```bash
cargo test test_harness_097 2>&1 | tail -5
```

- [ ] **Step 2: Run monadic bracket tests (must still work)**

```bash
cargo test test_harness_096 test_harness_129 2>&1 | tail -5
```

- [ ] **Step 3: Run full harness**

```bash
cargo test --test harness_test 2>&1 | tail -5
```

- [ ] **Step 4: Clippy**

```bash
cargo clippy --all-targets -- -D warnings 2>&1 | tail -5
```

- [ ] **Step 5: Final commit if cleanup needed**

---

## What not to change

- **Parser**: No changes to how bracket content is parsed. `BracketExpr` (soup) and `BracketBlock` (declarations) are already correct.
- **Cook phase**: No changes to `fill_gaps` or catenation insertion.
- **Monadic brackets**: `BracketBlock` desugaring is a completely separate code path.
