# Idiot Brackets: Split at Catenation After Cooking

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **MANDATORY**: Before writing ANY eucalypt (.eu) code, read: `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`, `docs/appendices/cheat-sheet.md`

**Goal:** Fix idiot bracket content collection so that items are gathered into a list by splitting at catenation boundaries after the cook phase has resolved operator precedence and call syntax.

**Architecture:** The desugar phase leaves bracket content as a normal `Expr::Soup`. The cook phase resolves it normally (grouping `f(x)` calls, resolving operators, inserting catenation). A post-cook transform then finds bracket function applications and splits the catenated argument at top-level `cat` boundaries into an `Expr::List`.

**Tech Stack:** Rust (desugarer `src/core/desugar/rowan_ast.rs`, cook `src/core/cook/`, expr `src/core/expr.rs`)

---

## Why the previous approach failed

Splitting `Expr::Soup` items into `Expr::List` at desugar time doesn't work because soup items at that stage still contain raw operators (`=`, `+`) and call pseudo-operators (`[e-pseudocall]`) that require the cook phase to resolve. The cook phase resolves `Expr::Soup` via `cook_soup` (anaphora → cook sub-expressions → shunt/precedence), but `Expr::List` elements go through generic `try_walk_safe` which doesn't do the shunt step.

## The correct approach

1. **Desugar**: `BracketExpr` desugars its inner soup normally (no split). Apply the bracket function to the result. Mark the application so the post-cook transform can find it.

2. **Cook**: The inner soup is cooked normally — call syntax is grouped, operators resolved, catenation inserted between adjacent value-like items. `⟦ f(x) g(y) ⟧` becomes `⟦⟧(cat(f(x), g(y)))`.

3. **Post-cook transform**: Walk the cooked expression. Find bracket applications (marked). Split the argument at top-level catenation boundaries. Replace with `Expr::List`.

### The split algorithm

The cook phase's shunter produces left-nested catenation trees for adjacent items:

```
⟦ a b c ⟧     → cat(cat(a, b), c)
⟦ a b (c f) ⟧ → cat(cat(a, b), cat(c, f))
⟦ f(x) g(y) ⟧ → cat(f(x), g(y))
⟦ 42 ⟧         → 42  (no catenation)
```

To split: if the expression is `App(App(cat, left), right)`, collect `right` as an item, recurse into `left`. When `left` is not a `cat` application, collect it as the first item. This produces items in order because we collect right-to-left then reverse (or prepend).

Anything with tighter precedence than catenation (e.g. `f(x)` calls, `1 + 2` arithmetic, `(c f)` parenthesised pipelines) appears as a leaf in the catenation tree and is never split.

### Marking bracket applications

The desugarer needs to mark `⟦⟧(inner)` so the post-cook transform can distinguish bracket applications from normal function calls. Options:

**Option A: Metadata marker.** Attach metadata to the `App` node: `App(bracket_fn, [inner]) // { :bracket-app: true }`. The post-cook transform checks for this metadata.

**Option B: Wrap in a sentinel expression.** Use an `Expr::Meta` wrapper around the argument that signals "split this at catenation". The post-cook transform unwraps and splits.

**Option C: New expression variant.** Add `Expr::BracketApp(Smid, RcExpr, RcExpr)` — like `App` but the argument gets split at catenation. This is the most explicit but touches the most code.

**Recommended: Option A** — minimal code change, uses existing metadata machinery.

---

## Task 1: Write comprehensive test file

**Files:**
- Modify: `tests/harness/097_idiot_brackets.eu`

The test file must exercise:

```eucalypt
"Idiot brackets: content items are collected into a list."

` "Identity brackets: return content as a list"
⟦ x ⟧: x

` "Angle brackets: extract single item"
⟨ x ⟩: x head

` "Lookup brackets: look up symbols in a block"
‹ xs ›: {
  lookups: xs map(lookup)
  f(x): lookups map(x _)
}.f

` "Coalescing brackets: first non-null"
⌊ xs ⌋: coalesce(xs)

zapp(fs, as): ((fs nil?) ∨ (as nil?)) then([], cons((fs head)(as head), zapp(fs tail, as tail)))

` "Idiom brackets: apply function across lists"
⌈ [f: lists] ⌉: foldl(zapp, repeat(f), lists)

` { target: :test }
test: {
  # Simple items become list elements
  id-nums: ⟦ 1 2 3 ⟧ //= [1, 2, 3]
  id-single: ⟦ 42 ⟧ //= [42]
  id-syms: ⟦ :a :b :c ⟧ //= [:a, :b, :c]

  # Parenthesised expressions are single items (catenation preserved inside)
  id-parens: (⟦ (1 + 2) (3 * 4) ⟧ head) = 3
  id-parens2: (⟦ (1 + 2) (3 * 4) ⟧ second) = 12

  # Sub-expressions preserve internal catenation
  id-pipeline: (⟦ ([1, 2, 3] count) ⟧ head) = 3

  # Function calls are single items (call binds tighter than catenation)
  id-calls: (⟦ inc(1) dec(3) ⟧ head) = 2
  id-calls2: (⟦ inc(1) dec(3) ⟧ second) = 2

  # List literals are single items
  id-lists: (⟦ [1, 2] [3, 4] ⟧ head) = [1, 2]

  # Angle brackets
  angle: ⟨ :hello ⟩ //= :hello

  # Lookup brackets
  lookup-test: { a: 1 b: 2 c: 3 d: 4 e: 5 } ‹ :a :b :e › //= [1, 2, 5]

  # Coalescing
  coal-mid: ⌊ null null 3 null ⌋ //= 3
  coal-all-null: ⌊ null null null ⌋ //= null

  # Idiom brackets with destructuring parameter
  g(a, b, c): a + b + c
  idiom: ⌈ g [0, 1, 2] [10, 20, 30] [100, 200, 300] ⌉ //= [110, 221, 332]

  RESULT: [ id-nums, id-single, id-syms
          , id-parens, id-parens2, id-pipeline
          , id-calls, id-calls2, id-lists
          , angle
          , lookup-test
          , coal-mid, coal-all-null
          , idiom
          ] all-true? then(:PASS, :FAIL)
}
```

Key test cases and what they validate:

| Test | Input | Expected | Validates |
|------|-------|----------|-----------|
| `id-nums` | `⟦ 1 2 3 ⟧` | `[1, 2, 3]` | Basic item splitting |
| `id-single` | `⟦ 42 ⟧` | `[42]` | Single item → singleton list |
| `id-syms` | `⟦ :a :b :c ⟧` | `[:a, :b, :c]` | Symbols as items |
| `id-parens` | `⟦ (1+2) (3*4) ⟧` | `[3, 12]` | Parens group sub-expressions |
| `id-pipeline` | `⟦ ([1,2,3] count) ⟧` | `[3]` | Pipeline in parens = one item |
| `id-calls` | `⟦ inc(1) dec(3) ⟧` | `[2, 2]` | Call syntax grouped before split |
| `id-lists` | `⟦ [1,2] [3,4] ⟧` | `[[1,2],[3,4]]` | List literals are single items |
| `idiom` | `⌈ g [0,1,2] ... ⌉` | `[110,221,332]` | Destructuring param + real use |

- [ ] **Step 1: Write the test file**
- [ ] **Step 2: Confirm it fails** (`cargo test test_harness_097`)
- [ ] **Step 3: Commit**

---

## Task 2: Mark bracket applications in desugaring

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs:1359-1389`

Change the `Element::BracketExpr` arm to mark the application with metadata so the post-cook transform can identify it.

The desugaring should:
1. Desugar the inner soup normally: `soup.desugar(desugarer)?` (produces `Expr::Soup`)
2. Apply the bracket function: `App(bracket_fn, [inner])`
3. Attach a metadata marker: `Meta(smid, app, {bracket-app: true})`

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

    let inner = if let Some(soup) = bracket.soup() {
        soup.desugar(desugarer)?
    } else {
        return Err(CoreError::InvalidEmbedding(
            "empty bracket expression".to_string(),
            smid,
        ));
    };

    let bracket_fn_name = RcExpr::from(Expr::Name(smid, pair_name));
    let bracket_fn = desugarer.varify(bracket_fn_name);
    let arg = desugarer.varify(inner);
    let app = RcExpr::from(Expr::App(smid, bracket_fn, vec![arg]));

    // Mark as bracket application for post-cook catenation splitting
    let marker = core::block(smid, vec![
        ("bracket-app".to_string(), RcExpr::from(Expr::Sym(smid, "true".to_string())))
    ]);
    Ok(RcExpr::from(Expr::Meta(smid, app, marker)))
}
```

**IMPORTANT**: Check how `Expr::Meta` is constructed. Look at existing `Meta` usage in the desugarer for the correct pattern. The marker block must not interfere with evaluation — verify that `Meta` is transparent to the evaluator.

- [ ] **Step 1: Implement the marking**
- [ ] **Step 2: Build** (`cargo build`)
- [ ] **Step 3: Verify existing lens tests still pass** (`cargo test test_harness_097` will still fail — that's expected)
- [ ] **Step 4: Commit**

---

## Task 3: Post-cook transform to split catenation

**Files:**
- Modify: `src/core/cook/mod.rs` or add a new transform in `src/core/transform/`
- Modify: `src/driver/source.rs` or `src/driver/prepare.rs` to apply the transform

Add a transform that runs after cooking. It walks the expression tree and when it finds a `Meta({bracket-app: true}, App(fn, [arg]))`:

1. Split `arg` at top-level catenation into a list of items
2. Replace `arg` with `Expr::List(items)`
3. Remove the `Meta` marker (or leave it — Meta is transparent)

The split function:

```rust
/// Split a cooked expression at top-level catenation boundaries.
///
/// The cook phase produces left-nested catenation: cat(cat(a, b), c).
/// This function collects [a, b, c].
///
/// Anything with tighter binding than catenation (function calls,
/// parenthesised expressions, arithmetic) appears as a leaf and is
/// not split.
fn split_at_catenation(expr: &RcExpr) -> Vec<RcExpr> {
    // Detect cat application: App(App(Var(cat_ref), left), right)
    // where cat_ref is the catenation operator global.
    //
    // Check: how is catenation represented after cooking? It's
    // App(App(cat_operator, left), right) where cat_operator is
    // an Operator node with name "⌢" or similar. Check the actual
    // representation by examining a cooked expression.
    //
    // Walk left-nested cat tree, collecting right children.
    let mut items = Vec::new();
    let mut current = expr.clone();
    while is_cat_application(&current) {
        let (left, right) = extract_cat_args(&current);
        items.push(right);
        current = left;
    }
    items.push(current);
    items.reverse();
    items
}
```

**IMPORTANT**: Before implementing, dump a cooked expression to see the exact `cat` representation. Run:
```bash
cat > /tmp/cat_check.eu << 'EOF'
t: 1 2 3
EOF
eu dump cooked --debug-format -B /tmp/cat_check.eu
```
This shows how `1 2 3` (catenation of three values) is represented after cooking. The `cat` operator might be an `Expr::Operator`, an `Expr::Var` referencing a global, or an intrinsic. The detection logic depends on this.

**Where to apply the transform**: After `cook()` in the prepare pipeline. Check `src/driver/source.rs` or `src/driver/prepare.rs` for where `cook()` is called, and apply the bracket-split transform immediately after.

- [ ] **Step 1: Dump cooked cat representation to determine detection logic**
- [ ] **Step 2: Implement `split_at_catenation`**
- [ ] **Step 3: Implement the tree walker that finds marked bracket apps and splits**
- [ ] **Step 4: Wire into the pipeline after cook**
- [ ] **Step 5: Build**
- [ ] **Step 6: Run test** (`cargo test test_harness_097`)
- [ ] **Step 7: Run full harness** (monadic brackets must still work)
- [ ] **Step 8: Clippy**
- [ ] **Step 9: Commit**

---

## Task 4: Verify edge cases and existing functionality

- [ ] **Step 1: Verify monadic brackets still work**
```bash
cargo test test_harness_096 test_harness_129
```

- [ ] **Step 2: Verify lens path syntax works**
Test in a file:
```eucalypt
‹xs›: { to-lens(x): if(x symbol?, at(x), if(x number?, ix(x), x)) }.(xs map(to-lens) foldr(∘, identity))
# Paths with mixed keys, indices, and lens functions
data: { items: [{id: 1, v: "a"}, {id: 2, v: "b"}] }
t: data view(‹:items item(_.id = 1) :v›)
```

- [ ] **Step 3: Full harness**
```bash
cargo test --test harness_test
```

- [ ] **Step 4: Final commit**

---

## What not to change

- **Parser**: No changes. `BracketExpr` and `BracketBlock` already correctly distinguished.
- **Cook phase core**: `cook_soup`, `fill_gaps`, shunting all unchanged. Catenation is still inserted normally.
- **Monadic brackets**: `BracketBlock` is a completely separate code path.
- **The `Expr` enum**: No new variants needed.
