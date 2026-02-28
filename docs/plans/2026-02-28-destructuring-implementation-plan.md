# Destructuring and Named Arguments Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add block and list destructuring in function parameters,
cons operator, juxtaposed call syntax, and call-site/callee fusion as
described in `docs/plans/2026-02-27-destructuring-named-args-design.md`.

**Architecture:** Destructuring patterns are parsed in function
parameter positions and desugared to single-parameter lambdas with
marker-tagged `Let` bindings that expand the pattern via lookups and
intrinsics. A new core-to-core pass after cook handles fusion (eliding
block/list construction when the call site literal matches the callee's
pattern). The STG compiler is unchanged.

**Tech Stack:** Rust, Rowan parser, moniker variable binding, existing
core expression infrastructure.

---

## Pipeline Overview

```
Parse → Desugar → Cook → [Destructure Pass] → Eliminate → Inline → Eliminate → Verify → STG
```

The new destructure pass sits after cook (so operator soup is resolved
into application trees, making call sites visible) and before eliminate
(so fused results benefit from DCE).

---

## Task 1: Core Infrastructure — LetType Markers and List Intrinsics

Add the types and intrinsics needed by all subsequent tasks.

**Files:**
- Modify: `src/core/expr.rs` (LetType enum, ~line 190)
- Modify: `src/eval/intrinsics.rs` (new list intrinsics)
- Modify: `src/eval/stg/support.rs` (intrinsic implementations)

**Step 1: Add LetType markers**

In `src/core/expr.rs`, extend the `LetType` enum:

```rust
pub enum LetType {
    DefaultBlockLet,
    OtherLet,
    DestructureBlockLet,
    DestructureListLet,
}
```

These markers let the fusion pass identify destructuring lets without
fragile structural pattern-matching. `LetType` remains `Copy`.

**Step 2: Add list intrinsics**

Block destructuring uses `Expr::Lookup` (already in core). List
destructuring needs intrinsics for element access and tail/drop since
prelude functions aren't resolved until after cook.

Add intrinsics: `__LIST_NTH(list, index)` and `__LIST_DROP(n, list)`.
Model on existing intrinsics like `__ADD`. These call into the existing
list representation at STG level.

**Step 3: Build and verify no regressions**

Run: `cargo test --lib`

**Step 4: Commit**

```
git commit -m "feat: add destructure LetType markers and list intrinsics"
```

---

## Task 2: Relax Parameter Validation in AST

Allow block and list patterns in function parameter positions instead
of requiring plain identifiers.

**Files:**
- Modify: `src/syntax/ast.rs` (~lines 613-651, `classify_declaration`)
- Modify: `src/syntax/rowan/error.rs` (new parse error variants)

**Step 1: Understand current validation**

In `src/syntax/ast.rs:623-635`, each arg in `ApplyTuple` is checked to
be a single `NormalIdentifier`. Anything else produces
`ParseError::InvalidFormalParameter`. This must be relaxed.

**Step 2: Define what constitutes a valid pattern**

A parameter is valid if it is one of:
- A single normal identifier (existing — simple param)
- A `BLOCK` node containing only names/colons (block pattern)
- A `LIST` node containing names/commas/colons (list pattern)

**Step 3: Relax the validation**

Replace the strict check with a function that accepts these pattern
forms. Invalid patterns (e.g. arbitrary expressions in param position)
still produce `ParseError::InvalidFormalParameter`.

**Step 4: Build and verify existing tests pass**

Run: `cargo test`

**Step 5: Commit**

```
git commit -m "feat: relax function parameter validation for destructuring patterns"
```

---

## Task 3: Desugar Block Destructuring

Parse block patterns `{x y}` and `{x: a y: b}` in function params
and desugar to Lam + DestructureBlockLet.

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs` (~lines 295-436)
- Modify: `src/core/desugar/rowan_ast.rs` (~lines 237-293)

**Step 1: Write a failing harness test**

Create `harness/test/090_destructure_block.eu`:

```eucalypt
tests: {
  ` "Basic block destructuring"
  basic: {
    f({x y}): x + y
    α: f({x: 1 y: 2}) //= 3
  }

  ` "Block destructuring with renaming"
  rename: {
    f({x: a  y: b}): a * b
    α: f({x: 3 y: 4}) //= 12
  }

  ` "Mixed shorthand and rename"
  mixed: {
    f({x  y: b}): x + b
    α: f({x: 10 y: 20}) //= 30
  }
}

RESULT: tests values map(values) map(all-true?) all-true? then(:PASS, :FAIL)
```

Register in `tests/harness_test.rs`.

Run: `cargo test test_harness_090` — expected: FAIL (patterns not
supported yet).

**Step 2: Recognise block patterns in function params**

In `extract_rowan_declaration_components` (lines 333-364), when
iterating `args_tuple.items()`, check if the Soup represents:
- A plain identifier (existing path)
- A block node `{ ... }` (new: block pattern)
- A list node `[ ... ]` (new: list pattern — handled in Task 5)

For a block pattern, extract the field names and optional rename
bindings from the block's declarations.

Create a helper enum:

```rust
enum ParamPattern {
    Simple(String),
    Block(Vec<(String, String)>),  // (field_name, binding_name)
    List(Vec<String>, Option<String>),  // (elements, tail)
}
```

**Step 3: Generate destructuring let in body**

When a parameter is `ParamPattern::Block(fields)`:

1. Create a fresh `FreeVar` for the synthetic parameter (`__p0`,
   `__p1` etc.)
2. Push the synthetic param name to environment
3. Create `FreeVar`s for each binding name and push to environment
4. Create let bindings:
   ```
   field_binding = Lookup(Var(__p0), field_name, None)
   ```
5. Desugar the function body with all bindings in scope
6. Wrap body in `Let(DestructureBlockLet, bindings, body)`
7. Wrap in `Lam([__p0], ...)`

Use `core::let_()` but pass `LetType::DestructureBlockLet` (will need
a variant of `let_` or construct the `Expr::Let` directly).

**Step 4: Run test to verify it passes**

Run: `cargo test test_harness_090`

**Step 5: Run full test suite**

Run: `cargo test`

**Step 6: Commit**

```
git commit -m "feat: block destructuring in function parameters"
```

---

## Task 4: Desugar List Destructuring (Fixed-Length)

Parse `[a, b, c]` patterns in function params and desugar using the
list intrinsics from Task 1.

**Files:**
- Modify: `src/core/desugar/rowan_ast.rs`

**Step 1: Write a failing harness test**

Create `harness/test/091_destructure_list.eu`:

```eucalypt
tests: {
  ` "Fixed-length list destructuring"
  fixed: {
    f([a, b, c]): a + b + c
    α: f([10, 20, 30]) //= 60
  }

  ` "Two-element list"
  pair: {
    first([a, b]): a
    second([a, b]): b
    α: first([1, 2]) //= 1
    β: second([1, 2]) //= 2
  }
}

RESULT: tests values map(values) map(all-true?) all-true? then(:PASS, :FAIL)
```

Register in harness. Run and confirm failure.

**Step 2: Handle list patterns in desugaring**

When a parameter is `ParamPattern::List(elements, None)`:

1. Create synthetic param `__p0`
2. For each element at index `i`, create binding:
   ```
   element_i = App(Intrinsic("__LIST_NTH"), [Var(__p0), Literal(i)])
   ```
3. Wrap in `Let(DestructureListLet, bindings, body)`
4. Wrap in `Lam([__p0], ...)`

**Step 3: Run test to verify it passes**

Run: `cargo test test_harness_091`

**Step 4: Commit**

```
git commit -m "feat: fixed-length list destructuring in function parameters"
```

---

## Task 5: List Head/Tail Destructuring with `:`

Parse `[x : xs]` and `[a, b : rest]` patterns.

**Files:**
- Modify: `src/syntax/ast.rs` or `src/core/desugar/rowan_ast.rs`

**Step 1: Write a failing harness test**

Add to `harness/test/091_destructure_list.eu`:

```eucalypt
  ` "Head/tail with colon"
  head-tail: {
    f([x : xs]): x
    α: f([1, 2, 3]) //= 1
  }

  ` "Multiple heads plus tail"
  multi-head: {
    f([a, b : rest]): a + b
    α: f([10, 20, 30, 40]) //= 30
  }

  ` "Tail binding"
  tail-bind: {
    f([x : xs]): xs
    α: f([1, 2, 3]) //= [2, 3]
  }
```

**Step 2: Parse colon in list patterns**

When parsing a list inside a parameter position, `:` separates the
fixed prefix elements from the tail binding. The Rowan parser already
lexes `:` as `COLON`. Detect it in the list pattern context:

- Items before `:` are fixed-position bindings
- The single item after `:` is the tail binding name

Produce `ParamPattern::List(heads, Some(tail_name))`.

**Step 3: Desugar head/tail patterns**

When tail is `Some(tail_name)`:
1. Fixed elements: `element_i = App(__LIST_NTH, [__p0, i])`
2. Tail: `tail = App(__LIST_DROP, [Literal(n), Var(__p0)])` where `n`
   is the number of fixed elements

**Step 4: Run tests**

Run: `cargo test test_harness_091`

**Step 5: Commit**

```
git commit -m "feat: head/tail list destructuring with colon syntax"
```

---

## Task 6: Cons Operator `‖` (U+2016)

Add `‖` as a right-associative list cons operator in expressions.

**Files:**
- Modify: `src/syntax/rowan/lex.rs` (recognise `‖` as operator char)
- Modify: `src/eval/intrinsics.rs` (cons intrinsic)
- Modify: `src/eval/stg/support.rs` (cons implementation)
- Modify: `lib/prelude.eu` (operator definition with fixity)

**Step 1: Write a failing harness test**

Create `harness/test/092_cons_operator.eu`:

```eucalypt
tests: {
  ` "Cons single element"
  single: {
    α: (1 ‖ [2, 3]) //= [1, 2, 3]
  }

  ` "Cons chain"
  chain: {
    α: (1 ‖ 2 ‖ [3]) //= [1, 2, 3]
  }

  ` "Cons onto empty"
  empty: {
    α: (1 ‖ []) //= [1]
  }
}

RESULT: tests values map(values) map(all-true?) all-true? then(:PASS, :FAIL)
```

**Step 2: Add `‖` to lexer**

In `src/syntax/rowan/lex.rs`, ensure `‖` (U+2016) is recognised as a
valid operator character in `is_oper_start()` / `is_oper_continue()`.

**Step 3: Implement cons intrinsic**

Add `__CONS(element, list)` intrinsic that prepends an element to a
list. Implement in STG support by constructing a new list with the
element at the head.

**Step 4: Define in prelude**

In `lib/prelude.eu`:

```eucalypt
` { doc: "`x ‖ xs` - prepend element `x` to list `xs`."
    associates: :right
    precedence: 55 }
(x ‖ xs): __CONS(x, xs)
```

Choose a precedence that works with existing operators (between
comparison and addition seems reasonable; adjust based on eucalypt's
precedence table).

**Step 5: Run tests**

Run: `cargo test test_harness_092`

**Step 6: Commit**

```
git commit -m "feat: add cons operator ‖ for list construction"
```

---

## Task 7: Juxtaposed Call Syntax

Add `f{...}` and `f[...]` as sugar for `f({...})` and `f([...])`.

**Files:**
- Modify: `src/syntax/rowan/lex.rs` (~line 504, whitespace detection)
- Modify: `src/syntax/rowan/parse.rs` (parse apply with block/list)
- Modify: `src/syntax/rowan/kind.rs` (possibly new SyntaxKind)

**Step 1: Write a failing harness test**

Create `harness/test/093_juxtaposed_call.eu`:

```eucalypt
tests: {
  ` "Juxtaposed block call"
  block-call: {
    f({x y}): x + y
    α: f{x: 1 y: 2} //= 3
  }

  ` "Juxtaposed list call"
  list-call: {
    f([a, b]): a + b
    α: f[10, 20] //= 30
  }

  ` "Named argument style"
  named-args: {
    greet({name greeting}): "{greeting}, {name}!"
    α: greet{name: "World" greeting: "Hello"} //= "Hello, World!"
  }
}

RESULT: tests values map(values) map(all-true?) all-true? then(:PASS, :FAIL)
```

**Step 2: Lexer — detect juxtaposed block/list**

Use the same mechanism as `OPEN_PAREN_APPLY` vs `OPEN_PAREN`. In
`lex.rs`, when lexing `{` or `[`:
- If the previous token is callable AND no whitespace since last token,
  treat as juxtaposed application
- Otherwise, treat as normal block/list literal

Add new `SyntaxKind` variants if needed (e.g. `OPEN_BRACE_APPLY`,
`OPEN_SQUARE_APPLY`) or handle in the parser.

**Step 3: Parser — handle juxtaposed forms**

When the parser sees a juxtaposed `{` or `[` after a callable, parse
the block/list normally and wrap as a single-argument application, as
if the user had written `f({...})` or `f([...])`.

**Step 4: Run tests**

Run: `cargo test test_harness_093`

**Step 5: Run full test suite for regressions**

Run: `cargo test`

**Step 6: Commit**

```
git commit -m "feat: juxtaposed call syntax f{...} and f[...]"
```

---

## Task 8: Destructure Fusion Pass

Implement the core-to-core pass that elides block/list construction
when a literal argument is immediately destructured.

**Files:**
- Create: `src/core/destructure.rs`
- Modify: `src/core/mod.rs` (add module)
- Modify: `src/driver/source.rs` (add `destructure()` method)
- Modify: `src/driver/prepare.rs` (wire into pipeline after cook)

**Step 1: Write a test that demonstrates fusion opportunity**

The harness tests from Tasks 3-5 already exercise fusion opportunities
(e.g. `f({x: 1 y: 2})` calling `f({x y})`). Fusion is a transparent
optimisation — same result, fewer allocations.

For verification, add a comment in the test file noting that fusion
should apply, and optionally add a benchmark test.

**Step 2: Create the destructure pass module**

Create `src/core/destructure.rs` with a function:

```rust
pub fn destructure_pass(expr: &RcExpr) -> RcExpr
```

This walks the expression tree. For each `App(func, args)`:
1. If `func` is a `Lam` with one parameter...
2. And the body starts with `Let(DestructureBlockLet, ...)`...
3. And `args` is a single `Block(literal)`...
4. Then fuse: replace each binding's lookup with the corresponding
   value from the block literal, change LetType to `OtherLet`.

Similar logic for `DestructureListLet` + `List(literal)`.

For non-fusible cases, leave the expression unchanged — the let
bindings with lookups/intrinsics already produce correct results.

**Step 3: Wire into the pipeline**

In `src/driver/source.rs`, add:

```rust
pub fn destructure(&mut self) -> Result<(), EucalyptError> {
    self.core.expr = destructure::destructure_pass(&self.core.expr);
    Ok(())
}
```

In `src/driver/prepare.rs`, call after cook and before eliminate-1:

```rust
loader.cook()?;
loader.destructure()?;  // NEW
loader.eliminate()?;
```

**Step 4: Run full test suite**

Run: `cargo test`

**Step 5: Commit**

```
git commit -m "feat: destructure fusion pass — elide block/list construction"
```

---

## Task 9: Error Cases

Add tests for error conditions: missing fields, short lists, empty
head/tail.

**Files:**
- Create: `harness/test/errors/XXX_destructure_missing.eu` + `.expect`
- Create: `harness/test/errors/XXX_destructure_short_list.eu` + `.expect`
- Create: `harness/test/errors/XXX_destructure_empty_cons.eu` + `.expect`

**Step 1: Determine next error test numbers**

Check existing error tests in `harness/test/errors/` for the next
available number.

**Step 2: Write error tests with `.expect` sidecars**

For each error case, create a `.eu` file that triggers the error and a
`.expect` file with the expected exit code and stderr pattern.

Example — missing block field:

```eucalypt
# harness/test/errors/0XX_destructure_missing.eu
f({x y}): x + y
RESULT: f({x: 1})
```

```
# harness/test/errors/0XX_destructure_missing.expect
exit: 1
stderr: "lookup"
```

**Step 3: Run error tests**

Run: `cargo test test_error`

**Step 4: Commit**

```
git commit -m "test: error cases for destructuring (missing field, short list, empty cons)"
```

---

## Task 10: Integration Tests

Comprehensive tests covering the full feature set including
interactions with existing language features.

**Files:**
- Expand: `harness/test/090_destructure_block.eu`
- Expand: `harness/test/091_destructure_list.eu`
- Create: `harness/test/094_named_args.eu`

**Step 1: Named argument integration test**

```eucalypt
tests: {
  ` "Named args via juxtaposed block + destructuring"
  named: {
    greet({name greeting}): "{greeting}, {name}!"
    α: greet{name: "World" greeting: "Hello"} //= "Hello, World!"
  }

  ` "Named args with renaming"
  renamed: {
    point-dist({x: px  y: py}): px * px + py * py
    α: point-dist{x: 3 y: 4} //= 25
  }

  ` "Destructuring in pipeline"
  pipeline: {
    data: [{x: 1 y: 2}, {x: 3 y: 4}]
    sum-xy({x y}): x + y
    α: data map(sum-xy) //= [3, 7]
  }
}

RESULT: tests values map(values) map(all-true?) all-true? then(:PASS, :FAIL)
```

**Step 2: Mixed parameter tests**

Test that positional and destructured params can coexist in separate
functions (not mixed in one function):

```eucalypt
  ` "Positional and destructured functions coexist"
  coexist: {
    add(a, b): a + b
    add-xy({x y}): x + y
    α: add(1, 2) //= 3
    β: add-xy({x: 1 y: 2}) //= 3
  }
```

**Step 3: Interaction with anaphora, pipelines, metadata**

Add tests for destructuring used alongside block anaphora, expression
anaphora, pipelines, and metadata to ensure no interference.

**Step 4: Run full test suite**

Run: `cargo test`

**Step 5: Final clippy and formatting check**

Run: `cargo fmt --all && cargo clippy --all-targets -- -D warnings`

**Step 6: Commit**

```
git commit -m "test: comprehensive integration tests for destructuring and named args"
```

---

## Summary of Deliverables

| Task | Feature | Key Files |
|------|---------|-----------|
| 1 | LetType markers + list intrinsics | `expr.rs`, `intrinsics.rs` |
| 2 | Relaxed param validation | `ast.rs` |
| 3 | Block destructuring | `rowan_ast.rs`, test 090 |
| 4 | Fixed-length list destructuring | `rowan_ast.rs`, test 091 |
| 5 | Head/tail list destructuring | `rowan_ast.rs`, test 091 |
| 6 | Cons operator `‖` | `lex.rs`, `prelude.eu`, test 092 |
| 7 | Juxtaposed call syntax | `lex.rs`, `parse.rs`, test 093 |
| 8 | Fusion pass | `destructure.rs`, `prepare.rs` |
| 9 | Error case tests | `harness/test/errors/` |
| 10 | Integration tests | test 094 |

## Task Dependencies

```
Task 1 (infrastructure)
  ├─→ Task 2 (AST validation)
  │     └─→ Task 3 (block destructuring)
  │           └─→ Task 4 (list fixed-length)
  │                 └─→ Task 5 (list head/tail)
  │
  ├─→ Task 6 (cons operator) — independent
  │
  └─→ Task 7 (juxtaposed call) — needs Task 3 for testing
        └─→ Task 8 (fusion) — needs Tasks 3-5 and 7
              └─→ Task 9 (error tests)
                    └─→ Task 10 (integration tests)
```

Tasks 6 (cons operator) can proceed in parallel with Tasks 3-5.
