# Typed Monad Metadata for Checking Monadic Block Bindings

**Status**: Spec  
**Bead**: eu-ggr9  
**Date**: 2026-04-22

## 1. Overview

Extend the `monad:` metadata field on monad namespace declarations to
optionally declare the monadic wrapper type. When present, the
desugarer injects type hints on binding values in monadic blocks,
enabling the type checker to catch wrong-type bindings (e.g. passing
a number where a list is expected in a `:for` block).

## 2. Metadata Format

The `monad:` metadata field on a namespace declaration can be:

| Value | Meaning |
|-------|---------|
| `true` | Untyped monad (backward compatible, no type checking) |
| `"[a]"` | Typed monad — bindings must be lists |
| `"IO(a)"` | Typed monad — bindings must be IO actions |
| Any type string | Typed monad — bindings must match the wrapper type |

The type string uses the same syntax as `type:` annotations. The
variable `a` represents the element/result type — it is freshened
independently for each binding.

### Prelude changes

```eu,notest
` { monad: "[a]" }
for: monad({bind(m, f): m mapcat(f), return(v): [v]})

` { monad: "IO(a)" }
io: monad({bind: io-bind, return: io-return}) { ... }

` { monad: true }
let: monad({bind(m, f): f(m), return: identity})
```

The `random` namespace needs its wrapper type determined — likely
`{{value: a, rest: any}}` or similar.

## 3. Implementation

### 3.1 Extract monad type from raw AST metadata

Modify `has_monad_true_in_raw_meta` to return the monad type:

```rust
/// Extract the monad metadata value from a declaration.
///
/// Returns:
/// - `None` — no monad: field
/// - `Some(None)` — monad: true (untyped)
/// - `Some(Some(type_str))` — monad: "type" (typed)
fn extract_monad_meta(decl: &rowan_ast::Declaration) -> Option<Option<String>>
```

The function checks the backtick metadata block for a `monad:` field.
If the value is the identifier `true`, returns `Some(None)`. If the
value is a string literal, returns `Some(Some(string_value))`.

### 3.2 Store monad type in MonadSpec

Add an optional type string to `MonadSpec`:

```rust
pub enum MonadSpec {
    Explicit {
        bind_name: String,
        return_name: String,
        monad_type: Option<String>,  // NEW
    },
    Namespace {
        name: String,               // was just Namespace(String)
        monad_type: Option<String>, // NEW
    },
}
```

Or simpler — add a field to the registration:

```rust
pub struct MonadSpec {
    pub kind: MonadSpecKind,
    pub monad_type: Option<String>,
}

pub enum MonadSpecKind {
    Explicit { bind_name: String, return_name: String },
    Namespace(String),
}
```

### 3.3 Inject type hints in desugar_monadic_block

In `desugar_monadic_block`, after desugaring each binding value,
wrap it with a `__type_hint` Meta node if the monad has a declared
type:

```rust
// After desugaring the binding value:
let value = if let Some(type_str) = &spec.monad_type {
    let hint = core::str(smid, type_str);
    let meta = core::block(smid, [("__type_hint".to_string(), hint)]);
    core::meta(smid, value, meta)
} else {
    value
};
```

This wraps each binding value (not the bind call) so the type
checker verifies the value directly against the wrapper type.

### 3.4 No type checker changes needed

The existing `synthesise_meta` path already:
1. Extracts `__type_hint` from Meta nodes
2. Freshens type variables (so each binding's `a` is independent)
3. Calls `check_against(inner, &working_type, smid)`
4. Warns if the inner expression doesn't match

## 4. What This Catches

```eu,notest
{ :for x: 42 }.(x * 2)
# warning: expression type does not match annotation
#   expected [a], found number

{ :io cmd: 42 }.(cmd)
# warning: expression type does not match annotation
#   expected IO(a), found number

{ :for x: [1,2,3], y: "hello" }.(x + y)
# warning on y: expected [a], found string
```

## 5. Bracket Pair Monads

Bracket pair monads get typed via backtick metadata on the bracket
pair definition:

```eu,notest
` { monad: "[a]" }
⟦{}⟧: { :monad bind: my-bind return: my-return }
```

**Implementation note**: namespace monads are registered at line 2366
by checking the declaration's backtick metadata via
`has_monad_true_in_raw_meta(decl)`. Bracket pair monads are
registered at line 2428 by extracting from the desugared body via
`extract_monad_spec_from_body`. These are separate code paths.

For bracket pairs, the `monad:` type must be extracted from the
declaration's backtick metadata (using `extract_monad_meta`) and
passed to the bracket registry alongside the spec extracted from
the body. The `extract_monad_meta` function works on any declaration
— it reads the backtick metadata block regardless of declaration
kind.

## 6. LSP Support

### 6.1 Completion for monad tags

After `{ :` in a block, the completion provider offers known monad
namespace names from the symbol table — those whose `SymbolInfo`
carries `monad:` metadata. This is an AST-level feature, no type
checker dependency.

### 6.2 Inlay hints on monadic block bindings

When the LSP detects a monadic block (block with monad tag in
metadata), it shows inlay hints on each binding declaration showing
the expected type from the monad's `monad:` metadata:

```
{ :for x: [1,2,3] }           x : [a]  (inlay hint)
{ :io  r: io.shell("cmd") }   r : IO(a)  (inlay hint)
```

This is AST-level — the LSP reads the monad tag, looks up the
namespace's metadata, and extracts the `monad:` type string.

### 6.3 Hover on monad tags

Hovering over `:for` / `:io` / `:random` in a monadic block shows
the monad description and binding type requirement.

### 6.4 Type warnings as diagnostics

Type warnings from `__type_hint` mismatches appear as
`DiagnosticSeverity::WARNING` with source `"eucalypt-types"` —
this uses existing infrastructure, no new LSP code needed.

## 7. What This Doesn't Catch

- Wrong types passed to monad functions directly (e.g.
  `for.bind(42, identity)`) — the monad functions themselves are
  still untyped. Addressed by eu-dme3.
- Type of the bound variable inside the block body — `x` in
  `{ :for x: [1,2,3] }` should be `number` (unwrapped), not `any`.
  Addressed by eu-z9zz.10.
- Type mismatches in the return expression.

## 8. Testing

### Typecheck tests (tests/harness/typecheck/)

- `009_for_number_binding.eu` — `{ :for x: 42 }.(x)` warns
- `010_for_string_binding.eu` — `{ :for x: "hello" }.(x)` warns
- `011_for_correct_binding.eu` — `{ :for x: [1,2,3] }.(x * 2)` no warning
- `012_io_number_binding.eu` — `{ :io x: 42 }.(x)` warns
- `013_let_any_binding.eu` — `{ :let x: 42 }.(x)` no warning (untyped)

### Harness tests

- Existing monadic block harness tests (096, 145) must still pass
- Verify `eu check lib/prelude.eu` — zero warnings

## 9. Files Changed

| File | Change |
|------|--------|
| `src/core/desugar/desugarer.rs` | `MonadSpec` gains `monad_type: Option<String>` |
| `src/core/desugar/rowan_ast.rs` | `extract_monad_meta` replaces `has_monad_true_in_raw_meta`; `desugar_monadic_block` injects `__type_hint`; bracket pair registration reads `monad:` from backtick metadata |
| `lib/prelude.eu` | `for` gets `monad: "[a]"`, `io` gets `monad: "IO(a)"` |
| `src/driver/lsp/completion.rs` | Monad tag completion after `{ :` |
| `src/driver/lsp/inlay_hints.rs` | Binding type hints in monadic blocks |
| `src/driver/lsp/hover.rs` | Hover info on monad tags |
| `src/driver/lsp/symbol_table.rs` | Track `monad:` metadata on symbols |
| tests/harness/typecheck/ | New typecheck tests for monadic bindings |

## 10. Acceptance Criteria

### Must pass

1. `{ :for x: 42 }.(x)` produces a type warning: "expected [a],
   found number" — verified by `eu check --strict` exit code 1
2. `{ :for x: [1,2,3] }.(x * 2)` produces NO type warning
3. `{ :io cmd: 42 }.(cmd)` produces a type warning: "expected IO(a),
   found number"
4. `{ :io r: io.shell("echo hi") }.(r)` produces NO type warning
   (requires --allow-io and an on-disk file, not -e)
5. `{ :let x: 42 }.(x)` produces NO type warning (monad: true is
   untyped)
6. Bracket pair monad with `monad: "[a]"` on its definition produces
   type warnings for non-list bindings
7. `eu check lib/prelude.eu` — zero warnings
8. All existing harness tests pass (096, 145, and others)
9. `monad: true` continues to work for backward compatibility
10. Type warnings point at the binding VALUE expression, not the
    monadic block or the bind call

### LSP acceptance

11. After `{ :` in a block, completion offers `:for`, `:io`,
    `:random`, `:let`
12. In a `{ :for x: ... }` block, inlay hint on `x:` shows expected
    type `[a]`
13. Hover on `:for` shows monad description and binding type
14. Wrong binding type shows as warning diagnostic in editor

### Performance

15. Type checking with monad hints adds negligible overhead (< 1ms on
    AoC-sized files, same as existing __type_hint for lenses)

## 11. Future Extensions

- Bound variable type hinting (eu-z9zz.10) — infer element type from
  monad wrapper type and annotate lambda parameters
- The monad type could eventually be inferred from the `return`
  function's type annotation rather than declared explicitly
- Row variables (eu-z9zz.5) would enable typing monad() output
