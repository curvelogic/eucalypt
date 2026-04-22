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

## 5. What This Doesn't Catch

- Wrong types passed to monad functions directly (e.g.
  `for.bind(42, identity)`) — the monad functions themselves are
  still untyped. Addressed by eu-dme3.
- Bracket pair monads without `monad:` metadata on their definition.
- Type mismatches in the return expression.

## 6. Testing

### Error tests (tests/harness/typecheck/)

- `for_number_binding.eu` — `{ :for x: 42 }.(x)` warns
- `for_string_binding.eu` — `{ :for x: "hello" }.(x)` warns
- `for_correct_binding.eu` — `{ :for x: [1,2,3] }.(x * 2)` no warning
- `io_number_binding.eu` — `{ :io x: 42 }.(x)` warns
- `io_correct_binding.eu` — `{ :io r: io.shell("cmd") }.(r)` no warning
- `let_any_binding.eu` — `{ :let x: 42 }.(x)` no warning (untyped)

### Harness tests

- Existing monadic block harness tests (096, 145) must still pass.
- Verify `eu check lib/prelude.eu` — zero warnings.

## 7. Files Changed

| File | Change |
|------|--------|
| `src/core/desugar/desugarer.rs` | `MonadSpec` gains `monad_type: Option<String>` |
| `src/core/desugar/rowan_ast.rs` | `extract_monad_meta` replaces `has_monad_true_in_raw_meta`; `desugar_monadic_block` injects `__type_hint` |
| `lib/prelude.eu` | `for` gets `monad: "[a]"`, `io` gets `monad: "IO(a)"` |
| tests/harness/typecheck/ | New typecheck tests for monadic bindings |

## 8. Future Extensions

- Bracket pair monads could get typed via backtick metadata on the
  bracket pair definition (same `monad:` field)
- The monad type could eventually be inferred from the `return`
  function's type annotation rather than declared explicitly
- Row variables (eu-z9zz.5) would enable typing monad() output
