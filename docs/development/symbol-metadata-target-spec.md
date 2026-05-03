# Symbol Metadata Shortcut for Targets

**Status**: Spec  
**Bead**: eu-7a84  
**Date**: 2026-05-03

## 1. Overview

A bare symbol in declaration metadata that is not a known shortcut
should define a target.  This reduces ceremony for the common case
of tagging a declaration as a named target.

Additionally, the bare symbol `:target` uses the declaration's own
name as the target name.

## 2. Current Behaviour

`normalise_metadata` in `src/core/metadata.rs` handles two symbol
shortcuts:

- `:suppress` → `{ export: :suppress }`
- `:main` → `{ target: :main }`

Any other symbol is passed through unchanged (no normalisation).

## 3. Proposed Behaviour

| Metadata | Becomes |
|----------|---------|
| `` ` :suppress `` | `{ export: :suppress }` (unchanged) |
| `` ` :main `` | `{ target: :main }` (unchanged) |
| `` ` :target `` | `{ target: :decl-name }` (NEW — uses declaration name) |
| `` ` :test `` | `{ target: :test }` (NEW — any unrecognised symbol) |
| `` ` :foo `` | `{ target: :foo }` (NEW) |

### Examples

```eu
# Before: verbose
` { target: :test }
my-test: ...

# After: shortcut
` :test
my-test: ...

# Self-naming target
` :target
my-output: ...
# equivalent to: ` { target: :my-output }
```

### Edge cases

- `:target` on unit-level metadata (not a declaration): ignored,
  passed through as-is.  Metadata is just data.
- `:target` on operator declarations: works, target name is the
  operator name (e.g. `+++`).  Operators as targets are unusual
  but not prohibited.
- Known shortcuts (`:suppress`, `:main`) take priority — they
  are NOT converted to targets.

## 4. Implementation

### 4.1 Modify `normalise_metadata`

Add an optional declaration name parameter:

```rust
pub fn normalise_metadata(expr: &RcExpr, decl_name: Option<&str>) -> RcExpr {
    match &*expr.inner {
        Expr::Literal(smid, prim) => match prim {
            Primitive::Str(_) => { /* doc shortcut, unchanged */ }
            Primitive::Sym(s) => match s.as_ref() {
                "suppress" => { /* unchanged */ }
                "main" => { /* unchanged */ }
                "target" => {
                    if let Some(name) = decl_name {
                        let target_sym = core::sym(*smid, name);
                        core::block(*smid, [("target".to_string(), target_sym)])
                    } else {
                        expr.clone() // unit-level, ignore
                    }
                }
                _ => {
                    // Any other symbol → target shortcut
                    core::block(*smid, [("target".to_string(), expr.clone())])
                }
            },
            _ => expr.clone(),
        },
        _ => expr.clone(),
    }
}
```

### 4.2 Update call sites

- `rowan_ast.rs:2465` — declaration binding: pass
  `Some(&components.name)` as `decl_name`
- `rowan_ast.rs:2618` and `:2761` — unit-level metadata: pass
  `None` as `decl_name`

## 5. Acceptance Criteria

1. `` ` :test `` on a declaration → `eu -t test` renders that
   declaration
2. `` ` :target `` on declaration `my-output` → `eu -t my-output`
   renders it
3. `` ` :suppress `` still works as export suppression (not
   converted to target)
4. `` ` :main `` still works as main target
5. `` ` :target `` on unit-level metadata → ignored (no crash,
   no effect)
6. `` ` :target `` on operator declaration → target name is the
   operator name
7. `` ` "docstring" `` still works as doc shortcut
8. `eu check lib/prelude.eu` — zero warnings
9. All existing harness tests pass

## 6. Files Changed

| File | Change |
|------|--------|
| `src/core/metadata.rs` | Add `decl_name` param, handle `:target` and unrecognised symbols |
| `src/core/desugar/rowan_ast.rs` | Pass declaration name to `normalise_metadata` at call sites |
| `tests/harness/` | New test for symbol target shortcut |

## 7. Dependencies

None.
