# Export :internal — unit-level declaration visibility

## Status: Spec — ready for implementation
## Release: 0.7.1
## Agent: Quill (front-end)
## Bead: eu-u2vn.3
## Depends on: W2 (Unit Interface)
## Prior spec: docs/development/unit-visibility-spec.md

## Problem

Every declaration in a eucalypt unit is visible to importers. There is
no way for a library to mark a binding as implementation-private.
Helpers leak into the importer's namespace, causing pollution and
ambiguity when two units contribute same-named helpers.

## Design

### Metadata syntax

```eu
` { export: :internal }
helper(x): x + 1
```

The `export: :internal` metadata marks a declaration as internal to its
unit. The default (no `export:` metadata) is `:public`.

### Implementation: rebody filtering

Today, imports work via rebody — unit A's Let wraps unit B's expression
as its body. Free variables in B that match A's binding names get bound
via `bind_free_vars`.

For internal bindings: **omit them from `bind_free_vars`** at merge
time. The internal binding still exists in its own unit's Let scope
(accessible to sibling bindings) but is not captured by the dependent's
free variables.

Specifically, in `rebody_int` / `prepare_capture_vars`, filter out
bindings whose visibility is `:internal`. The dependent's code can't
reference names that aren't in the capture set.

### UnitInterface integration

The `UnitInterface.visibility` field carries the visibility map. At
merge time, the rebody operation consults this map to decide which
bindings to include in the capture set.

### What happens when a dependent tries to use an internal binding

The name remains as a `Var::Free` — unresolved. This surfaces as a
compile error ("unresolved variable") or, at runtime, as a lookup
failure. The error message should indicate the binding exists but is
internal (requires checking the visibility map during error reporting).

## Code locations

- `src/core/expr.rs:537-543` — `prepare_capture_vars` (filter here)
- `src/core/expr.rs:548-571` — `rebody_int` (where binding happens)
- `src/core/desugar/` — extract `export:` metadata during desugar
- `src/eval/error.rs` — improve error message for internal binding
  access attempts

## Acceptance criteria

1. `export: :internal` metadata is recognised during desugar
2. Internal bindings are not captured by importing units during merge
3. Internal bindings remain accessible within their own unit
4. Attempting to use an internal binding from an importer produces an
   error (not a silent failure)
5. Two units with same-named internal helpers don't conflict
6. Visibility carried in `UnitInterface.visibility`
7. Regression tests cover: internal binding isolation, same-unit
   access, cross-unit error, name collision
8. All existing harness tests pass
