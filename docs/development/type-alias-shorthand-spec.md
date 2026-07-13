# Spec: Type-Alias Metadata Shorthand and `result-def`

**Status**: Specification — ready to implement.
**Date**: 2026-05-20
**Branch**: `type-system-exploration`
**Companions**: `alias-reference-tooling-spec.md` (A7 — alias-reference LSP support; shipped in 0.6.2/0.7.0, spec file removed, see `ROADMAP.md`), [unit-visibility-spec.md](./unit-visibility-spec.md) (related metadata-convention work).

**Scope note**: a metadata + alias-system extension; independent of the
type-system roadmap (TS-A/TS-B). Earns its own bead.

## Overview

`type-def` today registers a type alias whose name is supplied as an
explicit string (`type-def: "Foo"`). Two awkwardnesses follow: the
alias name is often a duplicate of the binding name (the binding is
named to reflect the type), and there is no way to alias a function's
*return type* — today `type-def` on a function captures the function
type, not the codomain.

This spec resolves both, entirely in the metadata channel — **no
`typeof` in the type DSL**. Three small pieces, each independent but
compositional:

1. **A general lone-keyword shorthand** — `` ` :key `` ≡
   `` ` { key: true } `` for any recognised metadata key with a defined
   `true` semantics.
2. **`type-def: true`** — register the alias using the binding's name
   (kills the name duplication for the natural case where binding and
   alias share a name).
3. **`result-def: true | "Name"`** — a new metadata key registering the
   alias from the function's *return type*.

The existing `type-def: "Name"` form is unchanged; everything new is
additive.

## Background — verified facts

- `extract_type_def_name` (check.rs:323) reads `type-def` from a
  binding's metadata, expecting a string value. The alias is registered
  globally in the `Checker`'s alias map during Let-binding synthesis
  (check.rs ~465–475).
- `normalise_metadata` (metadata.rs:38) handles bare-symbol metadata
  shorthands: `:suppress` → `{export: :suppress}` and `:target` →
  `{target: <binding-name>}`. Other bare symbols on a declaration become
  target shortcuts (metadata.rs:58–64).
- Metadata blocks attached to declarations survive into the core as
  `Meta` wrappers — confirmed by the renderer reading `export` from core
  metadata at render time (render.rs:332–340).

## The lone-keyword shorthand convention

A bare symbol `:key` as declaration metadata, where `key` is a
recognised metadata key with a defined `true` semantics, expands to
`{ key: true }`:

```
` :type-def        ≡   ` { type-def: true }
` :result-def      ≡   ` { result-def: true }
` :target          ≡   ` { target: true }
```

The interpretation of `true` is per key; the next sections define it
for the three keys above. For bare symbols whose names are *not*
recognised as keys with `true` semantics, the **existing target-shortcut
fallback applies unchanged** (metadata.rs:58–64). The `:suppress` legacy
special-case stays (it is a *value* on the `export` key, not a key
itself).

This is purely additive: any pre-existing `` ` :foo `` that worked as a
target shortcut keeps doing so unless `foo` is one of the recognised
keys (currently `type-def`, `result-def`, `target`). Future keys with a
useful `true` semantics opt in.

## `type-def: true | "Name"`

Extend the existing `type-def` key's value space:

```
type-def: true        # alias name = binding name; alias type = binding's type
type-def: "Foo"       # alias name = "Foo";        alias type = binding's type
```

`true` removes the string duplication for the natural case where the
binding and the alias should share a name. Values and type aliases live
in different namespaces (value scope vs. alias map), so naming both
`Person` is legitimate and cleanly disambiguated by context:

```
` :type-def
Person: { name: "Alice", age: 30 }
# value `Person` + type alias `Person`
```

vs the explicit form when the alias deliberately differs:

```
` { doc: "Canonical default colour."
    type-def: "Color" }
DefaultColor: { r: 0, g: 0, b: 0 }
# value `DefaultColor` + type alias `Color`
```

`extract_type_def_name` is extended to accept `true` (returns the
binding name) alongside a string (returns the string). The Let-binding
synthesis call site passes the binding name in.

## `result-def: true | "Name"`

A new metadata key, mirroring `type-def`'s shape, capturing the
function's **return type**:

```
result-def: true       # alias name = binding name; alias type = function's return type
result-def: "Thing"    # alias name = "Thing";      alias type = function's return type
```

For a function binding `(arg-types) -> return-type`, register the alias
against the `return-type` portion. For a **non-function** binding,
`result-def` has no return type to capture: emit a warning
("`result-def` on a non-function binding has no return type to capture
— use `type-def` instead") and skip the registration.

The `true` shortcut is most useful when the binding name is already the
desired alias name (typical for `type-def: true`); for constructor-style
functions named with verbs (`make-thing`, `build-config`), the explicit
form (`result-def: "Thing"`) reads more naturally:

```
` { doc: "Build a Thing from parts."
    result-def: "Thing" }
make-thing(name, value): { name: name, value: value }
# value-binding make-thing + type alias Thing
```

### Parametric return types

For a function whose return type contains free type variables (a
genuinely parametric constructor — `make-pair(a, b): { fst: a, snd: b }`
— return type `{fst: a, snd: b}`), the registered alias carries those
free variables. eucalypt's alias system resolves an alias name to a
`Type`; aliases with free type variables work uniformly when the
variables are taken to be fresh per use (as with the quantifiers of a
polymorphic scheme). Genuine **parametric aliases** with explicitly
named type parameters (`Pair(a, b) = …`) are a separate larger feature;
this spec does not extend the alias system. Constructors with
annotated, concrete parameter types yield concrete return types — the
cleanest case.

## `target: true` — corollary

Falls out of the shorthand convention. The existing `:target` bare-
symbol shorthand expanded to `{target: <binding-name>}`; the new general
rule expands `:target` to `{target: true}` with the same semantics
("use the binding name"). The two are equivalent; the implementation
unifies them. No behaviour change for existing units.

## Mechanism

### M1. `normalise_metadata` — the general shorthand

In `normalise_metadata` (metadata.rs:38), add a branch for bare-symbol
metadata where the symbol matches a recognised key with `true`
semantics — expand to `{ key: true }`. The initial set is
`{ type-def, result-def, target }`; future keys opt in by adding to the
set and defining their `true` interpretation. The `:suppress` legacy
case is retained. Other bare symbols continue to fall through to the
target-shortcut (metadata.rs:58–64) — preserving backwards
compatibility.

### M2. `type-def` accepts `true`

Extend `extract_type_def_name` (check.rs:323) to accept either a string
literal (current) or the boolean `true` (returns the binding name). The
Let-binding synthesis call site passes the binding name in.

### M3. `result-def` extractor

Add `extract_result_def_name` mirroring `extract_type_def_name`, reading
the `result-def` key with the same `true | string` value space.

### M4. Alias registration for `result-def`

In the Let-binding synthesis (check.rs ~465–475), after determining the
binding's type, if `result-def` is present:

- If the binding's type is `Type::Function(_, return_type)`, register
  the alias `name → return_type` via `register_alias`.
- Otherwise, emit a warning and skip registration.

A declaration may legitimately carry **both** `type-def` and
`result-def`, registering two distinct aliases — one for the function
type, one for the return type. Both extractors run.

### Operator and anonymous bindings — edge note

The "binding name" interpretation of `true` uses the declaration's name
as the alias name. For operator declarations (`(l + r): __ADD(l, r)`)
the binding name is `+`, which is not a meaningful alias name; for any
anonymous form, there is no name. Both cases should emit a warning
("`type-def: true` requires a normal-identifier binding name") and skip
registration. The explicit string form remains available.

## Examples

```
# Default-named type alias from a block declaration
` :type-def
Person: { name: "Alice", age: 30 }
# Registers alias: Person = {name: string, age: number}

# Type-def with other metadata, defaulting to binding name
` { doc: "Canonical Person record"
    target: :main
    type-def: true }
Person: { name: "Alice", age: 30 }

# Explicit alias name — value named differently from the type
` { doc: "Default rendering colour."
    type-def: "Color" }
DefaultColor: { r: 0, g: 0, b: 0 }

# Constructor return-type alias (explicit name, conventional)
` { doc: "Build a Thing."
    result-def: "Thing" }
make-thing(name, value): { name: name, value: value }
# Registers alias: Thing = the return type

# Two aliases on one declaration: the function type AND its return type
` { type-def: "MakeThing"
    result-def: "Thing" }
make-thing(name, value): { name: name, value: value }
```

## Out of scope

- **`typeof` in the type DSL** — deliberately not built. The
  metadata-channel mechanisms above cover the common cases (alias-from-
  this-binding's-type, alias-from-this-function's-return-type). The
  rare "alias an arbitrary external binding's type" case stays
  expressed via hand-written `types: { Foo: "type-string" }`.
- **Parametric aliases** — aliases with named type parameters
  (`Pair(a, b) = …`) are a separate larger feature. `result-def` on a
  parametric constructor produces an alias with free type variables,
  which works uniformly but isn't a proper parametric alias.
- **Declaration-level `types:` blocks** — `types:` is already
  mechanically legal on any metadata block (`register_aliases_from_meta`
  reads from any `Meta`). Lifting the convention to place `types:` next
  to relevant declarations is a separate small change (and requires a
  pre-pass that collects aliases before the main checking walk, so
  declaration order does not matter). Independent of this spec; composes
  cleanly.

## Test plan

- `` ` :type-def `` on `Person: {…}` registers alias `Person` from the
  block's type.
- `type-def: true` in a richer block (with `doc:`, `target:`) registers
  the alias using the binding name.
- `type-def: "Color"` registers under the given name (regression).
- `result-def: "Thing"` on `make-thing(…)` registers `Thing` = the
  return type; an annotation `x: Thing` resolves to the constructor's
  return type.
- `result-def: true` on a value-named function uses the binding name as
  the alias name.
- `result-def: true` on a non-function binding warns and does not
  register.
- A declaration carrying both `type-def` and `result-def` registers
  both aliases independently.
- `` ` :target `` still works (regression); `target: true` is
  equivalent.
- Other bare symbols continue to become target shortcuts (regression).
- An operator declaration carrying `type-def: true` warns and does not
  register.
- `cargo test` green; clippy clean.

## File-by-file change summary

| File | Change |
|------|--------|
| `src/core/metadata.rs` | `normalise_metadata` — recognise bare-symbol shorthand for keys with `true` semantics (`type-def`, `result-def`, `target`); keep `:suppress` legacy and the target-shortcut fallback for unrecognised symbols |
| `src/core/typecheck/check.rs` | `extract_type_def_name` accepts `true` (returns binding name); add `extract_result_def_name`; in Let-binding synthesis, register `result-def` aliases from `Type::Function`'s codomain (warn on non-function); warn on `type-def: true` / `result-def: true` for operator/anonymous bindings |
| `tests/` | cases from the test plan |
