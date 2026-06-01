# Spec: Row Polymorphism & Dict Types (Beads A1 / A2)

**Status**: Specification — ready to implement.
**Date**: 2026-05-18
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H3,
H19), [type-system-bead-plan.md](./type-system-bead-plan.md) (TS-A1,
TS-A2), [literal-types-and-narrowing-spec.md](./literal-types-and-narrowing-spec.md).

A1 and A2 are complementary block-typing features and ship as one PR.
**Row polymorphism** (A1) types *named, heterogeneous, shape-preserving*
block operations — `merge`, lens `over`. **`Dict(T)`** (A2) types
*uniform, key-agnostic* block operations — `map-values`, `group-by`,
`values`, `keys`. Neither subsumes the other; together they close the
block-typing gap.

## Decisions taken

1. **A1 scope — annotated only.** A1 makes *annotated* row-polymorphic
   types work end to end: the prelude's block functions get
   row-polymorphic annotations, and the (already capable) unifier
   propagates row content through call sites with known argument
   shapes. **Inference of fresh row variables at lambda boundaries** —
   so an *unannotated* generic block function becomes row-polymorphic —
   is deferred to Phase B (new bead **TS-B9**, §A1.6). The benefit of
   the annotated cut is concrete: `merge`'s result stops being the
   opaque `{..}` and carries the actual combined fields.

2. **A2 spelling — `Dict(T)` only.** One DSL form, parsed like the
   existing `IO(T)`. The `{symbol: T}` alternative is *not* adopted;
   the recursive-`Json` example in the evolution doc is rewritten to
   use `Dict`.

## Background — what exists today

From a read of `src/core/typecheck/`:

- `Type::Record { fields: BTreeMap<String, Type>, open: bool,
  row: Option<TypeVarId> }` (types.rs). `row = Some(id)` names a row
  variable; `open` marks an anonymous open tail.
- **The unifier already does row variables** (unify.rs): greedy
  absorption — common fields unified covariantly, a missing field on
  one side is absorbed by the other side's row variable, an unbound
  row variable is bound to a record of the extra fields, occurs-checked.
  `apply_subst` merges a row variable's bound fields into the parent
  record. This machinery is essentially complete.
- **Subtyping ignores the `row` field** (subtype.rs): record subtyping
  is width + depth, with the open/closed rule (a closed `T` rejects an
  open `S`). The `row` variable plays no part in the subtype relation.
- **Row variables are never inferred.** Every record the checker
  *synthesises* is `Record { open: true, row: None }` (check.rs). Fresh
  row variables are never allocated.
- The DSL parser (parse.rs) accepts `{..}`, `{..r}`, `{k: T, ..r}`,
  `{k: T, ..}`.
- There is **no** `Dict` type — `{k:T,..}` means "named key `k`, plus
  unknowns"; `{..}` means "some block"; neither expresses "all values
  of type `T`, arbitrary keys".

---

## A1 — Row polymorphism in inference (annotated scope)

### A1.1 Goal

Activate the dormant row machinery. The unifier already binds and
propagates row variables, and freshening/generalisation already handle
them (§A1.3, verified) — the gap is simply that **nothing is annotated**
to exercise any of it. Annotating `merge` and lens `over` lets block
content flow through them as honest row-polymorphic types.

### A1.2 Row algebra — Leijen scoped labels

The unifier's greedy absorption is **Leijen scoped labels** (ordered
rows, labels may repeat, extras absorbed left-to-right). This is the
correct choice — it maps directly onto eucalypt's rightmost-wins block
merge — and it is already what the code does. A1 **confirms and
documents** this; no algebra change.

### A1.3 Freshening row variables — already done (verified)

A polymorphic `TypeScheme` whose body contains row variables must
**freshen those row variables per use**, exactly as it freshens ordinary
`Type::Var`s — otherwise every call site of `merge` would share the same
row variables `r`, `s` and contaminate one another.

**A codebase review (2026-05-19) found this already works.** `freshen`
(unify.rs ~378) renames row variables through the same fresh-variable
substitution it applies to `Type::Var` — its own comment says so — and
the generalisation path `infer_scheme` / `collect_free_vars`
(unify.rs ~404) already collects row variables (the `Option<TypeVarId>`
in nested `Record`s) into the quantified set. `humanise` (types.rs ~248)
also walks the `row` field.

So A1 does **not** need to build this. The task here is reduced to a
**verification test** — confirm two instantiations of a row-polymorphic
scheme get distinct row variables (a unit test; see the test plan) —
not new machinery. This makes A1 smaller than first scoped: the
substantive work is the prelude annotations (§A1.5) and confirming the
subtyping treatment (§A1.4).

### A1.4 Subtyping and consistency — `..r` behaves as `..`

On reflection the subtyping change is **minimal**, contrary to the
initial brainstorm. In the *subtype relation* a named row variable
carries no more information than an anonymous open tail: `{x:A, ..r}`
is, for `is_subtype`, "has `x:A`, may have more" — exactly `{x:A, ..}`.
The current "ignore the `row` field" behaviour is therefore **sound**
and is kept. Specifically:

- `{x:A, ..r} <: {x:A, ..s}` — true (both reduce to open `{x:A}`).
- `{x:A, ..r} <: {x:A, y:B}` — false (an open record is not a subtype
  of a closed one — the existing open/closed rule, unchanged).
- A closed `{x:A, y:B} <: {x:A, ..r}` — true by width subtyping.

So A1 adds **no new subtyping rules**; it documents that `..r` is
treated as `..` and confirms the existing open/closed handling is
correct. The row variable's identity matters only in *unification*,
where it gets bound and the binding propagates — and that already
works. (If a later audit finds a concrete unsoundness, the fix is
local; none is expected.)

### A1.5 Prelude annotations

```
` { type: "{..r} -> {..s} -> {..r,..s}" }
merge: __MERGE          # prelude.eu:361
```

`{..r,..s}` is the row-concatenation result — Leijen scoped labels make
this well-formed (rightmost wins on label clash, matching `__MERGE`'s
runtime semantics).

**DSL syntax for row concatenation.** The type-DSL parser (parse.rs)
must be extended to accept multiple row-variable tails in a record:
`{..r, ..s}` parses as a `Record` with `row` referencing both
variables. Representation: since `Record.row` is currently
`Option<TypeVarId>` (a single variable), extend it to support row
concatenation — either:

- (a) A `RowConcat(TypeVarId, TypeVarId)` form in the row position, or
- (b) Treat `{..r,..s}` as sugar that the *unifier* resolves: `merge`'s
  return type is annotated `{..r,..s}` and the unifier, after binding
  `r` and `s` at the call site, merges their bound fields into a
  single record (which it already does for single row variables).

Option (b) is preferred — it requires **no new `Type` variant**. The
DSL parser accepts `{..r, ..s}` and emits a `Record` with
`row: Some(r)` *plus* a second row variable `s` stored as extra
fields to merge. In practice, `apply_subst` already merges a bound
row variable's fields into the parent record; with two bound row
variables, it merges both. The implementation verifies this works or
falls back to option (a) if the unifier cannot handle it.

Annotate the lens `over`-family in `lib/lens.eu`
analogously — a row-*preserving* update is `{k:a,..r} -> … ->
{k:a,..r}` (the exact form per the lens kernel; the user-facing `Lens`
type stays opaque). `merge-all` (prelude.eu:1662) folds `merge` and can
carry `[{..r}] -> {..}` (the fold loses the per-element rows — honest).

Acceptance: `a merge(b)` with `a : {x: number}`, `b : {y: string}`
synthesises `{x: number, y: string}`, not `{..}`.

### A1.6 Deferred to Phase B — TS-B9

Inference of fresh row variables at lambda boundaries — so
`f(a, b): a merge(b)` with `f` unannotated infers
`f : {..r} -> {..s} -> {..r,..s}` — is **out of scope**. It requires
the checker to allocate fresh row variables when a parameter is used as
a block and generalise over them. Captured as new Phase B bead **TS-B9
"Full row-variable inference at lambda boundaries"** (added to the bead
plan; H3 in the evolution doc gains a forward note). Until then,
unannotated generic block combinators stay `any`-typed in their block
parameters — sound, just imprecise.

---

## A2 — Homogeneous block (Dict) types

### A2.1 Goal

Add a type for "a block, arbitrary symbol keys, every value of type
`T`" — the honest type of `group-by` output, uniform config sections,
and `map-values`/`values`/`keys`.

### A2.2 Representation and display

Add to `Type` (types.rs):

```rust
/// Homogeneous block: arbitrary symbol keys, every value of type T.
Dict(Box<Type>),
```

Display: `Dict({inner})` — e.g. `Dict(number)`. `humanise` recurses into
the boxed value type (add to `collect_fresh_vars` and `replace`,
mirroring `Type::List`).

### A2.3 DSL

One `parse_primary` production: `Dict` `(` *type* `)` → `Type::Dict`.
Parses exactly like the existing `IO(T)`. Add a `Dict` keyword/token
and update the grammar comment.

### A2.4 Subtyping and consistency

`is_subtype` (subtype.rs) — add:

```
(Dict(a), Dict(b))   => is_subtype(a, b),               // covariant
(Record{fields,..}, Dict(b))
     => fields.values().all(|v| is_subtype(v, b)),       // a record IS a dict
(Dict(_), Record{fields, open: true, ..}) if fields.is_empty()
     => true,                                            // Dict <: {..}
```

- `Dict` is covariant in its value type.
- A record (closed or open) is a subtype of `Dict(b)` when *every*
  field value is `<: b`. So `{a: 1, b: 2} <: Dict(number)`. For an
  *open* record the unknown tail could hold anything, so strictly only
  a *closed* record should subtype a `Dict`; a record with `open:true`
  / a row variable does **not** (its tail is unconstrained). Restrict
  the rule to `open: false`.
- `Dict(T) <: {..}` (the empty open record) — a dictionary is a block.
- `Dict(T)` is **not** a subtype of any record requiring a named key —
  no individual key is guaranteed present. (No rule → `false`.)

`is_consistent` — add the structural `Dict`/`Dict`, `Record`/`Dict`
arms so `any` flows through; keep the closed-record restriction.

### A2.5 Unification

`is_subtype` does not solve for unification variables, so the value
type of a `Dict` reaching the checker through a call must be bound by
`unify`. Add (unify.rs):

```
(Dict(a), Dict(b))   => unify(a, b),
(Dict(t), Record{fields, open: false, row: None, ..})
     => unify(t, Type::union(fields.values().cloned())),   // §6.1 union ctor
(Record …, Dict(t))  => symmetric,
```

A `Dict(t)` unified with a closed record binds `t` to the union of the
record's field types — this is what lets `map-values(f, {a:1,b:2})`
infer `f`'s domain. A `Dict` unified with an *open* / row-variable
record cannot pin the value type precisely: bind `t` to `any` (the
unknown tail defeats precision) — sound, gradual.

### A2.6 Widening from record literals

No special synthesis. A block literal `{a: 1, b: 2}` synthesises a
closed record `{a: number, b: number}` as today; it **widens** to
`Dict(number)` on use — when checked against a `Dict` annotation or
unified with a `Dict` parameter — purely via the §A2.4 subtyping rule
and the §A2.5 unify rule. Widening-on-use, consistent with literal
types (A4) and the `Tuple` treatment (A6).

### A2.7 Prelude annotations

```
` { type: "(a -> b) -> Dict(a) -> Dict(b)" }
map-values: …           # prelude.eu:1726 — map-values(f, b)

` { type: "(a -> any) -> [a] -> Dict([a])" }
group-by: …             # prelude.eu:1562

` { type: "Dict(a) -> [a]" }
values: …               # prelude.eu:1678

` { type: "Dict(a) -> [symbol]" }
keys: …                 # prelude.eu:1674

` { type: "symbol -> Dict(a) -> a" }
lookup: __LOOKUP        # prelude.eu:412
```

Note `map-values`'s `f` takes only the value (`map-second(f)` in the
body), so it is `a -> b`, not `(symbol, a) -> b`. On a `Dict(a)` the
dependent `lookup` of H4a collapses to plain `symbol -> Dict(a) -> a` —
no indexed access needed. `lookup` is also used on *named records*
(where the result is the specific field type); the `Dict` annotation
covers the homogeneous case — the heterogeneous-record indexed `lookup`
is Phase B (TS-B6) and would be a constraint/overload, not a conflict.

---

## Interaction A1 × A2

The two features are disjoint and compose cleanly:

- `Dict(T) <: {..}` — a dict is an (empty) open record; nothing more is
  claimed.
- A closed record `<: Dict(union of values)` — §A2.4.
- A record with a *row variable* does **not** subtype a `Dict` (its
  tail is unconstrained) — §A2.4's `open: false` restriction.
- `merge` stays row-polymorphic (`{..r}`); merging two `Dict`s is not
  given a special type in 6.1 — `Dict(A) merge Dict(B)` falls to the
  generic path. Acceptable; rare.

---

## Sample diagnostics

**A1 — row content flows through merge:**
```
  a: {x: 1}
  b: {y: "hello"}
  a merge(b)          # synthesises {x: number, y: string}
```
No warning. Hover shows the precise merged record type.

**A2 — Dict type mismatch:**
```
warning: type mismatch
  ┌─ src/config.eu:8:12
  │
 8│   transform(settings)
  │             ^^^^^^^^ map-values expects Dict(number), found {name: string, count: number}
  │                      field name: string is not a subtype of number
```

**A2 — widening to Dict succeeds:**
```
  {a: 1, b: 2} values   # ok — {a: number, b: number} <: Dict(number)
```

## Sequencing

One PR, A1 + A2 together. Internal order: land `Dict` (A2 — self
contained) first, then the prelude annotations for both (A1's freshening
is already in place — §A1.3), then the harness tests. No dependency on
A4/A5/A6; no dependency from them.

## Test plan

Harness tests in `tests/harness/typecheck/`; Rust unit tests per module.

**A1** — unit (regression-style, since the machinery already exists):
`freshen` produces distinct row variables for two instantiations of a
row-polymorphic scheme; `humanise` renames row variables stably.
Harness: `{x:1} merge({y:2})` synthesises `{x:number, y:string}`; a row
clash resolves rightmost-wins; an annotated `over` preserves the row.

**A2** — unit: `Dict` subtyping (covariance; closed record `<: Dict`;
open record *not* `<: Dict`; `Dict <: {..}`; `Dict` not `<:` a
named-key record); `Dict`/`Record` unification binds the value type to
the field-type union. Harness: `map-values`/`group-by`/`values`/`keys`
type-check; `{a:1,b:2}` passed to a `Dict(number)` parameter is
accepted; passed to `Dict(string)` warns.

**Regression**: `eu check lib/prelude.eu` stays warning-free; full
`cargo test` green; clippy clean.

## File-by-file change summary

| File | A1 | A2 |
|------|----|----|
| `types.rs` | — (`humanise` already walks the `row` field) | `Dict` variant + display + `humanise` |
| `subtype.rs` | document `..r` ≡ `..`; no new rules | `Dict` `<:` + consistency arms |
| `unify.rs` | — (row unification, `freshen` and `infer_scheme` already handle row variables) | `Dict`/`Dict`, `Dict`/`Record` arms |
| `check.rs` | — | route the `Dict`/`Record` value-union through `Type::union` |
| `parse.rs` | — | `Dict(T)` token + production; grammar comment |
| `lib/prelude.eu` | annotate `merge`, `merge-all` | annotate `map-values`, `group-by`, `values`, `keys`, `lookup` |
| `lib/lens.eu` | row-preserving annotations for the `over`-family | — |
| `tests/harness/typecheck/` | row-flow tests | `Dict` tests |
