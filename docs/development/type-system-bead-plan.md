# Type System Evolution — Bead Hierarchy Plan

**Status**: Plan for import into beads (`bd`)
**Date**: 2026-05-17
**Branch**: `type-system-exploration`
**Companion**: [type-system-evolution.md](./type-system-evolution.md) — the
hypotheses (H1–H19) referenced throughout.

This document defines a bead hierarchy to be created locally. `bd` is
not available in the cloud container and the `.beads` database is not
in the repo, so the actual beads must be created on the machine that
holds the real database — this is the import plan.

## How to read the IDs

The `TS-*` identifiers below are **placeholder slugs**, not real bead
IDs. On `bd create`, real `eu-xxxx` IDs are allocated. Use the
placeholders only to wire up the parent/child and dependency
structure during import.

Existing beads keep their real IDs (`eu-oh3p`, `eu-ggr9`, etc.) — see
§4 for their disposition.

## Release mapping

| Phase | Epic | Milestone |
|-------|------|-----------|
| Phase A — close the existing system | `TS-A` | **6.1** |
| Phase B — expressiveness | `TS-B` | **7.0** |

Tag every child with its phase milestone and a `type-system` label
(plus `phase-a` / `phase-b`). Use whatever milestone/label mechanism
the local `bd` setup provides.

---

## 1. The tree

```
TS-ROOT  Type system evolution                              (epic)
│
├── TS-A   Phase A — close the existing system               [6.1]
│   ├── TS-A1   Row polymorphism in inference            (← eu-z9zz.5)
│   ├── TS-A2   Homogeneous block (Dict) types
│   ├── TS-A3   Recursive types (equirecursive)
│   ├── TS-A4   Literal types — string & bool
│   ├── TS-A5   Flow-sensitive narrowing + type subtraction
│   ├── TS-A6   NonEmpty refinement + branch-narrowing
│   ├── TS-A7   First-class alias references in the type DSL
│   ├── TS-A8   Per-module type summary cache (in-memory)
│   ├── TS-A9   Monadic block binding type hints          (← eu-ggr9)
│   └── TS-A10  Monadic bound-variable element-type hints (← eu-z9zz.10)
│
└── TS-B   Phase B — expressiveness                          [7.0]
    ├── TS-B1   Higher-kinded type variables
    ├── TS-B2   Structural operator constraints
    ├── TS-B3   Metadata-channel typing
    ├── TS-B4   Typed van Laarhoven lens internals (decision-gated)
    ├── TS-B5   Partial(T) opaque type
    ├── TS-B6   Dependent record indexed access
    ├── TS-B7   Persistent type summaries + LSP cross-file inference
    └── TS-B8   Type the monad namespaces with HKT          (← eu-dme3)
```

Three levels: root epic → phase epic → feature bead. A few feature
beads list sub-tasks inline in §2/§3; create those as sub-issues if
the grain is useful, otherwise keep them as checklist items.

---

## 2. Phase A — `TS-A` (milestone 6.1)

**Epic.** *Phase A — close the existing gradual type system.*
Completes work already half-built in the representation (row
variables, literal symbols) and lands the existing monad-checking
beads, so the gradual checker feels finished for everyday
data-transformation work. No new type-theoretic ambition. Parent:
`TS-ROOT`.

### Summary table

| ID | Title | Size | Prio | Depends on | H-ref |
|----|-------|------|------|-----------|-------|
| TS-A1 | Row polymorphism in inference | M | P1 | — | H3 |
| TS-A2 | Homogeneous block (Dict) types | S | P1 | — | H19 |
| TS-A3 | Recursive types (equirecursive) | M | P1 | A1, A2 (soft) | H14 |
| TS-A4 | Literal types — string & bool | S | P1 | — | H16 |
| TS-A5 | Flow-sensitive narrowing + type subtraction | M | P2 | A4 | H15 |
| TS-A6 | NonEmpty refinement + branch-narrowing | S | P2 | A5 | H7a, H7b, H7c |
| TS-A7 | First-class alias references in the type DSL | S | P2 | — | H12a |
| TS-A8 | Per-module type summary cache (in-memory) | M | P2 | — | H9 |
| TS-A9 | Monadic block binding type hints | M | P1 | — | H4c |
| TS-A10 | Monadic bound-variable element-type hints | S | P2 | A9 | — |

### TS-A1 — Row polymorphism in inference

Make the existing `Type::Record` row variable a working part of
inference, not just a representable shape. Adopt Leijen scoped labels
(matches eucalypt's rightmost-wins merge). Row-polymorphic types for
`merge` and row-*preserving* functions (`over`-family).

**Scope**: unifier extension for row variables; inference of row vars
at lambda boundaries; prelude annotations for `merge` and row-stable
block functions.
**Done when**: `merge`/`over` carry row-polymorphic types; row content
demonstrably flows through; `eu check lib/prelude.eu` clean; harness
tests for row-preserving inference pass.
**Note**: this *is* existing bead `eu-z9zz.5` — re-parent it here (see
§4), don't create a fresh bead.

### TS-A2 — Homogeneous block (Dict) types

Add `Dict(T)` — block with arbitrary symbol keys, all values of type
`T` — distinct from named records. Closes the gap that makes
`map-values`, `group-by`, `values`, dict-`lookup` un-typeable today.

**Scope**: new `Type::Dict(Box<Type>)`; covariance; subtyping rules
(`{a:A,b:B} <: Dict(A|B)`, `Dict(T) <: {..}`); one DSL production;
display; widening-on-use from record literals; prelude annotations.
**Done when**: `map-values`, `group-by`, `values`, `keys` carry
`Dict`-based types; subtyping tests pass.
**Coordinate with TS-A1** — adopt together; complementary, not
dependent.

### TS-A3 — Recursive types (equirecursive)

`Type::Mu` plus equirecursive equality/subtyping (memoised
pair-walking). Type-alias self-reference resolves to a `Mu` form.
Enables `Json`, `Tree`-shaped types.

**Scope**: `Mu` constructor; recursion detection in the alias
resolver; coinductive subtyping with an assumed-pairs set; µ-aware
printer (no infinite unfolding).
**Done when**: a recursive `Json` alias type-checks; subtyping over
recursive types terminates; round-trip display is finite.
**Soft dependency**: the `{symbol: Json}` use case wants A1+A2; the
`Mu` core itself is independent.

### TS-A4 — Literal types — string & bool

Extend literal singletons beyond `LiteralSymbol` to `LiteralString`
and `LiteralBool`. Subtype rule `LiteralX(v) <: X`. Widen-on-use.

**Scope**: two `Type` variants; subtyping; literal synthesis; widening
at annotation/recursive-call boundaries.
**Done when**: string/bool literals synthesise literal types; widening
keeps existing tests green. (`LiteralNumber` explicitly out of scope.)
**Foundational** — A5 and A6 depend on it.

### TS-A5 — Flow-sensitive narrowing + type subtraction

Branch-sensitive narrowing via type predicates (`number?`, `null?`,
`nil?`, …) and a `T - U` subtraction form over unions.

**Scope**: predicate→narrowing table; union subtraction; **branch-
combinator recognition table** — narrowing is not syntactic, it is a
special case in `synthesise_app` keyed on a fixed set of branch
functions (`if`, `then`, `cond`, `||`, `&&`); silently (soundly) skip
when the callee is a rebound non-prelude function.
**Done when**: `if(x null?, …, …)` narrows `x` in each branch; the
recognition table covers `if`/`then`/`cond`; rebinding `if` disables
narrowing without false positives.

### TS-A6 — NonEmpty refinement + branch-narrowing

`NonEmpty([a])` as a thin refinement constructor; `head`/`tail`-class
functions typed against it. Branch-narrowing integration (after a
`nil?` test). Includes the H7b sub-task: inliner suppresses refinement
warnings for values it can prove satisfy the refinement.

**Scope**: `NonEmpty` constructor; construction sites (`cons`, literal
non-empty lists, post-`nil?` branch); H7b constant-folded suppression.
**Done when**: `[] head` warns; `[1] head` does not; a post-`nil?`
branch narrows to `NonEmpty`.
**Depends on TS-A5** for the branch-narrowing machinery.

### TS-A7 — First-class alias references in the type DSL

Make alias references inside `type:` strings first-class to tooling.
Aliases stay in metadata — no new binding form.

**Scope**: type-string parser records source spans for identifiers;
LSP indexes `types:` blocks and `type-def:` declarations into an
alias-name → definition-site map; wire go-to-definition, hover,
rename.
**Done when**: go-to-def/hover/rename work for an alias referenced in
a `type:` string.

### TS-A8 — Per-module type summary cache (in-memory)

Cache the inferred/annotated types of each module's exports; importers
load summaries instead of re-checking. In-memory only at this stage
(persistence is TS-B7).

**Scope**: per-module export type summary; content-addressed in-memory
cache keyed on module hash; invalidation on upstream change.
**Done when**: re-checking a multi-file project reuses unchanged
module summaries; LSP re-check latency drops measurably.

### TS-A9 — Monadic block binding type hints

Land the typed-monad-metadata work: the `monad:` metadata declares the
wrapper type, the desugarer injects `__type_hint` nodes on monadic
block bindings, the checker warns on wrong-typed bindings.

**Scope**: per [monad-type-checking-spec.md](./monad-type-checking-spec.md).
**Done when**: acceptance criteria in that spec pass.
**Note**: this *is* existing bead `eu-ggr9` — re-parent it here (see
§4). It may carry its own sub-beads; bring them along.

### TS-A10 — Monadic bound-variable element-type hints

Infer the *unwrapped* element type of a monadic block's bound variable
from the monad wrapper type and annotate the lambda parameter — so `x`
in `{ :for x: [1,2,3] }` is `number`, not `any`.

**Scope**: per monad-type-checking-spec.md §11 / §7 third bullet.
**Done when**: bound variables inside monadic blocks synthesise their
unwrapped element type; inlay hints reflect it.
**Note**: this *is* existing bead `eu-z9zz.10` — re-parent it here.
**Depends on TS-A9.**

---

## 3. Phase B — `TS-B` (milestone 7.0)

**Epic.** *Phase B — type-system expressiveness.* Genuinely new theory
in the codebase: higher-kinded types, structural constraints, metadata
typing, dependent indexed access. End state: the prelude can be
honestly typed end to end (`monad()`, `min`/`max`); user-defined
monads inherit correct types. Parent: `TS-ROOT`.

### Summary table

| ID | Title | Size | Prio | Depends on | H-ref |
|----|-------|------|------|-----------|-------|
| TS-B1 | Higher-kinded type variables | M/L | P2 | — | H1 |
| TS-B2 | Structural operator constraints | M | P2 | B1 | H10 |
| TS-B3 | Metadata-channel typing | M/L | P3 | — | H2 (prereq) |
| TS-B4 | Typed van Laarhoven lens internals | M | P3 | B1, B2, B3 | H2b |
| TS-B5 | Partial(T) opaque type | S/M | P2 | — | H6b |
| TS-B6 | Dependent record indexed access | M | P2 | TS-A4 | H4a, H4b |
| TS-B7 | Persistent type summaries + LSP cross-file inference | M | P2 | TS-A8 | H9 |
| TS-B8 | Type the monad namespaces with HKT | M | P2 | B1 | H1 (apply) |

### TS-B1 — Higher-kinded type variables

A minimal kind system: `Type::App(Type, Type)`, `Kind` (`*` and
`Kind -> Kind`), kind-aware variable allocation in the unifier. Lets
type variables range over `* -> *` constructors. DSL gains constructor
application (`m a`) and optional explicit kinds.

**Scope**: kind representation; `App` type; kind-aware unification;
DSL grammar for application and `forall (m :: * -> *)`.
**Done when**: `monad()` can be given its HKT signature and
type-checks; freshening preserves kinds.
**Sub-task**: HKT-annotate `monad()`'s derived combinators — feeds
TS-B8.

### TS-B2 — Structural operator constraints

`Vec<Constraint>` on `TypeScheme` (already reserved) becomes live.
Constraints reference operator/function names structurally — no
classes. Multiple constraints per scheme, comma-separated before `=>`.

**Scope**: parse `fn(args), fn(args) => body` constraint prefix;
resolve constraints at call sites by finite enumeration over declared
overloads; conjunction of multiple constraints.
**Done when**: `min`/`max`/comparison carry constraint-based types;
union-overload annotations still accepted (forwards-compatible).
**Depends on TS-B1** (shared constraint/kind resolution machinery).

### TS-B3 — Metadata-channel typing

Give the type system a way to describe a value's metadata. Justified
on its own merits — fixity/precedence metadata, `target`, the `type:`
field itself — not only as a lens prerequisite.

**Scope**: a metadata-shape form in the type representation; checking
metadata against it; interaction with the gradual boundary.
**Done when**: a value's metadata can be typed and checked; design
note covers `with-meta`/`//` flow.
**Decision-gated** — see open question 7 in type-system-evolution.md.
If declined, this bead and TS-B4 are closed as won't-do.

### TS-B4 — Typed van Laarhoven lens internals

Type the *bodies* in `lib/lens.eu` — the encoding is metadata-dispatch
(`fmap`/`pure`/`ap`), so the honest type needs metadata typing plus
HKT plus structural constraints. User-facing `Lens`/`Traversal` stay
opaque regardless.

**Scope**: internal lens kernel type; check `lib/lens.eu` bodies;
internal types do not leak to users.
**Done when**: `eu check lib/lens.eu` exercises the internal kernel;
user-facing lens types unchanged.
**Depends on TS-B1, TS-B2, TS-B3.** Decision-gated with TS-B3.

### TS-B5 — Partial(T) opaque type

An opaque constructor for fallible functions (those that can raise
`ExecutionError`). DSL sugar `T?`. Documents partiality consistently;
zero runtime cost.

**Scope**: `Partial`/`T?` type; annotate partial prelude functions;
gradual-boundary rule.
**Done when**: partial prelude functions are annotated; calling them
where a total value is expected warns.

### TS-B6 — Dependent record indexed access

`lookup(k, b)` with literal `k` returns the exact field type `r[k]`;
literal `k` absent from a known row produces a **static warning**
(key typo); non-literal `k` falls back to `any`. Includes the H4b
sub-task: literal-index access on tuples/short lists.

**Scope**: indexed-access resolution for literal symbol keys;
missing-key warning; tuple index access.
**Done when**: `lookup(:naem, person)` warns; `lookup(:name, person)`
yields the field type.
**Depends on TS-A4** (literal types) — cross-phase dependency.

### TS-B7 — Persistent type summaries + LSP cross-file inference

Persist the TS-A8 summaries (content-addressed on disk) and wire the
LSP to do cross-file inference: an upstream change invalidates only
downstream summaries.

**Scope**: on-disk summary store; invalidation graph; LSP integration.
**Done when**: type info persists across sessions; cross-file
diagnostics update incrementally.
**Depends on TS-A8.**

### TS-B8 — Type the monad namespaces with HKT

Apply HKT (TS-B1) to give `io`, `for`, `random`, `state` and the
`monad()` output their proper polymorphic types, so the desugared
`bind` chains type-check directly (superseding the TS-A9 hint
mechanism for *checking* — the `monad:` field lives on for desugaring
and tooling).

**Scope**: HKT signatures for the monad namespaces and `monad()`
derived combinators; verify monadic blocks check without `__type_hint`
injection.
**Done when**: `{ :for x: 42 }` warns via direct checking; user
monads built with `monad()` inherit correct types.
**Note**: this *is* existing bead `eu-dme3` — re-parent it here.
**Depends on TS-B1.**

---

## 4. Existing beads — disposition

The hypotheses cite five existing beads. They must be reconciled into
the new hierarchy. **Verify each against its real state before
acting** — the recommendations below assume the base gradual type
system (per [gradual-typing-spec.md](./gradual-typing-spec.md)) is
substantially implemented, which the presence of `src/core/typecheck/`
and a working `eu check` indicates.

| Bead | What it is | Recommended disposition | Milestone |
|------|-----------|-------------------------|-----------|
| `eu-oh3p` | Gradual typing spec | **Close as done** — spec delivered, base system implemented. `TS-ROOT` + the evolution doc are the continuation. | — |
| `eu-z9zz` | Gradual typing implementation epic | **Close as done** once children below are re-homed (base system shipped). If not fully shipped, keep open until it is; future children move to `TS-A`. | 6.1 |
| `eu-z9zz.5` | Row variables | **Re-parent → `TS-A1`**, retitle to match. Preserves design history. | 6.1 |
| `eu-z9zz.10` | Monadic bound-variable type hints | **Re-parent → `TS-A10`**. | 6.1 |
| `eu-ggr9` | Typed monad metadata for monadic block checking | **Re-parent → `TS-A9`**. Bring any sub-beads along. Must land for 6.1. | 6.1 |
| `eu-dme3` | Type the monad functions directly | **Re-parent → `TS-B8`**. Its full ambition needs HKT, so it belongs in Phase B. | 7.0 |

If a bead is already closed, just record the link (`TS-A1` references
`eu-z9zz.5`, etc.) rather than re-parenting.

---

## 5. Dependency summary

Hard dependencies (blocker → blocked):

```
TS-A4  ──▶ TS-A5  ──▶ TS-A6
TS-A9  ──▶ TS-A10
TS-A4  ──▶ TS-B6
TS-A8  ──▶ TS-B7
TS-B1  ──▶ TS-B2
TS-B1  ──▶ TS-B8
TS-B1, TS-B2, TS-B3  ──▶ TS-B4
```

Soft / coordinate (not blockers):

- `TS-A1` ↔ `TS-A2` — adopt together; complementary.
- `TS-A3` benefits from `TS-A1` + `TS-A2` for the `{symbol: Json}`
  case; the `Mu` core is independent.
- `TS-B8` should follow `TS-A9`/`TS-A10` so monadic-block checking
  exists before HKT supersedes the hint mechanism.

Critical path to 6.1: `TS-A4 → TS-A5 → TS-A6`. Everything else in
Phase A is parallelisable.

---

## 6. Import procedure

1. Create `TS-ROOT` (epic).
2. Create `TS-A` and `TS-B` as children of `TS-ROOT`; set milestones
   6.1 and 7.0.
3. Create `TS-A2`–`TS-A8`, `TS-B1`–`TS-B7` as children of their phase
   epic. (`TS-A1`, `TS-A9`, `TS-A10`, `TS-B8` are re-homed existing
   beads — see step 5.)
4. Set the hard dependencies from §5.
5. Re-parent the existing beads per §4: `eu-z9zz.5 → TS-A1`,
   `eu-ggr9 → TS-A9`, `eu-z9zz.10 → TS-A10`, `eu-dme3 → TS-B8`.
   Close `eu-oh3p` and `eu-z9zz` as recommended.
6. Apply labels: `type-system` to all; `phase-a` / `phase-b` per
   phase. Set priorities from the §2/§3 tables.

After import, the next step is to brainstorm and spec each Phase A
bead in detail.
