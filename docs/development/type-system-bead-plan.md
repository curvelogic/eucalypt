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
│   ├── TS-A9   Monadic block binding type hints          (← eu-ggr9)
│   └── TS-A10  Monadic bound-variable element-type hints (← eu-z9zz.10)
│
└── TS-B   Phase B — expressiveness                          [7.0]
    ├── TS-B1   Higher-kinded type variables
    ├── TS-B2   Structural operator constraints
    ├── TS-B3   Metadata-channel typing (won't-do)
    ├── TS-B4   Typed van Laarhoven lens internals (won't-do)
    ├── TS-B5   Partial(T) opaque type
    ├── TS-B6   Dependent record indexed access
    ├── TS-B7   Prelude type-summary cache
    ├── TS-B8   Type the monad namespaces with HKT          (← eu-dme3)
    └── TS-B9   Full row-variable inference at lambda boundaries
```

Three levels: root epic → phase epic → feature bead. A few feature
beads list sub-tasks inline in §2/§3; create those as sub-issues if
the grain is useful, otherwise keep them as checklist items.

> **Revision note (2026-05-18).** TS-A8 ("per-module type summary
> cache, in-memory") has been **withdrawn from Phase A** and folded
> into TS-B7. A sound cache requires checking the prelude and each unit
> *standalone* against seeded summaries — a per-module-checking
> restructure, not a `HashMap` — because pruning runs *before* the
> checker and removes a different prelude subset for every user file
> (see §3 TS-B7 and evolution-doc H9). TS-B9 is **new**: the
> fresh-row-variable *inference* deferred out of the TS-A1 spec.

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

Branch-sensitive narrowing via type predicates (`number?`, `string?`,
`nil?`, …; null-discrimination via the `✓` not-null operator) and a
`T - U` subtraction form over unions.

**Scope**: predicate→narrowing table; union subtraction; **branch-
combinator recognition table** — narrowing is not syntactic, it is a
special case in `synthesise_app` keyed on a fixed set of branch
functions (`if`, `then`, `cond`, `||`, `&&`); silently (soundly) skip
when the callee is a rebound non-prelude function.
**Done when**: `if(x number?, …, …)` narrows `x` in each branch; the
recognition covers `if`/`then`/`cond`; rebinding `if` disables
narrowing without false positives. (The spec reworked recognition from
a name table to *structural* recognition — see it for the final
design.)

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

### TS-A8 — Per-module type summary cache — **withdrawn, folded into TS-B7**

Originally scoped as a small in-memory cache of the prelude's export
types. Investigation showed the modest form is **unsound**: `eliminate`
(dead-binding pruning) runs *before* the checker, so the prelude as the
checker sees it is pruned to a different subset for every user file —
a cache built from one run is missing bindings for the next. The sound
form checks the prelude and each unit *standalone* against seeded
summaries (a per-module-checking restructure), which is precisely
TS-B7's architecture. The in-memory / persistent split therefore no
longer makes sense; TS-B7 absorbs the whole thing. For 6.1 the prelude
is re-checked each run — a one-shot cost, acceptable for a CLI check;
the latency that matters (LSP re-check on every keystroke) is TS-B7's
remit. See evolution-doc H9.

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
in the codebase: higher-kinded types, structural constraints,
dependent indexed access. End state: the prelude can be honestly typed
end to end (`monad()`, `min`/`max`); user-defined monads inherit
correct types. Parent: `TS-ROOT`.

### Summary table

| ID | Title | Size | Prio | Depends on | H-ref |
|----|-------|------|------|-----------|-------|
| TS-B1 | Higher-kinded type variables | M/L | P2 | — | H1 |
| TS-B2 | Structural operator constraints | M | P2 | — | H10 |
| TS-B3 | Metadata-channel typing | — | — | — | **won't-do** |
| TS-B4 | Typed van Laarhoven lens internals | — | — | — | **won't-do** |
| TS-B5 | Partial(T) opaque type | S/M | P2 | — | H6b |
| TS-B6 | Dependent record indexed access | M | P2 | TS-A4 | H4a, H4b |
| TS-B7 | Prelude type-summary cache | S/M | P2 | — | H9 |
| TS-B8 | Type the monad namespaces with HKT | M | P2 | B1 | H1 (apply) |
| TS-B9 | Full row-variable inference at lambda boundaries | M | P3 | TS-A1 | H3 |

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
**Independent of TS-B1** — constraints are value-level (operators),
kinds are type-level; they share no essential machinery and B2 may
proceed in parallel. See the spec.

### TS-B3 — Metadata-channel typing — **won't-do (for now)**

**Closed as won't-do.** The genuine feature — a `Type::WithMeta`
lattice form, metadata subtyping, metadata flowing through inference —
was justified only by TS-B4, and the original plan gated the two
together. With TS-B4 won't-do there is no consumer that needs metadata
to *flow* through the type lattice, so the machinery would be
complexity without use. A fallback "metadata schema validation" pass
(linting metadata blocks against known-key shapes) was considered and
also dropped — it overlaps validation the cook and loader passes
already perform, and does not earn a bead. Revisit only if a feature
emerges that genuinely needs metadata in the type lattice.

### TS-B4 — Typed van Laarhoven lens internals — **won't-do**

**Closed as won't-do.** Typing the metadata-dispatch lens kernel in
`lib/lens.eu` would need the full B1+B2+B3 package for a payoff that
affects one library file's internals — complexity not worth its
keep. `Lens`/`Traversal` stay opaque at the interface (which is enough
for users) indefinitely. Recorded here for traceability; create no
active bead, or create it already closed.

*(Original scope, for reference:)*
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

### TS-B7 — Prelude type-summary cache

Absorbs the withdrawn TS-A8, scoped to the **prelude only**. Check the
prelude *standalone* once, producing a summary of its binding type
schemes and its type aliases; cache it (keyed on the prelude source
hash); seed every user-file check with it so the prelude is not
re-walked. User files are checked standalone against the seed.

Scoped to the prelude because the prelude's types are *stable* (it is
fixed, merged first, and references only itself), whereas general
per-user-module summaries are not guaranteed stable under positional
merge-override and offer a small payoff. The prelude is also the
dominant cost. LSP responsiveness — the point of typing in a tooling-
first language — is the motivation.

**Scope**: standalone prelude check → `PreludeSummary` (binding schemes
+ aliases); hash-keyed in-memory cache; a seeded standalone user-file
check path; LSP re-check off the cache.
**Done when**: the prelude is checked once per process, not per file;
seeded checking is behaviour-preserving (same warnings as the merged
check); LSP re-check latency drops measurably.

### TS-B8 — Type the monad namespaces with HKT

Apply HKT (TS-B1) to give the monad namespaces (`io`, `for`, `random`,
`let` — there is no `state` monad) and the `monad()` output their
proper polymorphic types, so the desugared `bind` chains type-check
directly (superseding the TS-A9 hint mechanism for *checking* — the
`monad:` field lives on for desugaring and tooling). `for`/`let`/
`random` are built via `monad()`, so they inherit once `monad()` is
typed.

**Scope**: HKT signatures for the monad namespaces and `monad()`
derived combinators; verify monadic blocks check without `__type_hint`
injection.
**Done when**: `{ :for x: 42 }` warns via direct checking; user
monads built with `monad()` inherit correct types.
**Note**: this *is* existing bead `eu-dme3` — re-parent it here.
**Depends on TS-B1.**

### TS-B9 — Full row-variable inference at lambda boundaries

Deferred out of TS-A1. A1 makes *annotated* row-polymorphic types work
(the unifier already propagates row content); B9 adds the *inference*
— the checker allocates fresh row variables when a parameter is used as
a block and generalises over them, so an unannotated generic block
combinator (`f(a, b): a merge(b)`) infers a row-polymorphic type
instead of leaving its block parameters `any`.

**Scope**: fresh row-variable allocation at lambda boundaries;
generalisation over row variables; bidirectional integration.
**Done when**: an unannotated function that merges/extends its block
parameters infers a row-polymorphic type; row content flows through
unannotated block code.
**Depends on TS-A1.** See [row-polymorphism-and-dict-spec.md](./row-polymorphism-and-dict-spec.md)
§A1.6 and evolution-doc H3.

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
TS-A1  ──▶ TS-B9
TS-B1  ──▶ TS-B8
```

(TS-B2 is **not** dependent on TS-B1 — see §3. TS-B4 is won't-do, so
its former `B1,B2,B3 ──▶ B4` dependency is dropped.)

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
3. Create `TS-A2`–`TS-A7`, `TS-B1`, `TS-B2`, `TS-B5`–`TS-B7`, `TS-B9`
   as children of their phase epic. (`TS-A1`, `TS-A9`, `TS-A10`,
   `TS-B8` are re-homed existing beads — see step 5. `TS-A8` is
   withdrawn — see §2. `TS-B3` and `TS-B4` are won't-do — see §3;
   create them already closed, or not at all.)
4. Set the hard dependencies from §5.
5. Re-parent the existing beads per §4: `eu-z9zz.5 → TS-A1`,
   `eu-ggr9 → TS-A9`, `eu-z9zz.10 → TS-A10`, `eu-dme3 → TS-B8`.
   Close `eu-oh3p` and `eu-z9zz` as recommended.
6. Apply labels: `type-system` to all; `phase-a` / `phase-b` per
   phase. Set priorities from the §2/§3 tables.

## 7. Specifications

Detailed specs exist for every Phase A bead and every actioned Phase B
bead:

| Beads | Specification |
|-------|---------------|
| A4, A5, A6 | [literal-types-and-narrowing-spec.md](./literal-types-and-narrowing-spec.md) |
| A1, A2 | [row-polymorphism-and-dict-spec.md](./row-polymorphism-and-dict-spec.md) |
| A3 | [recursive-types-spec.md](./recursive-types-spec.md) |
| A7 | [alias-reference-tooling-spec.md](./alias-reference-tooling-spec.md) |
| A9, A10 | [monad-type-checking-spec.md](./monad-type-checking-spec.md) |
| B1 | [higher-kinded-types-spec.md](./higher-kinded-types-spec.md) |
| B2, B8 | [operator-constraints-and-monad-types-spec.md](./operator-constraints-and-monad-types-spec.md) |
| B5, B6 | [partiality-and-indexed-access-spec.md](./partiality-and-indexed-access-spec.md) |
| B7 | [prelude-type-cache-spec.md](./prelude-type-cache-spec.md) |
| B9 | [row-inference-spec.md](./row-inference-spec.md) |

**TS-B3** (metadata-channel typing) and **TS-B4** (typed van Laarhoven
lens internals) are both **won't-do** — see §3. They were gated
together; with no consumer that needs metadata typed in the lattice,
neither earns its keep. `Lens`/`Traversal` stay opaque for users
indefinitely.
