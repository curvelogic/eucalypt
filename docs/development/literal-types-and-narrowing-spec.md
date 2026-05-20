# Spec: Literal Types, Flow Narrowing & NonEmpty (Beads A4 / A5 / A6)

**Status**: Specification — ready to implement.
**Date**: 2026-05-17
**Branch**: `type-system-exploration`
**Companions**: [type-system-evolution.md](./type-system-evolution.md) (H16,
H15, H7), [type-system-bead-plan.md](./type-system-bead-plan.md) (beads
A4, A5, A6), [gradual-typing-spec.md](./gradual-typing-spec.md).

This is the Phase A **critical path**: literal types (A4) → flow-sensitive
narrowing (A5) → `NonEmpty` refinement (A6). The three ship in order, each
its own PR. Together they let the checker catch `head([])`-class bugs and
discriminate unions by branch.

## Decisions taken

Two scoping questions were settled before writing this spec:

1. **Narrowing recognition scope (A5)** — *Core + prelude guards*. The
   checker recognises `if`, `then`, `cond` and the boolean operators
   (`and`/`∧`, `or`/`∨`) as branch forms; conditions are the type
   predicates (`number?`, `string?`, …), `nil?` and `non-nil?`. This
   covers every guard pattern the prelude itself uses. *Not* in scope for
   6.1: equality-against-literal narrowing (`x = :active`), `has(:k, …)`,
   `when`/`unless`, and `.field`-path narrowing.

2. **Partial list functions (A6)** — *annotate now*. `head`, `tail` and
   the other partial list functions are retyped with `NonEmpty` in
   `lib/prelude.eu` as part of 6.1. This is the headline payoff and it
   forces the A5 scope above (the prelude must stay warning-free).

## Background — what exists today

The checker is a freshen-and-unify bidirectional checker
(`src/core/typecheck/`). Relevant existing facts:

- `Type` (types.rs) already has `LiteralSymbol(String)`. There is **no**
  `LiteralString`/`LiteralBool`.
- `synthesise_primitive` (check.rs ~1022) maps `Primitive::Sym` →
  `LiteralSymbol`, but `Primitive::Str` → `Type::String` and
  `Primitive::Bool` → `Type::Bool` (base types, literal info discarded).
- Subtyping (subtype.rs) has `LiteralSymbol(_) <: Symbol`; consistency and
  `unify` both treat `LiteralSymbol ↔ Symbol` as compatible.
- `if` is `__IF` (an intrinsic exposed as a prelude binding); `then`,
  `cond` are ordinary prelude functions; `and`/`or` are `__AND`/`__OR`.
  The checker does **no** special-casing of any of them — `if(c, a, b)`
  is just an application of three sub-expressions, both branches always
  synthesised.
- `synthesise_app` (check.rs ~675) handles application generically.
- **Critical constraint**: `lookup_bound` (check.rs ~203) resolves bound
  variables by indexing `scope_stack` *directly* with the de Bruijn
  `bv.scope`. Pushing an extra scope frame shifts every index. Narrowing
  therefore **must not** push onto `scope_stack` — see §A5.4.
- List-literal synthesis (check.rs ~409): empty → `[any]`; 2–4 elements →
  `Tuple`; otherwise → `List`. Tuples are already `<: List`.
- There is no union smart-constructor — unions are built ad hoc with
  dedup but no absorption. See §6.1.

---

## A4 — Literal types for strings and booleans

### A4.1 Goal

Extend the literal-singleton treatment that `LiteralSymbol` already has to
string and boolean literals, so the checker can express and check
finite enumerations like `"r" | "w" | "rw"`. **`LiteralNumber` is out of
scope** (arithmetic on literal numbers is a type-level computation not
worth the cost — per H16).

### A4.2 Representation

Add two variants to `Type` (types.rs), beside `LiteralSymbol`:

```rust
/// Literal string type: a specific string value.
LiteralString(String),
/// Literal boolean type: a specific boolean value.
LiteralBool(bool),
```

`humanise` (types.rs ~248) needs no change — its `collect_fresh_vars`
and `replace` both fall through `_` arms for variant-free types.

### A4.3 Display

Add `Display` arms (types.rs ~155):

- `LiteralString(s)` → `"s"` (double-quoted; escape embedded `"` and `\`).
- `LiteralBool(true)` → `true`; `LiteralBool(false)` → `false`.

### A4.4 Synthesis

In `synthesise_primitive` (check.rs ~1022):

```rust
Primitive::Str(s)  => Type::LiteralString(s.clone()),
Primitive::Bool(b) => Type::LiteralBool(*b),
```

This is **always-on**, mirroring `LiteralSymbol`: every string and boolean
literal synthesises its literal type. Widening to the base type happens
*on use* — via subtyping and unification (below) — never on construction.
This keeps reasoning local and is consistent with how symbols already
behave.

### A4.5 Subtyping and consistency

`is_subtype` (subtype.rs) — add beside the `LiteralSymbol` arm (~line 65):

```
(LiteralString(_), String) => true,
(LiteralBool(_),   Bool)   => true,
```

Equal literals are covered by the reflexivity check at the top of
`is_subtype`; unequal literals fall through to `_ => false`. The base type
is **not** a subtype of a literal (`String </: "foo"`).

`is_consistent` (subtype.rs ~186) — extend the literal arm to both new
pairs, in both directions (mirrors the existing `LiteralSymbol`/`Symbol`
arm):

```
(LiteralString(_), String) | (String, LiteralString(_)) => true,
(LiteralBool(_),   Bool)   | (Bool,   LiteralBool(_))   => true,
```

### A4.6 Unification

`unify` (unify.rs ~84) — add beside the `LiteralSymbol` arm:

```
(LiteralString(_), String) | (String, LiteralString(_)) => Ok(()),
(LiteralBool(_),   Bool)   | (Bool,   LiteralBool(_))    => Ok(()),
```

Two *different* literal strings/bools do **not** unify (no arm → existing
mismatch error), exactly as for literal symbols.

### A4.7 DSL syntax

Extend the type-DSL parser (parse.rs) and its grammar comment (~lines
8–28):

- **Literal string**: a double-quoted string in a type position →
  `LiteralString`. Requires a `Token::StringLit(String)` in the DSL lexer
  (handle `\"` and `\\`). New `parse_primary` arm.
- **Literal bool**: `true` / `false` keyword tokens → `LiteralBool`. The
  DSL already lexes the `bool` keyword; add `true`/`false` similarly. New
  `parse_primary` arms.

Annotations that newly become expressible:

```
` { type: "\"r\" | \"w\" | \"rw\" -> any" }
open-mode(m): ...
```

### A4.8 Companion: union absorption

Always-on literal synthesis means unions of the form `LiteralString("y")
| string` will arise (e.g. a function returning a literal in one branch
and a computed string in another). These are *sound* — `is_subtype`
handles them — but ugly and noisy in diagnostics. The fix is the union
smart-constructor in §6.1, which absorbs any `LiteralX(v)` when its base
`X` is present in the same union. A4 should land §6.1 alongside it.

### A4.9 Blast radius

- `synthesise_block` records literal field types: `{ mode: "r" }` →
  `{mode: "r", ..}`. Harmless — `"r" <: string`, so any check against
  `{mode: string}` passes; LSP hover gets *more* precise.
- List literals: 2–4 string elements → `Tuple` of literal strings (kept —
  tuples carry more information); ≥5 → see §A6.3, the list path now
  produces `NonEmpty` of a (deduplicated, absorbed) element type. A list
  of many distinct strings should widen to `[string]` rather than a giant
  literal union — the union smart-constructor with literal absorption
  does **not** do this on its own, so §A6.3's element-type construction
  must widen a pure all-literal union of one base to that base. (Stated
  there because the list path is shared with A6.)
- Existing harness tests using string/bool literals (e.g.
  `126_type_predicates.eu`): re-run; expect no behavioural change because
  every `LiteralX <: X`.

---

## A5 — Flow-sensitive narrowing

### A5.1 Goal

After a recognised branch on a type predicate, the checker narrows the
tested variable's type within each branch:

```
x : number | string | null
if(x null?, A,        # in A:  x : null
            B)        # in B:  x : number | string
```

Narrowing is **not** a syntactic rule — `if`/`then`/`cond` are ordinary
functions. It is a special case inside `synthesise_app`, keyed on a fixed,
closed table of recognised callees. An unrecognised brancher gets no
narrowing (sound: the feature simply does not fire).

### A5.2 The mechanism

At the **start** of `synthesise_app` (check.rs ~675), before the generic
function/argument loop:

1. Flatten the application spine: `App(App(f, xs), ys)` → `(f, xs ++ ys)`.
   (`then` arrives by catenation as `cond then(t, f)` = `then(t, f, cond)`,
   sometimes as nested `App`s.)
2. Call `recognise_branch(f, &spine_args)`. If it returns a
   `BranchDescriptor`, delegate to `synthesise_branch`. Otherwise fall
   through to the existing generic path unchanged.

`synthesise_branch` does **not** re-use the generic application path; it
fully handles the call: analyses the condition, synthesises each branch
under its narrowing facts, and returns the union of branch result types.

### A5.3 Recognition table

`recognise_branch` matches the callee against this closed table. The
callee is identified by **name** (`Var::Free`/`Var::Bound`/`Name`) or by
**intrinsic** (`Expr::Intrinsic`), gated by the provenance check in §A5.6.

| Callee | Arity | Condition arg | Positive branch | Negative branch |
|--------|-------|---------------|-----------------|-----------------|
| `if` / `__IF` | 3 | arg 0 | arg 1 | arg 2 |
| `then` | 3 | arg 2 | arg 0 | arg 1 |
| `cond` | 2 | see §A5.7 | per-row | default (arg 1) |
| `and` / `∧` / `__AND` | 2 | — | see §A5.8 | — |
| `or` / `∨` / `__OR` | 2 | — | see §A5.8 | — |

`then(t, f, c): if(c, t, f)` — written `c then(t, f)`, so after spine
flattening the condition is the **last** argument. Any callee at an arity
other than the table's (a partial application) is **not** recognised and
falls through to the generic path.

### A5.4 Threading narrowed types — the `scope_stack` constraint

`lookup_bound` indexes `scope_stack` by de Bruijn index. Narrowing
**must not** push a frame onto `scope_stack`.

Instead, add a parallel narrowing stack to `Checker`:

```rust
/// Active narrowing facts. Each entry overrides a variable's type
/// within the branch currently being synthesised. NOT part of the
/// de Bruijn scope_stack — see lookup_bound.
narrowing: Vec<NarrowFrame>,
```

A `NarrowFrame` is a set of facts; each fact is keyed by a **stable
binding identity**, not a bare name:

- For `Var::Bound(bv)`: the identity is the *outermost-anchored* frame
  index `scope_stack.len() - 1 - bv.scope`, paired with `bv.name`. This
  index is invariant under inner `push_scope`/`pop_scope` (when an inner
  frame is pushed, both `len` and the variable's `bv.scope` grow by one),
  so a fact installed while synthesising a branch still matches the same
  physical variable deeper inside that branch — and does **not** match an
  inner rebinding of the same name (different anchored index).
- For `Var::Free(name)` / `Name(name)`: keyed by name; skip narrowing if
  the name is bound in any closer scope frame. (At check time — after
  varify — locals are `Var::Bound`; `Var::Free` narrowing is a
  best-effort fallback only.)

Lookup: in the `synthesise` arms for `Var::Bound` / `Var::Free` /
`Name`, consult the narrowing stack (innermost first) **before** the
normal `lookup_bound`/`lookup_name`. A hit returns the narrowed type
directly (no freshening — narrowed types are monomorphic).

`synthesise_branch` then, for each branch: push a `NarrowFrame` built
from that branch's facts, `synthesise` the branch sub-expression, pop.

### A5.5 Condition analysis

`analyse_condition(expr) -> ConditionFacts` walks the condition,
synthesising sub-expressions as it goes (so warnings inside the condition
are still emitted) and returning two fact sets:

- `positive`: facts that hold when the condition is **true**.
- `negative`: facts that hold when the condition is **false**.

Rules:

| Condition shape | `positive` | `negative` |
|-----------------|-----------|-----------|
| `x p?` (`p?` a recognised predicate) | `{x ↦ narrow(p?, ty(x))}` | `{x ↦ subtract(p?, ty(x))}` |
| `not(c)` / `¬c` | `negative(c)` | `positive(c)` |
| `a ∧ b` | `positive(a) ⊓ positive(b)` | `∅` |
| `a ∨ b` | `∅` | `negative(a) ⊓ negative(b)` |
| anything else | `∅` | `∅` |

`⊓` merges fact sets: for a variable appearing in both, intersect (take
the narrower) — in practice the two sides narrow different variables, so
it is usually a union of disjoint maps. The negative of `a ∧ b` is
`¬a ∨ ¬b` (no single fact reliably holds) → empty; the positive of
`a ∨ b` is likewise unreliable → empty. This asymmetry is exactly what
the prelude needs: `if((n zero?) ∨ (l nil?), [], cons(l head, …))` —
the **else** branch gets `negative(n zero?) ⊓ negative(l nil?)`, and the
`l nil?` term contributes `l ↦ NonEmpty`.

`x` here must be a plain variable reference. A predicate applied to any
other expression (`x.f number?`, `f(x) number?`) yields no fact (`∅`).
The predicate may be written by catenation (`x p?`) or prefix
(`p?(x)`) — both desugar to `App(p?, [x])`, so one match shape covers
both.

### A5.6 Provenance — defeat by rebinding

Recognition is by name/intrinsic, gated so a user who redefines a branch
combinator silently loses narrowing (never gets *wrong* narrowing):

1. **Local shadowing**: if the callee name is bound in any frame on
   `scope_stack`, do not recognise it.
2. **Global identity**: the `Checker` records, while walking the
   top-level (prelude) bindings, which of `if`/`then`/`cond`/`and`/`or`
   resolve to their expected definitions (`if`→`__IF` intrinsic,
   `and`→`__AND`, `or`→`__OR`, `then`/`cond`→a prelude lambda). It keeps
   a `recognised: HashSet<&str>`. If a later top-level binding rebinds
   one of these names, it is removed from the set. `recognise_branch`
   fires only for names still in the set. Calls written as the bare
   intrinsic (`__IF`, `__AND`, `__OR`) are always recognised — intrinsics
   cannot be rebound.

### A5.7 `cond`

`cond(l, d)` where `l` is a **list literal** of two-element list literals
`[[c₁, r₁], [c₂, r₂], …]` and `d` is the default. Process rows in order,
carrying an accumulated negative-fact set `acc` (initially empty):

- Row *i*: synthesise `rᵢ` under `positive(cᵢ) ⊓ acc`; then
  `acc := acc ⊓ negative(cᵢ)`.
- Default: synthesise `d` under `acc`.

Result type: union of all `rᵢ` and `d`. The accumulated negatives are
what let `cond([[l nil?, panic(…)], …])` narrow `l` to `NonEmpty` in
*every later row*. If `l` is not a literal of the expected shape, `cond`
is not recognised (falls through — no narrowing).

### A5.8 `and` / `or` as standalone calls

When `&&`/`∧`/`__AND` (resp. `||`/`∨`/`__OR`) is itself the recognised
callee (not merely a sub-term of a condition):

- `and(a, b)`: synthesise `b` under `positive(a)` — the left conjunct's
  facts hold while evaluating the right. Result: `Bool`.
- `or(a, b)`: synthesise `b` under `negative(a)`. Result: `Bool`.

This is what types `if(not(xs nil?) ∧ (xs head p?), …)` (`take-while`):
the `∧`'s right conjunct `xs head` is synthesised with `xs` already
narrowed to `NonEmpty` by the left conjunct.

### A5.9 Narrowing `any`, and result types

- Positive narrowing of `any`: `if(x number?, A, …)` with `x : any`
  narrows `x` to `number` in `A`. This is sound and useful (TypeScript
  does the same).
- Negative narrowing of `any`: stays `any` (nothing to subtract). The
  gradual boundary stays silent.
- Result of a recognised branch call: the union (via §6.1) of the branch
  result types — `if(c, 1, 2)` is `number`; `if(c, 1, "x")` is
  `number | string`. This is more precise than today's generic path and
  may surface new (correct) warnings downstream; the test plan covers it.

### A5.10 Limitations (documented, not bugs)

- Only bare variables narrow — not `.field` paths or call results.
- Narrowing flows only through the recognised combinators; an exotic
  user-defined brancher gets none.
- Negative narrowing only changes a *union*; on an unannotated `any` the
  else-branch stays `any`.

---

## A6 — `NonEmpty` refinement

### A6.1 Goal

A thin refinement type for non-empty lists, so the partial list functions
(`head`, `tail`, …) are typed honestly and `head([])`-class bugs become
static warnings.

### A6.2 Representation

Add to `Type` (types.rs):

```rust
/// Non-empty list. The boxed type is the *element* type:
/// `NonEmpty(a)` is the type of `[a]` lists known to have ≥1 element.
NonEmpty(Box<Type>),
```

Display: `NonEmpty([{elem}])` (e.g. `NonEmpty([a])`) — matches the DSL.
`humanise` recurses into the boxed element (add to both
`collect_fresh_vars` and `replace`, mirroring `Type::List`).

DSL (parse.rs): `NonEmpty` `(` *type* `)` where the inner type must be a
list `[T]`; store `T`. A non-list inner is a parse error
(`NonEmpty expects a list type, e.g. NonEmpty([a])`). Add a `Token` for
`NonEmpty` and a `parse_primary` arm.

### A6.3 Subtyping, consistency, unification, construction

**`is_subtype`** — add:

```
(NonEmpty(a), NonEmpty(b))      => is_subtype(a, b),   // covariant
(NonEmpty(a), List(b))          => is_subtype(a, b),   // non-empty list IS a list
(Tuple(elems), NonEmpty(b))     => !elems.is_empty()
                                   && elems.iter().all(|e| is_subtype(e, b)),
```

`List(_) <: NonEmpty(_)` is **deliberately absent** — that is the misuse
A6 catches. Reflexivity covers `NonEmpty(a) <: NonEmpty(a)`.

**`is_consistent`** — add structural arms so `any` flows through:
`(NonEmpty(a), NonEmpty(b))`, `(NonEmpty(a), List(b))` and the `Tuple`
case recurse with `is_consistent`. Do **not** add `(List, NonEmpty)` —
consistency must keep that asymmetry so passing a plain list where
`NonEmpty` is wanted warns.

**`unify`** — add `(NonEmpty(a), NonEmpty(b)) => unify(a, b)` only. Do
**not** unify `NonEmpty` with `List`: when `head`'s parameter
`NonEmpty(t)` meets a `List(a)` argument, unification fails and
`apply_one_with_subst` falls back to `is_subtype(List(a), NonEmpty(t))` —
which is `false` — producing the warning. A `NonEmpty`/`NonEmpty` or
`Tuple`/`NonEmpty` argument passes (unify or subtype fallback succeeds).

**Construction** — `synthesise` list-literal arm (check.rs ~409):

| Literal | Today | After A6 |
|---------|-------|----------|
| `[]` | `[any]` | `[any]` (unchanged — definitely empty) |
| `[x]` | `[T]` | `NonEmpty(T)` |
| `[a,b]`…`[a,b,c,d]` | `Tuple` | `Tuple` (unchanged — `Tuple <: NonEmpty`) |
| `[a,…]` ≥5 | `[T]` | `NonEmpty(T)` |

So a non-empty list literal that is not in the tuple range synthesises
`NonEmpty`. Concretely, `synthesise_list_type` gains a flag for "input
was non-empty"; the element type is built as today (dedup) and then
**widened**: a union all of whose members are literal types of a single
base collapses to that base (so `["jan","feb",…]` is `NonEmpty([string])`,
not a 12-way literal union). Wrap the result in `NonEmpty` when non-empty,
`List` when empty.

`cons` and the `‖` operator: annotate `a -> [a] -> NonEmpty([a])` in
`lib/prelude.eu` (find the exact bindings — `(x ‖ xs): __CONS(x, xs)` and
the `cons` binding). Every `cons`-built list is then `NonEmpty`.

### A6.4 `nil?` narrowing — the A5×A6 join

Add `nil?` and `non-nil?` to the A5 predicate table (§A5.5). Unlike the
type predicates, these are list-shape refinements:

| Predicate | `narrow` (positive) | `subtract` (negative) |
|-----------|---------------------|------------------------|
| `nil?` | unchanged | `List(a) → NonEmpty(a)`; `NonEmpty`/`Tuple` unchanged; `any → any` |
| `non-nil?` | `List(a) → NonEmpty(a)`; else unchanged | unchanged |

`non-nil?` is `nil? complement` in the prelude — the table just gives it
the swapped polarity directly. Narrowing `any` via `nil?` stays `any`
(do not manufacture `NonEmpty(any)` — keep the gradual boundary silent).

This is the rule that makes `if(xs nil?, …, xs head)` type-check: the
else branch narrows `xs : [a]` to `NonEmpty(a)`, and `head` accepts it.

### A6.5 Prelude annotations

Annotate in `lib/prelude.eu`:

```
` { type: "NonEmpty([a]) -> a" }
head: __HEAD

` { type: "NonEmpty([a]) -> [a]" }
tail: __TAIL
```

Audit and annotate the other functions that take the first/last element
without an internal guard (the implementer enumerates: `last`, `init`,
`first` if present, etc.). Functions that are *total* by construction —
`head-or`, `tail-or`, `coalesce` — keep their current total types; their
internal `head`/`tail` calls are guarded and narrowing handles them.

### A6.6 H7b — constant-folded refinement

H7b ("suppress the warning when a literal value provably satisfies the
refinement") is, for `NonEmpty`, **subsumed** by §A6.3: a literal
non-empty list already synthesises `NonEmpty`, so `[1] head` type-checks
with no special folding. No separate inliner work is needed for A6. (H7b
as a general mechanism matters only for arithmetic refinements, which are
out of scope.)

### A6.7 Prelude blast-radius audit — an explicit deliverable

Retyping `head`/`tail` re-checks every call in `lib/prelude.eu`. Each must
resolve one of three ways:

1. **On a literal / `cons`-built non-empty list** → `NonEmpty` by §A6.3.
2. **Inside a branch narrowed by a `nil?`/`non-nil?` guard** through a
   recognised combinator → `NonEmpty` by §A6.4. A survey of the prelude
   confirms the chosen A5 scope covers the patterns in use:
   - `if(xs nil?, …, xs head)` — `if`. ✓
   - `xs nil? then(d, xs head)` — `then`. ✓
   - `cond([[l nil?, panic(…)], …])` — `cond` with accumulated negatives.
     ✓
   - `__IF((n zero?) ∨ (l nil?), [], cons(l head, …))` — `__IF` intrinsic
     + `∨` negative facts (`take`, `drop`). ✓
   - `if(not(xs nil?) ∧ (xs head p?), …)` — `∧` threads the left conjunct
     into the right (`take-while`). ✓
   - nested `if(l nil?, [], if(l head p?, …))` — outer guard narrows `l`
     for the whole else branch including the inner condition
     (`drop-while`, `group-consecutive-by`). ✓
3. **Genuinely unguarded** — the audit *will* find a few (e.g. a
   `transpose`-style `aux(rs): if(rs head nil?, …)` where `rs head` sits
   in the condition with no guard on `rs`). Each such site is fixed
   individually: add the missing guard, restructure, or — if the function
   is legitimately partial — give the binding a `!`-asserted annotation.
   These fixes are part of the A6 PR.

**Acceptance gate**: after A6, `eu check lib/prelude.eu` emits **zero**
warnings. This gate is non-negotiable and is the reason A6 depends on A5.

---

## 6. Cross-cutting helpers

### 6.1 Union smart-constructor

Add `Type::union(variants: Vec<Type>) -> Type`:

1. Flatten nested `Union`s.
2. If any member is `Any` → return `Any`.
3. Drop `Never` members.
4. Deduplicate structurally.
5. **Absorb literals**: drop `LiteralSymbol`/`LiteralString`/`LiteralBool`
   members whose base (`Symbol`/`String`/`Bool`) is also present.
6. Length 0 → `Never`; length 1 → that member; else `Union`.

Route union construction through it: `synthesise_list_type`, the
heterogeneous-tuple path in `synthesise_meta`, the recognised-branch
result (§A5.9), and `subtract`'s rebuild (§6.2). This is a small,
self-contained change and should land with A4.

### 6.2 Type subtraction

Add `subtract(removed: &Type, from: &Type) -> Type` for negative-branch
narrowing:

- `from` is `Union(vs)` → `Type::union(vs without any v where
  is_subtype(v, removed))`.
- `from == removed`, or `is_subtype(from, removed)` → `Never`.
- `from` is `Any` → `Any` (cannot subtract from the gradual type).
- otherwise → `from` unchanged.

This is intentionally minimal — it only needs to be useful over unions.
Predicate-keyed subtraction (`subtract(number?, T)` = `subtract(Number,
T)`) maps each predicate to the base type it removes; `nil?`/`non-nil?`
are special-cased per §A6.4.

---

## 7. Sequencing

Three PRs, in order:

1. **A4** — literal types + §6.1 union smart-constructor. Self-contained;
   no dependency on A5/A6.
2. **A5** — flow narrowing (§6.2 subtraction included). With the chosen
   "Core + prelude guards" scope, A5 uses only type predicates and
   `nil?`/`non-nil?` — it does **not** depend on A4. (A4 becomes a hard
   dependency only if equality-against-literal narrowing is added later.)
   Note: A5's `nil?` table entry references `NonEmpty`; either land the
   `NonEmpty` variant (a few lines of types.rs) first, or gate the `nil?`
   row until A6. Cleanest: introduce the bare `Type::NonEmpty` variant in
   A5 and add its subtyping/construction/annotations in A6.
3. **A6** — `NonEmpty` subtyping, construction, prelude annotations, and
   the blast-radius audit. Depends on A5.

A4 and A5 can proceed in parallel; A6 is last.

## 8. Test plan

Harness tests live in `tests/harness/typecheck/NNN_*.eu` with paired
`.expect` files; add Rust unit tests in the relevant module.

**A4** — unit tests in subtype.rs, unify.rs, parse.rs (literal
string/bool subtyping, consistency, unification, DSL round-trip);
harness: literal-string annotation accepted/rejected, union absorption
display.

**A5** — harness: positive and negative narrowing through `if`, `then`,
`cond`; `∧` fact-threading and `∨` negative facts; nested narrowing;
`__IF`/`__AND`/`__OR` intrinsic recognition; **rebinding disables
narrowing** (shadow `if` locally and at top level, assert no narrowing);
narrowing of `any`. Unit tests for `analyse_condition` and `subtract`.

**A6** — harness: `head([])` warns; `head([1])` and `head([1,2,3])` do
not; `head` after a `nil?`/`then`/`cond` guard does not; `cons` result is
`NonEmpty`; `List`-typed argument to `head` warns. Unit tests for the
`NonEmpty` subtyping/consistency/unify arms.

**Regression gates**:
- `eu check lib/prelude.eu` → **zero warnings** after A6 (§A6.7).
- Full `cargo test` green after each PR.
- `cargo clippy --all-targets -- -D warnings` clean.

## 9. Open sub-questions

1. **List-of-literals widening** (§A6.3): the spec widens a pure
   single-base literal union to its base in *list* synthesis, but keeps
   literal element types in *tuples* (2–4 elements). Confirm this seam is
   acceptable — it mirrors the existing symbol behaviour but is a visible
   inconsistency.
2. **`LiteralBool` value**: included for consistency and because it is
   nearly free, but its practical payoff is small (two inhabitants, and
   no `LiteralNumber` to pair with). Acceptable as-is, or drop it and
   ship only `LiteralString`?
3. **Recognised-branch result type** (§A5.9): switching `if`'s result
   from the generic-path type to `union(then, else)` is more precise but
   may surface new warnings in user code. Land it, or keep the generic
   result and have narrowing *only* add facts? The spec assumes the
   former.
4. **`then`/`cond` provenance** (§A5.6): `then`/`cond` are recognised as
   "a prelude lambda bound to this name", not by inspecting the body. A
   top-level redefinition of `then` to a *different* lambda would still
   be recognised and could narrow wrongly. Is the local-shadowing guard
   plus "first top-level binding wins, later rebinding disables"
   sufficient, or should recognition also pin the prelude binding's
   `Smid`?

## 10. File-by-file change summary

| File | A4 | A5 | A6 |
|------|----|----|----|
| `types.rs` | `LiteralString`/`LiteralBool` variants + display; `Type::union` | — | `NonEmpty` variant + display + `humanise` |
| `subtype.rs` | literal `<:` + consistency arms | — | `NonEmpty` `<:` + consistency arms |
| `unify.rs` | literal unify arms | — | `NonEmpty` unify arm |
| `parse.rs` | literal-string/bool tokens + productions; grammar comment | — | `NonEmpty([T])` token + production |
| `check.rs` | `synthesise_primitive`; route unions through `Type::union` | `Checker.narrowing` field; `recognise_branch`, `synthesise_branch`, `analyse_condition`, `subtract`; narrowing-aware `Var` synthesis; `recognised` set | list-literal synthesis → `NonEmpty`; `nil?` table entries |
| `lib/prelude.eu` | — | — | annotate `head`/`tail`/`cons`/`‖`/…; blast-radius fixes |
| `tests/harness/typecheck/` | literal tests | narrowing tests | `NonEmpty` tests |
