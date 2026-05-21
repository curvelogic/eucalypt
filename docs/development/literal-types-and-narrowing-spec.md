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
   checker narrows through the branch intrinsics `__IF`/`__AND`/`__OR`
   (and their aliases `if`/`and`/`or`/`∧`/`∨`) and through pass-through
   wrappers — `then`, plus any user-defined `my-if` — recognised
   **structurally**, not by name (§A5.3). Conditions are the type
   predicates (`number?`, `string?`, …), `nil?` and `non-nil?`. This
   covers every guard pattern the prelude itself uses. `cond` is a
   recursive `foldr` and is *not* recognised; the few prelude
   partial-function `cond` sites are rewritten as explicit `if`
   (§A5.7). *Not* in scope for 6.1: equality-against-literal narrowing
   (`x = :active`), `has(:k, …)`, `when`/`unless`, `.field`-path
   narrowing, and general union discrimination through `cond`.

2. **Partial list functions (A6)** — *annotate now*. `head`, `tail` and
   the other partial list functions are retyped with `NonEmpty` in
   `lib/prelude.eu` as part of 6.1. This is the headline payoff and it
   forces the A5 scope above (the prelude must stay warning-free).

## Background — what exists today

The checker is a freshen-and-unify bidirectional checker
(`src/core/typecheck/`). Relevant existing facts:

- `Type` (types.rs) already has `LiteralSymbol(String)`. There is **no**
  `LiteralString`.
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

## A4 — Literal types for strings

### A4.1 Goal

Extend the literal-singleton treatment that `LiteralSymbol` already has to
string literals, so the checker can express and check finite
enumerations like `"r" | "w" | "rw"` — the form structured-data enums
(YAML/JSON config fields) take. **`LiteralNumber` is out of scope**
(arithmetic on literal numbers is a type-level computation not worth the
cost — per H16). **`LiteralBool` is also out of scope**: a union of bool
literals (`true | false`) is just `bool`, so there is no useful proper
subset to name — see §9.

### A4.2 Representation

Add one variant to `Type` (types.rs), beside `LiteralSymbol`:

```rust
/// Literal string type: a specific string value.
LiteralString(String),
```

`humanise` (types.rs ~248) needs no change — its `collect_fresh_vars`
and `replace` both fall through `_` arms for variant-free types.

### A4.3 Display

Add a `Display` arm (types.rs ~155):

- `LiteralString(s)` → `"s"` (double-quoted; escape embedded `"` and `\`).

### A4.4 Synthesis

In `synthesise_primitive` (check.rs ~1022):

```rust
Primitive::Str(s) => Type::LiteralString(s.clone()),
```

`Primitive::Bool` is left unchanged (`→ Type::Bool`). This is
**always-on**, mirroring `LiteralSymbol`: every string literal
synthesises its literal type. Widening to the base type happens *on use*
— via subtyping and unification (below) — never on construction. This
keeps reasoning local and is consistent with how symbols already behave.

### A4.5 Subtyping and consistency

`is_subtype` (subtype.rs) — add beside the `LiteralSymbol` arm (~line 65):

```
(LiteralString(_), String) => true,
```

Equal literals are covered by the reflexivity check at the top of
`is_subtype`; unequal literals fall through to `_ => false`. The base type
is **not** a subtype of a literal (`String </: "foo"`).

`is_consistent` (subtype.rs ~186) — extend the literal arm to the new
pair, in both directions (mirrors the existing `LiteralSymbol`/`Symbol`
arm):

```
(LiteralString(_), String) | (String, LiteralString(_)) => true,
```

### A4.6 Unification

`unify` (unify.rs ~84) — add beside the `LiteralSymbol` arm:

```
(LiteralString(_), String) | (String, LiteralString(_)) => Ok(()),
```

Two *different* literal strings do **not** unify (no arm → existing
mismatch error), exactly as for literal symbols.

### A4.7 DSL syntax

Extend the type-DSL parser (parse.rs) and its grammar comment (~lines
8–28):

- **Literal string**: a double-quoted string in a type position →
  `LiteralString`. Requires a `Token::StringLit(String)` in the DSL lexer
  (handle `\"` and `\\`). New `parse_primary` arm.

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
- List literals keep **full literal precision** at every element
  position. `["a", "b", "c"]` synthesises `Tuple(["a", "b", "c"])` (see
  §A6.3 — A6 makes all non-empty list literals tuples). There is **no
  widening** of literal element types to their base — a literal union
  like `"a" | "b"` is the maximally precise, sound type, and is left
  intact. Union *absorption* (§6.1) still applies, but absorption only
  drops a literal when its base is *also* present in the union, so a
  pure literal union is untouched. If a wide literal union ever reads
  badly in diagnostics, that is a *display* concern (truncate on
  render), not a reason to discard type information.
- Existing harness tests using string literals (e.g.
  `126_type_predicates.eu`): re-run; expect no behavioural change because
  every `LiteralString <: String`.

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

Narrowing is **not** a syntactic rule — `if`/`then` are ordinary
functions. It is a special case inside `synthesise_app`, keyed
**structurally** on the branch *intrinsics* (`__IF`/`__AND`/`__OR`) and
on the *shape* of wrapper definitions — never on a name (§A5.3). A
brancher the checker cannot classify gets no narrowing (sound: the
feature simply does not fire).

### A5.2 The mechanism

At the **start** of `synthesise_app` (check.rs:675), before the generic
function/argument loop:

1. **Flatten the spine.** Walk down the `func` position collecting
   argument vectors: `App(App(f, xs), ys)` → `(f, xs ++ ys)`, to any
   depth. This is mandatory, not an optimisation. `then` is the
   idiomatic branch form and is *always* used in pipeline style
   `x then(a, b)`, which cooks to the **nested** `App(App(then, [a, b]),
   [x])` — the partial application `then(a, b)` sits in the function
   position of the outer `App`. Flattening reassembles the full
   argument list `[a, b, x]`.
2. **Recognise at the outermost `App`.** `synthesise_app` flattens
   *before* doing anything else, so the intermediate node
   `App(then, [a, b])` is consumed by the spine — it is never
   type-checked as a standalone partial application. The checker
   synthesises the argument *expressions* directly.
3. Call `recognise_branch(head, &spine_args)` (§A5.3). On a
   `BranchDescriptor`, delegate to `synthesise_branch`; otherwise fall
   through to the existing generic path unchanged.

`synthesise_branch` does not re-use the generic application path: it
analyses the condition, synthesises each branch under its narrowing
facts, and returns the union of branch result types.

For `x then(a, b)` the flattened spine is `(then, [a, b, x])`; `then`'s
shape (§A5.3) puts the condition at parameter 2, so the condition is the
pipelined subject `x` and the branches are `a`, `b` — "if `x` then `a`
else `b`". All three are arguments of the *one* call expression, hence
in one scope, which is exactly what lets the condition narrow the
branches (see §A5.6, §A5.10).

### A5.3 Recognising branchers — structurally

Recognition is **structural**: it never matches a name. Given a
flattened spine `(head, args)`:

**Mechanism 1 — a raw branch intrinsic.** `head` is literally an
`Expr::Intrinsic` node for `IF`, `AND` or `OR`. Unforgeable. Fixed
shapes:

| Intrinsic | Arity | Condition | Branches |
|-----------|-------|-----------|----------|
| `IF`  | 3 | arg 0 | arg 1 positive, arg 2 negative |
| `AND` | 2 | — | see §A5.8 |
| `OR`  | 2 | — | see §A5.8 |

**Mechanism 2 — a binding with a branch shape.** `head` is a `Var`;
resolve it to its binding and consult the binding's memoised
`BranchShape`. A binding is classified **once** (on first need) and the
result cached:

- If the body (peeling metadata) is a bare branch intrinsic —
  `if: __IF` (prelude.eu:272) — the binding *inherits* that intrinsic's
  shape. This is alias chasing; `compress` may already have done it.
- If the body is an *application* whose head is an already-classified
  brancher and whose condition slot plus at least one branch slot are
  filled by this function's **own parameters** — `then(t,f,c): if(c,t,f)`
  — the binding is a **pass-through wrapper**: compose the inner shape
  with this function's parameter positions. `then` →
  `BranchShape { arity 3, condition: param 2, branches: [param 0, param 1] }`.
- Otherwise: no `BranchShape`.

Classification is one level deep, **memoised** (O(1) per call site
thereafter), composes bottom-up, and is guarded against recursion (a
function reaching itself during classification → no shape, which is
always correct — a recursive function is not a pass-through wrapper).
The *same* machinery covers an alias (`if`), a prelude wrapper (`then`),
and a user's own `my-if(c,t,e): if(c,t,e)` — narrowing works for all
three with zero name knowledge. Most functions fail the first check and
are dismissed; in the 6.1 prelude only `if`/`and`/`or` (aliases) and
`then` (wrapper) acquire a shape.

A spine matches when its flattened arity equals the shape's arity; the
condition and branch *expressions* are then the spine arguments at the
shape's recorded indices. An arity mismatch (a partial application) does
not match — see §A5.6.

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

Predicates are recognised **structurally** too, consistent with §A5.3 —
no predicate name table. A type predicate resolves to its underlying
test intrinsic; `nil?`/`non-nil?` are recognised as an equality test
against the empty-list literal `[]` (`nil?` is defined that way in the
prelude). The implementer confirms the intrinsic backing each predicate.

### A5.6 Recognition is structural — no name matching

Because recognition keys off the branch *intrinsics* and the structural
*shape* of definitions (§A5.3), there is no name to spoof and no
provenance table to maintain:

- A user who **rebinds** `if`/`then`/`and`/`or` gets recognition based
  on what the new definition *is*. Rebound to something still a
  pass-through wrapper → still narrows, correctly. Rebound to something
  that is not → classifies as "no shape" and narrowing simply does not
  fire. Never unsound.
- A user who **defines their own** brancher (`my-if`, `select`, …) gets
  narrowing for free, by classification.
- **Local shadowing** needs no special handling: a locally-bound `if`
  is a `Var` resolving to a local binding, classified by its
  definition like any other.

This dissolves the former open question about `then`/`cond` provenance.

**A partially-applied brancher is opaque — and that is sound.** A
brancher is recognised only when a *complete* spine — all condition and
branch slots filled — appears in **one** expression. If `if` is
partially applied and stored,

```
g: if(c)            # g : a -> a -> a — a stored partial application
... g(p, q)
```

the checker classifies `g` as having no `BranchShape` (its body is an
*incomplete* `__IF`, and `g` has no parameters to fill the open slots),
so `g(p, q)` is typed generically with no narrowing. This is sound — no
*wrong* narrowing — and nothing useful is lost. Narrowing is meaningful
only when the condition and the branches share variables in a shared
**scope**; a stored partial application necessarily splits the baked-in
condition (definition scope) from the later-supplied branches (call-site
scope), and a variable narrowed through the condition cannot appear in
branches from another scope. The checker therefore resolves a spine head
through *function/wrapper* definitions — parameter-hole substitution
keeps every concrete expression in its own scope — but treats a binding
whose *value* is a partial application as **opaque**. Chasing it would
mean splicing a concrete condition across a scope boundary (with de
Bruijn re-indexing) for a vacuous gain. See §A5.10.

### A5.7 `cond` is engineered out, not recognised

`cond(l, d): l foldr(uncurry(if), d)` (prelude.eu:1168) is a `foldr` —
recursive and data-driven. It does not reduce to a static `__IF` tree,
and the classification recursion guard (§A5.3) correctly gives it no
`BranchShape`. `cond` is therefore **not** a recognised brancher, and
narrowing does not flow through it.

The only `cond` uses that *need* narrowing for the prelude
warning-free gate are the partial-function sites — a `cond` whose first
row guards `nil?` and a later row calls `head`/`tail`. These are
**rewritten** in `lib/prelude.eu` as explicit nested / `nil?`-guarded
`if`, which narrow via Mechanism 1. The rewrite is part of the A6
prelude work; §A6.7's audit enumerates the sites and confirms the count
is small.

General narrowing through `cond` in *user* code — discriminating a union
across `cond` rows — is deferred. It belongs with the occurrence-typing
endgame (latent propositions; see `type-system-evolution.md`), not with
a `cond`-specific special case.

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
- Narrowing reaches a brancher only when it can be classified
  structurally (§A5.3): the `BranchShape` requires the condition and
  branch slots to be the function's own parameters. A brancher
  assembled by higher-order composition, or one whose branch structure
  is data-driven (`cond`, §A5.7), gets no shape and does not narrow.
- A partially-applied brancher stored in a binding is opaque (§A5.6):
  completing it elsewhere does not narrow. Sound, and the lost narrowing
  is vacuous — the stored partial application splits the condition from
  the branches across scopes, so nothing they share could be narrowed.
  The sole non-vacuous case — a partial application closing over an
  *outer-scope* variable that the later-supplied branches also mention —
  is pathologically rare and accepted.
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

**Construction** — `synthesise` list-literal arm (check.rs:409).

Today the arm has a 2–4-element cutoff: 2–4 elements → `Tuple`,
everything else → `List` (via `synthesise_list_type`, check.rs:1037).
That cutoff is a **behavioural cliff**, not a cosmetic one. A `Tuple`
records arity; a `List` does not, and there is correctly no
`List <: Tuple` rule (a value typed `List` has statically-unknown
length). So `[1, 2]` passes to a `(number, number)` parameter but
`[1, 2, 3, 4, 5]` — synthesised `List` — does **not** pass to a
`(number, number, number, number, number)` parameter, purely because
the cutoff discarded its length. A6 rewrites this arm anyway, so it
fixes the cliff:

| Literal | Today | After A6 |
|---------|-------|----------|
| `[]` | `[any]` | `[any]` (unchanged — definitely empty) |
| `[x]` … `[a,…]` up to the cap | `[T]` (n=1, n≥5) / `Tuple` (n 2–4) | `Tuple([…])` |
| `[a,…]` beyond the cap | `[T]` | `NonEmpty(<elem union>)` |

A non-empty list literal synthesises as `Tuple` of its element types at
**every** arity up to a practicality cap (`LIST_TUPLE_CAP`, ~16–32 —
purely to bound type size for pathological generated literals). `Tuple`
is the strictly most general principal type for such a literal: it
widens to everything the other forms need —

- `Tuple <: Tuple` → passes to a same-arity tuple parameter;
- `Tuple <: List` → passes to `[T]`;
- `Tuple <: NonEmpty` → passes to `head`/`tail`.

So there is no separate "1-element / ≥5-element" case and no widening of
element types — element positions keep their full (literal) precision.
Beyond the cap, fall back to `NonEmpty(<deduplicated element union>)`:
length is lost but non-emptiness is kept. `[]` stays `List(any)`.

**Precision across function boundaries.** This `Tuple` precision
propagates *through inference* but stops where the type system honestly
loses information — and the spec should not pretend otherwise:

- An **unannotated** function whose body is a literal (or an
  `if`/`cond` all of whose branches are same-shape tuples) gets a
  `Tuple` *inferred* return type; its callers can pass the result to a
  tuple parameter. The union smart-constructor (§6.1) dedups identical
  tuple branches; mismatched-arity branches stay a union and correctly
  fail a tuple parameter.
- A function **annotated** `[T]` returns `List(T)` — length is gone *by
  the author's choice*. To carry a fixed arity across a boundary,
  annotate the boundary `(T, T, …)`. The annotation *is* the contract.
- A value routed through a **length-erasing** operation (`map`, `++`,
  `reverse`, `tail`, recursion) becomes `List` (or `any` for an
  unannotated recursive call). `List` here genuinely means "unknown
  length" and rejecting a tuple parameter is sound and correct.
  Recovering this would need length-indexed types — out of scope.
  (A future, optional checker special-case could recognise
  structure-preserving combinators — `map`/`reverse`/`zip` on a `Tuple`
  argument yielding a `Tuple` result — but that is not part of 6.1.)

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
2. **Inside a branch narrowed by a `nil?`/`non-nil?` guard** → `NonEmpty`
   by §A6.4. A survey of the prelude confirms structural recognition
   (§A5.3) covers the patterns in use:
   - `if(xs nil?, …, xs head)` — `if` (alias of `__IF`). ✓
   - `xs nil? then(d, xs head)` — `then` (pass-through wrapper). ✓
   - `__IF((n zero?) ∨ (l nil?), [], cons(l head, …))` — raw `__IF`
     intrinsic + `∨` negative facts (`take`, `drop`). ✓
   - `if(not(xs nil?) ∧ (xs head p?), …)` — `∧` threads the left conjunct
     into the right (`take-while`). ✓
   - nested `if(l nil?, [], if(l head p?, …))` — outer guard narrows `l`
     for the whole else branch including the inner condition
     (`drop-while`, `group-consecutive-by`). ✓
   - `cond([[l nil?, panic(…)], …])` — `cond` is **not** recognised
     (§A5.7); these sites are **rewritten** as explicit `nil?`-guarded
     `if`. Enumerate them here and rewrite each as part of the A6 work.
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
5. **Absorb literals**: drop `LiteralSymbol`/`LiteralString` members
   whose base (`Symbol`/`String`) is also present.
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

**A4** — unit tests in subtype.rs, unify.rs, parse.rs (literal-string
subtyping, consistency, unification, DSL round-trip); harness:
literal-string annotation accepted/rejected, union absorption display.

**A5** — harness: positive and negative narrowing through `if` and
`then`; `∧` fact-threading and `∨` negative facts; nested narrowing;
raw `__IF`/`__AND`/`__OR` intrinsic recognition; **a user-defined
`my-if` narrows** (structural classification); a partially-applied
stored `if` does *not* narrow and is sound; narrowing of `any`. Unit
tests for `BranchShape` classification (alias, wrapper, recursion guard,
arity mismatch), `analyse_condition`, and `subtract`.

**A6** — harness: `head([])` warns; `head([1])` and `head([1,2,3])` do
not; `head` after a `nil?`/`then`/`cond` guard does not; `cons` result is
`NonEmpty`; `List`-typed argument to `head` warns. Arity-cliff
regression: a 5+-element list literal passes to a same-arity tuple
parameter, and *fails* a `[T]`-annotated value passed to a tuple
parameter. Unit tests for the `NonEmpty` subtyping/consistency/unify
arms.

**Regression gates**:
- `eu check lib/prelude.eu` → **zero warnings** after A6 (§A6.7).
- Full `cargo test` green after each PR.
- `cargo clippy --all-targets -- -D warnings` clean.

## 9. Open sub-questions

### Resolved during review

- **List-literal synthesis**: there is **no widening** of literal
  element types, and the 2–4-element tuple cutoff is **removed**. A
  non-empty list literal synthesises as `Tuple` at every arity up to a
  practicality cap (`NonEmpty` of an element union beyond it); `[]`
  stays `List(any)`. This keeps full element precision *and* fixes the
  pre-existing arity cliff where 5+-element literals could not be passed
  to tuple parameters — see §A6.3, which now also documents how far
  `Tuple` precision carries across function boundaries.
- **`LiteralBool` dropped**: A4 ships `LiteralString` only.
  `LiteralString` adds real expressiveness (arbitrary finite string
  enums, the form structured-data config takes). `LiteralBool` does not
  — a union of bool literals is just `bool` — and the one feature that
  could use it (literal-equality narrowing) is deferred past 6.1, so it
  would ship inert. `Primitive::Bool` keeps synthesising `Type::Bool`.
- **Recognised-branch result type**: a recognised `if`/`then` call is
  typed accurately as `union(branch types)` (§A5.9, §6.1) — not via the
  generic application path. This is the precise type (`if(c, 1, "x")`
  *is* `number | string`), and the new — correct — warnings it surfaces
  are the point. A5's PR carries the same zero-new-warning triage gate
  as A6 (prelude + harness).
- **Brancher recognition is structural, not nominal** (§A5.3, §A5.6):
  the checker recognises the raw `__IF`/`__AND`/`__OR` intrinsics, and
  classifies each binding once into an optional `BranchShape` (alias
  chasing for `if: __IF`; pass-through-wrapper composition for
  `then(t,f,c): if(c,t,f)`). No name table. This makes a user's `my-if`
  narrow for free and dissolves the former `then`/`cond` provenance
  question — there is no name to spoof. `cond` (a recursive `foldr`)
  gets no shape and is engineered out of the prelude's narrowing-
  critical sites (§A5.7).

### Still open

None. The design questions raised in review are resolved. Two values
are left to the implementation as audits, not design choices: the
`LIST_TUPLE_CAP` constant (§A6.3) and the exact set of partial-function
`cond` sites to rewrite (§A6.7).

## 10. File-by-file change summary

| File | A4 | A5 | A6 |
|------|----|----|----|
| `types.rs` | `LiteralString` variant + display; `Type::union` | — | `NonEmpty` variant + display + `humanise` |
| `subtype.rs` | literal `<:` + consistency arms | — | `NonEmpty` `<:` + consistency arms |
| `unify.rs` | literal unify arms | — | `NonEmpty` unify arm |
| `parse.rs` | literal-string token + production; grammar comment | — | `NonEmpty([T])` token + production |
| `check.rs` | `synthesise_primitive`; route unions through `Type::union` | `Checker.narrowing` field; spine flattening; `BranchShape` classification (memoised); `recognise_branch`, `synthesise_branch`, `analyse_condition`, `subtract`; narrowing-aware `Var` synthesis | list-literal arm — drop the 2–4 cutoff, synthesise `Tuple` up to `LIST_TUPLE_CAP` then `NonEmpty`; `nil?` predicate handling |
| `lib/prelude.eu` | — | — | annotate `head`/`tail`/`cons`/`‖`/…; blast-radius fixes |
| `tests/harness/typecheck/` | literal tests | narrowing tests | `NonEmpty` tests |
