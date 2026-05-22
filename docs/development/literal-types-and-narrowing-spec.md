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

A5 additionally carries an **explicit language goal**: rework the
existing `cond`. Today `cond` is a `foldr` over a list of two-element
lists — clumsy to write (`cond([[c, r], …], d)` — bracket soup) and
opaque to the type-checker (a fold it cannot see through). It is
replaced by a recognised intrinsic with a clean clause syntax
(`cond[c => r, …, d]`). This is not incidental tidy-up: the old `cond`
is the *one* prelude branching construct narrowing cannot penetrate, so
fixing it is on the critical path — and it is a worthwhile readability
win in its own right. See §A5.7.

## Decisions taken

Two scoping questions were settled before writing this spec:

1. **Narrowing recognition scope (A5)** — *Core + prelude guards*. The
   checker narrows through the branch intrinsics `__IF`/`__AND`/`__OR`
   (and their aliases `if`/`and`/`or`/`∧`/`∨`) and through pass-through
   wrappers — `then`, plus any user-defined `my-if` — recognised
   **structurally**, not by name (§A5.3). Conditions are the type
   predicates (`number?`, `string?`, …), `nil?` and `non-nil?`. This
   covers every guard pattern the prelude itself uses. `cond` is
   **reworked** into a recognised intrinsic `__COND` with a clean clause
   syntax — narrowing flows through it like any other recognised
   brancher (§A5.7). *Not* in scope for 6.1: equality-against-literal
   narrowing (`x = :active`), `has(:k, …)`, `when` (the predicate-applied
   condition shape), and `.field`-path narrowing.

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
- **Intrinsic naming.** An intrinsic is written `__IF` in eucalypt
  *source* (e.g. the prelude binding `if: __IF`); the desugarer strips
  the `__`, so the core `Expr::Intrinsic` node carries the **bare**
  name `IF` — verified: `eu dump cooked` shows `if = … IF;`. Where this
  spec recognises an intrinsic *node* it means the bare name (`IF`,
  `AND`, `OR`, `COND`, `CLAUSE`); where it writes `__X` it means the
  source-level binding the user or prelude writes.
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
`Expr::Intrinsic` node for `IF`, `AND`, `OR` or `COND`. Unforgeable.
Fixed shapes:

| Intrinsic | Arity | Condition | Branches |
|-----------|-------|-----------|----------|
| `IF`  | 3 | arg 0 | arg 1 positive, arg 2 negative |
| `AND` | 2 | — | see §A5.8 |
| `OR`  | 2 | — | see §A5.8 |
| `COND` | 1 | per-clause | the clause list — see §A5.7 |

`IF`/`AND`/`OR` have a fixed argument layout; `COND` takes a single
clause-list argument whose branch structure the checker reads by
patterning that argument (§A5.7).

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
The *same* machinery covers an alias (`if`, `cond`), a prelude wrapper
(`then`), and a user's own `my-if(c,t,e): if(c,t,e)` — narrowing works
for all with zero name knowledge. Most functions fail the first check
and are dismissed; in the 6.1 prelude only `if`/`and`/`or`/`cond`
(aliases of intrinsics) and `then` (wrapper) acquire a shape.

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
| `x✓` (postfix not-null operator) | `{x ↦ subtract(Null, ty(x))}` | `{x ↦ Null}` |
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
no predicate name table. The type predicates `number?`/`string?`/
`symbol?`/`bool?`/`list?`/`block?` resolve to their underlying test
intrinsics (`__ISNUMBER`, `__ISSTRING`, …, verified present in the
prelude). `nil?` (`= []`) and `null?` are recognised as an equality
test against a literal — `nil?` against the empty-list literal `[]`,
`null?` against `null` — the same structural shape (`__EQ` against a
literal). **`null?` does not exist in the prelude today**; A5 adds it,
`null?: = null` (mirroring `nil?: = []`) — a one-line prelude addition.
The postfix `✓` not-null operator is the existing form and is also
recognised (the `x✓` row above); `null?` and `x✓` are duals. The
implementer confirms the intrinsic backing each predicate.

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

### A5.7 Reworking `cond` into a recognised intrinsic

`cond(l, d): l foldr(uncurry(if), d)` (prelude.eu:1168) is eucalypt's
multi-way conditional, and it has two faults. It is **clumsy** to write
— `cond([[c₁, r₁], [c₂, r₂], …], d)`, a list of two-element lists,
bracket soup — and it is **opaque** to the checker: a `foldr`,
recursive, with no `BranchShape` (§A5.3's recursion guard correctly
refuses it). It is the one prelude branching construct narrowing cannot
see through. A5 reworks it — using **only operators and intrinsics**, no
new grammar syntax and no special desugaring.

**Two new intrinsics.**

- `__COND` — the multi-way conditional. One argument: a list of clauses
  with an optional trailing default. At runtime it walks the list,
  evaluating each clause's condition left-to-right; the first true
  clause yields its result; a trailing non-clause element is the
  default. Lazy in the obvious places (a clause result is forced only
  when its condition holds), exactly as the old `cond`.
- `__CLAUSE` — the clause builder, surfaced as the operator `=>` with
  its Unicode twin `⇒`: `(c => r): __CLAUSE(c, r)` and
  `(c ⇒ r): __CLAUSE(c, r)`. A clause is then the intrinsic node
  `App(__CLAUSE, [c, r])`. Both spellings cook to the same intrinsic, so
  the surface glyph is invisible to the checker.

`cond` keeps its name — the same concept (Lisp/Clojure `cond`: multi-way,
first-match-wins), better expressed — bound `cond: __COND`. There is no
deprecated alias for the old list-of-pairs form.

**Surface syntax.** Clauses are `c => r`; a bare trailing element is the
default. The clause list is a literal or `‖`-built:

```
classify(n): cond[n < 0 => :negative, n > 100 => :huge, :normal]
classify(n): cond(n < 0 => :negative ‖ n > 100 => :huge ‖ [:normal])
```

`cond[…]` is juxtaposed call syntax — a function directly followed by a
self-delimiting `[…]` needs no parens; `cond(…)` parens are needed only
when the argument is a bare expression (the `‖`-built form). Operator
fixity: `=>`/`⇒` bind below catenation (a clause result may be a
catenation such as `xs head`) and above `‖` (so each `c => r` groups
before being consed).

**Type-checking.** `__COND` is an intrinsic node → recognised by
Mechanism 1 (§A5.3), structurally, no name involved. On a recognised
`__COND` the checker inspects the clause-list argument:

- It is **statically readable** when it is a literal `List` node, or a
  `__CONS` spine ending in one (the checker flattens a `__CONS` spine as
  it flattens an `App` spine).
- The checker patterns each element: `App(__CLAUSE, [cᵢ, rᵢ])` is clause
  *i*; a trailing non-`__CLAUSE` element is the default.
- Per-row narrowing, carrying an accumulated negative-fact set `acc`
  (initially empty): synthesise `rᵢ` under `positive(cᵢ) ⊓ acc`, then
  `acc := acc ⊓ negative(cᵢ)`; synthesise the default under `acc`.
- Result type: the union (§6.1) of every `rᵢ` and the default.

All clauses and the default sit in the *one* `cond` expression, hence in
one scope, so the conditions can narrow the results (cf. §A5.6). When
the argument is **not** statically readable (a clause list computed at
runtime) the checker cannot pattern it: `__COND` is typed generically
with no narrowing — graceful, the intrinsic still runs correctly, only
static narrowing is forgone.

This is consistent with the whole structural-recognition design: `cond`
joins `if`/`and`/`or` as a recognised intrinsic, narrowing flows through
it for *user* code as well as the prelude, and the §A5.3 recursion-guard
problem disappears — there is no longer a `foldr` to see through.

**`if` is retained.** `__COND` strictly generalises `__IF`
(`if(c, t, e)` ≡ `cond[c => t, e]`), but `__IF` is kept as the fast
path: a direct ternary that allocates no list and destructures nothing.
`__IF` stays the primitive the prelude and desugarer emit for two-way
branching; `__COND` is the general multi-way form.

**Migration.** Changing `cond`'s clause form breaks its ~handful of call
sites (the prelude's own `cond` uses, plus any in the harness); each is
migrated to the new clause syntax as part of the A5 work. This
*replaces* the earlier plan to rewrite the partial-function `cond` sites
as explicit `if`-nests — they simply become new-form `cond[…]` and
narrow as recognised intrinsics (§A6.7).

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
- Narrowing reaches a brancher only when it is recognised structurally
  (§A5.3): a raw branch intrinsic, or a binding with a `BranchShape`. A
  brancher assembled by higher-order composition gets no shape and does
  not narrow. `cond` *is* recognised (§A5.7), but narrows only when its
  clause list is statically readable — a runtime-computed clause list is
  opaque.
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
   - `cond[l nil? => panic(…), …]` — `cond` is the recognised `__COND`
     intrinsic (§A5.7); the per-clause narrowing (accumulated negatives)
     narrows `l` to `NonEmpty` in later clauses. These sites narrow
     as-is once migrated to the new clause syntax — no rewrite to
     explicit `if` is needed. ✓
3. **Genuinely unguarded** — the audit *will* find a few (e.g. a
   `transpose`-style `aux(rs): if(rs head nil?, …)` where `rs head` sits
   in the condition with no guard on `rs`). Each such site is fixed
   individually: add the missing guard, restructure, or — if the function
   is legitimately partial — give the binding a `!`-asserted annotation.
   These fixes are part of the A6 PR.

**Acceptance gate**: after A6, `eu check lib/prelude.eu` emits **zero**
warnings. This gate is non-negotiable and is the reason A6 depends on A5.

### A6.8 Precise `head`/`tail` on tuples

`head`/`tail` are annotated `NonEmpty([a]) -> a` / `NonEmpty([a]) ->
[a]` (§A6.5), and a `Tuple` is `<: NonEmpty` (§A6.3) — so `[k, v] head`
*type-checks*, but via the annotation it yields the widened `k | v`,
not the precise first element. A6 adds a **structural call-site
special-case** on the `HEAD`/`TAIL` intrinsics — the same pattern as
A5's `__COND` recognition — to recover precision:

- `head`/`tail` resolve to the `HEAD`/`TAIL` intrinsics (`head: __HEAD`,
  `tail: __TAIL` — alias chasing, as §A5.3 already does for `if: __IF`).
- When the argument synthesises to a `Tuple`, the special-case
  *overrides* the annotation result:
  - `HEAD(Tuple([T₀, …, Tₙ]))` → `T₀`.
  - `TAIL(Tuple([T₀, T₁, …, Tₙ]))` → `Tuple([T₁, …, Tₙ])`; for a
    one-element tuple, `TAIL(Tuple([T₀]))` → `List(any)` (the empty
    list).
- For a non-`Tuple` argument (`List`/`NonEmpty`) the §A6.5 annotation
  applies unchanged.

So `[k, v] head` synthesises `k`'s type and `[k, v] tail` →
`Tuple([v-type])` — pair access by `head`/`tail`, and by their bare
aliases `first`/`key` (`first: head`, `key: head`), is precise from
Phase A. Accessor *functions* whose body is a `head`/`tail` composition
— `value` (`value: second`, `second(xs): xs tail head`) — need the
`ProjectionShape` classifier, which is bead B6.3 (Phase B,
[partiality-and-indexed-access-spec.md](./partiality-and-indexed-access-spec.md)).
This subsection is the *primitive* B6.3 builds on, pulled into A6
because it shares A6's `head`/`tail` handling and makes pair `head`/
`tail` access precise in 6.1.

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
2. **A5** — flow narrowing (§6.2 subtraction included) **and the `cond`
   rework** (§A5.7). With the chosen "Core + prelude guards" scope, the
   narrowing itself uses only type predicates and `nil?`/`non-nil?` — it
   does **not** depend on A4. (A4 becomes a hard dependency only if
   equality-against-literal narrowing is added later.) Note: A5's `nil?`
   handling references `NonEmpty`; either land the `NonEmpty` variant (a
   few lines of types.rs) first, or gate the `nil?` rule until A6.
   Cleanest: introduce the bare `Type::NonEmpty` variant in A5 and add
   its subtyping/construction/annotations in A6. A5 is the broadest of
   the three PRs — beyond the checker it adds the `__COND`/`__CLAUSE`
   intrinsics (VM), the `=>`/`⇒` operators and `cond: __COND` (prelude),
   and migrates existing `cond` call sites. Consider splitting the
   `cond` rework into its own commit within the A5 PR.
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
raw `__IF`/`__AND`/`__OR` intrinsic recognition; **user-defined
branchers narrow** by structural classification — test both a
straight wrapper `my-if(c,t,e): if(c,t,e)` *and* a
polarity-swapping one `unless(c,a,b): if(c,b,a)` (the latter confirms
the classifier *composes* the inner `BranchShape` — mapping `if`'s
positive slot to `unless`'s `b` — rather than copying it; `unless` is
not a prelude function, it is a deliberate test fixture); a
partially-applied stored `if` does *not* narrow and is sound; narrowing
of `any`. Unit tests for `BranchShape` classification (alias, wrapper,
polarity-swap, recursion guard, arity mismatch), `analyse_condition`,
and `subtract`.

**A5 — `cond` rework**: `cond[…]` and `cond(…)` parse and evaluate
correctly; `=>` and `⇒` both build `__CLAUSE`; runtime semantics match
the old `cond` (first-true-wins, lazy results); harness: per-clause
narrowing with accumulated negatives (a later clause sees earlier
conditions negated), narrowing through both a literal and a `‖`-built
clause list, and a runtime-computed clause list yielding no narrowing
(graceful). Confirm migrated prelude `cond` sites still behave.

**A6** — harness: `head([])` warns; `head([1])` and `head([1,2,3])` do
not; `head` after a `nil?`/`then`/`cond` guard does not; `cons` result is
`NonEmpty`; `List`-typed argument to `head` warns. Arity-cliff
regression: a 5+-element list literal passes to a same-arity tuple
parameter, and *fails* a `[T]`-annotated value passed to a tuple
parameter. Tuple precision (§A6.8): `[k, v] head` synthesises `k`'s
exact type (not `k | v`), `[k, v] tail` → a 1-tuple; `head`/`tail` on a
non-tuple list still type via the annotation. Unit tests for the
`NonEmpty` subtyping/consistency/unify arms.

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
  the checker recognises the raw `__IF`/`__AND`/`__OR`/`__COND`
  intrinsics, and classifies each binding once into an optional
  `BranchShape` (alias chasing for `if: __IF`, `cond: __COND`;
  pass-through-wrapper composition for `then(t,f,c): if(c,t,f)`). No name
  table. This makes a user's `my-if` narrow for free and dissolves the
  former `then`/`cond` provenance question — there is no name to spoof.
- **`cond` reworked** (§A5.7): the old `foldr`-based `cond` — clumsy
  (`cond([[c,r],…], d)`) and opaque to the checker — is replaced by a
  recognised intrinsic `__COND` with a clean clause syntax
  (`cond[c => r, …, d]`, `=>`/`⇒` → `__CLAUSE`). It costs two intrinsics
  and two operators, no new grammar syntax. `cond` narrows like any
  recognised brancher; `if`/`__IF` is retained as the two-way fast path.
  This is an explicit goal of the A5 work.

### Still open

None. The design questions raised in review are resolved. Two values
are left to the implementation as audits, not design choices: the
`LIST_TUPLE_CAP` constant (§A6.3) and the exact set of `cond` call sites
to migrate to the new clause syntax (§A5.7, §A6.7).

## 10. File-by-file change summary

| File | A4 | A5 | A6 |
|------|----|----|----|
| `types.rs` | `LiteralString` variant + display; `Type::union` | — | `NonEmpty` variant + display + `humanise` |
| `subtype.rs` | literal `<:` + consistency arms | — | `NonEmpty` `<:` + consistency arms |
| `unify.rs` | literal unify arms | — | `NonEmpty` unify arm |
| `parse.rs` | literal-string token + production; grammar comment | — | `NonEmpty([T])` token + production |
| `check.rs` | `synthesise_primitive`; route unions through `Type::union` | `Checker.narrowing` field; spine flattening; `BranchShape` classification (memoised); `recognise_branch`, `synthesise_branch`, `analyse_condition`, `subtract`; narrowing-aware `Var` synthesis; recognise `__COND` and pattern its clause-list argument (literal `List` / `__CONS` spine) | list-literal arm — drop the 2–4 cutoff, synthesise `Tuple` up to `LIST_TUPLE_CAP` then `NonEmpty`; `nil?` predicate handling; `HEAD`/`TAIL`-on-`Tuple` precise special-case (§A6.8) |
| `src/eval/` (intrinsics + STG) | — | `__COND` and `__CLAUSE` intrinsics — runtime semantics + STG compilation | — |
| `src/syntax/` (lexer/grammar) | — | tokenise the `=>`/`⇒` operator glyphs (if not already lexed generically) | — |
| `lib/prelude.eu` | — | `cond: __COND`; `=>`/`⇒` operators (→ `__CLAUSE`) with fixity; migrate existing `cond` call sites to the clause form | annotate `head`/`tail`/`cons`/`‖`/…; blast-radius fixes |
| `tests/harness/typecheck/` | literal tests | narrowing + `cond` rework tests | `NonEmpty` tests |
