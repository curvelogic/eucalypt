# 0013 — Embedded vs string type-DSL: deciding the H12 question

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Extends-Roadmap
- **Suggested horizon:** 0.9
- **Related:** H12 (type-system-evolution.md H12a–d), TS-A7
  (alias-reference-tooling-spec.md), open question 2
  (type-system-evolution.md §5), siblings
  [0011 — typeclasses-without-classes](0011-typeclasses-without-classes.md),
  [0009 — structural-contracts-validation](0009-structural-contracts-validation.md)

---

## Summary

Eucalypt type annotations live inside **strings** in `type:` metadata,
parsed by a hand-written recursive-descent DSL at
`src/core/typecheck/parse.rs`. This document works through H12's four
options for either keeping or replacing that design, argues that the
string DSL is the right long-term form for 1.0, and states the concrete
evidence that would justify revisiting that decision after 1.0. The
central finding is that **TS-A7** (first-class alias references via LSP
span indexing — already planned for Stage A) resolves the most
practically painful tooling friction without touching the string
boundary; the remaining friction points (escaping, phase separation) are
real but modest, and the costs of crossing into an embedded form are
higher than the evolution document acknowledges. Open question 2 gets a
direct answer: keep the string DSL indefinitely, with TS-A7 as the
investment.

---

## Motivation

### What we actually have

The type DSL is a completely separate language parsed by
`src/core/typecheck/parse.rs`. Its public entry point, `parse_type`
(`parse.rs:669`), accepts a string and returns a `Type` AST. The grammar
covers primitives, function arrows, union (`|`), lists, tuples, open and
closed record types, named row variables, literal symbols, and opaque
type constructors (`IO(T)`, `Lens(a,b)`, `Traversal(a,b)`). Capitalised
identifiers are parsed to `Type::Var` and resolved by the checker against
an `AliasMap` (`check.rs:57`, `check.rs:105`) populated by walking
`types:` sub-blocks in metadata expressions
(`check.rs:294–317`) and `type-def:` binding annotations (`check.rs:332`).
The crucial point is that **alias names are not eucalypt bindings** — they
live exclusively in the checker's alias table, not in the value scope the
evaluator or the inliner can see.

### The friction points in concrete terms

**1. Record types require double-brace escaping.** Eucalypt strings treat
`{expr}` as an interpolation site. A record type written literally inside a
type string must double every brace: `{{x: number}}` in the source
becomes `{x: number}` in the DSL string. The prelude alone has 326 type
annotations, of which 63 use `{{..}}` escaping (roughly 19 %). The error
message is clear (`src/core/error.rs:116`: "escape it by doubling"), but
the visual noise is real, particularly for multi-field record types:

```
` { type: "!{{bind:      IO(a) → (a → IO(b)) → IO(b),
             return:    a → IO(a),
             map:       (a → b) → IO(a) → IO(b),
             ..}}" }
io: monad{ ... }
```
(`lib/prelude.eu:30–39`)

**2. No editor support inside type strings.** `Person` in `"Person ->
[Person]"` is an alias reference. Today, hovering or going to definition
on `Person` does nothing — the LSP sees a string literal, not a named
reference. Renames do not propagate. This is the friction most likely to
be noticed by a user who relies on the type system for documentation.

**3. Aliases are not in the eucalypt value scope.** A type alias declared
in `{ types: { Point: "{x: number, y: number}" } }` cannot be imported by
name, used in expressions, or consumed by any eucalypt function. It is
invisible to everything except the checker. This is a deliberate
separation — but it means the alias namespace is a dead end for
meta-programming.

**4. String parse errors surface late.** A malformed type string is a
silently dropped annotation (aliases that fail `parse_type` are skipped;
`check.rs:312`). There is no compile-time rejection. In practice this is
a minor issue because the DSL is small, but it is a consequence of the
phase boundary.

### What the evolution document says

H12 in `type-system-evolution.md` is candid: "The string form is honest
about being a separate language and that honesty has value." Open
question 2 asks directly: "Is the metadata DSL acceptable indefinitely,
or is the friction visible enough to want an embedded form within a
year?" The document does not answer it; that is what this proposal does.

---

## Prior art & landscape

**Racket contracts.** The `#:contract` annotation DSL uses ordinary Racket
syntax — there is no string boundary. The annotation `(-> integer? string?)`
is just a Racket expression that evaluates to a contract object at
runtime. This is the "no boundary" approach and it pays for itself in full
Racket tooling support. The cost is that contracts and values share a
runtime and evaluation model, which Racket accepts; eucalypt deliberately
does not.

**TypeScript JSDoc vs `.ts` embedded types.** TypeScript supports types
in JSDoc comment strings (`/** @param {string} x */`), which have
degraded tooling (no full rename, limited inference) compared with
`.ts`-embedded annotations. The lesson is that the degraded form persists
indefinitely once established, even when the embedded form is available —
users who need the functionality migrate; users who do not, stay. This
suggests that friction alone does not force migration; it must be
*blocking* friction, not *cosmetic* friction.

**Idris 2 elaborator reflection and quoted types.** Idris allows
`quote`/`unquote` to embed type-level expressions in terms. The machinery
requires a full elaboration pass over the quoted term, executed at
compile time. This is the formal analogue of H12d, and it is correct: a
quoted type is not a free ride — it is a compile-time elaboration
problem, not just a parse-time convenience.

**Lean 4's `term` vs `tactic` distinction.** Lean uses syntax categories
to route constructs through different elaborators. A term embedded inside
a tactic block is elaborated as a term, not a tactic. The routing is
explicit and keyed on syntactic delimiters. The lesson is that embedding
a sub-language is tractable when the elaborator is already category-aware
and the sub-language shares the host's syntax tree.

**Dhall type ascriptions.** Dhall has no separate type DSL — every type
is a Dhall expression (types are values). This gives full tooling
coverage and eliminates escaping entirely. It also means Dhall's runtime
must evaluate types, which is sound because Dhall is total and strongly
normalising. Eucalypt is lazy, impure (IO), and non-total: making types
into values raises the same termination question H12c identifies.

**CUE and Nickel.** Both embed typing constraints directly in the value
language (CUE's unified type-value model; Nickel's gradual contract
layer). Both lose the clean phase separation that lets eucalypt erase
types before evaluation. In eucalypt the type checker runs before STG
compilation (`… → simplify → [TYPE CHECK] → inline → STG`), and types
are erased from the runtime. This design is load-bearing: it is why
the checker adds zero runtime overhead.

---

## Proposed design

No new syntax. The recommendation is **status quo plus TS-A7**.

The string DSL remains the sole form for type annotations. What changes in
Stage A is the tooling layer, per the already-specced TS-A7 (see
`docs/development/alias-reference-tooling-spec.md`):

1. `parse_type_with_refs` records byte spans for capitalised alias-name
   tokens (A7.1 — `parse.rs`).
2. The LSP maps those byte spans to source positions via the `Smid` on
   the string-literal AST node (A7.2 — `src/driver/lsp/`).
3. Per-document alias definition maps are built from `types:` blocks and
   `type-def:` entries visible in the AST (A7.3 — `src/driver/lsp/`).
4. Go-to-definition, hover, and rename work for alias references inside
   type strings (A7.4 — `src/driver/lsp/`).

TS-A7 is explicitly a tooling bead, not a type-theory bead. It touches
only `parse.rs` (span recording) and `src/driver/lsp/` (LSP handlers).
The checker, the alias map, the type representation, and the string DSL
are all unchanged. This is the cheapest path that addresses the second
friction point (no editor support) while leaving the architecture clean.

The remaining friction — `{{..}}` escaping — is addressed by a single
documentation note rather than a language change: the escape is mechanical
once learnt (the error message explains it), it is confined to annotations
that contain record types inside IO or function positions, and there is no
form of the grammar that would eliminate it without changing the surface
syntax of strings (which this language does not do).

---

## Interaction with the existing roadmap

**TS-A7 (Stage A, already specced).** This proposal endorses TS-A7 as
the right investment. TS-A7 is already on the bead plan; this document
provides the strategic rationale for not going further in Stage A or B.

**H12a (the "corrected" form).** H12 §H12a in `type-system-evolution.md`
already names TS-A7 as the implementation. This proposal confirms that
H12a is not only the cheapest option but the correct one for 1.0.

**H12b (symbolic/block form).** Dropped. The evolution document is right:
brutally verbose, no readability gain, no constituency. The only
conceivable use case is programmatic type construction, which is H12c.

**H12c (first-class type values).** Interesting for meta-programming but
deferred for the reasons in the next section. Revisit if the evidence
trigger fires (see §9).

**H12d (idiot-bracket embedding).** Not a free ride. The proposal
documents the phasing problem in full: idiot brackets desugar to runtime
function applications, which run after STG compilation. The type checker
runs *before* that. A type expressed via an idiot bracket is a runtime
value the checker cannot see. Making H12d work requires a compile-time
evaluator for the type sub-language, which is H12c plus an additional
constant-folding pass — a non-trivial engineering commitment. It is not
blocked permanently, but the cost is understated in earlier notes.

**Open question 2 (type-system-evolution.md §5.2).** This proposal's
direct answer: the metadata DSL is acceptable indefinitely for 1.0, and
TS-A7 covers the friction that matters most. The question should be
re-examined post-1.0 if the evidence trigger in §9 fires.

**0009 (structural contracts).** Contracts live in the same metadata
channel as type annotations. A decision to keep the string DSL applies
there too: contract expressions are strings parsed by the contract
checker. There is no conflict.

**0011 (typeclasses without classes).** Structural constraint annotations
(`<(a, a) => a -> a -> a`) are part of the type DSL string. They stay
there. H12a's span recording will cover constraint identifiers as well.

---

## Implementation sketch

TS-A7 is fully specced and costed in
`docs/development/alias-reference-tooling-spec.md`. The summary:

| File | Change | Size estimate |
|---|---|---|
| `src/core/typecheck/parse.rs` | Token end-offset tracking; `AliasRef` struct; `parse_type_with_refs` entry point | ~50 lines |
| `src/driver/lsp/` | String-offset ↔ source-position mapping; alias definition indexer; go-to-def / hover / rename handlers | ~200 lines across 3–4 files |
| `tests/` | Parser span unit tests; LSP alias-reference integration tests | ~80 lines |

Risk: low. TS-A7 is read-only for the checker path; the existing
`parse_type` call sites are unchanged. The LSP rename handler is the
only write-path addition and is guarded to plain, unescaped string
literals (interpolated type strings degrade gracefully with no crash).
Span arithmetic for sub-string offsets is straightforward given the
existing `Smid`/`SourceMap` infrastructure.

No changes to `src/core/typecheck/check.rs`, the `AliasMap`, the `Type`
enum, or any downstream consumer.

---

## Alternatives considered

### H12b — verbose symbol/block form

Rejected. The block encoding for even a simple function type is
unreadable:

```
` { type: { :fun args: [:a :b] result: :c } }
```

No user would prefer this over the string form. No tooling benefit. Drop.

### H12c — first-class type values

The appeal is real: if types are eucalypt values, they can be imported,
generated programmatically, and passed to macros or doc generators. The
obstacles are:

1. **Termination.** Eucalypt is non-total. The type checker runs at
   compile time and expects to terminate. A type that is computed by an
   arbitrary eucalypt expression does not have this guarantee. The type
   checker would either need to impose a termination check (expensive,
   ad-hoc) or fall back to `any` on non-termination (losing precision).
   Dhall can do this because Dhall is total; eucalypt is not.

2. **Phase boundary.** The value namespace is the evaluator's concern;
   the type namespace is the checker's. Collapsing them means the checker
   must either run after the evaluator (losing all the erasure benefits)
   or the evaluator must run a subset of eucalypt at compile time (a
   mini-interpreter for the type sublanguage only, duplicating logic).

3. **Readability.** `"Person -> string"` is more readable than a
   hypothetical `type-arrow(type-alias(:Person), string-type)`.

Defer unless user-generated types become a demonstrated need (see §9).

### H12d — idiot-bracket type expressions

The most seductive option, because eucalypt already has idiot brackets as
a user-extensible syntax extension mechanism. The argument "just bind a
bracket pair to a type-constructor function" appears to avoid adding any
new syntax. It does not, for the reason stated above: idiot brackets are
evaluated by the STG VM; the type checker runs before that. The desugared
bracket is an ordinary function application with no special status in the
checker's eyes. The checker would see `type_expr([a, :arrow, b])` — an
ordinary application of an ordinary function — and have no basis for
extracting a type from it.

The only route to making H12d work is static evaluation of the bracket
body: a compile-time interpreter that reduces the bracket application to
a `Type` value before the checker runs. That interpreter needs to handle
at minimum list construction, symbol literals, function application, and
alias lookup — essentially a stripped-down evaluator for a pure
first-order sub-language of eucalypt. That is real work, it blurs the
phase boundary the architecture keeps clean, and it depends on H12c (type
values) as a prerequisite. Not a free ride; a substantial commitment.

H12d is not ruled out permanently. If H12c becomes worth doing (see §9),
H12d is a natural surface form on top of it. As an independent option, it
is not viable.

---

## Risks & what would kill this

**Risk 1: User adoption stalls because of DSL friction.** If measured
feedback from real eucalypt users consistently names type annotation as
a barrier — specifically that the string boundary (not the escape) makes
annotations harder to write than a more integrated form — that is
falsifying evidence. The counter-evidence today is that the prelude's 326
annotations are mostly clear and that the escape pattern is mechanical.

**Risk 2: TS-A7 does not deliver enough.** If, after TS-A7 lands, users
still report that alias go-to-definition and rename are insufficient —
for example, because they need import-able alias names or doc-generation
from alias definitions — that signals a need for H12c rather than H12a.

**Risk 3: Record type friction is worse than measured.** The 63 `{{..}}`
instances in the prelude (`lib/prelude.eu:30, 63, 67, 71, 75, 79, 85,
103, 360, 364, 368, 399, 403, 407, 411, 415, 419, 423, 440, 448`)
represent types where the argument *or* return position is an open
block. If Stage B's new features (row polymorphism, `Dict(T)`) reduce
the need for bare `{..}` in function positions — because `Dict(T)` can
substitute — the escaping problem may shrink naturally. Monitor actual
annotation patterns after Stage A.

**Risk 4: TS-A7's span arithmetic is incorrect for edge cases.** The A7
spec notes that interpolated or escaped type strings degrade gracefully
(no crash, no alias tooling). The remaining risk is incorrect span
arithmetic for plain strings near source-map boundaries. This is a test
coverage question, not an architecture question.

---

## Success criteria

1. **TS-A7 shipped in Stage A (0.6.2 / 0.8):** go-to-definition, hover,
   and rename work for alias references inside `type:` strings in the
   VS Code extension and Emacs mode.

2. **No new escaping friction reported:** no filed issue citing `{{..}}`
   escaping as a blocker or significant pain in the six months following
   Stage A.

3. **Open question 2 closed:** the maintainer records a decision — in
   this document or in `type-system-evolution.md` — that the string DSL
   is the form through 1.0, with the evidence trigger documented below.

4. **Evidence trigger for H12c (post-1.0 reconsideration):** *either*
   (a) a documented use case where users need to programmatically
   generate or import type aliases as eucalypt values (e.g. codegen from
   JSON Schema that emits `types:` blocks dynamically), *or* (b) a
   measured count of `{{..}}` escapes in user-authored code (outside the
   prelude) exceeding 20 % of type annotations over more than 50
   annotation sites. If either trigger fires, H12c should be re-evaluated
   as a post-1.0 Stage C item.

---

## References

### Code

- `src/core/typecheck/parse.rs:669` — `parse_type` public API
- `src/core/typecheck/parse.rs:367–376` — alias reference parsing:
  capitalised `Token::Ident` → `Type::Var`; the comment at line 368
  is the canonical statement of the alias/variable duality
- `src/core/typecheck/check.rs:57` — `AliasMap` type alias
- `src/core/typecheck/check.rs:105` — `aliases` field on `Checker`
- `src/core/typecheck/check.rs:294–317` — `register_aliases_from_meta`:
  walks `types:` sub-blocks and populates the alias table
- `src/core/typecheck/check.rs:332` — `type-def:` alias extraction
- `src/core/typecheck/check.rs:1059` — comment documenting the
  `{{…}}`-escape / JOIN static-eval path
- `src/core/error.rs:116` — user-facing double-brace escape hint
- `lib/prelude.eu:30–39` — `io` namespace monad annotation (multi-line
  `{{..}}` usage)
- `lib/prelude.eu:63–85` — IO shell/exec annotations with `IO({{..}})`
- `lib/prelude.eu:360–423` — block combinators: `merge`, `elements`,
  `keys`, `lookup`, `lookup-or`

### Specs and design documents

- `docs/development/alias-reference-tooling-spec.md` — TS-A7 spec
  (A7.1–A7.4)
- `docs/development/type-system-evolution.md` — H12a–H12d, open
  question 2, Stage A/B/C staging
- `docs/development/type-system-bead-plan.md` — TS-A7 bead entry

### Prior art

- Felleisen et al., *Racket: A Programmable Programming Language*
  (2018) — contracts as host-language values
- TypeScript JSDoc type annotation documentation (Microsoft, 2024)
- Brady, *Idris 2: Quantitative Type Theory in Practice* (2021) —
  quoted types and elaboration reflection
- Moura & Ullrich, *The Lean 4 Theorem Prover and Programming Language*
  (CADE 2021) — term/tactic syntax categories
- Dhall language specification — types as values, totality requirement
- CUE language specification — unified type-value model
- Nickel documentation — gradual contracts in the value language
