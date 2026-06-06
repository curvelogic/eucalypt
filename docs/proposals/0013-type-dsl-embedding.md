# 0013 — Embedded vs string type-DSL: deciding the H12 question

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Extends-Roadmap
- **Suggested horizon:** 0.9
- **Related:** H12 (type-system-evolution.md H12a–d), TS-A7
  (alias-reference-tooling-spec.md), open question 2
  (type-system-evolution.md §5), siblings
  [0011 — typeclasses-without-classes](0011-typeclasses-without-classes.md),
  [0009 — structural-contracts-validation](0009-structural-contracts-validation.md),
  [0012 — algebraic-subtyping-fork](0012-algebraic-subtyping-fork.md)

---

## Summary

Eucalypt type annotations live inside **strings** in `type:` metadata,
parsed by a hand-written recursive-descent DSL at
`src/core/typecheck/parse.rs`. This document works through H12's four
options for either keeping or replacing that design, argues that the
string DSL is the right long-term form for 1.0, and states the concrete
evidence that would justify revisiting that decision after 1.0. The
central finding is that **TS-A7** (first-class alias references via LSP
span indexing — shipped in 0.6.2) resolves the most practically painful
tooling friction without touching the string boundary; the remaining
friction points (escaping, phase separation) are real but modest, and
the costs of crossing into an embedded form are higher than the evolution
document acknowledges. Open question 2 gets a direct answer: keep the
string DSL **for 1.0** — refined by one minimal string-prefix addition
(`s"…"`), not replaced by an embedded form — with TS-A7's delivery as the
vindication.

**One refinement has since surfaced and is recorded below.**
[0009](0009-structural-contracts-validation.md) needs to reference type
aliases from **value** positions (to reify a named type into a runtime
spec) — precisely this proposal's own §9 evidence-trigger firing ("users
need to reference type aliases as values"). A bare-symbol-plus-magical-
function surface for that crossing is unsatisfying. The resolution is the
**`s"…"` prefix** (§H12e): one new variant on the lexer's existing
string-prefix family (`c"`/`r"`/`t"`) that turns interpolation off and
parses its content to a `Type` at parse time. Crucially it is **still a
string** — a *refinement* of the string DSL this document keeps, not the
embedded form it rejects — so it preserves the phase boundary and TS-A7
tooling while killing `{{..}}` escaping and giving 0009 its value-position
reference. As a string-form refinement rather than new grammar, it is a
**1.0** change, not a deferral; the heavier *reserved-bracket* form
(§H12e's earlier draft) is recorded as rejected. See the revised
Alternatives §H12e.

---

## Motivation

### What we actually have

The type DSL is a completely separate language parsed by
`src/core/typecheck/parse.rs`. Its public entry point, `parse_type`
(`parse.rs:669`), accepts a string and returns a `Type` AST. The grammar
covers primitives, function arrows, union (`|`), lists, tuples, open and
closed record types, named row variables, literal symbols, opaque type
constructors (`IO(T)`, `Lens(a,b)`, `Traversal(a,b)`), and — as of 0.7.0
— `forall (m :: * -> *)` higher-kinded quantification, kind annotations,
equirecursive `Mu` aliases, and structural operator constraints
(`<(a, a) => …`). Capitalised
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

**2. Editor support inside type strings — resolved in 0.6.2 by TS-A7.**
`Person` in `"Person -> [Person]"` is an alias reference. Prior to 0.6.2,
hovering or going to definition on `Person` did nothing — the LSP saw a
string literal, not a named reference, and renames did not propagate. This
was the friction most likely to be noticed by a user who relied on the
type system for documentation. TS-A7 resolved it: alias references now
have LSP hover (showing the resolved type), go-to-definition, and rename
support (`CHANGELOG.md` 0.6.2). The friction point remains in this list
as historical context and as the primary evidence for the recommendation
— its resolution by TS-A7 (without touching the string boundary) is
exactly the argument for keeping the string DSL.

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

Keep the string DSL — and add one minimal refinement to it. TS-A7 has
already delivered the key *tooling* improvement; the only language change
is the **`s"…"` prefix** (§H12e), a string-form addition (not an embedded
form) that retires `{{..}}` escaping and serves 0009's value-position
references. The string boundary, the phase separation, and the erasure
guarantee are all preserved.

The string DSL remains the sole form for type annotations. TS-A7, shipped
in 0.6.2 (see `docs/development/alias-reference-tooling-spec.md`),
delivered the tooling layer that resolves the principal friction:

1. `parse_type_with_refs` records byte spans for capitalised alias-name
   tokens (A7.1 — `parse.rs`).
2. The LSP maps those byte spans to source positions via the `Smid` on
   the string-literal AST node (A7.2 — `src/driver/lsp/`).
3. Per-document alias definition maps are built from `types:` blocks and
   `type-def:` entries visible in the AST (A7.3 — `src/driver/lsp/`).
4. Go-to-definition, hover, and rename work for alias references inside
   type strings (A7.4 — `src/driver/lsp/`).

TS-A7 was explicitly a tooling bead, not a type-theory bead. It touched
only `parse.rs` (span recording) and `src/driver/lsp/` (LSP handlers).
The checker, the alias map, the type representation, and the string DSL
were all unchanged. This is the cheapest path that addressed the second
friction point (editor support) while leaving the architecture clean — and
it shipped without touching core syntax, confirming the design thesis.

The remaining friction — `{{..}}` escaping — is **retired** by the `s"…"`
prefix rather than merely documented: because `s"…"` lexes with
interpolation **off** (like `t"…"`), record braces inside it are literal,
so `s"{host: string}"` needs no doubling. This is *not* a change to the
surface syntax of ordinary strings (which this language does not do) — it
is a new prefix on the existing string-prefix channel (`c"`/`r"`/`t"`),
the same conservative move by which other constructs have been added. The
plain `"…"` form is retained and stays valid (the escape rule still
applies there); `s"…"` is the de-escaped form authors reach for. That the
string DSL has continued to absorb substantial new expressiveness in 0.7.0
(`forall` quantification, kind annotations, `Mu` recursive aliases,
structural operator constraints) without any new *core* syntax is further
evidence that the form scales.

---

## Interaction with the existing roadmap

**TS-A7 (Stage A, shipped in 0.6.2).** This proposal endorses TS-A7 as
the right investment — and TS-A7 has now validated that bet. This document
provides the strategic rationale for not going further in Stage B or beyond.

**H12a (the "corrected" form).** H12 §H12a in `type-system-evolution.md`
names TS-A7 as the implementation. TS-A7 has shipped. This proposal
confirms that H12a is not only the cheapest option but the correct one for
1.0, and that its delivery removes the main argument for going further.

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

**`cond[...]` and `=>` operator (shipped 0.6.2).** A tangentially relevant
data point: `cond[c1 => r1, c2 => r2, default]` is a new surface
construct that rides the existing operator channel (`=>` at precedence 15)
and idiot-bracket mechanism (`cond[...]`), with no new keywords or
statement forms. This illustrates the general conservatism principle: new
surface constructs can be added through the established channels. It does
not change the type-embedding analysis, but it reinforces that the
constraint on syntax is productive, not merely restrictive.

**Open question 2 (type-system-evolution.md §5.2).** This proposal's
direct answer: the metadata DSL is the right form for 1.0 — kept, and
refined by the `s"…"` prefix (§H12e), which retires the escaping friction
and serves 0009's value-position references without leaving the string
form. TS-A7 covers the tooling friction. Only H12c (arbitrary *computed*
type values) stays deferred post-1.0.

**0009 (structural contracts).** *This interaction has changed since both
documents were first drafted, and is now resolved.* 0009 no longer posits
separate "contract strings"; it reuses the **type** vocabulary — reifying
named type aliases into runtime specs (with value-pattern predicates living
outside the type world). That puts type expressions in **value** positions
for the first time, which is exactly the trigger for **H12e** above. The two
decisions are **coupled** and now aligned: the value-position surface is the
**`s"…"` prefix** (§H12e), a string-form refinement adopted at 1.0 — so both
documents land on *keep, and minimally refine, the string DSL*. 0009 ships
the reification mechanism; `s"…"` is its surface; no reserved bracket, no
embedded form, no conflict.

**0011 (typeclasses without classes).** Structural constraint annotations
(`<(a, a) => a -> a -> a`) are part of the type DSL string — and shipped
as part of the 0.7.0 type grammar, demonstrating that the string form can
absorb new constraint syntax without touching core syntax. They stay
there. TS-A7's span recording covers constraint identifiers as well.

**0012 (algebraic-subtyping fork).** Decided **won't-do**: the inference
*engine* is not being rebuilt either. So both halves of the type-system
question are now settled in the conservative direction — 0013 keeps the
*surface* a string DSL (refined by `s"…"`), and 0012 keeps the *engine* the
predictable hand-rolled core. The string DSL is therefore the long-term form
on a stable engine, not merely a 1.0 holding position.

---

## Implementation sketch

TS-A7 was specced in `docs/development/alias-reference-tooling-spec.md`
and shipped in 0.6.2. For the record, the actual scope matched the
estimate:

| File | Change | Size estimate |
|---|---|---|
| `src/core/typecheck/parse.rs` | Token end-offset tracking; `AliasRef` struct; `parse_type_with_refs` entry point | ~50 lines |
| `src/driver/lsp/` | String-offset ↔ source-position mapping; alias definition indexer; go-to-def / hover / rename handlers | ~200 lines across 3–4 files |
| `tests/` | Parser span unit tests; LSP alias-reference integration tests | ~80 lines |

Risk: realised as low. TS-A7 was read-only for the checker path; the
existing `parse_type` call sites were unchanged. The LSP rename handler
was the only write-path addition and is guarded to plain, unescaped
string literals (interpolated type strings degrade gracefully with no
crash). Span arithmetic for sub-string offsets was straightforward given
the existing `Smid`/`SourceMap` infrastructure.

No changes were made to `src/core/typecheck/check.rs`, the `AliasMap`,
the `Type` enum, or any downstream consumer. No further implementation
work is needed for H12a.

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

### H12e — de-magicking value-position references: the `s"…"` prefix (adopted)

This option is **not** in the original H12 set. It is surfaced by
[0009](0009-structural-contracts-validation.md), which needs to reference
type aliases from value positions (to reify a named type into a runtime
spec). A bare-symbol-in-a-magical-function surface (`as-spec(:Server)`) is
unsatisfying: nothing on the page signals that `:Server` crosses into the
type world.

**The adopted surface is the `s"…"` prefix** — one new variant on the
lexer's existing string-prefix family (`c"`/`r"`/`t"`,
`src/syntax/rowan/lex.rs:36-43`). Like `t"…"` (the ZDT literal) it lexes
with interpolation **off** and parses its content to a typed value at parse
time — here a `Type`, via `parse_type`:

```
` { type: s"Server -> number" }            # annotation: checker reads the Type
config: load-config validate(s"Server")    # value: compile-time-reified spec
shape:  s"{host: string, port: number}"    # inline; interpolation off, no {{..}}
```

In annotation position the checker reads the `Type` and it erases; in value
position the node is lowered by the same compile-time reification 0009
specifies (resolve the alias, lower the `Type` to predicate core). 0009's
`as-spec` is the explicit transform `s"…"` feeds, and consumers auto-lower a
literal, so `validate(s"Server")` needs no wrapping call.

**Why a prefix, not the reserved bracket.** An earlier draft of this
section proposed a dedicated *reserved* bracket pair (`⟬Server⟭`) the lexer
would mode-switch into the type sub-grammar. It reached the same parse-time
`Type` node, but at a real price: a new core delimiter carved out of
bracket-space, a sub-parser wired into the main grammar (LALRPOP/Rowan),
plus tree-sitter, LSP, and editor-mode work — and it **abandons the string
boundary this document concludes to keep**. The `s"…"` prefix reaches the
*identical* parse-time `Type` node for one `StringPrefix` variant and **no**
new grammar, and the type content stays a string, so TS-A7's span-indexed
alias tooling keeps working unchanged. The prefix dominates the bracket on
every axis but raw visual distinctiveness — not worth a grammar carve-out.

**Not H12d's phasing problem, not H12c's termination problem.** `s"…"` is
not an idiot bracket: it is not a runtime function application the checker
cannot see — it emits a `Type` at **parse time**, exactly as a `"…"` type
string is parsed today, only marked and interpolation-free. And its body is
the **fixed type sub-grammar**, *parsed* never *evaluated*, so there is no
user computation to diverge (the hazard that defers H12c). A
*runtime-computed* type name — the genuinely H12c-shaped case — stays out of
scope and deferred; `s"…"` is a literal.

**What it buys.** One surface across both contexts (annotation *and* value
reference); `{{..}}` escaping (friction point 1, ~19 % of prelude
annotations) gone, because record braces are no longer interpolation sites;
TS-A7 tooling retained, because the content is still a string; the
bare-symbol magic removed; and the 0009 asymmetry visible on the page —
inside `s"…"` is the type world (dual-use, structural), outside is the value
world (`refine { … }`, runtime-only predicates).

**The honest cost.** `s"…"` *is* new surface syntax — the thing this
language most resists — but the smallest possible form of it: a new
string-prefix letter on a channel that already carries three
(`c"`/`r"`/`t"`), with no new grammar, operator, or keyword, so it sits
within non-negotiable #1. The plain `"…"` annotation form is retained for
back-compat. It is a bounded commitment (a lexer variant + the compile-time
reification 0009 already specifies + tree-sitter/LSP prefix recognition),
not the grammar-integration project the bracket would have been.

**Recommendation.** Adopt at **1.0**, as a refinement of the string DSL —
not a deferral. 0009 ships the *mechanism* (compile-time reification) and
`s"…"` is its surface; the two are one change, and `as-spec` is the explicit
form the literal feeds. The reserved bracket is **rejected** — cost without
benefit over the prefix. H12c (arbitrary *computed* type values) remains
deferred on the termination hazard.

---

## Risks & what would kill this

**Risk 1: User adoption stalls because of DSL friction.** If measured
feedback from real eucalypt users consistently names type annotation as
a barrier — specifically that the string boundary (not the escape) makes
annotations harder to write than a more integrated form — that is
falsifying evidence. The counter-evidence today is that the prelude's 326
annotations are mostly clear and that the escape pattern is mechanical.

**Risk 2: TS-A7 does not deliver enough.** TS-A7 has shipped. If users
still report that alias go-to-definition and rename are insufficient —
for example, because they need importable alias names or doc-generation
from alias definitions — that signals a need for H12c rather than H12a.

**Risk 3: Record type friction is worse than measured.** The 63 `{{..}}`
instances in the prelude (`lib/prelude.eu:30, 63, 67, 71, 75, 79, 85,
103, 360, 364, 368, 399, 403, 407, 411, 415, 419, 423, 440, 448`)
represent types where the argument *or* return position is an open
block. Row polymorphism and `Dict(T)` shipped in 0.6.2 and may reduce
the need for bare `{..}` in function positions — because `Dict(T)` can
substitute — so the escaping problem may shrink naturally. Monitor actual
annotation patterns as these features are adopted.

**Risk 4: TS-A7's span arithmetic is incorrect for edge cases.** The A7
spec notes that interpolated or escaped type strings degrade gracefully
(no crash, no alias tooling). The remaining risk is incorrect span
arithmetic for plain strings near source-map boundaries. This is a test
coverage question, not an architecture question.

---

## Success criteria

1. **TS-A7 shipped in 0.6.2:** go-to-definition, hover, and rename work
   for alias references inside `type:` strings in the VS Code extension
   and Emacs mode. *This criterion is met.*

2. **No new escaping friction reported:** no filed issue citing `{{..}}`
   escaping as a blocker or significant pain in the six months following
   0.6.2. The 0.7.0 grammar additions (`forall`, `Mu`, structural
   constraints) expand the annotation surface and should be monitored for
   any uptick in escaping complaints.

3. **Open question 2 closed:** the maintainer records a decision — in
   this document or in `type-system-evolution.md` — that the string DSL
   is the form through 1.0, with the evidence trigger documented below.

4. **Evidence trigger for H12c (post-1.0 reconsideration):** *either*
   (a) a documented use case where users need to programmatically
   generate or import type aliases as eucalypt values (e.g. codegen from
   JSON Schema that emits `types:` blocks dynamically), *or* (b) a
   measured count of `{{..}}` escapes in user-authored code (outside the
   prelude) exceeding 20 % of type annotations over more than 50
   annotation sites. *Status: the value-**reference** need (trigger a) has
   fired* — [0009](0009-structural-contracts-validation.md) references type
   aliases as values — *and is met at 1.0 by the `s"…"` prefix* (§H12e), a
   string-form refinement, **not** by H12c. The distinction is load-bearing:
   `s"…"` is a parsed type **literal**, so it carries no termination hazard;
   **H12c proper** — *computed* type values from arbitrary expressions
   (including a runtime-resolved alias name) — remains deferred on exactly
   that hazard, to be re-evaluated post-1.0 if a concrete codegen need
   appears. Trigger (b) is dissolved: `s"…"` retires the `{{..}}` escape it
   measures.

---

## References

### Code

- `src/core/typecheck/parse.rs:1172` — `parse_type` public API
- `src/core/typecheck/parse.rs:1242` — `parse_type_with_refs` entry point (TS-A7)
- `src/core/typecheck/parse.rs:631–648` — alias reference parsing:
  capitalised `Token::Ident` → `Type::Var`; the comment at line 632
  is the canonical statement of the alias/variable duality; `AliasRef`
  span recording (A7.1) is at lines 643–646
- `src/core/typecheck/check.rs:182` — `AliasMap` type alias
- `src/core/typecheck/check.rs:206` — `aliases` field on `Checker`
- `src/core/typecheck/check.rs:749–798` — `register_aliases_from_meta`
  and `register_aliases_from_types_expr`: walk `types:` sub-blocks and
  populate the alias table
- `src/core/typecheck/check.rs:800–813` — `extract_type_def_name`:
  `type-def:` alias extraction
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
