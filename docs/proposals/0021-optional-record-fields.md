# 0021 — Optional (presence-annotated) record fields

- **Status:** Draft proposal for review
- **Track:** C — type system & language
- **Classification:** Extends-Roadmap
- **Suggested horizon:** 1.0 (annotated form); presence-*inference* deferred post-1.0
- **Related:** H3 (row polymorphism — `type-system-evolution.md`), TS-B9 (row inference, shipped 0.7.0); sibling proposals [0009](0009-structural-contracts-validation.md) (the `match?`/spec enrichment), [0019](0019-host-language-interop.md) (the prerequisite this unblocks), [0016](0016-eu-doc.md) (`eu doc`), [0012](0012-algebraic-subtyping-fork.md) (hand-rolled vs MLsub)

---

## Summary

Eucalypt's record types express a field as **required** (a closed record `{k: T}`) or permit **arbitrary extra fields** (an open record `{k: T, ..}`), but they cannot express *"this field may be absent, and has type `T` when present"* — the single most common shape in configuration data and in the JSON Schema / Kubernetes-CRD ecosystem [0019] targets. This proposal adds **presence-annotated fields**: a `name?: T` surface (a currently-unused syntax slot, distinct from `name: T?`), a per-record required/optional partition in the type representation, the corresponding presence subtyping, and the matching rule in [0009]'s `match?`/spec engine. It is the *annotated* case of H3's field-presence; presence-*polymorphic inference* is a deferrable later step. It is hand-rolled on the predictable core, consistent with [0012]'s decision not to rebuild on MLsub.

## Motivation

The record type is `Type::Record { fields: BTreeMap<String, Type>, open: bool, rows: Vec<TypeVarId> }` (`src/core/typecheck/types.rs:238`). There is **no per-field presence flag**: every declared field is required, and `open` only governs whether *undeclared* fields are tolerated. Subtyping enforces this — there is a `missing_required_field` test (`src/core/typecheck/subtype.rs:704`) that rejects a record lacking a declared field. So the system can say:

- `s"{name: string}"` — exactly `name`, required, no extras;
- `s"{name: string, ..}"` — `name` required, plus arbitrary extra fields (untyped tail);

…but it **cannot** say `s"{name: string, age?: number}"` — "`name` required, `age` optional but typed when present." That is the dominant real-world shape:

- **Configuration is optional-field-dominated.** Most config keys have defaults and may be omitted; "required vs optional with a type" is the everyday case for a config/data language (non-negotiable #5).
- **JSON Schema's `required`/`properties` split is universal** ([0019]): a Kubernetes CRD `spec` typically has one or two required fields and dozens of optional-but-typed ones. Without per-field presence, [0019]'s CRD *import* cannot be faithful — it is the gap that blocks the headline interop use case.
- **[0009] contracts inherit the gap.** The value-pattern matcher's field rule, `me?`, is `(t has(k)) ∧ mv?(v, t lookup(k))` (`lib/prelude.eu:548-549`) — it *requires* the key to be present. So a spec cannot express "if `age` is present it must be a number, but it may be absent."

None of the available encodings closes the gap:

| Attempt | Why it fails |
|---|---|
| All declared fields required | Rejects valid data that omits an optional field. |
| Open record `s"{name: string, ..}"` | The optional field becomes an *untyped* extra — its `number` type is lost, and it is indistinguishable from any other stray key. |
| `s"{name: string, age: number?}"` | `T?` is **`Partial(T)`** = `T \| ExecutionError` (`parse.rs:541-543`): "present, possibly an error value", **not** "may be absent". And `me?` still requires presence, so a missing `age` fails. |

The conflation in the third row is the trap: eucalypt already overloads `?` for *fallibility* (`Partial`); field *presence* is an orthogonal axis that has no surface today.

## Prior art & landscape

**Row-presence theory.** The clean account of "a field that may or may not be there" is *presence polymorphism*. H3 ("Finish row polymorphism", `type-system-evolution.md:253`) already names the two row algebras: **Wand/Rémy "absent labels"** (each row variable carries the labels it cannot contain — the presence-tracking algebra, used in OCaml polymorphic variants) and **Leijen "scoped labels"** (ordered, repeatable labels — which H3 *recommends* because it matches eucalypt's rightmost-wins merge, `:297`). The key observation for this proposal: the recommended algebra (Leijen) was chosen for **merge**, not presence — so optional fields are *not* a free consequence of it, and must be added explicitly.

**TypeScript optional properties** are the direct surface analogue: `interface C { name: string; age?: number }` means exactly "name required, age optional, number when present", and `age?: T` is distinct from `age: T | undefined`. TypeScript's `?:` is the obvious surface to borrow — and the slot is free in eucalypt.

**Pkl / KCL / JSON Schema** all distinguish required from optional per property (Pkl's `prop: Type?` nullable vs required-by-default; JSON Schema's `required` array). For a tool that wants to *exchange* schemas ([0019]), matching this distinction is table stakes.

**What to borrow:** TypeScript's `name?:` surface and the present-or-absent semantics; the presence subtyping. **What not to do yet:** full presence-*polymorphic inference* (a function generic over "has field X or not"). The annotated form covers [0019] and [0009]; inference is the presence analogue of how H3 deferred *unannotated* row inference to TS-B9.

## Proposed design

### Surface: `name?: T`

A field key may carry a trailing `?`, marking the field **optional**:

```eu,notest
{ types: {
    Server: s"{ host: string, port?: number, replicas?: number }"
} }
```

`host` is required; `port` and `replicas` are optional and typed `number` when present. The `?` sits on the **key**, which is an unused slot today — the record-field parser reads `Ident : type` with no key-side `?` handling (`src/core/typecheck/parse.rs:971-1012`), and `?` is otherwise only a *type suffix* meaning `Partial` (`:541-543`). The two compose without ambiguity:

- `port: number` — required, plain number;
- `port?: number` — optional; absent, or a number when present;
- `port: number?` — required key, value is `Partial(number)` (may be an error);
- `port?: number?` — optional; absent, or `Partial(number)` when present.

(Examples use the `s"…"` type literal from [0013]; records inside it need no `{{..}}` doubling.)

### Representation

`Type::Record` gains a presence partition — the minimal change is one field, orthogonal to `open`/`rows`:

```rust
Record { fields: BTreeMap<String, Type>, optional: BTreeSet<String>, open: bool, rows: Vec<TypeVarId> }
```

`optional ⊆ fields.keys()`. Everything not in `optional` is required (preserving today's default). `open`/`rows` continue to govern the *undeclared* tail — presence is about *declared* fields, a separate axis.

### Subtyping

Presence generalises the existing `missing_required_field` rule. For a value/source record `S` checked against an expected record `T`, per declared field `k` of `T`:

- `k` **required** in `T`: `k` must be present in `S` and `S[k] <: T[k]` (today's rule).
- `k` **optional** in `T`: either `k` is absent in `S`, or `S[k] <: T[k]`.

And a *required* field in `S` satisfies an *optional* expectation in `T` (providing more than asked is fine). This is ordinary width/depth subtyping with a presence lattice `required <: optional` (a required field is usable wherever an optional one is expected, never the reverse).

### Merge × presence — the one real design point

Eucalypt's record merge is rightmost-wins, and Leijen scoped-labels was chosen to model it. Presence must define how merging two record *types* combines field presence. The proposed rule, consistent with rightmost-wins: **a field's presence in the merge is that of the rightmost contributor that declares it** (the override winner sets presence), and a field declared optional on one side and absent on the other stays optional. This is the rule most likely to match `block merge` semantics at the value level; it must be pinned with tests, because it is the place presence actually touches the merge algebra rather than sitting orthogonal to it.

### [0009] enrichment: `me?` gains an optional arm

The value-pattern field rule (`lib/prelude.eu:548-549`) currently requires presence. The spec engine learns the optional case: for an optional field `k`, `(t has(k)) ⟹ mv?(v, t lookup(k))` — present ⇒ must match; absent ⇒ pass. So `validate(s"{name: string, port?: number}", {name: "h"})` succeeds, and `{name: "h", port: "x"}` fails. This is the runtime half; the static and runtime sides gain presence together.

### [0019]: the required-set mapping

With presence, [0019]'s export maps cleanly: required fields → JSON Schema `required: [...]`, optional fields → present in `properties` but absent from `required`; and import does the reverse (`required` membership → required, else `name?:`). The mapping tables in [0019] should be updated to carry the presence column this proposal supplies.

## Interaction with the existing roadmap

- **H3 (rows/records).** This is the *annotated presence* member of the H3 family. It is **orthogonal to the Wand-vs-Leijen choice**: keep Leijen scoped-labels for merge and the open/extra tail; add presence as a per-record partition. Presence-*polymorphic inference* (Wand/Rémy-style presence variables) is the deferred extension — the presence analogue of TS-B9's deferral of unannotated row inference. (`type-system-evolution.md` is the now-stale origin sketch; this proposal is where the decision lives.)
- **[0019]** depends on this — it is the prerequisite that makes CRD/JSON-Schema import faithful (the optional-field-dominated case). [0019]'s "1.0-adjacent" core is gated on it.
- **[0009]** is enriched by it — specs gain optional-field matching, which is core to validating real (default-bearing) config.
- **[0016]** `eu doc` shows per-field optionality once present.
- **[0012]** — another presence feature *hand-rolled on the predictable core*, exactly the "keep extending the current core" stance 0012 settled; MLstruct's row lattice would give presence for free, but we hand-roll the annotated case as we did rows, unions, and constraints.

## Implementation sketch

Front-end + spec-engine; no STG/VM/GC change; types stay erased.

1. **Representation + parser (~S).** Add `optional` to `Type::Record` (`types.rs`); thread it through `humanise`/display. Teach the record-field parser the key-side `?` (`parse.rs:971-1012`). Default-empty `optional` keeps every existing annotation meaning identical.
2. **Subtyping + unification (~M).** Add the presence arm to record subtyping (`subtype.rs`, generalising `missing_required_field`, `:704`); extend record unification (`unify.rs:292-447`) and the merge rule (the design point above) to combine presence.
3. **Synthesis (~S).** `synthesise_block` (`check.rs:1125`) infers a *literal* block as all-required (every field is present) — correct and unchanged in spirit; presence only appears via annotations or merge. Presence-polymorphic *inference* is explicitly **not** in scope.
4. **Spec engine (~S).** The `me?` optional arm in `lib/prelude.eu` (and `valid?`/`validate` inherit it).
5. **Consumers.** [0019] required-set mapping; [0016] doc rendering.

Risk: **medium**, concentrated in (2) — the merge × presence rule and presence's interaction with row variables and unions. The annotated form is bounded; the unbounded part (inference) is deferred. Each new subtype/merge rule needs harness coverage.

## Alternatives considered

- **Open records as a stand-in.** Use `s"{name: string, ..}"` and rely on the tail. Rejected: the optional field loses its type and is indistinguishable from arbitrary extras.
- **Overload `T?` (Partial) for optionality.** Rejected: `Partial` means *present-but-possibly-error*, a different axis; conflating it with presence breaks both, and the spec `me?` rule would still demand presence.
- **Full presence-polymorphic inference now (Wand/Rémy presence variables).** Deferred, not rejected: the annotated form covers [0019]/[0009]; inference is the natural follow-on (TS-B9 analogue) once demand appears.
- **Switch H3's row algebra to Wand/Rémy for presence.** Rejected: Leijen scoped-labels is the right fit for eucalypt's merge; presence is cleanly orthogonal as a per-field partition, so the tail algebra need not change.
- **Do nothing.** Leaves [0019] unable to import the common CRD shape and [0009] unable to validate optional config fields — the status quo gap.

## Risks & what would kill this

- **Merge × presence underspecified.** If the rightmost-wins presence rule does not match value-level `block merge` behaviour, annotations will mislead. *Mitigation:* pin the rule against the evaluator's merge with harness tests before landing.
- **Subtyping complexity creep.** Presence × open × rows × unions is more cases than today. *Mitigation:* keep presence a simple per-field partition (not a presence-variable lattice); defer inference.
- **It only fixes the presence axis.** JSON Schema's *value* constraints (`minLength`, `pattern`, `minimum`, …) remain unmapped ([0019] risk 3) — optional fields do not change that; users still reach for [0009] refinements for those.
- **Demand.** If real schemas users import are mostly all-required, the feature is over-built. *Falsifier:* a representative CRD corpus that is predominantly required-fielded. (Unlikely — Kubernetes CRDs are overwhelmingly optional.)

## Success criteria

- `s"{name: string, age?: number}"` parses; `{name: "x"}` checks clean (absent `age` OK); `{name: "x", age: "y"}` warns (present, wrong type).
- `validate(s"{name: string, age?: number}", {name: "x"})` passes; with `age: "y"` it fails — the [0009] `me?` optional arm.
- A merge of two record types combines presence by a defined, tested rule.
- [0019] round-trips a CRD's required/optional split: import → `name?:`/required, re-export → correct `required` array.
- No regression in `cargo test --test harness_test`; `cargo clippy --all-targets -- -D warnings` clean.

## References

- `src/core/typecheck/types.rs:238` — `Type::Record { fields, open, rows }` (no presence today).
- `src/core/typecheck/subtype.rs:704` — `missing_required_field` (declared fields are required).
- `src/core/typecheck/parse.rs:541-543` — `?` as the `Partial` type suffix; `:971-1012` — record-field parsing (`Ident : type`, no key-side `?`).
- `src/core/typecheck/check.rs:1125` — `synthesise_block` (literal blocks as all-present).
- `src/core/typecheck/unify.rs:292-447` — record unification / row absorption.
- `lib/prelude.eu:548-549` — `me?`, the value-pattern field rule that requires presence (the [0009] hook).
- `docs/development/type-system-evolution.md:253,288-311` — H3 row polymorphism and the Wand/Rémy vs Leijen row algebras.
- Sibling proposals: [0009](0009-structural-contracts-validation.md), [0019](0019-host-language-interop.md), [0016](0016-eu-doc.md), [0012](0012-algebraic-subtyping-fork.md).
- TypeScript optional properties (`age?: T`); Wand (1987), Rémy (1994), Leijen "Extensible Records with Scoped Labels" (2005); JSON Schema `required`/`properties` (draft-2020-12).
