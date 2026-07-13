# SV — Prefix-list type (design note)

- **Bead:** eu-2sa6.3 (P2), child of eu-2sa6 (0.13 epic — Frames, annotations,
  type-gated codegen, prefix-lists)
- **Pillar:** SV — the type-value surface (`ROADMAP.md` §Pillar SV, ~line 703;
  roadmap table line 1008)
- **Status:** design note for owner sign-off. **No implementation.**
- **Scope:** frontend / type checker (`src/core/typecheck/`) plus the prelude
  `t-*` projection consumers (`lib/reflect.eu`, `lib/prelude.eu`) and JSON-Schema
  export (`src/driver/doc/render.rs`).

---

## 0. Executive summary

The type DSL has fixed-arity tuples (`(A, B)`, `Type::Tuple`) and homogeneous
lists (`[T]`, `App(Con("List"), T)`) but nothing between them. The canonical
hiccup/markup element — `[tag, attrs, …content]` = `[Symbol, Block, (String |
Element)…]`, the shape `lib/markup.eu` is built on (`tag = head`, `attrs =
second`, `content = _ tail tail`, `lib/markup.eu:34-41`) — is neither: it has a
**fixed-shape prefix** (`Symbol`, `Block`) followed by a **homogeneous variable
tail** (zero or more `String | Element`). This note proposes a **prefix-list
type**: surface form `[A, B, C…]`, a new internal `Type::PrefixList { prefix,
tail }`, subtyping that carries literal-synthesised tuples into prefix-list
annotations, and precise `head`/`second`/`tail` projection that plugs into the
existing tuple-projection machinery.

**Premise-check result (verified with `eu check`, §1.1):** the surface
`[A, B, C…]` is entirely unclaimed today. Brackets currently accept exactly one
type — any comma inside is a parse error (`expected RBracket, got Comma`), the
`…` codepoint (U+2026) is not lexed at all (`unexpected character '…'`), and
`...` inside brackets errors as `DotDot`. There is therefore **no
backward-compatibility hazard**: every candidate spelling is a clean parse error
now, so accepting it is purely additive.

---

## 1. Premise-check (mandatory — per `2026-07-12-bytecode-transition-review.md` §5.2)

All checks run against a clean `--release` build of this worktree
(`origin/master` @ `dded5b90`). Each annotation is placed in `type:` metadata on
a binding and fed to `eu check`.

### 1.1 What the DSL parser accepts today

| # | Annotation string            | `eu check` result                                             |
|---|------------------------------|---------------------------------------------------------------|
| PC1 | `[symbol, block, string...]` | `parse error at position 7: expected RBracket, got Comma`   |
| PC2 | `[symbol, block, string…]`   | `parse error at position 7: expected RBracket, got Comma`   |
| PC3 | `[string...]`                | `parse error at position 7: expected RBracket, got DotDot`  |
| PC4 | `(symbol, block, string)`    | **clean — no warning** (baseline tuple)                       |
| PC5 | `[symbol, string]`           | `parse error at position 7: expected RBracket, got Comma`   |
| PC6 | `[string … ]`                | `parse error at position 8: unexpected character '…'`         |
| PC7 | `(symbol, block, string...)` | `parse error: expected ',' or ')' in tuple type`              |
| PC8 | `(symbol, block, ...string)` | `parse error: expected a type, got DotDot`                    |
| PC9 | `[symbol, block, ...[string]]` | `parse error at position 7: expected RBracket, got Comma`   |

### 1.2 Conclusions drawn from the premise-check

1. **Comma-in-brackets is free.** `parse_primary`'s `LBracket` arm
   (`src/core/typecheck/parse.rs:656-661`) parses exactly one `type` then
   `expect(RBracket)`. Every `[…, …]` form is rejected at the first comma today
   (PC1, PC2, PC5, PC9). So `[A, B, C…]` claims virgin syntax.
2. **`…` (U+2026) is not lexed.** The lexer (`parse.rs:150-308`) has no arm for
   `…`; it falls through to `unexpected character` (PC6). Accepting it means
   adding one lexer arm — additive.
3. **`...` collides with `DotDot`.** The `.` arm (`parse.rs:202-210`) only
   recognises `..` → `Token::DotDot` (used by record rows, `parse.rs:850+`).
   Three ASCII dots currently lex as `DotDot` then `Dot` (PC3). A `...` /`…`
   ellipsis token must be lexed **before** `..`, longest-match-first.
4. **Tuples are untouched** (PC4 clean). Keeping `(…)` as the tuple bracket and
   `[…]` as the list/prefix-list bracket preserves the existing tuple surface
   verbatim.

### 1.3 Chosen surface (rationale below in §2)

`[A, B, C…]` — brackets, comma-separated prefix, the **last** element carries a
trailing ellipsis marking it as the homogeneous tail. Both `…` (U+2026) and
`...` (three ASCII dots) are accepted as the marker, mirroring how the value
lexer treats unicode/ASCII equivalents elsewhere and keeping the form ASCII-
typeable.

---

## 2. Surface syntax

### 2.1 Grammar

Extend the `primary` production's list arm (`parse.rs` grammar header lines
20-33, implementation `parse_primary` `Token::LBracket`, `parse.rs:656`):

```text
list        ::= '[' type ']'                          # existing homogeneous list (unchanged)
             | '[' type ( ',' type )* ELLIPSIS ']'    # NEW prefix-list
ELLIPSIS    ::= '…'   (U+2026)
             | '...'  (three ASCII dots)
```

- The elements before the final one form the **fixed prefix**; the element
  immediately preceding `ELLIPSIS` is the **tail element type**.
- `[A, B, C…]` → prefix `[A, B]`, tail `C`.
- `[C…]` (no comma) → prefix `[]`, tail `C` → **normalises to `List(C)`**
  (a pure homogeneous list; no reason to keep a distinct node).
- The tail element may be any `type`, including a grouped union:
  `[Symbol, Block, (String | Element)…]` — the ellipsis is a **postfix** binding
  to the immediately preceding primary/grouped type, exactly analogous to the
  existing `?` postfix in `parse_application` (`parse.rs:541-543`).

**Deliberately *not* accepted in the first cut:** `[A, B]` **without** a
trailing ellipsis stays a parse error. We reserve comma-in-brackets exclusively
for the ellipsis-terminated form so there are never two spellings of a fixed
tuple (`(A, B)` remains the sole tuple surface). This keeps the surface minimal
and the error messages unambiguous.

### 2.2 Why brackets, not parens

The value-level literal is written with brackets — `[:div, {}, "hello"]` — and
`lib/markup.eu` constructs elements as bracketed lists. A **list**-shaped type
should wear the list bracket; parens stay the tuple/grouping bracket. This also
sidesteps the tuple-vs-prefix-list ambiguity that a paren form (PC7/PC8) would
introduce.

### 2.3 Lexer changes

Add `Token::Ellipsis`. In `next_token` (`parse.rs:150`):

- In the `'.'` arm (`parse.rs:202`), check `starts_with("...")` **before**
  `starts_with("..")` (longest match first) → `Token::Ellipsis`.
- Add a new arm for `'…'` (U+2026, 3 bytes UTF-8) → `Token::Ellipsis`.

`…`/`...` outside the final bracket position remains an error (the parser only
consults `Ellipsis` in the list arm), so the token is otherwise inert.

### 2.4 `s"…"` single-brace mode (SV1 interaction)

Prefix-lists are authored identically in `type:` metadata and in the SV1 `s"…"`
value surface (both feed `parse_type`/`parse_scheme`, `parse.rs:1184/1220`). No
special `s"…"` handling is required — the grammar addition is shared. Example
value-context authoring once SV1 lands: `element-type: s"[Symbol, Block,
(String | Element)…]"`.

---

## 3. Type representation

### 3.1 New variant

Add to `enum Type` (`src/core/typecheck/types.rs:242`), sitting alongside
`Tuple(Vec<Type>)` (line 292) and the `App(Con("List"), _)` list encoding:

```rust
/// Prefix-list: a fixed-shape prefix followed by a homogeneous variable tail.
/// `[A, B, C…]` = PrefixList { prefix: [A, B], tail: C }.
PrefixList { prefix: Vec<Type>, tail: Box<Type> },
```

- **Kind** `*` (add to `kind_of`, `types.rs:126`, alongside `Tuple`).
- **Display** (`Display for Type`, `types.rs:546`): render prefix comma-joined,
  then the tail type, then `…`: `[symbol, block, string | Element…]`. Round-trips
  through `parse_type` (a `roundtrip` unit test, `parse.rs:1650`).
- Smart constructor `Type::prefix_list(prefix, tail)` that **normalises**: empty
  prefix → `Type::list(tail)`; this keeps `[C…]` and `[C]`-of-homogeneous
  identical and avoids a redundant node.

### 3.2 Relationship to existing nodes (why a new variant, not reuse)

`PrefixList` is the general form that subsumes both neighbours:

- `Tuple([e₀…eₙ])` ≅ `PrefixList { prefix: [e₀…eₙ], tail: Never }` (empty tail:
  the residue list must be `[Never]`, i.e. exactly the prefix).
- `List(T)` ≅ `PrefixList { prefix: [], tail: T }`.

We nevertheless **keep `Tuple` and the `List` encoding as the canonical forms**
for their own shapes and only ever *produce* `PrefixList` from (a) the DSL
rest-form and (b) tail-projection (§5). Rationale:

- **BC / churn.** `Tuple` and `List` drive a large amount of existing display,
  subtyping, JSON-Schema, and `t-*` projection code. Re-encoding them as
  `PrefixList` would touch all of it for no user-visible gain and risks
  regressions in a MINOR.
- **False-positive containment (§7).** By making `PrefixList` **annotation-
  only** — it never arises from list-literal synthesis, which continues to
  produce `Tuple`/`NonEmpty` (`docs/guide/type-checking.md:186-192`) — no
  existing synthesis path changes shape, so no new over-warning surface is
  introduced.

Helpers (add to the `Decomposition helpers` block, `types.rs:402`):
`as_prefix_list(&self) -> Option<(&[Type], &Type)>`, and a `widen_literals`
arm (`types.rs:465`), a `humanise` arm (`types.rs:703`), and an `unfold_mu` arm
(`types.rs:826`) — all mechanical, mirroring the `Tuple` arms.

---

## 4. Subtyping rules

Added to `is_subtype_impl` (`src/core/typecheck/subtype.rs`, alongside the Tuple
arms at lines 130-148) and the meet/join dispatcher (lines 386-395). Let
`P = PrefixList{p, s}` (source) and `P' = PrefixList{p', t}` (target).
All positions are **covariant** (consistent with list/tuple covariance,
`subtype.rs:13-14`).

### 4.1 Prefix-list vs prefix-list (prefix widening + tail covariance)

`PrefixList{p, s} <: PrefixList{p', t}` iff

1. `len(p) ≥ len(p')` — the source may fix **more** leading positions;
2. for `i < len(p')`: `p[i] <: p'[i]` — pointwise on the shared prefix;
3. for `i ≥ len(p')` (the source's extra fixed elements): `p[i] <: t` — they
   are absorbed by the target's tail;
4. `s <: t` — tail covariance.

This is **prefix widening**: a longer, more specific prefix is a subtype of a
shorter one whose homogeneous tail can absorb the extra fixed elements. E.g.
`[Symbol, Block, String…] <: [Symbol, (Block|String)…]`.

### 4.2 Tuple vs prefix-list (the load-bearing rule)

`Tuple([e₀…eₙ]) <: PrefixList{p', t}` iff `n+1 ≥ len(p')`, `eᵢ <: p'[i]` for
`i < len(p')`, and `eᵢ <: t` for `i ≥ len(p')`.

**This rule is make-or-break.** A literal markup element
`[:div, {}, "a", "b"]` synthesises to
`Tuple([:div-sym, {}-record, string, string])`
(`docs/guide/type-checking.md:189-190`). For it to check against a
`[Symbol, Block, String…]` annotation, `Tuple <: PrefixList` must hold. Without
it, prefix-lists would be uninhabitable by ordinary literals.

The reverse — `PrefixList <: Tuple` — holds **only** in the degenerate empty-tail
case (`tail = Never`, prefix pointwise `<:` the tuple, equal lengths); otherwise
a prefix-list's length is unbounded and cannot satisfy a fixed tuple. First cut:
implement only the degenerate direction (or omit — a prefix-list value is never
required to flow into a tuple annotation in the markup use case).

### 4.3 Prefix-list vs `List(U)`

- **`PrefixList{p, s} <: List(U)`** iff every `pᵢ <: U` and `s <: U`
  (homogenise). E.g. `[Symbol, Block, String…] <: [any]` and `[String,
  String…] <: [String]`.
- **`List(U) </: PrefixList{p, t}` when `len(p) ≥ 1`.** An arbitrary list may be
  empty (or shorter than the prefix), so it cannot guarantee the fixed prefix.
  This is *correct and desirable*: it stops an unstructured list being passed
  where a markup element (≥ tag + attrs) is required. `NonEmpty([U])` guarantees
  only ≥1 element, so it is likewise `</: PrefixList` when `len(p) ≥ 2`.

### 4.4 Gradual consistency with `any`

Consistency (`is_consistent`, the gradual relation) treats `any` as consistent
with `PrefixList` in both directions, and is element-wise: `PrefixList{p, s}` is
consistent with `PrefixList{p', t}` when the prefixes are consistent up to the
shorter length (extra elements consistent with the other's tail) and `s`
consistent with `t`. An `any` prefix element or tail is vacuously consistent —
the standard gradual escape hatch, so a partially-annotated element never
over-warns.

### 4.5 Meet / join (first cut may be conservative)

`join(PrefixList, PrefixList)`: align prefixes to the shorter length, join
pointwise, fold each side's overflow prefix into the tail join, join tails.
`meet` dual. Where lengths differ awkwardly the first cut may **degrade to
`List(join/meet of all element types)`** or to `any` rather than compute the
exact aligned form — this is sound for a gradual system and avoids a fiddly
alignment bug in the first landing. Note the degrade explicitly in code so it
can be tightened later.

---

## 5. Indexed access / projection

The checker already projects tuples precisely through two mechanisms; both gain
a `PrefixList` arm.

### 5.1 `head` / `tail` — `apply_head_tail_to_tuple` (`check.rs:2906`)

Rename conceptually to "apply to tuple **or prefix-list**" and add:

```text
head  on PrefixList{p, t}:
  p non-empty → p[0]            (precise, guaranteed present)
  p empty     → (unreachable: normalised to List(t) at construction)

tail  on PrefixList{p, t}   (drop first element):
  len(p) ≥ 1  → PrefixList{ p[1..], t }   (normalises to List(t) when p[1..] empty)
  len(p) == 0 → List(t)                    (dropping from a homogeneous list stays homogeneous)
```

Worked example — the markup element `[Symbol, Block, (String|Element)…]`:

| Prelude accessor (`lib/markup.eu`) | Expression        | Synthesised type                     |
|------------------------------------|-------------------|--------------------------------------|
| `tag = head`                       | `head`            | `Symbol`  (prefix[0])                |
| `attrs = second`                   | `second`          | `Block`   (prefix[1], §5.2)          |
| `content = _ tail tail`            | `tail` once       | `PrefixList{[Block], String|Element}`|
|                                    | `tail` twice      | `List(String | Element)`             |

`content` lands as `[String | Element]` — precisely the intended content type.

### 5.2 Named projections — `ProjectionShape` (`check.rs:1381-1407`, §B6.3)

`recognise_projection_index` (`check.rs:2475`) maps `second`/`value` etc. to an
index `i`. The precise-typing block (`check.rs:1389-1407`) currently matches
only `Type::Tuple`. Add a `PrefixList{p, t}` arm:

```text
project index i on PrefixList{p, t}:
  i < len(p)  → p[i]                       (precise fixed element)
  i ≥ len(p)  → t                          (a tail element; note it MAY be absent)
```

`attrs = second` → index 1 on prefix `[Symbol, Block]` → `Block`. For
`i ≥ len(p)` the element is only *possibly* present (the tail may be empty), so
the result should be widened to `t?` (`t | ExecutionError`, `Type::partial`,
`types.rs:385`) or accompanied by an "index may be out of range" note — mirroring
the existing out-of-range tuple warning (`check.rs:1396-1402`). First cut:
return `t` with a soft note rather than a hard warning, to stay warning-quiet.

---

## 6. Flow-through (SV pipeline)

Prefix-lists must survive the full type-value surface. The `t-*` projection
schema is the **versioned surface** and tolerates *additive* growth
(`ROADMAP.md` Pillar SV, "Backward compatibility"), so a new `:t-prefix-list`
tag is a compatible addition.

### 6.1 `to-data` / `from-data` — the Type embedding (`lib/reflect.eu`)

Add a `:t-prefix-list` vocabulary term mirroring the checker constructor 1:1
(the `t-*` family, `reflect.eu:62-76`):

```text
s"[Symbol, Block, (String | Element)…]" to-data
  → [:t-prefix-list [[:t-con :Symbol] [:t-con :Block]] [:t-union …]]
      #                └ prefix (list of type-data) ┘  └ tail type-data ┘
```

- `to-data` (`reflect.eu:15`, `__TYPE_TO_DATA`) gains a `PrefixList` case in the
  native projection.
- `from-data` (`reflect.eu:19`) and the `to-str` renderer (`reflect.eu:56-78`)
  gain a `:t-prefix-list` arm rendering `[p₀, …, tail…]`.

### 6.2 `as-spec` / `match?` runtime validation (`lib/prelude.eu:2213`)

`to-spec-td` (`prelude.eu:2214`) gains a `:t-prefix-list` arm building a
`prefix-list-spec(prefix, tail)` predicate: check the value is a list of length
≥ `len(prefix)`, each prefix element matches its spec, and every remaining
element matches the tail spec. This slots beside the existing `tuple-spec`
(`prelude.eu:2224`) and `list-spec` (`prelude.eu:2220`). Default-fill can supply
missing tail elements as the empty list.

### 6.3 JSON-Schema export (`src/driver/doc/render.rs:207`)

`type_to_json_schema` already emits JSON-Schema-2020-12 `prefixItems` for
tuples with `"items": false` (`render.rs:207-217`). A prefix-list is the exact
2020-12 idiom with an **open** tail: emit `prefixItems` for the fixed prefix and
`"items": <tail schema>` (instead of `false`):

```json
{ "type": "array",
  "prefixItems": [ {"type":"string"}, {"type":"object"} ],
  "items": { "type": "string" } }
```

Add a `Type::PrefixList` arm to `type_to_json_schema` (`render.rs:178`).
(`prefixItems` + `items` is precisely the JSON-Schema 2020-12 tuple-with-rest
form — worth a one-line comment noting the correspondence.)

---

## 7. Checker impact & false-positive risk

The 0.11 optional-fields work needed **six** checker fixes to kill spurious
warnings. Prefix-lists avoid re-introducing that class by construction:

1. **Annotation-only production (the key lever).** `PrefixList` is produced
   *only* by the DSL parser and by tail-projection of an existing `PrefixList`.
   List-literal synthesis is **unchanged** — literals stay `Tuple`/`NonEmpty`
   (`docs/guide/type-checking.md:186-192`). No existing synthesis site changes
   shape, so nothing that used to check clean can newly warn.
2. **Tuple `<:` PrefixList carries literals across (§4.2).** Because literals are
   `Tuple`, the single most important correctness property is that a
   literal-built element checks against a prefix-list annotation. This is one
   subtyping rule, unit-testable in isolation (`subtype.rs` tests, line 679+).
3. **`tail` residue is never `List(Never)`.** Projecting `tail` off a
   prefix-list yields `List(tail)` (a genuine element type), not the empty-list
   sentinel `List(Never)` that triggers the "head of empty list" warning
   (`check.rs:1356-1374`). So the markup `content = _ tail tail` chain stays
   warning-quiet.
4. **Out-of-prefix projection is soft.** Named projection past the fixed prefix
   (`i ≥ len(p)`, §5.2) returns `t` with a note, not a hard warning, so a
   `second`/`third` on a short-prefix element does not spuriously error.
5. **`List </: PrefixList` is intentional, not a false positive** (§4.3). If a
   user annotates a genuinely unstructured list as a markup element they *should*
   be warned; the escape hatch is `any` or widening the annotation.

Checker touch-points, all additive arms beside existing `Tuple` handling:
`subtype.rs` (§4), `check.rs:1348` + `check.rs:1389` + `check.rs:2906` (§5),
`unify.rs` (a `PrefixList`/`PrefixList` structural unification arm, mirroring the
`Tuple` arm), and `resolve_typedata.rs:55` (an alias-resolution arm so aliases
inside a prefix-list's prefix/tail resolve — mirrors the `Tuple` arm at line 55).

---

## 8. Test plan

### 8.1 Unit tests (co-located, following existing patterns)

- **`parse.rs` tests** (line 1281+): `[A, B, C…]` and `[A, B, C...]` parse to the
  same `PrefixList`; `[C…]` normalises to `List(C)`; `[Symbol, Block, (String |
  Element)…]` parses with a grouped-union tail; round-trip (`roundtrip`,
  `parse.rs:1650`) for each; `[A, B]` (no ellipsis) still errors; ellipsis
  longest-match vs `..` (`[A…]` vs record `{..r}` unaffected).
- **`types.rs` tests** (line 880+): `Display` round-trip; `kind_of` = `Star`;
  `widen_literals`, `humanise`, `unfold_mu` arms.
- **`subtype.rs` tests** (line 679+): every §4 rule — prefix widening,
  `Tuple <: PrefixList` (§4.2), `PrefixList <: [any]`, `List </: PrefixList`,
  `any` consistency both directions, degenerate `PrefixList <: Tuple`.
- **`check.rs` tests**: `head`/`tail`/`second` projection on a prefix-list-typed
  binding gives the precise element types from §5.1's table.

### 8.2 Harness tests

- **`tests/harness/typecheck/`** (pattern `NNN_*.eu` + `.expect`, e.g.
  `005_no_warnings.eu`): a positive test where a literal markup element
  `[:div, {}, "a", "b"]` checks clean against a `[Symbol, Block, String…]`
  annotation (exit 0, no warning); a negative test in
  `tests/harness/typecheck/` or `tests/harness/errors/` where a wrong-shape value
  (e.g. `[42]` — number where `Symbol` prefix expected, or a bare `[string]`
  where a 2-prefix is required) produces a located warning.
- **`tests/harness/NNN_*.eu`**: an end-to-end SV flow test once SV1/SV2 land —
  `s"[Symbol, Block, (String | Element)…]" to-data` round-trips through
  `from-data`, and `as-spec`/`match?` accepts a valid element and rejects a
  malformed one.

### 8.3 The headline end-to-end test — typing `lib/markup.eu`

The acceptance goal (ROADMAP Pillar SV "Success"): annotate the markup element
type and check `lib/markup.eu`'s accessors end-to-end.

```eu,notest
` { type-def: s"[Symbol, Block, (String | Element)…]" }
Element: ...
` { type: "Element → Symbol" }  tag = head
` { type: "Element → Block"  }  attrs = second
` { type: "Element → [String | Element]" }  content = _ tail tail
```

`eu check lib/markup.eu` must report **no** type warnings for these accessors,
confirming `head`/`second`/`tail` project the prefix-list precisely and the
`content` residue lands as `[String | Element]`. This is the concrete
sign-off criterion.

---

## 9. Non-goals (first cut)

1. **No prefix-list inference from list literals.** Literals stay
   `Tuple`/`NonEmpty`; `PrefixList` is annotation-only (§3.2, §7). Inferring a
   prefix-list from a heterogeneous-then-homogeneous literal is a possible later
   refinement.
2. **No middle rests / multiple tails.** TypeScript allows `[A, ...B[], C]` (rest
   not in tail position). We accept the trailing tail only. One tail, at the end.
3. **No optional prefix elements** (`[A, B?, C…]`). Composing prefix optionality
   with the tail is deferred; it interacts with the optional-fields work and is
   not needed for markup.
4. **No exact meet/join alignment.** First cut may conservatively degrade to
   `List`/`any` for awkward length mismatches (§4.5).
5. **No dedicated runtime `Native` representation.** Prefix-list *values* are
   ordinary lists at runtime; only the *type* is new. `to-spec` validation walks
   the ordinary list (§6.2).
6. **`PrefixList <: Tuple` beyond the degenerate empty-tail case** is out of
   scope (§4.2) — not required by the markup use case.

---

## 10. Sequencing & sign-off

Independent of the runtime pillars; pure frontend + prelude + doc work. Suggested
landing order, each a reviewable PR to `master`:

1. **Lexer + parser + `Type::PrefixList` + Display/round-trip** (§2, §3) — the
   surface and representation, with unit tests. No checker behaviour yet.
2. **Subtyping + unify + projection** (§4, §5) — makes annotations meaningful;
   the `Tuple <: PrefixList` rule (§4.2) and the markup projection tests are the
   gate.
3. **Flow-through** (§6) — `t-prefix-list` in `to-data`/`from-data`/`to-spec`
   and JSON-Schema export; the end-to-end `lib/markup.eu` test (§8.3).

Docs to update at implementation time (not in this note): `docs/guide/type-
checking.md` (a "Prefix-lists" subsection after "Tuples", line 195),
`docs/reference/agent-reference.md` and `docs/appendices/cheat-sheet.md` (the
type-DSL summary).

**Open questions for owner sign-off:**

- **Q1 — ellipsis spelling.** Accept both `…` and `...`, or ASCII `...` only for
  editor-friendliness? (Recommendation: both, with `…` canonical in Display.)
- **Q2 — out-of-prefix projection.** Return `t` (tail element) with a soft note,
  or `t?` (partial, acknowledging possible absence)? (Recommendation: `t?` is
  more honest; soft `t` is quieter. Leaning `t?`.)
- **Q3 — normalise `[C…]` to `List(C)`?** (Recommendation: yes — no distinct
  meaning, avoids a redundant node.)
