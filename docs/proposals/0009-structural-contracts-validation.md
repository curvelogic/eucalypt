# 0009 — Structural contracts & runtime schema validation

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Whitespace
- **Suggested horizon:** 1.0
- **Related:** H7 (refinement types) and H13c (opt-in boundary casts) in [type-system-evolution.md](../development/type-system-evolution.md); §5 open questions 6 & 7; sibling proposals [0002 — gradual-typing boundary policy](0002-gradual-typing-boundary-policy.md), [0010 — capability & determinism types](0010-capability-determinism-types.md), [0019 — host-language & schema interop](0019-host-language-interop.md)

## Summary

Eucalypt's gradual type checker is static, whole-program, advisory, and erased before execution. That is the right design for *code*, but a configuration tool's real correctness risk is not its code — it is the **external data** it ingests at runtime: YAML/JSON/TOML parsed from strings, imported files, CLI arguments, environment. Static types cannot vouch for the *content* of any of those, and as of 0.7.0 they say so precisely: `parse-as` is now typed `symbol → string → any?` — a `Partial` result (`lib/prelude.eu:1102`), `any` on the success path, `ExecutionError` on the failure path. The shape that comes back is still unknown. This proposal argues eucalypt should grow a first-class, **structural contracts/validation** mechanism — the *runtime* complement to the static checker — that validates data at ingress against a shape, produces blame-quality errors, reuses the *existing type-DSL vocabulary*, and lives metadata-first. The lever this proposal once described as future is now in place: the type DSL already expresses `Dict(T)`, equirecursive `Mu`, literal-string types and `NonEmpty` (shipped in 0.6.2/0.7.0), so a contract is simply *one of these shipped shapes run as a predicate-with-blame over data*. Every serious peer (Nickel, CUE, Pkl, KCL) treats runtime data validation as a headline feature; eucalypt, alone among them, has no answer beyond ad-hoc `assert`/`panic`. This is the pragmatic resolution of the boundary-policy tension [0002] leaves open, and a pragmatic alternative to typing the metadata channel (open question 7, closed *won't-do*).

## Motivation

**The static checker stops exactly where the risk starts.** Eucalypt is a tool for generating and transforming YAML/JSON/TOML before it is a language; its job is to turn external data into a rendered artefact. The type checker is "an optional, advisory type checker [that] never prevents your code from running" (`docs/guide/type-checking.md:1-6`). It catches mistakes in *eucalypt expressions* — IO-action misuse, misspelled fields on a *typed* block, broken lens composition. It explicitly does **not** reason about runtime-dependent values: "If a function returns `any` (because its type depends on a runtime value, like `lookup`), the checker makes no assumptions about the result" (`docs/guide/type-checking.md:42-45`).

As of 0.7.0 the checker has gone one honest step further: the fallible ingress functions now carry `Partial(T)` / `T?` types — sugar for `T | ExecutionError` (`src/core/typecheck/parse.rs:115`, `src/core/typecheck/types.rs:210`) — and a warning fires when a partial result flows into a *total* position (`src/core/typecheck/check.rs:1581-1582, 2042-2050`, helper at `:3075-3081`). That is exactly the **data-ingress boundary** this proposal is about, now visible to the checker as a *failure* possibility. But the checker can only flag *that* a value may fail; it still cannot say what *shape* the success path has. Every channel through which external data enters is, by construction, either `any`-typed or `any?`-typed:

- **`parse-as`** parses a string of structured data and returns eucalypt data, "parsed as inert data" (`lib/prelude.eu:1101-1103`). As of 0.7.0 it is typed `symbol → string → any?` (`lib/prelude.eu:1102`) — `any` on success, `ExecutionError` on failure. Its intrinsic `__PARSE_STRING` is still `(symbol, string) -> any` (`src/eval/intrinsics.rs:828-829`). Nothing downstream knows the shape.
- **Imports** load external YAML/JSON/TOML/CSV/XML/EDN files (`src/import/`). The loaded block enters as data with no static shape.
- **`parse-args`** turns `[string]` CLI arguments into a block, typed `{{..}} -> [string] -> {{..}}` (`lib/prelude.eu:2152-2154`) — the result is "some block", no known fields.
- **`render-as`** is the dual: `__RENDER_TO_STRING` is `(any, symbol) -> string` (`src/eval/intrinsics.rs:823-824`). It will serialise *whatever* it is given.

So the type system genuinely **stops at the I/O boundary**: `any`/`any?` in, `string` out, and the checker stays silent about *shape* in between, by design ([0002]; `any` is consistent with every type, `src/core/typecheck/subtype.rs:100`).

**And the consequence of a silent shape error is unusually expensive,** because `lookup` *raises* rather than returning null. `lookup(s, b)` is documented "error if not found" and now typed `symbol → Dict(a) → a?` (`lib/prelude.eu:410-412`) — its `Partial` annotation makes the raise explicit in the type, while the runtime behaviour is unchanged. The existence of `lookup-or` with an explicit default (`lib/prelude.eu:418-420`) is the tell, and at the STG level a missing key compiles to `LOOKUP_FAIL`, which produces "Key not found: {key}" (`src/eval/stg/block.rs:1104-1130, 1751`). So a manifest assembled from data whose shape is *slightly* wrong — a renamed upstream key, a string where a number was expected — does not render a wrong-but-plausible artefact; it aborts deep in evaluation, at the *use* site, with an error naming the symbol but not the *source* of the bad data. The user learns `:replicas` is missing; not that `config.yaml` omitted it. The `Partial` annotation now *predicts* this failure statically, but it cannot *prevent* it or locate the bad data — the gap between "where the data was wrong" and "where evaluation noticed" is still the whole problem.

Today the only tools are `assert(p?, s, v)` (`lib/prelude.eu:1226-1227`, a thin `panic` wrapper) and the test-only `//=` / `//!` expectation operators (`lib/prelude.eu:1236-1280`). Neither is a *schema*. There is no way to say, once, at ingress: *this data must look like this, and if it does not, tell me precisely how it failed and where.*

## Prior art & landscape

Runtime data validation is the feature every configuration peer leads with. The shape they converge on is instructive.

| System | Validation mechanism | Reuses type vocabulary? | Blame quality |
|---|---|---|---|
| **Nickel** | runtime *contracts* applied at boundaries | yes — same annotation is a type *or* a contract | high — labels, message, notes, blame location |
| **CUE** | constraints via value/type unification | yes — "types are values", one lattice | schema-level |
| **Pkl** | type annotations with inline constraint predicates | yes — `String(!isEmpty)`, `Int(isBetween(0,130))` | per-property |
| **KCL** | `check` block of conditional expressions in a schema | partial — schema + checks | per-check message |
| **JSON Schema** | external declarative schema document | no — a separate language | path-based |

**Nickel** is the closest and the most borrowable. Contracts are checked at runtime; the *same* syntax can describe both a static type and a runtime contract; and a contract is built from built-in pieces (records, arrays, `Number`/`String`) freely mixed with custom predicates — contracts act like schemas. Crucially, Nickel's **validators** return not a boolean but an enum: `'Ok` on success or `'Error` carrying optional `message`, `notes`, and `blame_location` fields, and **blame** ends execution with a detailed report assembled from a *label* threaded through the check. Nickel can even validate untyped data non-invasively: `nickel eval config.json --apply-contract schema.ncl`. This is the design we should study hardest. The intellectual root is **Findler & Felleisen, *Contracts for Higher-Order Functions* (ICFP 2002)**, which introduced *blame* — assigning a contract failure to the right side (positive blame for the producer, negative for the context) — and which the literature credits as "the catalyst for gradual typing".

**CUE** validates *and* removes boilerplate by unifying types and values into one lattice: "constraints are values because all types are values", and a unified constraint both checks data and supplies defaults. This is powerful but it is exactly what eucalypt should **not** borrow: CUE's whole-language unification model *is* the language — every value is a constraint, merging is unification. Grafting that onto eucalypt's catenation core would be a second, incompatible evaluation model, not a feature. Eucalypt already has a merge with deliberately *different* semantics; we want validation, not a value lattice.

**Pkl** keeps validation lightweight: a type annotation may carry an arbitrary constraint expression — `String(!isEmpty)`, `Int(isBetween(0, 130))`, `String(matches(Regex(...)))`. **KCL** puts a `check` block of conditional-expression-plus-message lines inside a schema and validates JSON/YAML against it. Both confirm the same lesson: validation belongs *attached to the shape*, expressed in the host language's own predicates, with a per-failure message.

What to borrow: Nickel's contract-as-schema model, its `'Ok`/`'Error`-with-blame validator result, and Pkl/KCL's inline predicate refinements. What not to borrow: CUE's unify-everything core, and JSON Schema's *separate* schema language (eucalypt should validate against shapes written in the vocabulary users already know — its own type DSL — not a foreign document).

## Proposed design

**A contract is a shape, evaluated as a runtime predicate-with-blame over data.** The design has one principle: *do not make users learn a second shape language*. Eucalypt already has a shape vocabulary — the type DSL parsed by `src/core/typecheck/parse.rs`, which today expresses records (open/closed/row — `parse.rs:603, 824`, `types.rs:238`), unions (`Type::Union` — `types.rs:249`, `parse.rs:513`), literal symbols *and* literal strings (`Type::LiteralSymbol` / `Type::LiteralString` — `types.rs:253, 255`, `parse.rs:729, 691`), alongside `[T]`, tuples, and the `IO`/`Lens` constructors. Crucially, the shapes this proposal once treated as roadmap items have **shipped**: 0.6.2/0.7.0 delivered `Dict(T)` for homogeneous blocks, the `NonEmpty` refinement, literal-string singletons, and equirecursive `Mu` types — now all represented uniformly as `Con`/`App` constructor application (`types.rs:299` `Dict`, `:305` `NonEmpty`, `:266` `Mu`), with full row inference for unannotated block parameters. The vocabulary validation wants therefore *exists and is typed today*; the only remaining roadmap dependency is H7's arithmetic refinements (`Positive`, `Nonzero`). The contract system **reads the same shapes**. A shape that the static checker uses for advice, the contract engine uses for enforcement — and because the two now draw on one shipped representation, there is no risk of the contract DSL and the type DSL diverging into two vocabularies.

Expose it **metadata-first** and at the **natural ingress points**. Two surfaces, both conservative (non-negotiable #1 — machinery in metadata, symbols, operators, prelude forms, never new keywords):

**(1) A `validate(shape, data)` prelude form** — the explicit, composable core (`validate`, `valid?`, and the refinement predicates below are the *proposed* additions). `shape` is a contract value; `data` is anything; the result is the data on success or a structured error on failure. The companion `valid?(shape, data)` returns a boolean. A shape is built either from a type-DSL string (reusing the existing parser) or from a predicate:

```eu,notest
{ import: "raw.eu"
  types: { Server: "{host: string, port: number, replicas: number}" } }

# Validate imported data at ingress; blame points at the source, not a deep use site
config: imported-data validate(:Server)

# A refinement contract from a predicate, named in metadata
` { contract: positive? }
port: config.port
```

**(2) A `check:`/`contract:` block annotation** in declaration metadata, mirroring KCL's `check` and Nickel's contract attachment. A block's fields each name a shape; each `check:` entry is a `[predicate, message]` pair evaluated against the block (anaphoric `_` is the block):

```eu,notest
` { contract: { host: string?, port: number?, replicas: number? }
    check: [ [(_.port > 0),      "port must be positive"],
             [(_.replicas >= 1),  "need at least one replica"] ] }
server(raw): raw
```

Validation is **structural, not nominal** (non-negotiable #2): `:Server` is a *shape*, and any block presenting the right fields satisfies it — there is no declared class, no `implements`. This is what distinguishes the proposal from a schema-registry approach and keeps it native to eucalypt's "typeclasses without classes" stance.

Insert contracts at the boundaries that already exist. The high-value integration is making the ingress functions *contract-aware* rather than asking users to remember to call `validate`:

```eu,notest
# parse-as / imports / parse-args grow an optional shape argument
data: json-string parse-as(:json, :Response)  # validated on parse
opts: parse-args(defaults, cli-args)          # defaults block carries field contracts
```

Because `parse-args` already coerces values against the `defaults` block (`lib/prelude.eu:2172-2174`, via `parse-as`) and already raises on unknown options, contract-checking its output is a natural extension of behaviour it half-performs today.

**Semantics.** A contract runs as ordinary core — it is compiled and evaluated like any other expression, so it imposes **zero** cost where it is not invoked, and it requires **no** change to STG, the heap, or the erasure guarantee [0002] preserves. On success it returns the data unchanged (so it threads transparently through a pipeline). On failure it raises a single `ExecutionError` carrying a *structured* report (below). Contracts are *shallow by default*: a record contract checks the named fields and their immediate types; nesting is opt-in by nesting the shape (`{server: :Server}`), never an automatic deep proxy — this is the transient/shallow discipline [0002] endorses, deliberately *not* the pervasive higher-order wrapping that the sound-gradual-typing literature measures at catastrophic cost.

## Interaction with the existing roadmap

This is the **runtime half** of the boundary story that [0002] frames statically. [0002] ratifies the *type* boundary as permanently optimistic and silent (H13a) — `any → T` is trusted, no cast, no blame — for sound performance reasons (AOT, no JIT, GC-bound). The 0.7.0 `Partial(T)` annotations sharpen exactly that boundary: the checker now *names* the points where ingress can fail (`parse-as`, `lookup`, `nth`), and warns when a failure flows on unhandled — but by design it stops there, trusting the success path's shape. It then explicitly hands off the "I want my manifest validated" use case to *this* document: contracts validate **where the user writes them** (Findler–Felleisen pay-as-you-go), at data ingress, rather than the type system imposing checks at *every* boundary. The two compose into one coherent policy: static checking via `eu check --strict` in CI (now including the partial-flow warnings); optimistic, erased type boundaries in production; explicit contracts at the few ingress points where runtime validation earns its keep. Contracts are the *answer*; [0002] is the *reason that answer is contracts rather than sound casts*.

It draws its **shape vocabulary** from shapes that have largely **shipped**. `Dict(T)` (the H19 item) landed in 0.6.2 along with the `NonEmpty` refinement; only H7's arithmetic refinements (`Positive`, `Nonzero`) remain outstanding, and they are described in the roadmap as "type constructors. Cheap to implement, useful, no solver needed". Whether shipped or pending, refinements are *more* valuable as **contracts** than as static types: a static `Positive` can rarely be proven of `any`-typed input, but a runtime `Positive` *contract* checks it exactly when the value arrives — and the same is already true of the shipped `NonEmpty`, which a checker can seldom establish for imported data but a contract can verify on ingress. This proposal gives the refinements their highest-leverage consumer. It is also the pragmatic resolution of **open question 7** ("is typing the metadata channel worth doing?", closed *won't-do*): rather than make the type lattice flow through metadata, we let *contracts* — runtime, structural, opt-in — police the data that metadata-typing would have policed statically, at a fraction of the cost.

It must **not** become the H13b sound-cast road [0002] rejects. The discipline is: contracts fire only at explicit ingress points and explicit `validate` calls, never pervasively, never as automatic higher-order proxies.

## Implementation sketch

- **Contract representation & engine** — a new module that interprets a `Type` (from the existing `parse.rs`) *as* a predicate over a runtime value, reusing the predicate intrinsics that already exist (`__ISBLOCK`, `__ISLIST`, `__ISNUMBER`, … — `src/eval/intrinsics.rs:862` onward; prelude `block?`/`list?`/`number?` at `lib/prelude.eu:376-392`). Medium size; the core is a recursive `check(shape, value) -> Result`.
- **Prelude surface** — `validate`, `valid?`, and the refinement predicates (`positive?`, `non-empty?`, …) added to `lib/prelude.eu`. Small.
- **Structured blame** — extend `src/eval/error.rs`, which already builds `codespan_reporting` diagnostics with `help:` hints and "available keys" suggestions (`error.rs:5, 277-287`), with a `ContractViolation` variant carrying: the failed field/path, the expected shape, the actual value's kind, and the ingress source span. This is the make-or-break work (below).
- **Ingress integration** — optional shape arguments on `parse-as`, the import path (`src/import/`), and `parse-args` (`lib/prelude.eu:2152`). Small per site; behaviour-preserving when the argument is omitted.
- **No STG/GC/typecheck-core change.** Contracts are ordinary evaluated core; erasure and the [0002] boundary guarantee are untouched.

Sequencing: land `validate`/`valid?` + the engine + structured blame first (self-contained, testable with `//=`); add the `contract:`/`check:` annotation; wire ingress integration last; fold in H7 refinements as they arrive.

## Error-message quality is the make-or-break

A contract that says only "validation failed" is worse than the `LOOKUP_FAIL` it was meant to improve on. The entire value proposition is **Clarion-grade structured blame**: a failure must report (a) the precise path to the offending field (`config.servers[2].port`), (b) the *expected* shape in type-DSL syntax, (c) the *actual* value and its kind, (d) the *ingress source* — which file/parse/CLI arg the bad data entered through — and (e) an actionable `help:` line. This is Nickel's label-and-`'Error`-record model (`message`, `notes`, `blame_location`) realised in eucalypt's existing diagnostic infrastructure. The standard to beat is internal: eucalypt's own "available keys: …" suggestion on a bad lookup (`error.rs:287`). If contract errors are not at least that good, users will keep reaching for `lookup-or` and ad-hoc `assert`, and the feature dies.

## Alternatives considered

- **Do nothing; rely on `assert` + `//=`.** Rejected: `assert` is a `panic` wrapper (`lib/prelude.eu:1226-1227`) with no structure, and `//=` is test-only. Neither is a schema; neither tells the user *where the data was wrong*.
- **A separate schema language (JSON Schema-style).** Rejected: it forces users to learn a second shape vocabulary divorced from the type DSL they already read, and it could not share the shapes the type DSL *already ships* — `Dict(T)`, `Mu`, literal-string singletons, `NonEmpty` — nor the H7 refinements still to come. Structural contracts in the native DSL keep one vocabulary.
- **CUE-style unification of types and values.** Rejected as too radical: a whole second evaluation model, incompatible with catenation and eucalypt's existing merge (non-negotiable #4).
- **Sound boundary casts everywhere (H13b).** Rejected, consistent with [0002]: pervasive higher-order contracts are the road the gradual-typing literature measures at up to ~100× on a JIT-less, GC-bound runtime. Contracts at chosen ingress points are pay-as-you-go.
- **Static-only refinements (the shipped `NonEmpty`, the pending H7 `Positive`).** Insufficient: a static `NonEmpty` or `Positive` cannot be proven of `any`-typed external input — and `NonEmpty` already ships as a static type whose advisory power evaporates precisely at the ingress boundary, which is the *only* case that matters here. The runtime contract is what closes that gap.

## Risks & what would kill this

- **One shape-language, two masters.** The central design tension is that a single DSL must now serve both *static types* (advisory, erased, must stay decidable) and *runtime contracts* (enforced, evaluated, may want arbitrary predicates). If the two demands pull the DSL in incompatible directions — e.g. contracts wanting general boolean expressions the type checker cannot interpret — the vocabulary fractures. *Mitigation:* keep refinements *finite and named* (the H7 discipline); confine arbitrary predicates to the `check:` escape hatch and to predicate-built contracts, where the checker is not asked to understand them.
- **Blame quality falls short.** If the structured report is not materially better than `LOOKUP_FAIL`, the feature has no reason to exist (see above). *Falsifier:* a seeded shape error whose contract message is no more locating than the raw key-not-found error.
- **Runtime overhead at validation points.** Bounded by design — opt-in, at boundaries only, shallow by default — but a careless deep-validation idiom on large imported data could cost. *Falsifier:* validating a multi-megabyte manifest dominates render time. *Mitigation:* shallow-by-default; document the cost of explicit nesting.
- **Scope creep toward CUE.** If contracts start supplying defaults and merging, they drift into a value lattice. *Guard:* contracts validate and pass data through; they do not construct it.

This is the maintainer's call on **open question 6** (boundary policy: the recommendation, with [0002], is *silent static boundary + explicit runtime contracts at ingress*) and on **open question 7** (contracts are the pragmatic substitute for typing the metadata channel).

## Success criteria

- A user can validate `parse-as`/imported/CLI data against a shape written in the existing type DSL, in one line, metadata-first.
- A contract failure reports field path, expected shape, actual value, and ingress source — demonstrably more locating than the current `LOOKUP_FAIL` "Key not found".
- Zero measurable change to render latency or heap behaviour for code that does not invoke a contract (ordinary erased core).
- The shipped `NonEmpty` refinement (and H7's `Positive` when it lands) gains a runtime consumer; the same shape serves `eu check` advice and `validate` enforcement with no second vocabulary.
- A seeded "renamed upstream key" error is caught at ingress with a clear message, where today it aborts at a deep `lookup` use site.

## References

**Eucalypt source.** `src/eval/intrinsics.rs:823-824` (`RENDER_TO_STRING : (any, symbol) -> string`), `:828-829` (`PARSE_STRING : (symbol, string) -> any`), `:853-854` (`EXPECT`), `:862` onward (predicate intrinsics); `lib/prelude.eu:410-412` (`lookup` raises, typed `… → Dict(a) → a?`), `:418-420` (`lookup-or`), `:1101-1103` (`parse-as`, typed `… → any?`), `:1097-1099` (`render-as`), `:2152-2175` (`parse-args`, coercion via `parse-as`), `:1226-1227` (`assert`), `:1236-1280` (`//=`, `//=?`, `//!`), `:376-392` (`block?`/`list?`/`number?`/… predicates); `src/eval/stg/block.rs:1104-1130, 1751` (`LOOKUP_FAIL` "Key not found"); `src/core/typecheck/parse.rs:115` (`T?` → `… | ExecutionError`), `:513, 603, 691, 729, 824` (union / record / literal-string / literal-symbol grammar); `src/core/typecheck/types.rs:210` (`ExecutionError`), `:238, 249, 253, 255, 266` (`Record`, `Union`, `LiteralSymbol`, `LiteralString`, `Mu`), `:299, 305` (`Dict`/`NonEmpty` as `Con`/`App`); `src/core/typecheck/check.rs:1581-1582, 2042-2050, 3075-3081` (partial-result-in-total-position warning); `src/core/typecheck/subtype.rs:100` (`any` consistent with every type); `src/eval/error.rs:5, 277-287` (`codespan_reporting` diagnostics, `help:`, "available keys"); `src/import/` (untyped data ingress).

**Eucalypt docs.** `docs/guide/type-checking.md:1-6, 42-45` (advisory checker; `any` from runtime values); `docs/guide/testing.md` (`//=`/`//!` harness); `docs/development/type-system-evolution.md` H7 (arithmetic refinements, still pending), H13c (opt-in casts), H19 (`Dict`, shipped 0.6.2), §5 questions 6 & 7; `CHANGELOG.md` 0.6.2/0.7.0 (`Dict`, `Mu`, literal-string, `NonEmpty`, `Partial(T)`/`T?`, row inference); `docs/proposals/_house-style.md` §1 non-negotiables.

**Literature.** Findler & Felleisen, *Contracts for Higher-Order Functions* (ICFP 2002) — blame, positive/negative responsibility; Siek & Taha, *Gradual Typing for Functional Languages* (2006); Wadler & Findler, *Well-Typed Programs Can't Be Blamed* (2009).

**Peer languages.** Nickel — runtime contracts as schemas; validators returning `'Ok`/`'Error` with `message`/`notes`/`blame_location`; `--apply-contract` non-invasive validation. CUE — types-as-values unification (constraints validate *and* reduce boilerplate). Pkl — type annotations with inline constraints (`String(!isEmpty)`, `Int(isBetween(0,130))`). KCL — schema `check` blocks of conditional-expression-plus-message. JSON Schema — external declarative schema (the separate-language approach this proposal declines).
