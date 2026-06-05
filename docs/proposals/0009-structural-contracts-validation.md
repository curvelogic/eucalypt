# 0009 — Structural contracts & runtime schema validation

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Whitespace
- **Suggested horizon:** 1.0
- **Related:** H7 (refinement types) and H13c (opt-in boundary casts) in [type-system-evolution.md](../development/type-system-evolution.md); §5 open questions 6 & 7; the existing `match?` structural predicate (`lib/prelude.eu:536-554`); 0000 F3 (unify cross-unit shape contributions); sibling proposals [0002 — gradual-typing boundary policy](0002-gradual-typing-boundary-policy.md), [0003 — conformance testing & fuzzing](0003-conformance-testing-fuzzing.md), [0010 — capability & determinism types](0010-capability-determinism-types.md), [0019 — host-language & schema interop](0019-host-language-interop.md)

## Summary

Eucalypt already owns the seed of this feature and does not know it. `match?` (`lib/prelude.eu:536-554`) is a working **structural-predicate engine**: a *pattern* is an ordinary value interpreted by type — blocks recurse field-wise with **open matching** (extra keys ignored), lists recurse element-wise with exact length, a **saturated** value matches by equality, and an **unsaturated function is applied as a predicate** (`__SATURATED(p) ? p = actual : p(actual)`, `:542-543`). `any?` (`:533-534`, `const(true)`) is the match-anything pattern; the regex `str.matches?` family (`:1000-1002`) are string predicates. Separately, the static type DSL (`src/core/typecheck/parse.rs`) spells the *same* shapes as **strings** — `{host: string}`, `Dict(T)`, `Mu`, literal singletons, `NonEmpty` — and metadata `types:` blocks **name** them. So eucalypt has **three overlapping spellings of one shape vocabulary** — value-patterns, type-DSL strings, and bare predicates — but they are fragmented: `match?` returns a bare bool with no blame and no static reading; the type DSL is static-only and erased; neither validates external data with a locating error, and there is no registry, no destructuring, no data generation. This proposal argues eucalypt should **unify the three spellings into one contracts/validation mechanism** — the *runtime* complement to the static checker — modelled on **Clojure spec** (specs are ordinary predicates/data; the `valid?`/`conform`/`explain`/`gen` quartet; a registry of named specs) with **Nickel**'s blame quality. The first-order priority is **ease of definition**: a spec must cost no more than writing a predicate, a literal, or a block of either — which `match?` already proves is native — and the payoff for unifying that cheap definition with the type DSL is that *one* definition buys static advice, runtime validation, and match/dispatch — bridged by an explicit `as-spec` reifier, since type annotations are otherwise erased and a bare symbol stays an ordinary value. This is the pragmatic resolution of the boundary-policy tension [0002] leaves open, and the value-level realisation of 0000's F3 ("one shape vocabulary, many consumers").

## Motivation

### The static checker stops exactly where the risk starts

Eucalypt is a tool for generating and transforming YAML/JSON/TOML before it is a language; its job is to turn external data into a rendered artefact. The type checker is "an optional, advisory type checker [that] never prevents your code from running" (`docs/guide/type-checking.md:1-6`); it explicitly does **not** reason about runtime-dependent values: "If a function returns `any` (because its type depends on a runtime value, like `lookup`), the checker makes no assumptions about the result" (`:42-45`). As of 0.7.0 it goes one honest step further — the fallible ingress functions carry `Partial(T)` / `T?` types (sugar for `T | ExecutionError`, `src/core/typecheck/parse.rs:115`, `types.rs:210`), and a warning fires when a partial result flows into a *total* position (`check.rs:1581-1582, 2042-2050`). That is exactly the **data-ingress boundary**, now visible as a *failure* possibility — but the checker can only flag *that* a value may fail; it cannot say what *shape* the success path has. Every channel through which external data enters is `any`-typed or `any?`-typed:

- **`parse-as`** parses structured-data strings, typed `symbol → string → any?` (`lib/prelude.eu:1101-1103`); its intrinsic `__PARSE_STRING` is `(symbol, string) -> any` (`src/eval/intrinsics.rs:828-829`).
- **Imports** load external YAML/JSON/TOML/CSV/XML/EDN (`src/import/`) as data with no static shape.
- **`parse-args`** turns `[string]` CLI args into "some block", typed `{{..}} -> [string] -> {{..}}` (`lib/prelude.eu:2152-2154`).

So the type system genuinely **stops at the I/O boundary**: `any`/`any?` in, `string` out ([0002]; `any` is consistent with every type, `subtype.rs:100`). And the consequence of a silent shape error is unusually expensive, because `lookup` *raises* rather than returning null (`lib/prelude.eu:410-412`, typed `symbol → Dict(a) → a?`; the existence of `lookup-or` at `:418-420` is the tell; at STG a missing key is `LOOKUP_FAIL` → "Key not found: {key}", `src/eval/stg/block.rs:1104-1130`). A manifest assembled from data whose shape is *slightly* wrong — a renamed upstream key — does not render a wrong-but-plausible artefact; it aborts deep in evaluation, at the *use* site, naming the symbol but not the *source* of the bad data. The `Partial` annotation now *predicts* this statically, but it cannot *prevent* it or locate the bad data.

### We already have the pieces — but fragmented

The striking thing is how much of the answer is already in the tree, unconnected:

- **`match?` is a structural-predicate engine** (`lib/prelude.eu:536-554`). It already does the hard interpretive work — recurse blocks (open), recurse lists (exact length), equality for saturated values, **apply unsaturated functions as predicates**. It is a *contract check* in all but name. What it lacks is **blame** (it returns a bare bool — it cannot say *why* it failed or *where* the data was wrong), a **static reading** (it is pure runtime), and a **name/registry**.
- **`any?`** (`:533-534`) is the top spec; the **`str.matches?` regex family** (`:1000-1002`) are string specs — predicates already, so they compose into `match?` patterns unchanged.
- **The type DSL** spells the same shapes as strings — records open/closed/row, unions, literal symbols *and* strings, `[T]`, tuples, and the shipped `Dict(T)`/`Mu`/`NonEmpty` represented as `Con`/`App` (`types.rs:238, 249, 253, 255, 266, 299, 305`). Static-checkable, but **erased before execution** — it cannot run over data.
- **Metadata `types:` blocks** already *name* shapes and 0.7.0 made alias references first-class.

The gap is that these do not compose into one thing. `match?` cannot tell you why it returned false; the type DSL cannot run; nothing generates data from a shape; nothing is named-and-registered for reuse across static, runtime, and dispatch uses. So at each ingress the user hand-rolls a *third* idiom — `assert(p?, s, v)` (`:1226-1227`, a thin `panic` wrapper) or the test-only `//=`/`//!` operators (`:1236-1280`) — none of which is a schema, and none of which tells the user *where the data was wrong*.

## Prior art & landscape

Runtime data validation is the feature every configuration peer leads with, and the shape they converge on is instructive.

| System | Validation mechanism | Reuses host vocabulary? | Blame quality |
|---|---|---|---|
| **Clojure spec** | predicates-as-specs + registry; `valid?`/`conform`/`explain`/`gen` | yes — specs are ordinary predicates/data, no separate type system | `explain-data`: path `:in`, pred `:pred`, value `:val`, spec-path `:via` |
| **Nickel** | runtime *contracts* applied at boundaries | yes — same annotation is a type *or* a contract | high — labels, message, notes, blame location |
| **CUE** | constraints via value/type unification | yes — "types are values", one lattice | schema-level |
| **Pkl** | type annotations with inline constraint predicates | yes — `String(!isEmpty)`, `Int(isBetween(0,130))` | per-property |
| **KCL** | `check` block of conditional expressions in a schema | partial — schema + checks | per-check message |
| **JSON Schema** | external declarative schema document | no — a separate language | path-based |

**Clojure spec is the model for the *shape of the system*.** Its central insight is that specs are **ordinary predicates and data**, not a separate type system: `(s/def ::port pos-int?)` registers a predicate as a named spec; specs compose with plain combinators (`s/and`, `s/or`, `s/keys`, `s/coll-of`, `s/map-of`). On top sits a quartet that is the whole value proposition:

- **`valid?`** — a boolean check. *This is exactly what `match?` already is.*
- **`conform`** — validate *and* destructure: return the value on success, and for an `s/or` spec tag *which* branch matched (`[:branch value]`), so downstream code can dispatch on the result.
- **`explain` / `explain-data`** — **blame**: the path into the data, the predicate that failed, the offending value, and the spec path traversed.
- **`gen` / `exercise`** — **generate** data satisfying a spec, which drives property-based testing for free.

A global **registry** maps namespaced keywords → specs, so specs are named, referenced, and reused. The lesson eucalypt should take wholesale: specs are *values you already know how to write*, defined wherever you like, one vocabulary serving validate + destructure + generate. That is precisely what `match?` half-implements; spec shows the whole shape.

**Nickel** is the model for *blame quality*: the same syntax describes a static type or a runtime contract; a contract is built from built-in pieces freely mixed with custom predicates; and its **validators** return not a boolean but `'Ok` or `'Error` carrying `message`, `notes`, and `blame_location`, with **blame** ending execution in a detailed report assembled from a *label* threaded through the check. The intellectual root of both is **Findler & Felleisen, *Contracts for Higher-Order Functions* (ICFP 2002)**, which introduced *blame* and which the literature credits as "the catalyst for gradual typing".

**What not to borrow.** **CUE** unifies types and values into one lattice — powerful, but it *is* the language; grafting it onto eucalypt's catenation core would be a second, incompatible evaluation model (and eucalypt already has a merge with deliberately different semantics). **JSON Schema** is a *separate* shape language — exactly what we avoid by validating against shapes written in the vocabulary users already know. From **Pkl/KCL** we take only the lesson that validation belongs *attached to the shape*, expressed in the host's own predicates, with a per-failure message. And from spec we decline its **sequence-regex operators** (`s/cat`/`s/alt`/`s/*` over flat sequences): eucalypt has no seq-regex model, so `conform` here stays *validate + union-tag*, not full positional destructuring.

## Proposed design

### Principle: ease of definition first — one vocabulary, three spellings

The governing constraint is that **defining a spec must cost no more than writing a predicate, a literal, or a block/list of them.** `match?` already proves the **value-pattern** spelling is native and cheap. The unification is that this same cheap value also has a **type-DSL string** spelling (static-checkable, erasable) and that a bare **predicate** is itself a spec — three surface syntaxes for *one* semantic object: a predicate-with-blame over values.

```eu,notest
# predicate spelling — a bare function is a spec
positive?

# value-pattern spelling — exactly what match? interprets today (open block)
{ host: string?, port: positive?, replicas: number? }

# type-DSL-string spelling — the same shape, statically checkable
"{host: string, port: number, replicas: number}"
```

The gradual split falls out cleanly: **everything is a runtime spec; the subset also expressible in the type DSL additionally gets static advice and erasure.** A pure type-DSL string denotes a spec the checker can also read; an arbitrary predicate is runtime-only (exactly spec's situation, and `match?`'s today). One vocabulary, two reaches.

### The quartet — grown from `match?`

- **`valid?(spec, x)`** — boolean. *This is `match?` today* (value-pattern specs are specs); promote it to the spec surface. Cheap, total, no blame.
- **`validate(spec, x)` / `conform`** — returns the data unchanged on success (so it threads transparently through a pipeline), a structured error on failure; for a union/`or` spec it returns a tagged `[which, value]` so a downstream `case`/dispatch knows the branch. (Validate + union-tag — not positional sequence destructuring; see above.)
- **`explain(spec, x)`** — **blame**: the path to the offending field (`config.servers[2].port`), the expected shape in type-DSL syntax, the actual value and its kind, the **ingress source** span, and a `help:` line. This is the make-or-break (own section); it is the single capability `match?` most conspicuously lacks.
- **`gen(spec)`** — generate sample data satisfying a spec. **Automatic for the structural/type-DSL subset** (blocks, lists, literals, `Dict`, `NonEmpty`, named refinements); a bare-predicate spec needs a supplied generator (spec's `with-gen`), because generating from an arbitrary predicate is undecidable. This ties directly to **[0003]**'s property testing — *a spec is a generator* — and is the highest-leverage reason to make shapes first-class data.

### Named specs & a registry

The registry need not be new machinery: named bindings, the `types:` metadata block, and 0.7.0's first-class alias references already give one. But a type alias is **inert at runtime** — annotations are consumed only by the checker and erased, so without `--type-check` the `types:` block does nothing, and a bare `:Server` handed to `match?` already means *match the literal symbol* (`:Server` is a saturated value → equality, `lib/prelude.eu:542`). Reifying a type as a runtime spec therefore needs an **explicit signal** — call it `as-spec` — that resolves a named alias (or an inline type string) and yields a runtime spec *value*. With it, one name resolves to the same shape across all three consumers — static type, runtime contract, match/dispatch — which is what makes unification pay:

```eu,notest
# define once — a named shape in the type-DSL vocabulary
{ types: { Server: "{{ host: string, port: number, replicas: number }}" } }

# use as a TYPE (static, advisory — works today; erased without --type-check)
` { type: "Server -> number" }
replica-count(s): s.replicas

# use as a CONTRACT — as-spec reifies the SAME alias into a runtime spec value
config: load-config validate(as-spec(:Server))
match?(as-spec(:Server), config)   # conform to the shape
match?(:Server, config)            # unchanged: literal symbol equality
```

`as-spec(:Server)` is the **one explicit bridge** from the type world to the spec-value world. It resolves the alias against the *same* alias map the checker uses and lowers the resolved `Type` to a runtime predicate-with-blame, but as a **separate consumer** of that map, so it runs *whether or not* `--type-check` is on (the checker decides whether to *warn*; `as-spec` decides whether to *reify*). Preferred implementation: lower at **compile time** to ordinary core — the `as-spec` argument is a literal name/string, resolvable then — which keeps advisory annotations erased and needs no runtime type-DSL parser (the explicit, no-inference-needed corner of type-directed compilation [0007]); a runtime-reflective variant (retain the alias strings, parse on demand) is the heavier "first-class type values" alternative. Everywhere a spec is expected (`validate`, `valid?`/`match?`, `conform`, `gen`) you pass a spec *value*: a value-pattern, a predicate, or `as-spec(:Name)`. A bare symbol is therefore *always* an ordinary value — never silently a type — which is exactly what keeps `match?` unambiguous and erasure intact.

**`as-spec` is the *mechanism*; its *surface* is resolved in the next subsection.** The function spelling is honest but **magical**: nothing at the use site signals that `:Server` reaches into another world — the type namespace — so a config author reading `validate(as-spec(:Server), x)` cannot see that a type expression is involved. The fix is to **promote the crossing to syntax**, and the resolved surface is a **string-prefix spec literal `s"…"`** (next subsection), which supersedes both the magical function *and* an earlier-considered *reserved type-expression bracket* (`⟬Server⟭`, [0013]'s H12e). A reserved bracket would emit a `Type` node at **parse time** — checker-visible in annotation position, compile-time-reified in value position, dodging the [0013] phasing and termination hazards — but at the price of carving a delimiter pair out of bracket-space and mode-switching a sub-grammar into the parser, *abandoning the string boundary 0013 concludes to keep for 1.0*. The prefix-string reaches the **same** parse-time `Type` node by adding one variant to the lexer's existing `StringPrefix` family (`c"`/`r"`/`t"`; `src/syntax/rowan/lex.rs:36-43`) — no new grammar, and the type content stays a string, so TS-A7 alias tooling is untouched. **Throughout the rest of this document, read `as-spec(:Name)` as the pre-resolution spelling of `s"Name"`**: the mechanism (compile-time reification) is unchanged; only its surface is now fixed.

The `Server` alias is the single source of truth: the annotation references it as a capitalised identifier resolved through the checker's alias map (`src/core/typecheck/parse.rs:632-643`; `docs/guide/type-checking.md:353` shows a `types:` alias used inside a `type:` string), and `as-spec(:Server)` resolves the *same* name to the *same* `Type`. **The duality is an asymmetry, not free interconversion.** A type-DSL shape *lowers* to a predicate, so it is genuinely dual-use; a bare predicate does **not** generally *lift* to a type, so `match?` value-patterns and `check:` predicates are **runtime-only**, with no static reading. "Define once, use as both" therefore means precisely: *write the structural shape in the type DSL once* — one alias serving the checker and (via `as-spec`) the contract engine — while arbitrary predicates stay in the runtime tier. A constraint the DSL cannot name (e.g. "port in `1..65535`", absent any range refinement — H7 adds only `Positive`/`Nonzero`) is expressed as a runtime-only `check:` predicate alongside the dual-use structural alias:

```eu,notest
` { type: "Server -> Server"                                              # structural: dual-use
    check: [ [((_.port > 0) ∧ (_.port < 65536)), "port out of range"] ] } # range: runtime-only
server(raw): raw validate(as-spec(:Server))
```

The status quo this closes: **today there is no single definition usable as both** — `match?` takes value-patterns, the checker takes type strings, and the two never meet. Unifying them on the named-alias is the core of this proposal.

### Resolution: the `s"…"` spec literal — crossing by position

**The crossing is not a verb but a position.** The shape vocabulary lives in two worlds: *type world* — static, advisory, **erased**, what the checker reads in **annotation** position — and *spec world* — runtime predicate-with-blame **values**, what `validate`/`valid?` consume in **value** position. The realisation that retires the magic is that a single surface form can inhabit **both**, and *where* it is written performs the crossing. `s"…"` is that form: in annotation position the checker reads it and it erases; in value position it reifies to a runtime spec. So `s"Server"` in value position simply *is* `as-spec(:Server)` with the magic removed — the bare symbol that silently reached into the type namespace becomes an explicit, marked literal whose `s` prefix announces "type/spec world". No function is needed to move between worlds when the literal already lives in both.

**Why a string prefix, not a keyword or a bracket — non-negotiable #1 intact.** eucalypt already lexes a `StringPrefix` family — `c"…"`, `r"…"`, and the ZDT literal `t"…"` (`src/syntax/rowan/lex.rs:36-43`, detected at `:259-298`; `t"…"` validated to a `DateTime` at `src/syntax/rowan/kind.rs:106`). `s"…"` adds **one variant to that enum and one lexer branch** — no new keyword, no new operator, no new block construct, so house-style non-negotiable #1 holds, with three-fold precedent. It is strictly *less* of a step than [0013]'s H12e bracket: the type content **stays inside a string** — exactly the boundary 0013 concludes to keep for 1.0, so TS-A7 span-indexed alias tooling is untouched — whereas the bracket abandons it. Like `t"`/`r"`, the `s"` prefix lexes with **interpolation off**, which as a by-product retires the `{{..}}` doubling the type-string spelling needs today (`Server: "{{ host: string }}"`, above): a literal `s"{host: string}"` is no longer an interpolation site.

**Aliases resolve inside `s"…"`, lexically.** A nominal reference inside an `s"…"` resolves against the **same** alias map the checker uses for annotations (`src/core/typecheck/parse.rs:632-643`; `check.rs:57, 105`), at **compile time** — so `s"Server"`, `s"[Server]"`, and `s"Server -> number"` all work, and the value-position reification bakes in the resolved shape. This is exactly the reifier of the Implementation sketch ("`as-spec` reifier"), now triggered by the **literal at parse time** rather than a function call; advisory annotations stay erased and no runtime type-DSL parser is required. Resolving a *runtime-computed* name — the one thing a literal categorically cannot spell — is deliberately **out of scope**: that needs reflective, runtime alias resolution, a separate optional **registry** feature (a `spec.resolve`-style lookup), never a symbol silently reinterpreted as a type.

**What `s"…"` reifies to: a `match?` value, not a new `Native` and not a runtime AST.** In value position `s"…"` introduces **no** new `Native` variant (`src/eval/memory/syntax.rs:37-58`) and produces **no** parallel tagged `{kind: …}` tree as a runtime value — a fourth representation would reintroduce the very fragmentation this proposal exists to remove. It reifies into the **`match?` vocabulary** (predicates and data), so `valid?(s"…", x)` is `match?(reify(s"…"), x)` and the existing `mv?`/`mb?`/`ml?` engine is the interpreter. The tagged `Type` remains the checker's internal IR (`types.rs`); the runtime value is an ordinary spec value — zero GC/render/emit change, and it sidesteps the `Native: Eq` obligation (`src/eval/memory/syntax.rs:79`) that a refinement closure would violate. Only the **structural** fragment reifies; the **type-only** fragment (function arrows, quantifiers) has no runtime spec and **erases**, consistent with the asymmetry above — `s"Server -> number"` is meaningful to the checker but, in value position, contributes only its structural parts.

**Two deltas with `match?` to reconcile (flagged, not yet settled).** Unifying `valid?` with `match?` exposes two points where the type-DSL structural fragment and `match?` value-patterns visibly disagree: (1) **`[T]` arity** — `match?`'s list rule is fixed-length positional (`lib/prelude.eu:551-552`: `[p]` matches a *one-element* list), whereas the type DSL's `[T]` means *list of `T`, any length*; the unified engine must distinguish homogeneous `[T]` from tuple `[A, B]`. (2) **Open vs closed records** — `match?` blocks are open-only (`:545-546`), so a closed/row record spec must lower to the `match?` block check *plus* a no-extra-keys assertion. Both are tractable, but must be decided before `valid?` literally *is* `match?` over reified specs.

**Consequence for `as-spec`.** As a named, symbol-taking **function**, `as-spec` is retired as a 1.0 surface: it was this proposal's deliberately "no-new-syntax" bridge, and `s"…"` is the minimal-*new*-syntax bridge that removes its magic while keeping the type content in a string. The trade is explicit and worth stating for review — 0009 originally prized adding *no* syntax; this accepts one new string-prefix (not a keyword) in exchange for de-magicking the crossing and retiring `{{..}}`. The reification *mechanism* is unchanged; the function spelling simply ceases to be the surface.

### One canonical form — why the type↔contract relationship is not circular

Three arrows now relate types, contracts and values, which can look circular: `as-spec` (type → contract value), `type-def` (a declaration → the *inferred* type of its value, `docs/guide/type-checking.md:727-746`), and the dual-use alias (one shape → type *and* contract). It is a **DAG**, not a cycle, because the arrows act on different objects and the only back-arrow is **lossy**. A contract is a *value* (a predicate); `type-def` over a contract value yields the predicate's *function* type (`a -> bool`) — **not** the shape it validates — so `contract → type-def` cannot reconstruct a shape and the loop never closes. The shape lives only in its **canonical definitional form** — the structural shape itself — supplied either as a type-DSL string (`types: { Server: … }`) or derived from a canonical value whose inferred type is named (`type-def`). Everything else is a **forward projection** of that one form — the checker reads it as a type; `as-spec` lowers it to a contract.

Two "define once, get both" *spellings* of that one canonical form therefore fall out with **no new mechanism**:

- **type-string spelling:** `types: { Server: "…" }` → static type + `as-spec(:Server)` contract.
- **value spelling (example-as-schema):** a canonical instance under `` ` { type-def: "Point" } `` → inferred type + `as-spec(:Point)` contract. The pipeline is value → *(infer)* → type → *(`as-spec`)* → contract — forward and lossy, never a loop. Writing the example *is* writing the schema.

The discipline that *keeps* it acyclic is two prohibitions: **never lift an arbitrary predicate to a type** (undecidable, and self-referential if the predicate names the type — so predicate-built contracts stay runtime-only, per the asymmetry above); and the compile-time reifier must **reject the one real cycle** — reifying a `type-def` alias *inside the very declaration that defines it* (`origin: … validate(as-spec(:Point))` under `` ` { type-def: "Point" } ``), where the alias's type depends on a value that depends on the alias.

### Decorating a shape with runtime predicates

The dual-use canonical form above is **structural-only**, and unavoidably so: a *type alias* cannot carry an arbitrary predicate like "`port` in `1..65535`" — types are erased, decidable and advisory, so there is nothing to attach a runtime check *to*. Worse, the `check:` predicates on a declaration (below) are stranded on *that binding*; they do not travel when the shape is reused via `as-spec(:Server)` elsewhere. So the reusable named shape, as described so far, validates structure but not refinements — the gap that makes a structural mirror less than a real contract.

The resolution keeps the asymmetry honest: **predicates live in *spec values*, not in type aliases.** A value-pattern already carries predicates (`match?` applies unsaturated functions), so the only addition is a combinator — `refine` / `and-spec` — that conjoins the dual-use structural core with a predicate-carrying value-pattern into one reusable spec **value**:

```eu,notest
{ types: { Server: "{{ host: string, port: number, replicas: number }}" } }   # structural core (dual-use)

# a named spec VALUE: structure + predicates, travels wherever it is used
valid-server: as-spec(:Server) refine { port: (_ > 0) ∧ (_ < 65536), replicas: (_ >= 1) }
```

`valid-server` carries structure *and* predicates and is reusable (`validate(valid-server, x)`). The static reading it keeps is the structural core (`Server`); the predicate layer is **runtime-only** — not a defect but the asymmetry applied consistently, and exactly Clojure spec's situation (`s/and pos-int? …`: spec predicates are not types).

Deliberately, refinements are **not** attached to the type-alias name. The same object legitimately needs **different specs in different contexts** — a lenient ingress spec, a stricter export spec, a test spec — so binding one refinement set to `:Server` would conflate "the structural type" with "one particular contract on it". Instead `refine` produces *distinct, separately-named* spec values (`ingress-server`, `export-server`, …) that all share the single structural core via `as-spec(:Server)`. The type is defined once and stays a clean structural mirror; the contracts are as many as the contexts demand, each a named value — never an attempt to push predicates into the erased type, which the asymmetry forbids.

### Surfaces (metadata-first, conservative)

Two surfaces, both honouring non-negotiable #1 (machinery in metadata, symbols, prelude forms — never new keywords):

```eu,notest
# (1) validate / valid? prelude forms. valid? = match? grown up.
{ import: "raw.eu"
  types: { Server: "{host: string, port: number, replicas: number}" } }

config: imported-data validate(as-spec(:Server))   # blame points at the source, not a deep use

# (2) contract:/check: block metadata (KCL/Nickel-style); anaphoric `_` is the block
` { contract: { host: string?, port: number?, replicas: number? }
    check: [ [(_.port > 0),     "port must be positive"],
             [(_.replicas >= 1), "need at least one replica"] ] }
server(raw): raw
```

Integration must respect **fixed arity** — eucalypt has no optional or variadic arguments, so there is no "add a shape argument to `parse-as`". Two routes instead. **Composition is the default and needs no new function:** because `validate` returns its data unchanged on success, it threads straight through a pipeline — `raw-string parse-as(:json) validate(as-spec(:Server))`. **A fused form is justified only where it buys better blame:** a separate `validate` step cannot see the parse's *source span*, so to carry "this came from `config.json` line 12" into the failure we add a *new, distinctly-named* fixed-arity function (e.g. `parse-checked(format, shape, string)` — arity 3), never an overloaded `parse-as`. For CLI, no new arity is needed at all: the contract rides in the `defaults` block's field metadata that `parse-args` already coerces against (`lib/prelude.eu:2172-2174`, via `parse-as`; it already raises on unknown options), so validating its output extends behaviour it half-performs today.

### Structural, not nominal; and semantics

Validation is **structural** (non-negotiable #2): `as-spec(:Server)` denotes a *shape*; any block presenting the right fields satisfies it — no declared class, no `implements`. This keeps it native to "typeclasses without classes" and is exactly `match?`'s existing open-matching discipline. A spec runs as **ordinary core** — compiled and evaluated like any expression — so it imposes **zero** cost where it is not invoked and needs **no** change to STG, the heap, or the [0002] erasure guarantee. On success it returns the data unchanged; on failure it raises one `ExecutionError` carrying a structured report. Specs are **shallow by default**: a block spec checks named fields and their immediate types; nesting is opt-in by nesting the shape (`{server: as-spec(:Server)}`), never an automatic deep proxy — the transient/shallow discipline [0002] endorses, deliberately *not* the pervasive higher-order wrapping the sound-gradual-typing literature measures at catastrophic cost.

## Unifying `match?` and the regex family

This is the part the maintainer flagged as missing, and it is load-bearing. The unification is concrete, not aspirational:

- **`match?` value-patterns *are* specs.** Its four interpretation rules (`:540-552`) become the spec interpretation rules; `valid?` is `match?`. We *add* two things it lacks: **blame** (the same traversal that returns `false` instead accumulates a path + reason — `explain`), and a **static reading** for the subset whose pattern is type-DSL-expressible.
- **`any?`** is the top spec; **`str.matches?(re)`** and friends are **string specs** (predicates) that drop into any larger pattern with no adaptation.
- The single vocabulary then serves **three consumers** — the static checker (advice), the runtime validator (enforcement + blame), and **match/dispatch** (`match?` / case selection). That third consumer is why this is more than a validation feature: it is the "common language for types and contracts" extended to *dispatch*, and it is the value-level realisation of 0000 **F3** (one shape vocabulary, seeded into many phases/consumers rather than re-implemented per use).

Concretely, today's `match?` already accepts the value-pattern spelling:

```eu,notest
# match? today: value-pattern spec, open on blocks, returns a bare bool
{ host: "h", port: 8080, extra: 1 } match?({ host: string?, port: number? })   # true
```

The proposal keeps that working verbatim, and makes the value-pattern `{host: string?, port: number?}` and the reified alias `as-spec(:Server)` *both* spec values usable wherever a spec is expected — `validate`, `valid?`/`match?`, `conform` — while the alias additionally serves as a static type. The bare symbol `:Server` is untouched: still a literal value, never silently a shape.

## Interaction with the existing roadmap

This is the **runtime half** of the boundary story [0002] frames statically. [0002] ratifies the *type* boundary as permanently optimistic and silent (H13a) — `any → T` trusted, no cast, no blame — for sound performance reasons; the 0.7.0 `Partial(T)` annotations *name* the points where ingress can fail and warn when a failure flows on unhandled, but by design stop there. They hand the "I want my manifest validated" use case to *this* document: specs validate **where the user writes them** (Findler–Felleisen pay-as-you-go), at ingress, rather than the type system checking *every* boundary. Static checking via `eu check --strict` in CI; optimistic, erased type boundaries in production; explicit specs at the few ingress points where runtime validation earns its keep.

It draws its **shape vocabulary** from shapes that have largely **shipped** — `Dict(T)`, `Mu`, literal-string singletons, `NonEmpty` (0.6.2/0.7.0) — leaving only H7's arithmetic refinements (`Positive`, `Nonzero`) outstanding, and those are *more* valuable as runtime specs than as static types: a static `Positive` can rarely be proven of `any`-typed input, but a runtime `Positive` *spec* checks it exactly when the value arrives. `gen` ties the system to **[0003]**: a structural spec generates conforming data, so property tests come for free. And the whole construction is the pragmatic resolution of **open question 7** ("is typing the metadata channel worth doing?", closed *won't-do*): rather than flow the type lattice through metadata, we let *specs* — runtime, structural, opt-in — police the data that metadata-typing would have policed statically.

It must **not** become the H13b sound-cast road [0002] rejects: specs fire only at explicit ingress points and explicit `validate` calls, never pervasively, never as automatic higher-order proxies.

## Implementation sketch

- **Spec engine — generalise `match?`, do not replace it.** Lift the existing recursive `mv?`/`mb?`/`ml?` (`lib/prelude.eu:538-553`) into a `check(spec, value) -> Result` that (a) accepts a parsed `Type` (from `parse.rs`) as well as a value-pattern, and (b) accumulates a path + reason rather than collapsing to a bool. Medium size; the interpretive core already exists and is tested.
- **`as-spec` reifier — the one type→value bridge.** Resolve a *literal* alias name (or inline type string) against the alias map and lower the resolved `Type` to a spec value. Preferably a **compile-time** pass (alias resolution + lowering to core), so it runs independently of `--type-check` and retains no runtime type-DSL parser; this is the single new crossing between the type and value worlds, and the only place a bare symbol is interpreted as anything but a literal value. Everything downstream is ordinary core.
- **Prelude surface** — `validate`, `valid?` (= promoted `match?`), `conform`; the spec combinators `refine`/`and-spec` (conjoin a structural core with predicate refinements into a distinct named spec value — Clojure spec's `s/and`) and `or-spec` (union); and the refinement predicates (`positive?`, `non-empty?`, …). Small; combinators are ordinary functions over spec values.
- **Structured blame** — extend `src/eval/error.rs` (which already builds `codespan_reporting` diagnostics with `help:` hints and "available keys" suggestions, `:5, 277-287`) with a `ContractViolation` variant carrying failed path, expected shape, actual kind, and ingress span. The make-or-break work (below).
- **Registry** — `as-spec` name resolution over the existing alias/`types:` mechanism; no new global state if alias references suffice.
- **`gen`** — sample-data generation for the structural/type-DSL subset; `with-gen` for bare predicates. Pairs with [0003]; can land later.
- **Ingress integration** — primarily *composition* (`parse-as … validate …`; no new function); optionally a *new, distinctly-named* fixed-arity fused form (e.g. `parse-checked`) where carrying the ingress source span into blame matters; `parse-args` reads contracts from the `defaults`-block metadata it already coerces against. No optional/variadic arguments — eucalypt is fixed-arity.
- **No STG/GC/typecheck-core change.** Specs are ordinary evaluated core; the only addition is `as-spec`'s compile-time reification (alias resolution + lowering). Advisory `type:` annotations remain erased, the checker is unchanged, and the [0002] boundary guarantee holds.

Sequencing: promote `match?` → `valid?` and land `validate` + the path-carrying engine + structured blame first (self-contained, testable with `//=`); add `conform` + the `contract:`/`check:` annotation; wire ingress integration; add `gen` (with [0003]); fold in H7 refinements as they arrive.

## Error-message quality is the make-or-break

A spec that says only "validation failed" is worse than the `LOOKUP_FAIL` it was meant to improve on — and it is *precisely* `match?`'s current bare-bool failure. The entire value proposition is **Clarion-grade structured blame**: a failure must report (a) the precise path to the offending field (`config.servers[2].port`), (b) the *expected* shape in type-DSL syntax, (c) the *actual* value and its kind, (d) the *ingress source* — which file/parse/CLI arg the bad data entered through — and (e) an actionable `help:` line. This is Nickel's label-and-`'Error`-record model and spec's `explain-data` realised in eucalypt's existing diagnostic infrastructure. The standard to beat is internal: eucalypt's own "available keys: …" suggestion on a bad lookup (`error.rs:287`). If spec errors are not at least that good, users keep reaching for `lookup-or` and ad-hoc `assert`, and the feature dies.

## Alternatives considered

- **Leave `match?` and the type DSL separate (status quo).** Rejected: that is three unconnected idioms (value-patterns, type strings, `assert`), no blame, no shared static/runtime/dispatch reading, no generation. The whole point is to unify them.
- **Do nothing; rely on `assert` + `//=`.** Rejected: `assert` is a `panic` wrapper (`:1226-1227`); `//=` is test-only. Neither is a schema; neither says *where* the data was wrong.
- **A separate schema language (JSON Schema-style).** Rejected: a second shape vocabulary divorced from the type DSL users already read, unable to share the shipped `Dict(T)`/`Mu`/`NonEmpty` shapes or the value-patterns `match?` already interprets.
- **Full Clojure-spec sequence regex (`s/cat`/`s/alt`/`s/*`).** Out of scope: eucalypt has no seq-regex model; `conform` stays validate + union-tag, not positional destructuring.
- **CUE-style unification of types and values.** Rejected as too radical: a whole second evaluation model, incompatible with catenation and eucalypt's existing merge (non-negotiable #4).
- **Sound boundary casts everywhere (H13b).** Rejected, consistent with [0002]: pervasive higher-order contracts are the road the literature measures at up to ~100× on a JIT-less, GC-bound runtime. Specs at chosen ingress points are pay-as-you-go.

## Risks & what would kill this

- **One shape-language, two masters.** A single DSL must serve *static types* (advisory, erased, decidable) and *runtime specs* (enforced, evaluated, arbitrary predicates allowed). If the demands pull it apart, the vocabulary fractures. *Mitigation:* keep refinements *finite and named* (the H7 discipline); confine arbitrary predicates to the predicate-spelling and the `check:` escape hatch, where the checker is not asked to understand them — the same split `match?` already lives with (`__SATURATED` predicates are runtime-only).
- **Ease-of-definition regresses under unification.** If unifying with the type DSL makes the cheap value-pattern spelling *harder* to write, the proposal defeats itself. *Falsifier:* the simplest spec a user writes is longer or more ceremonious than today's `match?` pattern. *Guard:* the value-pattern spelling must remain a no-annotation path; the type-DSL string is an *opt-in* for static reach, never a requirement. The `as-spec` reifier is a deliberate **use-site** tax (not a definition-site one): the cost of keeping types erased-by-default and `match?` unambiguous is one wrapping call where a *type* alias is used as a contract — paid only when reaching across the type/value boundary, never when writing a plain value-pattern.
- **Blame quality falls short.** If the report is no better than `LOOKUP_FAIL`, the feature has no reason to exist. *Falsifier:* a seeded shape error whose message is no more locating than the raw key-not-found.
- **`conform`/`gen` scope creep.** `gen` from arbitrary predicates is undecidable; `conform` toward full sequence destructuring re-imports spec's regex model. *Guard:* `gen` automatic only for the structural subset (`with-gen` otherwise); `conform` = validate + union-tag.
- **Runtime overhead.** Bounded by design — opt-in, at boundaries, shallow by default — but a careless deep-validation idiom on large imports could cost. *Mitigation:* shallow-by-default; document the cost of explicit nesting.

This is the maintainer's call on **open question 6** (boundary policy: *silent static boundary + explicit runtime specs at ingress*) and on **open question 7** (specs are the pragmatic substitute for typing the metadata channel).

## Success criteria

- A user validates `parse-as`/imported/CLI data against a shape written in the existing type DSL — or a `match?`-style value-pattern — in one line, metadata-first.
- `match?` is reframed as `valid?` / the value-pattern spelling with **no loss**: every existing `match?` pattern keeps working, and the *same* shape is usable as a type annotation, a `validate` argument, and a dispatch pattern.
- A spec failure reports field path, expected shape, actual value, and ingress source — demonstrably more locating than `match?`'s bare bool and the current `LOOKUP_FAIL`.
- A structural spec **generates** conforming data for a [0003] property test with no extra definition.
- Zero measurable change to render latency or heap behaviour for code that does not invoke a spec.
- The shipped `NonEmpty` (and H7's `Positive` when it lands) gains a runtime consumer; one named spec serves `eu check` advice, `validate` enforcement, and `match?` dispatch — no second vocabulary.

## References

**Eucalypt source.** `lib/prelude.eu:533-554` (`any?`, `match?` — the structural-predicate engine: `mv?`/`mb?`/`me?`/`ml?`, `__SATURATED` predicate application), `:1000-1002` (`str.matches?`) and `:977-998` (the regex `match`/`matches`/`extract` family — string specs), `:386-396` (`string?`/`symbol?`/`bool?` predicates), `:376-392` (`block?`/`list?`/`number?`), `:410-412` (`lookup` raises, typed `… → Dict(a) → a?`), `:418-420` (`lookup-or`), `:1101-1103` (`parse-as`, typed `… → any?`), `:2152-2175` (`parse-args`, coercion via `parse-as`), `:1226-1227` (`assert`), `:1236-1280` (`//=`/`//!`); `src/eval/intrinsics.rs:823-824, 828-829, 862` onward (render/parse/predicate intrinsics); `src/eval/stg/block.rs:1104-1130` (`LOOKUP_FAIL`); `src/core/typecheck/parse.rs:115` (`T?`), `:513, 603, 691, 729, 824` (union/record/literal grammar), `:312-347, 632-643, 1242` (`AliasRef` / capitalised-ident alias resolution — the map `as-spec` reifies against); `src/core/typecheck/types.rs:210, 238, 249, 253, 255, 266, 299, 305` (`ExecutionError`, `Record`, `Union`, literals, `Mu`, `Dict`/`NonEmpty`); `check.rs:1581-1582, 2042-2050` (partial-in-total warning); `subtype.rs:100` (`any` consistent with every type); `src/eval/error.rs:5, 277-287` (`codespan_reporting`, `help:`, "available keys"); `src/import/` (untyped ingress).

**Eucalypt docs & proposals.** `docs/guide/type-checking.md:1-6, 42-45` (advisory checker), `:727-746` (`type-def` — name the inferred type of a declaration; the value→type arrow); `docs/development/type-system-evolution.md` H7, H13c, §5 Q6 & Q7; `CHANGELOG.md` 0.6.2/0.7.0; `_house-style.md` §1 non-negotiables; `0000-priority-fixes.md` F3 (unify the shape vocabulary); `0003-conformance-testing-fuzzing.md` (`gen` ↔ property tests).

**Literature & peers.** Findler & Felleisen, *Contracts for Higher-Order Functions* (ICFP 2002) — blame; Siek & Taha, *Gradual Typing for Functional Languages* (2006); Wadler & Findler, *Well-Typed Programs Can't Be Blamed* (2009). **Clojure spec** (Hickey) — predicates-as-specs, registry, `valid?`/`conform`/`explain`/`gen`, `with-gen`. **Nickel** — contracts-as-schemas, `'Ok`/`'Error` with `message`/`notes`/`blame_location`, `--apply-contract`. **CUE** — types-as-values unification (declined). **Pkl** — inline constraints. **KCL** — schema `check` blocks. **JSON Schema** — external schema (declined).
