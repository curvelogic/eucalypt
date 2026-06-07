# 0019 — Host-language & schema interop: JSON Schema emit/ingest and codegen

- **Status:** Draft proposal for review
- **Track:** E — ecosystem, interop & safety
- **Classification:** Whitespace
- **Suggested horizon:** post-1.0 (JSON Schema core: 1.0-adjacent)
- **Related:** TS-A2 (Dict types, shipped 0.6.2), TS-A3 (recursive types, shipped 0.6.2),
  [0009 — structural contracts & runtime schema validation](0009-structural-contracts-validation.md),
  [0016 — `eu doc`](0016-eu-doc.md),
  [0018 — module & package system](0018-module-package-system.md),
  [0021 — optional record fields](0021-optional-record-fields.md) (prerequisite for faithful CRD import)

---

## Summary

Eucalypt can produce YAML, JSON and TOML, but it cannot exchange *schemas* with
the typed ecosystems that consume its output, and it cannot ingest the
CRDs and JSON Schemas those ecosystems publish. This proposal adds two
complementary subcommands — `eu schema export` (eucalypt typed-block shapes →
JSON Schema) and `eu schema import` (JSON Schema / Kubernetes CRDs → eucalypt
type aliases and [0009] contracts) — as the **highest-leverage interop surface**
eucalypt can offer before 1.0. Full per-language code generation (Java, Kotlin,
Swift, Go) is explicitly scoped out of this proposal and delegated to a
post-1.0, community-driven effort, for reasons of maintenance cost argued below.
The thesis is that JSON Schema is the lingua franca of the structured-data
ecosystem: one bidirectional bridge delivers 80 % of the interop value at
roughly 20 % of the ongoing maintenance burden of per-language SDKs.

---

## Motivation

Eucalypt's driver (`src/driver/options.rs:86–105`) offers these subcommands
today: `run`, `test`, `dump`, `version`, `explain`, `list-targets`, `fmt`,
`lsp`, `check`. There is no subcommand that concerns itself with schemas,
codegen, or type-exchange. The export side (`src/export/`) handles JSON, YAML,
TOML, EDN, HTML, text and table; the import side (`src/import/`) handles CSV,
YAML, TOML, XML, JSON-lines, text and EDN. Neither side has any notion of
*types* or *schemas* — the type checker (`src/core/typecheck/`) is advisory and
erased before evaluation; its output goes nowhere except diagnostic warnings.

The gap matters for a specific, concrete class of user: teams writing eucalypt
templates that generate Kubernetes manifests, cloud-provider configurations, or
API call payloads. Those ecosystems are *heavily schema-governed*. Kubernetes
Custom Resource Definitions (CRDs) are OpenAPI v3 schemas; Helm chart
`values.yaml` validation uses JSON Schema; cloud providers publish JSON Schema
for their resource models. A eucalypt author today must:

1. Hand-transcribe the CRD's field names and types into `type:` annotations in
   their `.eu` file — with no tooling to check the transcription.
2. Accept that when the upstream CRD changes (it always does), the transcription
   drifts silently.
3. Hand-document the schema of their own eucalypt templates for downstream
   consumers, who cannot machine-read it.

This is the adoption gap. Eucalypt produces the *artefacts* these ecosystems
want (YAML manifests, JSON configs), but cannot participate in the *schema
conversation* those ecosystems have with each other.

The type system is already structurally well-suited to bridge this gap. Closed
and open records (`{k: T}`, `{k: T, ..}`), union types (`A | B`), list types
(`[T]`), homogeneous blocks (`Dict(T)`, shipped in 0.6.2), literal symbol types
(`:name`), literal string types (also shipped 0.6.2), `NonEmpty` refinement
(shipped 0.6.2), and equirecursive type aliases via `Mu` (shipped 0.6.2) all map
naturally to JSON Schema constructs. This is a genuine advantage over
nominal-typed configuration languages: structural types compose with JSON Schema
structurally, without an impedance mismatch.

---

## Prior art & landscape

### Pkl (Apple, 2024–)

Pkl is the clearest prior art and the strongest adopter of the bidirectional
interop model. It provides:

- **Code generation** for Java, Kotlin, Swift and Go, via
  `pkl-codegen-java`, `pkl-codegen-kotlin`, and equivalents in the Go and Swift
  bindings. These read a Pkl schema and emit native-language data classes,
  constants and constructors. As of version 0.29 (2025), generation is mature
  and used in production at Apple.
- **JSON Schema support** via the `org.json_schema` package in the pkl-pantry,
  supporting draft-06 through draft-2020-12. A separate CRD generator
  (`k8s.contrib.crd`) can consume a Kubernetes CRD YAML and emit Pkl module
  definitions.
- **pkldoc**, a documentation website generator that extracts structured
  information from Pkl modules — sharing its machinery with the code-generation
  path.

The per-language code generators are a significant maintenance commitment: each
target language has its own code generator, its own binding library, and its own
release cadence. Pkl has a dedicated team and is backed by Apple. This is the
central lesson for eucalypt: the codegen breadth is the *expensive part*, not
the schema representation.

### KCL (CNCF Sandbox, 2023–)

KCL takes an import-first approach. Its `kcl import` tool ingests JSON Schema,
OpenAPI, Terraform provider schemas, and Kubernetes CRDs as KCL schema
definitions. The tool produces `.k` files that users then annotate and extend.
KCL also ships SDKs for Rust, Go, Python, Java, .NET, Node.js, Kotlin, Swift,
C, C++ and WASM — a remarkable breadth, but one that requires a large and
active community to sustain. The import tooling is well-regarded; the SDK
breadth is the source of most of KCL's maintenance overhead.

### CUE

CUE's `cue import` subcommand ingests JSON Schema and OpenAPI, generating CUE
definitions. CUE's type system is unified with its value system (types are
values), making the mapping to JSON Schema particularly direct. The `cue export`
direction (CUE → JSON Schema) exists but is less commonly used. CUE has no
per-language codegen.

### Nickel

Nickel 1.x ships a JSON Schema export (`nickel export --format json-schema`) for
contracts annotated with `#Nickel.json_schema.from_schema`; the import direction
is not yet in the main CLI. Nickel's is the closest model to what this proposal
recommends for eucalypt: a single bidirectional JSON Schema bridge, no
per-language SDKs.

### JSON Schema as interchange lingua franca

JSON Schema (draft-2020-12, IETF RFC 8927) is the de facto universal schema
language of the structured-data ecosystem. Kubernetes CRDs embed OpenAPI v3
(a JSON Schema superset); Helm uses `values.schema.json`; AWS, GCP and Azure
publish JSON Schema or OpenAPI for their resource models. Any tool that can
speak JSON Schema participates in this ecosystem without maintaining per-language
bindings.

**What eucalypt should and should not borrow.** From Pkl: the JSON Schema
import/export pairing and the shared machinery with documentation extraction.
Not from Pkl: the per-language codegen commitment. From KCL: the `import`
subcommand UX and the CRD-to-schema pipeline. From Nickel: the conservative
scope.

---

## Proposed design

### Core concept: `eu schema` as a new subcommand group

A new top-level subcommand `schema` is added to the driver
(`src/driver/options.rs`), with two actions:

```
eu schema export [--draft 2020-12|2019-09|07] [-t <target>] <files...>
eu schema import [--contracts] <schema-or-crd.yaml> [-o <output.eu>]
```

Neither subcommand evaluates eucalypt code. Both operate on the *type
information* in eucalypt source, not on the runtime output. This is a
deliberate architectural choice: it keeps the schema tools fast (no heap
allocation, no VM) and composable with `eu check`.

### `eu schema export`: eucalypt types → JSON Schema

The subcommand reads one or more `.eu` files, runs the type-checking front-end
through annotation collection (`src/core/typecheck/check.rs`), and for each
top-level binding that has a `type:` annotation, emits a JSON Schema object.

**Type mapping table:**

| Eucalypt type DSL | JSON Schema |
|---|---|
| `string` | `{"type": "string"}` |
| `number` | `{"type": "number"}` |
| `bool` | `{"type": "boolean"}` |
| `null` | `{"type": "null"}` |
| `symbol` | `{"type": "string"}` *(symbols serialise as strings)* |
| `:name` (literal symbol) | `{"const": "name"}` |
| `[T]` | `{"type": "array", "items": <T>}` |
| `{k: T}` (closed record) | `{"type": "object", "properties": {...}, "additionalProperties": false, "required": [...]}` |
| `{k: T, ..}` (open record) | `{"type": "object", "properties": {...}}` |
| `Dict(T)` (shipped 0.6.2) | `{"type": "object", "additionalProperties": <T>}` |
| `A \| B` | `{"anyOf": [<A>, <B>]}` |
| `"hello"` (literal string, 0.6.2) | `{"const": "hello"}` |
| `NonEmpty([T])` (shipped 0.6.2) | `{"type": "array", "items": <T>, "minItems": 1}` |
| `any` | `{}` *(accept anything)* |
| `never` | `{"not": {}}` |
| `datetime` | `{"type": "string", "format": "date-time"}` |
| Recursive alias via `Mu` (shipped 0.6.2) | `{"$defs": {...}, "$ref": "#/$defs/Name"}` |

Records are the most direct mapping. A closed record `{name: string, age: number}`
becomes an `object` with `"additionalProperties": false` and both fields in
`"required"`. An open record `{name: string, ..}` drops `additionalProperties`
and the non-annotated tail. The structural type system maps *naturally* to
JSON Schema's structural model — this is the genuine advantage of eucalypt's
non-nominal design.

Union types map to `anyOf`. Literal symbols map to `const`, enabling
discriminated unions (tagged variants) that JSON Schema and TypeScript both
handle well.

**Recursive types (shipped 0.6.2).** Equirecursive types via the `Mu` binder
landed in 0.6.2, enabling self-referential aliases such as
`type-def: { Json: "number | string | bool | null | [Json] | Dict(Json)" }`
with coinductive subtyping (`CHANGELOG.md:44–46`). The `$defs` / `$ref`
mechanism in JSON Schema maps directly to the `Mu` form: each named recursive
alias becomes a `$def`, and all self-references become `$ref` pointers.
Nested and recursive JSON Schema ↔ eucalypt-type round-tripping is no longer
blocked — the `Mu` binder is available now.

**Example.** Given:

```eu
{ types: {
    Env: ":production | :staging | :development"
    Config: "{name: string, replicas: number, env: Env, ..}"
} }
```

`eu schema export config.eu` emits:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$defs": {
    "Env": { "anyOf": [
      {"const": "production"},
      {"const": "staging"},
      {"const": "development"}
    ]},
    "Config": {
      "type": "object",
      "properties": {
        "name": {"type": "string"},
        "replicas": {"type": "number"},
        "env": {"$ref": "#/$defs/Env"}
      }
    }
  }
}
```

The `types:` metadata block is the natural locus for schema declarations: it is
already the place where eucalypt aliases are registered (`src/core/typecheck/check.rs`).
No new syntax is introduced; the existing `type:` DSL (`src/core/typecheck/parse.rs`)
is the schema language.

### `eu schema import`: JSON Schema / CRD → eucalypt type aliases + contracts

The subcommand reads a JSON Schema or Kubernetes CRD (which embeds an OpenAPI v3
schema) and emits a `.eu` file containing:

1. A `{ types: { ... } }` block with eucalypt type alias strings for each named
   definition in the schema's `$defs` / `components/schemas` / CRD `spec`.
2. Optionally (with `--contracts`), a `{ ... }` block of binding declarations
   with `type:` annotations and [0009]-style runtime contract wrappers, ready to
   validate incoming YAML/JSON against the imported schema.

**CRD support.** Kubernetes CRDs embed their schema at
`spec.versions[*].schema.openAPIV3Schema`. The importer peels this envelope and
processes the embedded JSON Schema. Field descriptions become backtick doc
metadata; `x-kubernetes-preserve-unknown-fields` maps to open records (`..`).

**Reverse mapping table** (JSON Schema → eucalypt type DSL):

| JSON Schema | Eucalypt type DSL |
|---|---|
| `{"type": "string"}` | `string` |
| `{"type": "number"}` or `"integer"` | `number` |
| `{"type": "boolean"}` | `bool` |
| `{"type": "null"}` | `null` |
| `{"const": "x"}` | `:x` (literal symbol, if a valid identifier) |
| `{"type": "array", "items": S}` | `[<S>]` |
| `{"type": "object", "properties": {...}, "additionalProperties": false}` | `{k: T, ...}` (closed) |
| `{"type": "object", "properties": {...}}` | `{k: T, ..}` (open) |
| `{"type": "object", "additionalProperties": S}` | `Dict(<S>)` |
| `{"anyOf": [...]}` / `{"oneOf": [...]}` | `A \| B \| ...` |
| `{}` / `{"type": ["string","number",...]}` heterogeneous | `any` |
| `{"$ref": "#/$defs/X"}` | type alias reference `X` |

The importer lowers to eucalypt's type DSL, not to Rust types — it emits `.eu`
source text. This is intentional: the output is human-readable and can be
committed alongside the project's own `.eu` files, reviewed in pull requests,
and extended by the author.

### Relationship to [0009] contracts

An imported schema *is* a contract. When `eu schema import --contracts` is used,
each top-level definition with a `type:` annotation becomes a binding with a
[0009]-style contract assertion. The user can then write:

```eu
{ import: "k8s-deployment.eu" }  # generated by eu schema import

` { type: "Deployment" }
my-deployment: {
  metadata: { name: "api", namespace: "default" }
  spec: { replicas: 3 }
}
```

At runtime the [0009] contract checks that `my-deployment` satisfies the
`Deployment` type at the point of use. The schema import is the *source of
truth*; the contract is the *enforcement*. This is the natural division: `eu
schema import` populates the alias map; [0009] enforces it at the boundary.

---

## Interaction with the existing roadmap

| Dependency | Nature |
|---|---|
| **TS-A2** (Dict types, shipped 0.6.2) | Provides `Dict(T)` ↔ `additionalProperties` mapping |
| **TS-A3** (recursive types, shipped 0.6.2) | Provides `Mu`-based `$ref`/`$defs` round-tripping and JSON-shaped schemas |
| **[0009]** structural contracts | `--contracts` mode wraps imported aliases in runtime assertions |
| **[0016]** `eu doc` | Both `eu doc` and `eu schema export` walk the same annotated type information; the AST traversal is shared infrastructure |
| **[0018]** module & package system | Imported schemas could be versioned packages (`eu schema import pkg://...`) once [0018] lands |

The shared-infrastructure point with [0016] is load-bearing. Both tools need to
collect all named type annotations from a set of source files, resolve aliases,
and walk the resulting type graph. Implementing `eu schema export` and `eu doc`
together — or ensuring the schema exporter reuses whatever traversal `eu doc`
builds — avoids duplicating that logic across two pipeline entry points in
`src/driver/`.

---

## Implementation sketch

**Phase 1 — JSON Schema export (~Medium, low risk).**
New module `src/driver/schema.rs`, new `Schema` variant in
`src/driver/options.rs:Commands`. Collect `type:` annotations, resolve aliases
via the existing `check.rs` alias map, fold the `Type` enum
(`src/core/typecheck/types.rs`) into `serde_json::Value`. No heap, no VM.
`serde_json` is already present (`src/export/json.rs`). Recursive alias
support via `Type::Mu` is available now (shipped 0.6.2); the `Con`/`App`
constructor-application representation of `Dict`, `NonEmpty`, and other
parametric types (`src/core/typecheck/types.rs:1–17`) is the current form.

**Phase 2 — JSON Schema import (~Medium, moderate risk).**
New module `src/driver/schema_import.rs`. Read JSON Schema with `serde_json`,
walk the object graph, emit eucalypt source text (`.eu` file). CRD support adds
envelope peeling for `openAPIV3Schema`. The `--contracts` flag emits [0009]-style
binding declarations. Risk: the JSON Schema spec has edge cases (`allOf`, `not`,
`if`/`then`/`else`); unsupported constructs degrade to `any` with a warning.

**Phase 3 — Host-language codegen (post-1.0, community-driven).**
Deliberately not designed here. Eucalypt emits valid JSON Schema; any JSON Schema
consumer (`quicktype`, `json-schema-to-typescript`, `jsonschema2pojo`) can do
per-language generation. This avoids maintaining N language-specific generators.

**Files affected:**
- `src/driver/options.rs` — add `Schema(SchemaArgs)` variant to `Commands`
- `src/driver/schema.rs` — new: type-to-JSON-Schema fold
- `src/driver/schema_import.rs` — new: JSON Schema parser and `.eu` emitter
- `src/driver/mod.rs` — dispatch to schema subcommands
- `src/core/typecheck/check.rs` — expose alias map for schema exporter reuse
- Test harness (`tests/harness/`): schema round-trip tests

---

## Alternatives considered

**Full per-language codegen now.** Rejected. Pkl's Java, Kotlin, Swift and Go
generators are a significant ongoing maintenance burden even with a dedicated
Apple team. KCL's 11-language SDK breadth requires an active CNCF community.
For a single-maintainer project, the JSON Schema bridge delivers the same
adoption surface at a fraction of the cost — existing JSON Schema toolchains
do the per-language step.

**`eu run -x json-schema`.** Rejected. Schema emission operates on type
annotations, not runtime values; conflating the two requires evaluating the
programme to discover annotations. A distinct subcommand is the correct boundary.

**CUE or Dhall as the interchange format.** Rejected. JSON Schema has broader
ecosystem support (Kubernetes, OpenAPI, Helm, `ajv`, `quicktype`) and is
standardised (IETF RFC 8927). CUE is a valid community plugin target, not a
core commitment.

**Ingest JSON Schema as in-memory type aliases (no source emit).** Rejected.
Invisible to the user, uncacheable, and not version-controllable. Source-text
output is auditable and can be reviewed in pull requests.

---

## Risks & what would kill this

1. **Type-system foundation — one remaining prerequisite: optional fields.**
   `Dict(T)` and equirecursive `Mu` types (TS-A2, TS-A3) shipped in 0.6.2, so
   the type DSL can express the homogeneous and recursive schemas in Kubernetes
   CRDs. But records cannot yet express **optional (may-be-absent) fields** —
   the dominant CRD shape (`required` vs `properties`). Faithful CRD import
   therefore depends on [0021 — optional record fields](0021-optional-record-fields.md)
   (`name?: T`, the required/optional partition, and the mapping to JSON
   Schema's `required` set); with it, the record-mapping rows above carry
   presence, and without it import must approximate optional fields as open
   records or all-required, gutting the K8s use case. The other remaining risk
   is the JSON Schema edge-case coverage described in risk 2.

2. **JSON Schema version fragmentation.** JSON Schema has five major drafts in
   active use (draft-04, draft-06, draft-07, draft-2019-09, draft-2020-12) with
   significant differences in `$ref`, `$defs`, `if`/`then`/`else`, and
   `unevaluatedProperties`. Supporting all of them is expensive; targeting only
   draft-2020-12 may exclude some real-world CRDs. The pragmatic answer is to
   default to draft-2020-12 on export and accept the three most common dialects
   on import, with explicit `--draft` selection.

3. **The import is lossy.** JSON Schema's constraint system (patterns,
   `minLength`, `multipleOf`, `minimum`, `maximum`) has no eucalypt type DSL
   equivalent. Imported schemas with such constraints will be approximated by
   the base type plus a comment. Users must add [0009] contracts manually to
   enforce the lost constraints. This is a fundamental limitation of the
   structural type DSL, not a bug in the importer.

4. **Adoption requires annotating existing `.eu` files.** `eu schema export`
   only emits schema for *annotated* bindings. A eucalypt file without `type:`
   annotations produces an empty schema. This is correct gradual-typing
   behaviour, but it means the feature's value accrues over time as users
   annotate their files, not immediately.

---

## Success criteria

- `eu schema export` produces valid JSON Schema (draft-2020-12) for all
  annotated bindings in the standard test corpus; validated by `ajv` in CI.
- `eu schema import k8s/deployment-crd.yaml | eu check -` passes without
  unknown-type warnings on the three most common Kubernetes core CRDs.
- Round-trip property: `eu schema export` followed by `eu schema import`
  produces type aliases that, when checked against the original source,
  generate no additional type warnings (modulo lossily-mapped constraints).
- No regression in `cargo test --test harness_test` and
  `cargo clippy --all-targets -- -D warnings`.
- At least one community-contributed integration (e.g. a `eu schema export`
  → `quicktype` pipeline for TypeScript type generation) appears within six
  months of the feature landing.

---

## References

**Eucalypt sources verified:**
- `src/driver/options.rs` — current driver subcommands (no schema subcommand as of 0.7.0)
- `src/export/json.rs:1–30` — JSON export via `serde_json`; crate already present
- `src/import/` — CSV, EDN, TOML, XML, YAML, JSON-lines, text; no schema format
- `src/core/typecheck/types.rs:1–17` — module doc explaining `Con`/`App` as the
  uniform representation for parametric constructors (`Dict`, `NonEmpty`, `List`, …)
  replacing the old dedicated enum variants (changed in 0.7.0)
- `src/core/typecheck/types.rs:73–82` — `constructor_kind`: `Dict`, `NonEmpty`,
  `List`, `IO` registered as `* → *` constructors
- `src/core/typecheck/types.rs:106` — `Type::Mu` in the `kind_of` match (equirecursive,
  shipped 0.6.2)
- `src/core/typecheck/parse.rs:1–28` — type DSL grammar summary
- `CHANGELOG.md:40–65` — 0.6.2: `Dict(a)`, equirecursive `Mu`, literal string types,
  `NonEmpty`, flow narrowing all shipped
- `CHANGELOG.md:5–39` — 0.7.0: `Con`/`App`/`Kind`/`forall` HKT; six dedicated
  variants (`List`, `IO`, `Dict`, `NonEmpty`, `Lens`, `Traversal`) folded into
  `Con`/`App`

**External references:**
- Pkl code generation: https://pkl-lang.org/main/current/kotlin-binding/codegen.html ;
  Go codegen: https://pkl-lang.org/go/current/codegen.html
- Pkl JSON Schema package: `pkg.pkl-lang.org/pkl-pantry/org.json_schema` (draft-06
  through draft-2020-12)
- Pkl CRD generator: `pkg.pkl-lang.org/pkl-pantry/k8s.contrib.crd`
- KCL import tool (JSON Schema, OpenAPI, CRD):
  https://www.kcl-lang.io/docs/tools/cli/kcl/import
- KCL CRD → KCL schema: https://www.kcl-lang.io/docs/0.4/tools/cli/openapi/crd-to-kcl
- JSON Schema specification (draft-2020-12): https://json-schema.org/specification
- Amadio & Cardelli, "Subtyping Recursive Types", TOPLAS 1993 (equirecursive
  foundation for the `Mu`-to-`$ref` mapping)
