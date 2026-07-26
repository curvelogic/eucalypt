# SV3 — Structural contracts & runtime validation — design

**Status:** draft for owner review · **Date:** 2026-07-26 · **Bead:** eu-u9xj.1 (SV3, W16) · **Epic:** eu-1tkk (0.14)

> This document writes up a design settled in an owner brainstorm on 2026-07-26.
> Where the owner made a call, the call and its rationale are recorded in
> §11 ("Decisions and rationale"). Every eucalypt snippet is in real catenation
> syntax; illustrative snippets are fenced `eu,notest` because the doctest CI
> executes bare `eu` fences under `docs/`.

---

## 1. Purpose and scope

**Purpose.** Guard the *ingress boundary*. Validate external data — imported
YAML, JSON, TOML, CSV, EDN, XML, or anything else that arrives from outside the
program — against a spec written as an ordinary `s"…"` type literal, and report
**precisely what is wrong**, with a path to each offending position.

ROADMAP §667–671 defines SV3 as:

> Apply specs explicitly at data ingress (`parse-as`/import sites and user
> checkpoints), with cost paid only where written — the runtime dual of the
> optimistic erased boundary.

Everything in this design follows from those two clauses: **explicit** (never
compiler-inserted) and **cost paid only where written** (nothing is forced that
the spec does not name).

**In scope (v1):**

- A **reporting interpreter** over the `t-*` type projection that walks spec and
  value together and accumulates violations with paths.
- `validate(spec, data)` — pure, total, returns a list of violations.
- `ensure(spec, data)` — returns `data` unchanged on success, raises on failure.
- A new `ExecutionError::ContractViolation` variant rendering through the
  existing codespan substrate.
- The projection change that makes record **closedness** visible at runtime.

**Out of scope (v1)** — each with a documented future path in §13:

- Declarative `ensure:` sugar at import sites.
- Data-file blame (`config.yaml:12`) for formats other than YAML/JSON.
- Presence *inference* (separate spec, bead **eu-1tkk.4**).
- Enforcing optionality at *read* sites ("piece A" — warning when `name?: T` is
  read without handling absence, plus a `has(k)` narrowing predicate).
- Default-fill (a spec supplying the value for an absent optional field).

**Explicitly not this design.** SV3 is **not** H13b of
`docs/development/type-system-evolution.md:905-946` — unconditional runtime casts
at every `any → T` boundary, which that document rejects on the Takikawa et al.
(2016) performance evidence. SV3 is **H13c**: opt-in, explicit, value-level
checks that a user writes where they want them. Compile-time types remain fully
erased before STG compilation (ROADMAP §292, §556, §648); nothing in this design
inserts a check the user did not write.

---

## 2. What exists today (verified on master `b9b34df4`)

The SV1/SV2 chain is shipped and stays **unchanged**:

```
s"{…}"  →  __TYPE_TO_DATA  →  t-* tagged data  →  as-spec  →  match?  →  bool
```

| Piece | Location | Behaviour |
|---|---|---|
| `Type::Record { fields, open, rows }` | `src/core/typecheck/types.rs:306-312` | fields is `BTreeMap<String, FieldPresence>` |
| `FieldPresence::{Required, Optional}` | `src/core/typecheck/types.rs:29-36` | surface `k: T` / `k?: T` |
| `TYPE_TO_DATA` projection | `src/eval/stg/typedata.rs:87-107` | **destructures `open: _, rows: _`** |
| `as-spec` | `lib/prelude.eu:2192-2257` (record arm `:2230-2242`) | lowers `t-*` to a `match?` pattern |
| `match?` | `lib/prelude.eu:553-570` | returns a **bare boolean**; hardwired **open** |
| `to-data` / `from-data` | `lib/reflect.eu:15-37` | projection and its inverse |

**The gap.** `match?` answers one bit. For a 400-line imported manifest, one bit
is close to useless: the user learns that *something* is wrong and nothing about
*what* or *where*. Closing that gap — turning the bit into a located report — is
the entire feature.

**The second gap.** The projection **discards closedness**. `typedata.rs:87-107`
destructures `Type::Record { fields, open: _, rows: _ }`, so `s"{a: number}"`
and `s"{a: number, ..}"` project to *identical* `t-*` data. Two observable
consequences, both verified on master:

```
$ eu -e 's"{a: number, ..}"'
"{a: number, ..}"                       # the surface knows it is open

$ eu -L lib -e '…to-data from-data…'    # via reflect.eu
"{a: number}"                           # the round-trip has lost the `..`
```

So `from-data ∘ to-data` is **already lossy for open records** today. A runtime
validator that wants to report `:unexpected` extras has no way to know whether
extras are allowed. Fixing the projection fixes both at once.

---

## 3. Architecture

SV3 extends the shipped chain with one new stage. `as-spec` and `match?` are not
touched.

```
s"{…}"  →  __TYPE_TO_DATA  →  t-* tagged data  →  [new] reporting interpreter  →  violation report
                                     │
                                     └────────→  as-spec  →  match?  →  bool     (unchanged)
```

The reporting interpreter is the dual of `as-spec`: where `as-spec` collapses a
type to a predicate and loses the reason, the interpreter walks spec and value
**together**, carrying a path, and emits an entry for every mismatch it finds.

Four components:

| # | Component | Where | Nature |
|---|---|---|---|
| 1 | Closedness in the `t-record` projection | `src/eval/stg/typedata.rs` | Rust, additive schema change |
| 2 | `reflect.type-str` — expose the `t-*` renderer | `lib/reflect.eu` | eucalypt, pure extraction |
| 3 | `validate` — the reporting interpreter | `lib/contract.eu` (new) | eucalypt |
| 4 | `ensure` + `ExecutionError::ContractViolation` | `lib/contract.eu`, `src/eval/` | eucalypt + Rust |

---

## 4. Component 1 — closedness in the `t-record` projection

### 4.1 The change

`typedata.rs:87-107` currently emits:

```
[:t-record, { k: [:t-field, :required, <type-data>], … }]
```

It gains a third, trailing element — a boolean:

```
[:t-record, { k: [:t-field, :required, <type-data>], … }, <open?>]
```

where

```rust
let open_flag = *open || !rows.is_empty();
```

### 4.2 Why row variables collapse into the boolean

A record type may carry a named row variable (`{k: T, ..r}`), and rows are
`Type` values. Projecting them faithfully would leak *type variables* into
runtime data, which no runtime consumer can interpret — a runtime validator
cannot instantiate `r`. Since a named row variable and a bare `..` mean the same
thing to a validator ("extra fields are permitted"), they collapse to the single
boolean `open || !rows.is_empty()`. **No type variables enter the runtime
projection.**

The cost is that the projection is lossy for the *name* of the row variable, so
`from-data ∘ to-data` maps `{a: number, ..r}` to `{a: number, ..}`. This is
strictly less lossy than today (which maps it to `{a: number}`) and the lost
information is meaningless outside the checker.

### 4.3 Schema compatibility

ROADMAP §717–724 makes the `t-*` projection **the versioned surface**:
"documented and tolerant of additive growth", while the type-DSL grammar and the
opaque value internals stay Experimental. This change is exactly the sanctioned
additive growth: a **new trailing element on one node type**, no existing element
moved or retyped.

Two obligations follow:

1. **Consumers must tolerate a missing trailing element.** Users construct `t-*`
   data by hand (an advertised use — `docs/guide/type-checking.md:1011` shows
   hand-written tags). A two-element `[:t-record, {…}]` must keep working and be
   read as **closed**. In eucalypt this needs an explicit arity guard, and the
   guard needs parentheses because comparison binds tighter than catenation:

   ```eu,notest
   # `rest` is the tail after the :t-record tag
   open-flag(rest): ((rest count) > 1) then(rest second, false)
   ```

2. **Documented and changelogged.** `docs/guide/type-checking.md:1003-1011` (the
   `t-*` tag list and the worked `:t-record` example) is updated, and the
   CHANGELOG records the additive schema change under the Experimental tier.

### 4.4 Blast radius

| Consumer | Effect |
|---|---|
| `as-spec` record arm (`lib/prelude.eu:2230-2242`) | **None.** It reads `rest first` only; the extra element is ignored. `match?` stays open. |
| `reflect.from-data` `record-str` (`lib/reflect.eu`) | Must render `, ..` when open — see §5. This *fixes* the existing round-trip loss. |
| Hand-written `t-*` data | Unaffected, per the arity guard above. |
| Anything else | Nothing else reads `:t-record`; verified by `grep -rn "t-record" docs/ lib/ src/`. |

---

## 5. Component 2 — `reflect.type-str`

### 5.1 Why it is needed

A violation report has to say *what was expected*, in the user's own type
vocabulary: `expected: "number"`, `expected: "[string]"`. The canonical renderer
for a `t-*` node already exists — but it is a **local binding inside
`from-data`'s block** in `lib/reflect.eu`, so it cannot be reached from outside.

The obvious alternative does not work. Type-data values do not render through
`str.of`; verified on master:

```
$ eu -e 'str.of(s"[number]")'
error: expected a primitive value but found a structured value (block or list)
```

So the interpreter must render from the **tagged list**, which is what it is
walking anyway.

### 5.2 The change

Hoist `to-str` out of `from-data`'s block into a top-level exported binding:

```eu,notest
` { doc: "`type-str(td)` - render a t-* tagged list as its canonical type-DSL string."
    type: s"any → string" }
type-str(td): …

` { doc: "`from-data(td)` - construct a type-data value from a t-* tagged list."
    type: s"any → any" }
from-data(td): td type-str __TYPE_FROM_STRING
```

This is a **pure extraction** — no behaviour change to `from-data` — plus the one
behavioural fix required by §4: `record-str` gains the open flag and appends
`, ..` when set, closing the round-trip loss demonstrated in §2.

`type-str` is useful in its own right (it is the natural "print this type" for
any `t-*` consumer) and is documented alongside `to-data`/`from-data`.

---

## 6. Component 3 — `validate`

### 6.1 Signature and contract

```eu,notest
validate(spec, data)   # -> [violation]
```

- **Argument order** follows the prelude catenation idiom: the receiver is the
  **last** parameter, so `data validate(spec)` reads correctly.
- **Pure.** No IO, no emission, no mutation.
- **Never raises** for any *data* — however malformed, unexpected, or partial.
  A non-conformant value produces violations, not an error. (A malformed
  **spec** is a different matter — §6.7.)
- **Total in its result:** returns a list. `[]` means conformant.

`spec` is either a type-data value or an already-projected `t-*` tagged list; it
is normalised on entry exactly as `as-spec` does it
(`type-data?(t) then(__TYPE_TO_DATA(t), t)`, `lib/prelude.eu:2193`).

### 6.2 The violation entry

Each violation is a block with exactly four keys:

```
{ path: "servers[2].port", kind: :type-mismatch, expected: "number", found: "string" }
{ path: "servers[3]",      kind: :missing,       expected: "host",   found: :absent  }
{ path: "",                kind: :unexpected,    expected: :closed,  found: [:debug] }
```

The four kinds, and the meaning of `expected`/`found` in each:

| `kind` | Raised when | `path` points at | `expected` | `found` |
|---|---|---|---|---|
| `:type-mismatch` | a value is present but does not match its spec | the **value** | string — canonical type-DSL rendering of the spec at that path (`type-str`) | string — the runtime type name of the value |
| `:missing` | a required record field is absent | the **containing record** | string — the missing field's key | `:absent` |
| `:unexpected` | a **closed** record has surplus keys | the **record** | `:closed` | list of surplus key symbols |
| `:length` | a tuple or prefix-list has the wrong arity | the **list** | number — required length (prefix-list: minimum) | number — actual length |

Runtime type names used in `found` for `:type-mismatch` are exactly the
predicate vocabulary the language already exposes: `"number"`, `"string"`,
`"symbol"`, `"bool"`, `"null"`, `"datetime"`, `"list"`, `"block"`, `"function"`,
`"type-data"`.

The report is a plain list of plain blocks — ordinary eucalypt data. It can be
filtered, grouped, counted, rendered to a table, or emitted as YAML like anything
else. That is the point of returning data rather than a string.

### 6.3 Path grammar

`path` is a string. It is the **primary locator**: format-independent, always
available, and meaningful for every input format including those with no source
provenance at all (§9.2).

| Step | Rendering |
|---|---|
| root | `""` (the empty string) |
| record field, identifier-shaped key | `.name`, with the leading `.` elided at the root — so `port`, `server.port` |
| record field, other key | `.'my key'` — quoted-identifier form, matching eucalypt's own `'…'` syntax |
| list/tuple index | `[3]`, appended with no separator — `servers[2].port` |

"Identifier-shaped" means the key would lex as a bare eucalypt identifier. The
rendered path is therefore a **valid eucalypt lookup expression** relative to the
validated value whenever every step is identifier-shaped or quoted — so it can be
pasted into a lens path or a `.`-chain to inspect the offending value.

### 6.4 Per-constructor semantics

The interpreter dispatches on the `t-*` tag, mirroring `as-spec`'s
`to-spec-td` (`lib/prelude.eu:2245-2256`) so the two agree on what conforms.

| Tag | Behaviour |
|---|---|
| `:t-prim` `number`/`string`/`symbol`/`bool`/`null`/`datetime` | force to WHNF, apply the corresponding predicate; on failure one `:type-mismatch` |
| `:t-prim` `any` / `top` | **accept without forcing** |
| `:t-prim` `never` | always one `:type-mismatch` (no value inhabits it) |
| `:t-list` | force spine; if not a list, one `:type-mismatch`; otherwise recurse into each element with path `[i]` |
| `:t-tuple` | force spine; if not a list, `:type-mismatch`; if arity differs, one `:length` and **no** element recursion; otherwise recurse elementwise |
| `:t-prefix-list` | force spine; if not a list, `:type-mismatch`; if shorter than the prefix, one `:length`; otherwise recurse into the prefix positionally and the tail homogeneously |
| `:t-record` | see §6.5 |
| `:t-union` | force to WHNF; if **no** branch validates cleanly, **one** `:type-mismatch` at the union's own path, `expected` = the union's rendered type. Branch-level violations are **not** expanded |
| `:t-partial` (`T?`) | accept `null`, else validate against `T` — parity with `as-spec`'s `partial-spec` |
| `:t-fn` | accept iff the value is an unsaturated function (`__SATURATED not`) — parity with `as-spec`'s `fun-spec` |
| `:t-lit-str` / `:t-lit-sym` | accept iff the value is equal to the literal; else `:type-mismatch` with `expected` the literal's rendering |
| `:t-forall` | erase the quantifier, recurse into the body |
| `:t-var` | **accept without forcing** — a type variable is unconstrained at runtime |
| any other tag (`:t-con`, `:t-app`, `:t-mu`, …) | **accept without forcing** — parity with `as-spec`'s `any?` fallthrough |

Two notes on the table:

- **Unions report one violation, not N.** A failed 3-branch union has three
  incomparable explanations; emitting all three is noise, and nesting them
  destroys the flat, filterable shape of the report. The union's own rendered
  type is the honest single expectation.
- **`t-con`/`t-app`/`t-mu` accept.** `Dict(T)`, `NonEmpty([T])` and recursive
  `Mu` types are not validated in v1. This is deliberate **parity with
  `as-spec`**, so the two never disagree about what conforms; extending both
  together is future work (§13.6).

### 6.5 Records

For a `[:t-record, fields, open?]` node against a value `v`:

1. If `v` is not a block → one `:type-mismatch`, `expected` the record's
   rendered type and `found` the runtime type name of `v` (`"list"`,
   `"string"`, …). **Stop**; do not descend.
2. Enumerate `v`'s keys (`keys` — spine only, see §6.6).
3. For each field `k` in the spec, in spec order:
   - present → recurse into `v lookup(k)` at path `…​.k`;
   - absent and `:required` → one `:missing` at the record's path;
   - absent and `:optional` → **nothing**. Absence is conformant.
4. If the record is **closed** and `v` has keys not named in the spec → **one**
   `:unexpected` at the record's path, listing every surplus key.
5. If the record is **open** → surplus keys are ignored entirely.

Step 4/5 is why Component 1 exists. A spec literal must **mean the same thing
statically and at runtime**: `s"{a: number}"` is closed to the checker, so it is
closed to the validator; `s"{a: number, ..}"` is open to both. One syntax with
two meanings — the alternative, where the runtime silently treated every record
as open — was explicitly rejected (§11).

### 6.6 Forcing discipline — "cost paid only where written"

The interpreter forces **only the paths the spec names**. Stated precisely:

- A **record** spec forces the value to WHNF (it must know it is a block) and
  enumerates its keys. Key enumeration walks the block's spine and does **not**
  force any value. Only the values under keys the spec *names* are forced.
  Closedness detection is therefore cheap: it is a key-set comparison, never a
  value forcing.
- A **list**, **tuple** or **prefix-list** spec forces the spine, and forces each
  element only as far as its element spec demands.
- A **primitive** spec forces its value to WHNF — the minimum needed to name a
  type.
- `any`, `top`, `:t-var`, and unrecognised tags force **nothing at all**.

Consequences worth stating because users will rely on them:

- `s"{}"` against a 10 000-key imported block forces the spine and nothing else.
- `s"{name: string, ..}"` against that block forces exactly `name`.
- A spec is safe to apply to a value with expensive or diverging subtrees, as
  long as it does not name them.

`ensure` inherits this discipline exactly — it forces what `validate` forces, and
returns the same (now partially forced) value.

### 6.7 Malformed specs

A **malformed spec** is a program bug, not a data problem, and must not be
reportable as a violation — otherwise a typo in a schema masquerades as bad input
data. `validate` raises when `spec` is neither type-data nor a `t-*`-shaped
tagged list (a non-empty list whose head is a symbol):

```eu,notest
{ a: 1 } validate({ not: "a spec" })
# error: validate: not a type spec
```

This is raised through the existing `panic` route
(`lib/prelude.eu:251`, `src/eval/stg/panic.rs:27` → `ExecutionError::UserPanic`)
— a **different** `ExecutionError` variant from `ContractViolation`, so the two
are distinguishable both by the user reading stderr and by an error test's
`.expect` sidecar.

Note the distinction from §6.4: an *unrecognised tag inside a well-formed spec*
(`:t-mu`, `:t-con`, …) is **not** malformed. It is valid type-data that this
version cannot validate, so it accepts. Only a value that is not a spec at all
raises.

### 6.8 Ordering and determinism

The report order is fully determined, because tests depend on it:

1. Depth-first, in **spec order**. Record fields come from the projection's
   `BTreeMap`, so spec order is **lexicographic by key** — stable across runs and
   independent of the data's key order. List elements are in index order.
2. Within one record: field violations first, then its `:unexpected` entry.
3. A `:type-mismatch` on a container **terminates** that subtree — no violations
   are reported beneath a value that is not even the right shape.

---

## 7. Component 4 — `ensure` and `ContractViolation`

### 7.1 `ensure`

```eu,notest
ensure(spec, data)   # -> data, or raises
```

Returns `data` **unchanged** on success, so it drops into a pipeline without
restructuring the surrounding code:

```eu,notest
{ import: ["contract.eu", "cfg=config.yaml"] }

schema: s"{ name: string, port: number, tags?: [string] }"

config: cfg ensure(schema)
```

Illustrative definition (real shape; the rendering helpers are elided):

```eu,notest
ensure(spec, data): {
  :let
  violations: data validate(spec)
}.((violations nil?) then(data,
                          __CONTRACT_FAIL(headline(spec, violations),
                                          violations map(render-violation))))
```

Two details that matter and are easy to get wrong:

- `then(t, f, c)` takes the condition **last**, so the receiver form is
  `(violations nil?) then(data, …)` — and the parentheses around `violations
  nil?` are required, since `then` would otherwise be catenated onto the wrong
  operand.
- `then` is lazy in its branches, so `__CONTRACT_FAIL` is not evaluated on the
  success path. `ensure` on conforming data costs exactly one `validate`.

**Naming.** The owner renamed this from `conform`. Clojure's `conform` returns a
*destructured* value — it changes the data. This returns the data untouched, so
borrowing the name would promise something the function does not do (§11).

### 7.2 The error variant

```rust
#[error("{}", format_contract_violation(&.1.0, &.1.1))]
ContractViolation(Smid, Box<(String, Vec<String>)>),
```

Placed alongside `UserPanic` (`src/eval/error.rs:862`) and `AssertionFailed`
(`:773-774`) in `ExecutionError` (`src/eval/error.rs:749`). Boxed, following the
established convention for multi-field variants (see `LookupFailure`,
`src/eval/error.rs:773`). It carries:

- the **call-site `Smid`** — `machine.annotation()`, exactly as `Panic` does
  (`src/eval/stg/panic.rs:27`);
- a **headline** string (the type the data failed, plus the violation count);
- one **rendered line per violation**.

It reports an error **code** of `EU-EVAL-CONTRACT`, joining `EU-EVAL-TYPE` in
`ExecutionError::code()` (`src/eval/error.rs:1464-1470`).

### 7.3 The marshalling boundary — a call this design makes explicit

The violations are eucalypt blocks; the error variant is a Rust type. Something
must cross. **Decision: the boundary is strings.** The prelude renders each
violation to a line, and `__CONTRACT_FAIL(headline, lines)` takes a string and a
list of strings.

Rationale: the report's *presentation* then lives in eucalypt where it is
readable, testable by harness test, and changeable without touching Rust; the
Rust side stays a dumb carrier. The supporting helper already exists —
`str_list_arg` (`src/eval/stg/support.rs:303`) collects a forced list of native
strings into a `Vec<String>` — and the variant shape mirrors `LookupFailure`,
which already carries `Box<(String, Vec<String>, Vec<String>)>`.

The structured report is not lost: it is exactly what `validate` returns, and a
user who wants structure calls `validate` instead of `ensure`.

### 7.4 Rendering

`to_diagnostic` (`src/eval/error.rs:1017-1022`) already builds a codespan
`Diagnostic` from the variant's `Smid` via the `SourceMap`, and the driver
renders it (`src/driver/error.rs:66-77`). `ContractViolation` needs no new
machinery: the headline becomes the diagnostic message, the `Smid` becomes the
primary label, and each violation line becomes a **note**. Shape:

```
error[EU-EVAL-CONTRACT]: contract violation: 2 violations against { name: string, port: number }
  ┌─ deploy.eu:7:13
  │
7 │ config: raw ensure(schema)
  │             ^^^^^^^^^^^^^^ contract violation
  │
  = servers[2].port: expected number, found string
  = servers[3]: missing required field 'host'
```

### 7.5 `ContractViolation` is not a `TypeWarning`

`TypeWarning` (`src/core/typecheck/error.rs:14-24`) is a **disjoint type** from
`ExecutionError` — a struct with its own `message`/`smid`/`expected`/`found`,
surfaced by `eu check` and gated behind `--strict`. A contract failure is an
`ExecutionError`: it happens during evaluation, it is not advisory, it is not
suppressible by `--suppress-type-warnings`, and it aborts the program. The two
never mix. This is the same separation the roadmap draws between the erased
static layer and the explicit runtime layer.

---

## 8. Where the code lives

`validate` and `ensure` go in a **new `lib/contract.eu`** that imports
`reflect.eu`, alongside `lib/lens.eu`, `lib/state.eu` and `lib/markup.eu`:

```eu,notest
{ requires: ">=0.14"
  import: "reflect.eu"
  doc: "Structural contracts: validate and ensure data against s-string type specs." }
```

Rationale: the interpreter needs `reflect.type-str`, which lives outside the
prelude; contracts are an opt-in feature, and an opt-in feature that must be
imported reads honestly against "cost paid only where written"; and the prelude
stays lean. Users write one import line, matching every other library in `lib/`.

---

## 9. Blame

### 9.1 What v1 gives

Two locators, and the design is explicit about which is primary:

1. **The report `path`** — *primary*. Format-independent, always available, and
   the thing that actually tells the user which field to fix.
2. **The raised error's source location** — the **`ensure` call site**, in the
   user's `.eu` file. It answers "which contract failed", not "which byte of the
   data file".

### 9.2 Data-file blame is deferred, and here is why

Pointing at `config.yaml:12` requires the importer to have minted a `Smid` for
that value. Provenance today is **YAML-only**:

- `src/import/yaml.rs:97,450,455-530` mints Smids from the YAML parser's markers
  (`new_smid(span)` at `:450`).
- `src/import/mod.rs:54` routes **`"yaml" | "json"`** through that same reader,
  so JSON inherits the provenance.
- `src/import/{toml,csv,edn,xml,text}.rs` have **no** `Smid` tracking whatsoever
  (verified: zero occurrences of `Smid` in each).
- `src/import/jsonl.rs` has minimal `Smid` use and is not per-value.

So five of the seven importers would each need `Smid` plumbing before data-file
blame could be offered *uniformly*. Offering it for YAML/JSON only would make the
quality of an error message depend on the file extension — the worst of both.
Future path in §13.2.

---

## 10. Worked examples

Given `servers.yaml`:

```yaml
servers:
  - host: "a.example.com"
    port: 8080
  - host: "b.example.com"
    port: 9090
  - host: "c.example.com"
    port: "7070"
  - port: 6060
debug: true
```

and the spec:

```eu,notest
{ import: ["contract.eu", "data=servers.yaml"] }

schema: s"{ servers: [{ host: string, port: number }] }"
```

**`data validate(schema)`** returns:

```yaml
- path: "servers[2].port"
  kind: type-mismatch
  expected: "number"
  found: "string"
- path: "servers[3]"
  kind: missing
  expected: "host"
  found: absent
- path: ""
  kind: unexpected
  expected: closed
  found: [debug]
```

Three separate facts, three separate entries — this is the "ALL violations
collected" rule (§11) doing its work. Under `match?` the user would have seen
`false`.

**`data ensure(schema)`** raises with all three as notes.

**Open the spec** — `s"{ servers: [{ host: string, port: number }], .. }"` — and
the `:unexpected` entry disappears; the other two remain. That is the whole
observable effect of Component 1.

**Optional fields:**

```eu,notest
{ import: "contract.eu" }

schema: s"{ host: string, port?: number }"

a: { host: "x" }              validate(schema)   # [] — absence is fine
b: { host: "x", port: 80 }    validate(schema)   # [] — present and correct
c: { host: "x", port: "80" }  validate(schema)   # one :type-mismatch at "port"
```

---

## 11. Decisions and rationale

Each of these was settled by the owner on 2026-07-26.

| # | Decision | Rationale |
|---|---|---|
| 1 | **All** violations are collected, not first-only | A boundary check exists to let the user fix the data in one pass. Stopping at the first violation turns one round trip into N. |
| 2 | A spec literal means the **same thing** statically and at runtime | The alternative — the checker treats `{a: number}` as closed while the validator treats it as open — gives one syntax two meanings, which is the kind of thing users never stop tripping over. This decision is the *reason* Component 1 exists. |
| 3 | Row variables collapse to a boolean in the projection | Faithful projection would leak type variables into runtime data, which no runtime consumer can interpret. Named and anonymous openness are indistinguishable to a validator. |
| 4 | `k?: T` — absent is fine, present must match | Reuses the shipped optional arm (`FieldPresence::Optional`, `types.rs:29-36`; `as-spec` optional arm, `prelude.eu:2230-2242`). No new surface. |
| 5 | **Spec-directed forcing** — force only the paths the spec names | ROADMAP §667–671's "cost paid only where written", taken literally. Closedness enumerates keys without forcing their values, so even a closed spec is cheap. |
| 6 | A malformed **spec** is an error, distinct from a violation | A schema typo is a program bug. Reporting it as a data violation would send the user to inspect the wrong file. |
| 7 | Blame v1 = `path` (primary) + `ensure` call site | `path` is the format-independent locator and is always available; data-file blame is gated on importer `Smid` plumbing (§9.2). |
| 8 | `ensure`, **not** `conform` | Clojure's `conform` returns a destructured value. This returns the data unchanged, so the name would mislead. |
| 9 | `ContractViolation` is an `ExecutionError`, not a `TypeWarning` | It aborts evaluation and is not advisory; `TypeWarning` is a disjoint, suppressible, check-time type (§7.5). |
| 10 | Contracts are **explicit value-level constructs**, never compiler-inserted | Types are fully erased before STG (ROADMAP §292, §556, §648); this is H13c, and `type-system-evolution.md:905-946` explicitly rejects the unconditional H13b. |
| 11 | `as-spec` and `match?` are **unchanged** | They are shipped surface with users. SV3 adds a stage; it does not renegotiate an existing one. |

Two consistency decisions this write-up makes explicit because the brainstorm did
not name them, resolved in the direction of "never disagree with the shipped
chain" (both flagged for owner confirmation in §15):

| # | Decision | Rationale |
|---|---|---|
| 12 | A failed union yields **one** violation at the union's path, not one per branch | N incomparable explanations is noise, and nesting them would break the flat filterable report. |
| 13 | `t-con`/`t-app`/`t-mu` (`Dict(T)`, `NonEmpty([T])`, recursive types) **accept** in v1 | Parity with `as-spec`'s `any?` fallthrough, so `validate` and `match?` never disagree about what conforms. Extending both together is §13.6. |

---

## 12. Testing strategy

Every regression test is **fault-injection verified**: break the code under test,
confirm the harness test FAILs, restore, confirm it PASSes. The PR states that
this was done, per `CLAUDE.md` and `docs/guide/testing.md`.

### 12.1 Harness tests

Following `tests/harness/189_r9oy_union_as_spec.eu` and
`tests/harness/182_typedata_alias_resolution.eu`, each target computes `RESULT`
from its checks so that every assertion is genuinely in the verdict:

```eu,notest
RESULT: if([t1, t2, t3, …] all-true?, :PASS, :FAIL)
```

Coverage:

| Test | Gates |
|---|---|
| projection closedness | `s"{a: number}" to-data` third element is `false`; `s"{a: number, ..}"` and `s"{a: number, ..r}"` give `true` |
| projection tolerance | a hand-written two-element `[:t-record, {…}]` still renders and still validates, treated as closed |
| round-trip | `s"{a: number, ..}" to-data from-data` renders `"{a: number, ..}"` — the fix to the loss demonstrated in §2 |
| conformance | conforming data at every `t-*` constructor gives `[]` |
| each violation kind | one focused target per kind: `:type-mismatch`, `:missing`, `:unexpected`, `:length` |
| accumulation | a value with ≥3 independent faults yields exactly 3 entries |
| paths | nested record-in-list-in-record produces `servers[2].port`; a non-identifier key produces the quoted form |
| optional fields | absent / present-and-correct / present-and-wrong |
| open vs closed | the same data, the same fields, `..` toggling exactly the `:unexpected` entry |
| ordering | the report order is exactly as §6.8 specifies |
| forcing | a spec that does not name a diverging subtree completes; the same spec naming it does not — the executable statement of §6.6 |
| `ensure` success | `data ensure(spec)` is `=` to `data` |
| `as-spec` agreement | for a corpus of spec/value pairs, `(validate = [])` agrees with `match?(as-spec …)` wherever both are defined |

### 12.2 Error tests

`tests/harness/errors/` with `.expect` sidecars, in the established format
(`exit:` + a `stderr:` regex):

- `ensure` on non-conforming data → exit 1, stderr matching the
  `EU-EVAL-CONTRACT` headline and at least one violation note.
- `validate` on a malformed spec → exit 1, stderr matching the *distinct*
  `validate: not a type spec` message — this is what makes decision 6 testable.

### 12.3 Both engines

The suite runs under the default bytecode engine and under `EU_HEAPSYN=1`, per
the standing differential-testing rule. The new intrinsic is the only
engine-sensitive surface; it must behave identically on both.

### 12.4 Gates

`cargo test`, `cargo clippy --all-targets -- -D warnings`, `cargo fmt --all`.
Documentation snippets under `docs/` are `eu,notest` unless they are genuinely
meant to execute, since the doctest CI runs bare `eu` fences.

---

## 13. Deferred, with future paths

The owner's standing concern is that deferred features must not be **designed
into a corner**. Each item below records the path that stays open.

### 13.1 Declarative `ensure:` sugar at import sites

```eu,notest
{ import: "cfg=config.yaml"
  ensure: { cfg: s"{ name: string, port: number }" } }
```

**Path: pure sugar over the function form.** It desugars to `cfg ensure(spec)`
wrapping the imported binding — no new semantics, no new error variant, no
change to `validate`. It is deferred only because the function form must prove
itself first, and because the import-metadata surface deserves its own
discussion. Nothing in v1 forecloses it.

### 13.2 Data-file blame

Reporting `config.yaml:12` rather than the `ensure` call site.

**Path: importer `Smid` plumbing, then an optional fifth key on the violation.**
YAML/JSON already mint per-value `Smid`s (§9.2); the other five importers each
need the same treatment. Once every importer carries provenance, a violation
gains an optional `at:` locator and the diagnostic gains a secondary label. The
violation block is already an open-ended map of keys, so adding one is additive —
the report format does not have to change shape.

### 13.3 Presence inference — pieces B and C

**Separate spec, bead eu-1tkk.4. Not folded in here.** Inferring optionality
rather than requiring it to be annotated is checker work in
`src/core/typecheck/`, on the *static* side of the erasure boundary. SV3 consumes
`FieldPresence` from the projection and is indifferent to how a field came to be
optional, so the two can land in either order.

### 13.4 Enforcing optional at read sites — "piece A"

Making `name?: T` warn when read without handling absence, plus a `has(k)`
narrowing predicate so that `if(b has(:name), b.name, default)` type-checks.

**Path: checker-side, flow-sensitive narrowing.** The narrowing machinery already
exists for type predicates (`number?`, `string?`, …); `has(k)` is one more
narrower in the same framework. Deferred by the owner; independent of SV3, which
neither needs nor blocks it.

### 13.5 Default-fill

A spec supplying the value for an absent optional field (ROADMAP §SV notes it as
composing with optional fields).

**Path: a third function beside `validate`/`ensure`.** It is a *transformer* —
it returns different data — so it must not be `ensure` under a different flag.
`validate` already computes exactly the information it needs (which optional
fields were absent), so the interpreter is reusable as-is.

### 13.6 `Dict(T)`, `NonEmpty([T])`, recursive `Mu` types

Currently accepted unconditionally (decision 13).

**Path: extend `as-spec` and `validate` together, in one change.** They must
never disagree about what conforms, so a bead that adds `Dict(T)` to one adds it
to the other. Both dispatch on the same tags, so the extension is a new arm in
each.

### 13.7 Literal types in `as-spec`

`validate` checks `:t-lit-str`/`:t-lit-sym` by equality (§6.4). `as-spec` does
**not** — `to-spec-td` has no arm for either tag, so they fall through to `any?`
(`lib/prelude.eu:2245-2256`), meaning `s"\"prod\"" as-spec` currently matches
anything. That looks like a defect in the shipped `as-spec`, not an intended
divergence. Flagged for a follow-up bead; SV3 does the right thing on its own
side rather than replicating the weaker behaviour.

---

## 14. Implementation phases

| Phase | Content | Gate |
|---|---|---|
| **P1** | Projection: emit closedness on `:t-record` (`typedata.rs`); `record-str` renders `, ..`; arity-tolerant read; `docs/guide/type-checking.md` + CHANGELOG | projection + round-trip + tolerance harness tests green on both engines |
| **P2** | `reflect.type-str` extraction; `from-data` reduced to a one-liner over it; doc entry | existing reflect tests unchanged and green |
| **P3** | `lib/contract.eu` with `validate`: the interpreter, the report format, paths, ordering, forcing discipline | conformance, per-kind, path, ordering, forcing, and `as-spec`-agreement tests |
| **P4** | `ExecutionError::ContractViolation` + `EU-EVAL-CONTRACT` + `__CONTRACT_FAIL` intrinsic + `ensure` | error tests with `.expect` sidecars; rendering check |
| **P5** | Guide chapter, prelude/library reference regeneration (`eu doc`), CHANGELOG | full CI rollup green, doctest CI included |

P1 and P2 are independently useful and independently mergeable — P1 alone fixes
the open-record round-trip loss. P3 depends on both. P4 depends on P3.

---

## 15. Points flagged for owner confirmation

These are places where the design as briefed did not determine an answer. Each
has been resolved above in the least inventive direction; none is silently
assumed.

1. **`expected` is not uniformly typed across kinds.** In the agreed entry shape
   it is a *type string* for `:type-mismatch`, a *key name* for `:missing`, and
   the *symbol* `:closed` for `:unexpected`. Written up faithfully (§6.2), but a
   consumer cannot treat `expected` uniformly. If uniformity is wanted, the
   cheapest fix is to make `:missing` carry the field's rendered **type** and move
   the key name into the path (`servers[3].host`).
2. **`:missing` does not carry the missing field's type.** Follows from 1 — the
   user is told `host` is missing but not what `host` should have been.
3. **Union violations** — resolved as one entry per union, not one per branch
   (decision 12). Confirm.
4. **`Dict(T)`/`NonEmpty([T])`/`Mu`** — resolved as accept-in-v1 for `as-spec`
   parity (decision 13). `Dict(T)` in particular is a common ingress shape ("a
   block of string → number"), so this may be worth pulling forward.
5. **The Rust/eucalypt marshalling boundary is strings** (§7.3). The brainstorm
   said the variant "carries the violations"; carrying *structured* eucalypt
   values into a Rust error variant would mean marshalling blocks in Rust, so
   rendered lines were chosen instead. The structured report remains available
   from `validate`.
6. **`lib/contract.eu` rather than the prelude** (§8) — forced by `type-str`
   living in `reflect.eu`, and consistent with contracts being opt-in, but it
   does mean `ensure` needs an import line.
7. **The grounding brief cited `src/import/json.rs`; no such file exists.** JSON
   is routed through the YAML reader (`src/import/mod.rs:54`), so JSON *does*
   have `Smid` provenance. The formats genuinely lacking it are TOML, CSV, EDN,
   XML, text and (per-value) JSONL. §9.2 is written to the verified position.

---

## 16. Grounding citations

All verified on master `b9b34df4`.

| Fact | Citation |
|---|---|
| `FieldPresence::{Required, Optional}` | `src/core/typecheck/types.rs:29-36` |
| `Type::Record { fields, open, rows }`, `BTreeMap<String, FieldPresence>` | `src/core/typecheck/types.rs:306-312` |
| Projection discards closedness (`open: _, rows: _`) | `src/eval/stg/typedata.rs:87-107` |
| `as-spec`; record arm and optional arm | `lib/prelude.eu:2192-2257`; `:2230-2242` |
| `match?` returns a bare boolean, hardwired open | `lib/prelude.eu:553-570` |
| `to-data` / `from-data` | `lib/reflect.eu:15-37` |
| `panic` → `UserPanic` | `lib/prelude.eu:251`; `src/eval/stg/panic.rs:27` |
| `ExecutionError` enum | `src/eval/error.rs:749` |
| `LookupFailure`, `AssertionFailed` | `src/eval/error.rs:773-774` |
| `UserPanic` | `src/eval/error.rs:862` |
| `to_diagnostic` | `src/eval/error.rs:1017-1022` |
| `ExecutionError::code()` (`EU-EVAL-TYPE`) | `src/eval/error.rs:1464-1470` |
| Diagnostic rendering via codespan + `SourceMap` | `src/driver/error.rs:66-77` |
| `str_list_arg` support helper | `src/eval/stg/support.rs:303` |
| `TypeWarning` — disjoint from `ExecutionError` | `src/core/typecheck/error.rs:14-24` |
| YAML provenance: `Smid`s minted from markers | `src/import/yaml.rs:97,450,455-530` |
| JSON routed through the YAML reader | `src/import/mod.rs:54` |
| No `Smid` tracking in TOML/CSV/EDN/XML/text | `src/import/{toml,csv,edn,xml,text}.rs` |
| SV3 definition — "cost paid only where written" | ROADMAP §667-671 |
| `t-*` projection is the versioned surface | ROADMAP §717-724 |
| Types fully erased before STG compile | ROADMAP §292, §556, §648 |
| H13b rejected; H13c endorsed | `docs/development/type-system-evolution.md:905-946` |
| `t-*` tag list and worked `:t-record` example | `docs/guide/type-checking.md:1003-1011` |
| Harness test pattern computing `RESULT` | `tests/harness/189_r9oy_union_as_spec.eu`; `tests/harness/182_typedata_alias_resolution.eu` |
| Error-test `.expect` format | `tests/harness/errors/*.eu.expect` |
