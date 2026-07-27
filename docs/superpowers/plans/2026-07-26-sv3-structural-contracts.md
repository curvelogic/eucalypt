# SV3 — Structural contracts & runtime validation — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship runtime structural contracts — `validate(spec, data)` returning a
located violation report and `ensure(spec, data)` raising a
`ContractViolation` — so that external data can be checked against an `s"…"`
type literal at the ingress boundary.

**Architecture:** Extend the shipped SV1/SV2 chain with one new stage. The
`t-record` projection gains a trailing closedness boolean; `reflect.type-str` is
hoisted out of `from-data` so the canonical type renderer is reachable; a new
`lib/contract.eu` resource carries a *reporting interpreter* that walks the
`t-*` spec and the value together, accumulating `{path, kind, expected, found}`
entries; and a new `CONTRACT_FAIL` intrinsic raises
`ExecutionError::ContractViolation` (code `EU-EVAL-CONTRACT`) through the
existing codespan substrate. `as-spec` and `match?` are untouched.

**Tech Stack:** Rust (`src/eval/stg/typedata.rs`, `src/eval/stg/contract.rs`,
`src/eval/error.rs`, `src/eval/intrinsics.rs`, `src/driver/resources.rs`,
`src/driver/error_codes.rs`), eucalypt (`lib/reflect.eu`, `lib/contract.eu`),
harness tests (`tests/harness/`), `cargo xtask prelude-compile`.

**Authority:** `docs/superpowers/specs/2026-07-26-sv3-structural-contracts-design.md`
(owner-approved, merged as a1af1f0b). Where this plan and the spec differ, the
spec wins; every deliberate departure is listed in §"Departures from the spec"
at the end and must be reported in the PR.

## Global Constraints

- Branch `feat/quill-sv3-contracts`, branched from and PR'd to `master`. One
  bead (eu-u9xj.1) per PR. Never self-merge; never close the bead.
- **Do not touch** `max-of`, `min-of`, `max`, `min`, or `src/eval/stg/parallel/`
  — `pp-fix` is concurrently rewriting those on `feat/furnace-pp-parallelism`.
  All new eucalypt lives in a **new file**, `lib/contract.eu`; `lib/prelude.eu`
  is **not modified at all** by this plan.
- UK English in all prose, comments and documentation.
- Gates, in order, before every commit: `cargo fmt --all`,
  `cargo clippy --workspace --all-targets -- -D warnings`, `cargo test`.
- Every `eu` invocation is wrapped in `timeout` and passes `--heap-limit-mib`,
  e.g. `timeout 60 ./target/debug/eu --heap-limit-mib 2048 -L lib file.eu`.
- Every harness test computes `RESULT` from its checks
  (`RESULT: if([t1, t2, …] all-true?, :PASS, :FAIL)`) so each assertion is in
  the verdict, following `tests/harness/189_r9oy_union_as_spec.eu`.
- Every regression test is **fault-injection verified**: break the code under
  test, confirm the harness test FAILs, restore, confirm it PASSes. The PR body
  states this was done, per test.
- Everything runs under the default bytecode engine **and** `EU_HEAPSYN=1`, and
  output must be byte-identical. GC soundness is exercised under
  `EU_GC_VERIFY=2`.
- Documentation snippets under `docs/` use ` ```eu,notest ` fences unless they
  are genuinely meant to execute — the doctest CI runs bare ` ```eu ` fences.
- The `t-*` projection is the **versioned** surface (ROADMAP §717–724). The
  change is additive: a new trailing element on `:t-record` only; consumers must
  tolerate its absence and read a two-element node as **closed**.

## File structure

| File | Status | Responsibility |
|---|---|---|
| `src/eval/stg/typedata.rs` | modify | emit the closedness boolean on `:t-record` |
| `lib/reflect.eu` | modify | hoist `type-str` to top level; render `, ..` for open records; render `:t-lit-str` |
| `lib/contract.eu` | **create** | `validate`, `ensure`, the reporting interpreter, path/rendering helpers |
| `src/driver/resources.rs` | modify | register `contract` as a baked-in resource |
| `src/eval/stg/contract.rs` | **create** | the `CONTRACT_FAIL` intrinsic |
| `src/eval/stg/mod.rs` | modify | declare + register the new intrinsic module |
| `src/eval/intrinsics.rs` | modify | intrinsic table entry for `CONTRACT_FAIL` |
| `src/eval/stg/support.rs` | modify | `call::bif::contract_fail` helper |
| `src/eval/error.rs` | modify | `ContractViolation` variant, formatter, smid, notes, `code()` |
| `src/driver/error_codes.rs` | modify | `EU-EVAL-CONTRACT` catalogue entry |
| `build.rs`, `xtask/src/main.rs` | modify | bump `BYTECODE_WIRE_FORMAT_VERSION` 5 → 6 |
| `tests/harness/19*_*.eu` | create | the harness suite (per-task, listed below) |
| `tests/harness/errors/0NN_*.eu(.expect)` | create | `ensure` failure + malformed-spec error tests |
| `docs/guide/type-checking.md` | modify | `:t-record` tag documentation |
| `docs/guide/contracts.md` | create | the guide chapter |
| `docs/reference/error-codes.md` | modify | `EU-EVAL-CONTRACT` entry |
| `CHANGELOG.md` | modify | Unreleased entry |

---

## Task 1: Closedness in the `t-record` projection

Spec §4. The projection currently destructures `Type::Record { fields, open: _,
rows: _ }` and emits a two-element node, so `s"{a: number}"` and
`s"{a: number, ..}"` are indistinguishable at runtime, and `from-data ∘ to-data`
loses the `..`.

**Files:**
- Modify: `src/eval/stg/typedata.rs:87-108`
- Modify: `lib/reflect.eu` (`record-str`, `to-str` `:t-record` arm)
- Test: `tests/harness/190_sv3_projection_closedness.eu`

**Interfaces:**
- Produces: `[:t-record, <fields block>, <open? bool>]` where
  `open? = open || !rows.is_empty()`. All later tasks read the flag with the
  arity-tolerant accessor `open-flag(rest)` defined in Task 3.
- Produces: `from-data` renders `{a: number, ..}` for an open record.

- [ ] **Step 1: Write the failing harness test**

Create `tests/harness/190_sv3_projection_closedness.eu`:

```eu
{ import: "reflect.eu"
  doc: "SV3 (eu-u9xj.1): the t-record projection carries closedness." }

closed: s"{a: number}" to-data
open: s"{a: number, ..}" to-data
rowvar: s"{a: number, ..r}" to-data

# A closed record projects a third element, `false`.
t1: (closed count) = 3
t2: (closed !! 2) = false

# `..` projects `true`.
t3: (open count) = 3
t4: (open !! 2) = true

# A named row variable collapses into the same boolean — no type
# variable leaks into runtime data.
t5: (rowvar !! 2) = true

# Nested records carry their own flag.
nested: s"{a: {b: number}}" to-data
inner: ((nested second).a) !! 2
t6: (inner !! 2) = false

# Round-trip: the `..` survives to-data → from-data (it did not before).
t7: (s"{a: number, ..}" to-data from-data str.of) = "{a: number, ..}"
t8: (s"{a: number}" to-data from-data str.of) = "{a: number}"

# Arity tolerance: a hand-written two-element node still renders, read as closed.
t9: ([:t-record, { a: [:t-field, :required, [:t-prim, :number]] }] from-data str.of) = "{a: number}"

RESULT: if([t1, t2, t3, t4, t5, t6, t7, t8, t9] all-true?, :PASS, :FAIL)
```

Note on `t6`: `nested second` is the fields block, `.a` is the `:t-field`
wrapper `[:t-field, :required, <record-node>]`, `!! 2` is the record node. The
outer `!! 2` then reads its closedness flag. Verify the exact shape with
`timeout 60 ./target/debug/eu --heap-limit-mib 2048 -L lib -e 's"{a: {b: number}}" to-data'`
before trusting the indices, and adjust if the shape differs.

- [ ] **Step 2: Register the test and run it to verify it fails**

The harness discovers files under `tests/harness/` via `tests/harness_test.rs`.
Check whether a per-file test function must be added:

```bash
grep -n "189_r9oy" tests/harness_test.rs
```

If a function exists for 189, add the matching one for 190 in the same style.

Run: `cargo test test_harness_190`
Expected: FAIL — `closed count` is 2, not 3.

- [ ] **Step 3: Emit the closedness flag from the projection**

In `src/eval/stg/typedata.rs`, replace the `Type::Record` arm
(currently lines 87–108) with:

```rust
        Type::Record { fields, open, rows } => {
            let field_exprs: Vec<(String, RcExpr)> = fields
                .iter()
                .map(|(k, fp)| {
                    let presence_sym = if fp.is_optional() {
                        "optional"
                    } else {
                        "required"
                    };
                    let inner = type_to_rcexpr(fp.ty());
                    let wrapper =
                        expr::core::list(s, vec![sym("t-field"), sym(presence_sym), inner]);
                    (k.clone(), wrapper)
                })
                .collect();
            let block_expr = expr::core::block(s, field_exprs);
            // A named row variable and a bare `..` mean the same thing to a
            // runtime consumer ("extra fields are permitted"), and projecting
            // rows faithfully would leak type variables into runtime data that
            // no runtime consumer can interpret. They collapse into one
            // boolean (SV3 design §4.2).
            let open_flag = *open || !rows.is_empty();
            let open_expr = if open_flag {
                expr::core::t(s)
            } else {
                expr::core::f(s)
            };
            expr::core::list(s, vec![sym("t-record"), block_expr, open_expr])
        }
```

Confirm the boolean constructors' names first:

```bash
grep -n "pub fn t(\|pub fn f(\|pub fn bool" src/core/expr.rs
```

If they are named differently (e.g. `expr::core::bool_(s, open_flag)`), use the
actual names — do not invent one.

- [ ] **Step 4: Render `, ..` and literal strings in `lib/reflect.eu`**

In `lib/reflect.eu`, replace `record-str` and the `:t-record` / `:t-lit-str`
handling inside `from-data`'s block:

```eu
    open-flag(rest): ((rest count) > 1) then(rest second, false)

    record-str(rec, open?):
      { body: rec elements map(field-str) str.join-on(", ")
        tail: open? then(", ..", "")
      }.((rec elements nil?) ∧ open? then("{{ .. }}", "{{ {body}{tail} }}"))
```

and in the `to-str` `cond`:

```eu
        , tag = :t-record  => record-str(r0, open-flag(rest))
        , tag = :t-lit-str => c"\"{r0}\""
```

`open-flag` is the arity guard demanded by spec §4.3: `rest` is the tail after
the `:t-record` tag, so a hand-written two-element node has
`rest count = 1` and reads as **closed**. The parentheses around
`(rest count) > 1` are required — comparison (50) binds tighter than
catenation (20).

The `:t-lit-str` arm is an addition beyond spec §5.2: without it, a literal
string type renders through the `"any"` fallthrough, which would make
`expected:` wrong for every violation involving one. It is recorded as a
departure at the end of this plan.

- [ ] **Step 5: Run the test to verify it passes**

Run: `cargo build && cargo test test_harness_190`
Expected: PASS.

Then check nothing else regressed on the projection:

Run: `cargo test`
Expected: all green. If a `to-data`/`from-data` test now sees three elements
where it asserted two, fix the *test* only if it was asserting the old lossy
behaviour; otherwise the projection change is wrong.

- [ ] **Step 6: Fault-injection verify**

Revert `open_flag` to a hardcoded `false` in `typedata.rs`; run
`cargo build && cargo test test_harness_190`; expect FAIL on `t4`/`t5`/`t7`.
Restore; expect PASS. Then revert the `open-flag` arity guard to
`rest second`; run again; expect FAIL on `t9`. Restore; expect PASS.
Record both results for the PR body.

- [ ] **Step 7: Update the tag documentation**

In `docs/guide/type-checking.md`, in the `to-data`/`from-data` section (the
`:t-record` worked example near line 1003–1011), update the example and add the
closedness note:

````markdown
```eu,notest
{ import: "reflect.eu" }
b: s"{ name: string }" to-data
# b => [:t-record, { name: [:t-field, :required, [:t-prim, :string]] }, false]
```

The third element of a `:t-record` node is its **closedness**: `false` for a
closed record (`{k: T}`), `true` for an open one (`{k: T, ..}` or
`{k: T, ..r}` — a named row variable collapses to the same boolean, so no type
variable ever appears in runtime data). The element is additive: a hand-written
two-element `[:t-record, {…}]` remains valid and is read as closed.
````

- [ ] **Step 8: Commit**

```bash
git add src/eval/stg/typedata.rs lib/reflect.eu tests/harness/190_sv3_projection_closedness.eu tests/harness_test.rs docs/guide/type-checking.md
git commit -m "feat(sv3): carry record closedness through the t-* projection (eu-u9xj.1)

Co-Authored-By: Claude <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01E7q98Ubgo4WKJbS6CWNPFz"
```

---

## Task 2: `reflect.type-str`

Spec §5. The canonical `t-*` renderer is a local binding inside `from-data`'s
block, so the reporting interpreter cannot reach it. Hoist it. Type-data values
do not render through `str.of` (verified: `str.of(s"[number]")` errors), so the
interpreter must render from the tagged list.

**Files:**
- Modify: `lib/reflect.eu`
- Test: `tests/harness/191_sv3_type_str.eu`

**Interfaces:**
- Produces: `type-str(td)` — top-level, exported: renders a `t-*` tagged list as
  its canonical type-DSL string. `from-data(td): td type-str __TYPE_FROM_STRING`.

- [ ] **Step 1: Write the failing harness test**

Create `tests/harness/191_sv3_type_str.eu`:

```eu
{ import: "reflect.eu"
  doc: "SV3 (eu-u9xj.1): type-str renders a t-* tagged list to type-DSL." }

t1: (s"number" to-data type-str) = "number"
t2: (s"[number]" to-data type-str) = "[number]"
t3: (s"{a: number}" to-data type-str) = "{a: number}"
t4: (s"{a: number, ..}" to-data type-str) = "{a: number, ..}"
t5: (s"number | string" to-data type-str) = "number | string"
t6: (s"(number, string)" to-data type-str) = "(number, string)"
t7: (s"number?" to-data type-str) = "number?"
t8: (s"{a?: number}" to-data type-str) = "{a?: number}"

# from-data is unchanged in behaviour — it is now type-str plus the wrap.
t9: (s"{a: number, ..}" to-data from-data str.of) = "{a: number, ..}"

RESULT: if([t1, t2, t3, t4, t5, t6, t7, t8, t9] all-true?, :PASS, :FAIL)
```

Before trusting the expected strings, print each one:

```bash
timeout 60 ./target/debug/eu --heap-limit-mib 2048 -L lib -e 's"(number, string)"'
```

and use the canonical form the `Type` `Display` impl actually produces.

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test test_harness_191`
Expected: FAIL — `unresolved variable 'type-str'`.

- [ ] **Step 3: Hoist the renderer**

Restructure `lib/reflect.eu` so the whole helper block becomes the body of a
new top-level `type-str`, and `from-data` is a one-liner over it:

```eu
` { doc: "`type-str(td)` - render a t-* tagged list as its canonical type-DSL string."
    type: s"any → string" }
type-str(td):
  { prim-names: {
        error: "ExecutionError"
      }

    prim-str(name-sym): prim-names lookup-or(name-sym, str.of(name-sym))

    needs-parens: set.from-list[:t-fn, :t-union, :t-partial]

    wrap(td, s): (td first) ∈ needs-parens then("({s})", s)

    fn-str(a, b): {
      a-r: wrap(a, a to-str)
      b-r: wrap(b, b to-str)
    }."{a-r} → {b-r}"

    field-str[k, fp]:
      { is-tf: (fp first) = :t-field
        is-opt: is-tf ∧ ((fp second) = :optional)
        ty: is-tf then(fp !! 2, fp) to-str
        q: is-opt then("?", "")
      }."{k}{q}: {ty}"

    open-flag(rest): ((rest count) > 1) then(rest second, false)

    record-str(rec, open?):
      { body: rec elements map(field-str) str.join-on(", ")
        tail: open? then(", ..", "")
      }.((rec elements nil?) ∧ open? then("{{ .. }}", "{{ {body}{tail} }}"))

    app-str(f, x):
      { x-str: to-str(x)
        f-str: to-str(f)
      }.(cond[
          f = [:t-con, :IO]       => "IO({x-str})"
        , f = [:t-con, :Dict]     => "Dict({x-str})"
        , f = [:t-con, :NonEmpty] => "NonEmpty([{x-str}])"
        , "{f-str} {x-str}"
        ])

    tuple-str(ts): ts map(to-str) str.join-on(", ") "({})"

    prefix-list-str(prefix, tail):
      { tail-str: to-str(tail)
      }.(prefix map(to-str) snoc("{tail-str}…") str.join-on(", ") "[{}]")

    to-str[tag : rest]:
      { r0: rest first
        r1: rest second
        s0: to-str(r0)
        s1: to-str(r1)
      }.(cond[
          tag = :t-prim    => prim-str(r0)
        , tag = :t-list    => "[{s0}]"
        , tag = :t-fn      => fn-str(r0, r1)
        , tag = :t-con     => "{r0}"
        , tag = :t-app     => app-str(r0, r1)
        , tag = :t-union   => rest map(to-str) str.join-on(" | ")
        , tag = :t-partial => "{s0}?"
        , tag = :t-tuple   => tuple-str(rest)
        , tag = :t-prefix-list => prefix-list-str(r0, r1)
        , tag = :t-record  => record-str(r0, open-flag(rest))
        , tag = :t-var     => "{r0}"
        , tag = :t-lit-sym => ":{r0}"
        , tag = :t-lit-str => c"\"{r0}\""
        , tag = :t-mu      => "{r0}"
        , tag = :t-forall  => "forall {r0}. {s1}"
        , tag = :t-lam     => "λ{r0}. {s1}"
        , "any"
        ])
  }.(to-str(td))

` { doc: "`from-data(td)` - construct a type-data value from a t-* tagged list."
    type: s"any → any" }
from-data(td): td type-str __TYPE_FROM_STRING
```

This is Task 1's `record-str`/`open-flag`/`:t-lit-str` edits landing in their
final home — if Task 1 already applied them to the old inline block, they move
here verbatim.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test test_harness_191 && cargo test test_harness_190 && cargo test`
Expected: all PASS. `from-data`'s observable behaviour is unchanged apart from
the two fixes (`, ..` and literal strings), so every existing reflect test must
still pass untouched.

- [ ] **Step 5: Fault-injection verify**

Change `record-str`'s `tail` to always `""`; run `cargo test test_harness_191`;
expect FAIL on `t4`/`t9`. Restore; expect PASS.

- [ ] **Step 6: Document `type-str`**

In `docs/guide/type-checking.md`, immediately after the `to-data`/`from-data`
subsection, add:

````markdown
`type-str(td)` renders a `t-*` tagged list back to its canonical type-DSL
string without wrapping it as a type-data value — useful when you are already
walking the tagged list and want to name the type you are looking at:

```eu,notest
{ import: "reflect.eu" }
s: s"{ name: string, ..}" to-data type-str
# s => "{name: string, ..}"
```

`from-data` is exactly `type-str` followed by the type-data wrap.
````

- [ ] **Step 7: Commit**

```bash
git add lib/reflect.eu tests/harness/191_sv3_type_str.eu tests/harness_test.rs docs/guide/type-checking.md
git commit -m "feat(sv3): expose reflect.type-str as a top-level renderer (eu-u9xj.1)

Co-Authored-By: Claude <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01E7q98Ubgo4WKJbS6CWNPFz"
```

---

## Task 3: `lib/contract.eu` — `validate`

Spec §6. The reporting interpreter. This is the bulk of the feature.

**Files:**
- Create: `lib/contract.eu`
- Modify: `src/driver/resources.rs`
- Test: `tests/harness/192_sv3_validate.eu`

**Interfaces:**
- Produces: `validate(spec, data) -> [violation]`. Receiver last, so
  `data validate(spec)` reads correctly. Never raises for any *data*; raises
  `panic("validate: not a type spec")` for a malformed **spec**.
- Produces: violation entries `{ path, kind, expected, found }` with
  `kind ∈ {:type-mismatch, :missing, :unexpected, :length}`.
- Produces (internal, consumed by Task 5): `render-violation(v) -> string`,
  `headline(spec, violations) -> string`.

- [ ] **Step 1: Register the resource**

In `src/driver/resources.rs`, inside `Default::default`, after the `reflect`
entry:

```rust
        content.insert(
            "contract".to_string(),
            String::from_utf8(include_bytes!("../../lib/contract.eu").to_vec())
                .expect("contract.eu is valid UTF-8"),
        );
```

`read_fs_input` (`src/driver/source.rs:1199`) falls back to the baked-in
resource keyed by the filename **stem**, so `{ import: "contract.eu" }` resolves
with no `-L` flag once this entry exists.

- [ ] **Step 2: Write the failing harness test**

Create `tests/harness/192_sv3_validate.eu`. Note `` ` :suppress `` on the data
bindings — a target's other bindings are rendered, and a block built with
`block([[…]])` deliberately holds an unforced `panic` thunk.

```eu
{ import: "contract.eu"
  doc: "SV3 (eu-u9xj.1): validate reports located violations." }

# ── conformance: [] means conformant ────────────────────────────────────

t1: ({ a: 1 } validate(s"{a: number}")) nil?
t2: ([1, 2, 3] validate(s"[number]")) nil?
t3: ("x" validate(s"string")) nil?
t4: ({ a: 1, b: "x" } validate(s"{a: number, ..}")) nil?
t5: ({ host: "x" } validate(s"{host: string, port?: number}")) nil?
t6: ({ host: "x", port: 80 } validate(s"{host: string, port?: number}")) nil?
t7: ([1, "x"] validate(s"(number, string)")) nil?
t8: (42 validate(s"number | string")) nil?
t9: (null validate(s"number?")) nil?
t10: ({ a: { b: 1 } } validate(s"{a: {b: number}}")) nil?

# ── :type-mismatch ──────────────────────────────────────────────────────

` :suppress
m: { a: "wrong" } validate(s"{a: number}")

t11: (m count) = 1
t12: (m first).path = "a"
t13: (m first).kind = :type-mismatch
t14: (m first).expected = "number"
t15: (m first).found = "string"

# ── :missing ────────────────────────────────────────────────────────────

` :suppress
mi: { } validate(s"{a: number}")

t16: (mi count) = 1
t17: (mi first).path = ""
t18: (mi first).kind = :missing
t19: (mi first).expected = "a"
t20: (mi first).found = :absent

# ── :unexpected (closed only) ───────────────────────────────────────────

` :suppress
un: { a: 1, b: 2 } validate(s"{a: number}")

t21: (un count) = 1
t22: (un first).kind = :unexpected
t23: (un first).expected = :closed
t24: (un first).found = [:b]

# The same data against an open spec has no :unexpected entry.
t25: ({ a: 1, b: 2 } validate(s"{a: number, ..}")) nil?
t26: ({ a: 1, b: 2 } validate(s"{a: number, ..r}")) nil?

# ── :length ─────────────────────────────────────────────────────────────

` :suppress
ln: [1] validate(s"(number, string)")

t27: (ln count) = 1
t28: (ln first).kind = :length
t29: (ln first).expected = 2
t30: (ln first).found = 1

# ── accumulation: three independent faults, three entries ───────────────

` :suppress
acc: { a: "x", c: 1 } validate(s"{a: number, b: string}")

t31: (acc count) = 3
t32: (acc map(_.kind)) = [:type-mismatch, :missing, :unexpected]

# ── paths ───────────────────────────────────────────────────────────────

` :suppress
deep: { servers: [{ host: "a", port: 1 }, { host: "b", port: "no" }] }
       validate(s"{servers: [{host: string, port: number}]}")

t33: (deep count) = 1
t34: (deep first).path = "servers[1].port"

` :suppress
quoted: block([[:'my key', 1]]) validate(s"{'my key': string}")

t35: (quoted first).path = "'my key'"

# ── ordering (spec order = lexicographic by key; fields then unexpected) ─

t36: (acc first).path = "a"
t37: (acc second).expected = "b"

# ── unions report one entry, not one per branch ─────────────────────────

` :suppress
u: :sym validate(s"number | string")
t38: (u count) = 1
t39: (u first).expected = "number | string"

# ── a container mismatch terminates the subtree ─────────────────────────

` :suppress
notblk: "hello" validate(s"{a: number, b: number}")
t40: (notblk count) = 1
t41: (notblk first).kind = :type-mismatch
t42: (notblk first).found = "string"

# ── as-spec agreement ───────────────────────────────────────────────────

agrees(spec, v): ((v validate(spec)) nil?) = (v match?(spec as-spec))
t43: agrees(s"{a: number, ..}", { a: 1 })
t44: agrees(s"{a: number, ..}", { a: "x" })
t45: agrees(s"[number]", [1, 2])
t46: agrees(s"[number]", [1, "x"])
t47: agrees(s"number | string", 42)
t48: agrees(s"number | string", :sym)
t49: agrees(s"(number, string)", [1, "x"])
t50: agrees(s"number?", null)

RESULT: if([t1, t2, t3, t4, t5, t6, t7, t8, t9, t10,
            t11, t12, t13, t14, t15, t16, t17, t18, t19, t20,
            t21, t22, t23, t24, t25, t26, t27, t28, t29, t30,
            t31, t32, t33, t34, t35, t36, t37, t38, t39, t40,
            t41, t42, t43, t44, t45, t46, t47, t48, t49, t50] all-true?,
           :PASS, :FAIL)
```

Note on `t44`: `as-spec` matches **open**, so an agreement corpus must use open
record specs — a closed spec legitimately disagrees with `match?` on surplus
keys, which is the whole point of Component 1. Only open/non-record specs belong
in the agreement corpus.

- [ ] **Step 3: Run the test to verify it fails**

Run: `cargo test test_harness_192`
Expected: FAIL — `contract.eu` does not exist.

- [ ] **Step 4: Write `lib/contract.eu`**

```eu
{ requires: ">=0.14"
  import: "reflect.eu"
  doc: "Structural contracts: validate and ensure data against s-string type specs.

`validate(spec, data)` walks the spec and the value together and returns a list
of violations, each carrying a path. `ensure(spec, data)` returns the data
unchanged when it conforms and raises otherwise. Only the paths the spec names
are forced." }

_ : eu.requires(">=0.14")

##
## Violation entries
##

` :internal
violation(p, k, e, f): { path: p, kind: k, expected: e, found: f }

##
## Runtime type names — the vocabulary used in `found`
##

` { doc: "`type-name(v)` - the runtime type name of `v`, as used in violation reports."
    type: s"any → string" }
type-name(v):
  if(__SATURATED(v) not, "function",
  if(v null?,      "null",
  if(v number?,    "number",
  if(v string?,    "string",
  if(v symbol?,    "symbol",
  if(v bool?,      "bool",
  if(v datetime?,  "datetime",
  if(v list?,      "list",
  if(v block?,     "block",
  if(v type-data?, "type-data",
                   "unknown"))))))))))

##
## Paths
##

` :internal
ident-key?(k): str.of(k) str.matches?("^[a-zA-Z][a-zA-Z0-9_*!?-]*$")

` :internal
key-step(k): { s: str.of(k) }.(ident-key?(k) then(s, "'{s}'"))

` :internal
join-key(path, k): { step: key-step(k) }.((path = "") then(step, "{path}.{step}"))

` :internal
join-index(path, i): "{path}[{i}]"

##
## The reporting interpreter
##

` :internal
prim-preds: {
  number:   number?
  string:   string?
  symbol:   symbol?
  bool:     bool?
  null:     null?
  datetime: datetime?
}

` :internal
spec-shaped?(td): (td list?) ∧ (td non-nil?) ∧ ((td first) symbol?)

` { doc: "`validate(spec, data)` - check `data` against type spec `spec`, returning a list of violations. An empty list means conformant. Never raises for any data; raises only when `spec` is not a type spec. Forces only the paths the spec names."
    type: s"any → any → [{{path: string, kind: symbol, expected: any, found: any}}]" }
validate(spec, data):
  { td: type-data?(spec) then(__TYPE_TO_DATA(spec), spec)

    open-flag(rest): ((rest count) > 1) then(rest second, false)

    mismatch(td, path, v): [violation(path, :type-mismatch, td type-str, type-name(v))]

    prim-check(td, name, path, v):
      if(name ∈ accepting-prims, [],
      if(name = :never, [violation(path, :type-mismatch, "never", type-name(v))],
         { p: prim-preds lookup-or(name, any?) }.(p(v) then([], mismatch(td, path, v)))))

    accepting-prims: set.from-list[:any, :top, :error, :set, :vec, :array]

    list-check(td, inner, path, v):
      if(v list? not, mismatch(td, path, v),
         { idx: range(0, v count)
           elem(i, e): walk(inner, join-index(path, i), e)
         }.(zip-with(elem, idx, v) concat))

    tuple-check(td, ts, path, v):
      if(v list? not, mismatch(td, path, v),
         { n: ts count
           m: v count
           idx: range(0, n)
           elem(i, e): walk(nth(i, ts), join-index(path, i), e)
         }.(if(n != m,
               [violation(path, :length, n, m)],
               zip-with(elem, idx, v) concat)))

    prefix-check(td, prefix, tail, path, v):
      if(v list? not, mismatch(td, path, v),
         { n: prefix count
           m: v count
           idx: range(0, n)
           head-elem(i, e): walk(nth(i, prefix), join-index(path, i), e)
           tail-idx: range(n, m)
           tail-elem(i, e): walk(tail, join-index(path, i), e)
         }.(if(m < n,
               [violation(path, :length, n, m)],
               (zip-with(head-elem, idx, v take(n)) concat)
                 ++ (zip-with(tail-elem, tail-idx, v drop(n)) concat))))

    field-check(v, path, [k, fp]):
      { is-tf: (fp first) = :t-field
        is-opt: is-tf ∧ ((fp second) = :optional)
        inner: is-tf then(fp !! 2, fp)
      }.(if(v has(k),
            walk(inner, join-key(path, k), v lookup(k)),
            if(is-opt, [], [violation(path, :missing, str.of(k), :absent)])))

    record-check(td, fields, open?, path, v):
      if(v block? not, mismatch(td, path, v),
         { spec-keys: fields keys set.from-list
           extra: v keys filter(surplus?)
           surplus?(k): (k ∈ spec-keys) not
           field-vs: fields elements mapcat(field-check(v, path))
           extras: (open? ∨ (extra nil?)) then([], [violation(path, :unexpected, :closed, extra)])
         }.(field-vs ++ extras))

    union-check(td, ts, path, v):
      { branch-ok?(b): walk(b, path, v) nil?
      }.(ts any(branch-ok?) then([], mismatch(td, path, v)))

    partial-check(inner, path, v): v null? then([], walk(inner, path, v))

    fn-check(td, path, v): (__SATURATED(v) not) then([], mismatch(td, path, v))

    lit-check(td, lit, path, v): (v = lit) then([], mismatch(td, path, v))

    walk[tag : rest]:
      { r0: rest first
        r1: rest second
      }.({ p: •  v: • }.(cond[
            tag = :t-prim        => prim-check([tag] ++ rest, r0, p, v)
          , tag = :t-list        => list-check([tag] ++ rest, r0, p, v)
          , tag = :t-tuple       => tuple-check([tag] ++ rest, rest, p, v)
          , tag = :t-prefix-list => prefix-check([tag] ++ rest, r0, r1, p, v)
          , tag = :t-record      => record-check([tag] ++ rest, r0, open-flag(rest), p, v)
          , tag = :t-union       => union-check([tag] ++ rest, rest, p, v)
          , tag = :t-partial     => partial-check(r0, p, v)
          , tag = :t-fn          => fn-check([tag] ++ rest, p, v)
          , tag = :t-lit-str     => lit-check([tag] ++ rest, r0, p, v)
          , tag = :t-lit-sym     => lit-check([tag] ++ rest, r0, p, v)
          , tag = :t-forall      => walk(r1, p, v)
          , []
          ]))

  }.(spec-shaped?(td) then(walk(td, "", data),
                           panic("validate: not a type spec")))
```

**Implementation warning — the `walk` signature.** The sketch above uses a
cons-pattern head (`walk[tag : rest]`) *and* two further parameters, then reaches
for block anaphora to bind them. That combination is fragile: block anaphora
cannot be nested and its scoping is easy to get wrong. Write `walk` with
**plain named parameters** instead and destructure inside:

```eu
    walk(td, path, v):
      { tag: td first
        rest: td tail
        r0: rest first
        r1: rest second
      }.(cond[
            tag = :t-prim        => prim-check(td, r0, path, v)
          , tag = :t-list        => list-check(td, r0, path, v)
          , tag = :t-tuple       => tuple-check(td, rest, path, v)
          , tag = :t-prefix-list => prefix-check(td, r0, r1, path, v)
          , tag = :t-record      => record-check(td, r0, open-flag(rest), path, v)
          , tag = :t-union       => union-check(td, rest, path, v)
          , tag = :t-partial     => partial-check(r0, path, v)
          , tag = :t-fn          => fn-check(td, path, v)
          , tag = :t-lit-str     => lit-check(td, r0, path, v)
          , tag = :t-lit-sym     => lit-check(td, r0, path, v)
          , tag = :t-forall      => walk(r1, path, v)
          , []
          ])
```

This is the version to implement — it keeps `td` intact for `type-str` and
avoids reconstructing `[tag] ++ rest` at every call site.

Points that will bite if written differently:

- `mapcat(field-check(v, path))` catenates the element as the **last**
  argument, which is why `field-check` is `(v, path, [k, fp])` and not
  `([k, fp], v, path)`.
- `(open? ∨ (extra nil?)) then([], …)` — `then` takes the condition **last**,
  and the parentheses are required because `∨` (30) binds tighter than
  catenation (20) but `then` would otherwise catenate onto the wrong operand.
- `surplus?` is referenced by `extra` before its own declaration; block bindings
  are mutually recursive so this is fine, but it must not be named `filter`-like
  anything that shadows a prelude name.
- `n != m` inside a `cond`/`if` argument: `!=` is precedence 40, well above
  catenation, so no parentheses are needed there.
- Do **not** use `{x: •}` pseudo-lambdas anywhere; use named helpers, as above.
- `set.from-list[...]` is juxtaposed-call syntax (no space before `[`).
  Verify `set.from-list` and `∈` exist:
  `grep -n "from-list\|(x ∈ " lib/prelude.eu`. If `∈` is not defined for sets,
  use `spec-keys has-member(k)` or fall back to
  `(spec-keys-list any(= k)) not` — do not invent an operator.

- [ ] **Step 5: Iterate until the test passes**

Run: `cargo build && cargo test test_harness_192`

When a check fails, debug the *eucalypt*, not the Rust: write a scratch
`probe.eu` in the worktree root and run

```bash
timeout 60 ./target/debug/eu --heap-limit-mib 2048 -L lib probe.eu
```

to print `data validate(spec)` directly and compare with the expectation. Delete
`probe.eu` before committing.

Expected: PASS.

- [ ] **Step 6: Fault-injection verify**

Run each of these, confirming FAIL then PASS after restoring:

1. Make `record-check` ignore `open?` (always emit `:unexpected`) → `t25`/`t26`
   fail.
2. Make `field-check` emit `:missing` for optional fields too → `t5` fails.
3. Make `union-check` emit one violation per branch → `t38` fails.
4. Make `join-index` produce `.{i}` instead of `[{i}]` → `t34` fails.
5. Make `record-check` descend into a non-block value → `t40` fails.

Record all five for the PR body.

- [ ] **Step 7: Commit**

```bash
git add lib/contract.eu src/driver/resources.rs tests/harness/192_sv3_validate.eu tests/harness_test.rs
git commit -m "feat(sv3): lib/contract.eu with the validate reporting interpreter (eu-u9xj.1)

Co-Authored-By: Claude <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01E7q98Ubgo4WKJbS6CWNPFz"
```

---

## Task 4: Spec-directed forcing — the executable statement of §6.6

Spec §6.6 and testing-strategy row "forcing". The claim is that `validate`
forces **only** the paths the spec names. A test that would blow up if an
unmentioned subtree were forced is the only honest way to gate it.

**Two facts established empirically on this branch, which the test must respect:**

1. Demand analysis makes a **block literal**'s field values effectively strict —
   `{ a: 1, b: panic("X") } block?` raises. A bomb cannot be planted with a block
   literal.
2. `block([[:a, 1], [:b, panic("X")]])` **is** lazy in its values —
   `keys`, `has`, `lookup` of another key, and `block?` all succeed. This is the
   construction the test must use.

Verify both again before writing the test, since demand analysis may change.

**Files:**
- Test: `tests/harness/193_sv3_forcing_discipline.eu`

- [ ] **Step 1: Write the test**

Create `tests/harness/193_sv3_forcing_discipline.eu`:

```eu
{ import: "contract.eu"
  doc: "SV3 (eu-u9xj.1): validate forces only the paths the spec names.

Each check below would raise (bomb) or diverge (infinite list) if the
interpreter forced a subtree the spec does not name. A regression in the
forcing discipline turns this file from PASS into a hard error, which the
harness reports as a failure." }

# A block whose `b` value raises when forced. Built with `block(...)` rather
# than a block literal because demand analysis makes literal fields strict.
` :suppress
bomb-blk: block([[:a, 1], [:b, panic("SV3: unmentioned subtree was forced")]])

# `{}` names nothing: the spine is enumerated, no value is forced.
t1: (bomb-blk validate(s"{ .. }")) nil?

# An open spec naming only `a` forces exactly `a`.
t2: (bomb-blk validate(s"{a: number, ..}")) nil?

# A CLOSED spec still only enumerates keys — closedness is a key-set
# comparison, never a value forcing. `b` is surplus, so one :unexpected
# entry is reported, and its `found` is the key symbol, not the value.
` :suppress
closed-r: bomb-blk validate(s"{a: number}")
t3: (closed-r count) = 1
t4: (closed-r first).kind = :unexpected
t5: (closed-r first).found = [:b]

# `any` and `top` accept without forcing at all.
t6: (bomb-blk validate(s"{a: any, b: any}")) nil?
t7: (bomb-blk validate(s"{a: top, b: top}")) nil?

# A type variable is unconstrained at runtime and forces nothing.
t8: (bomb-blk validate(s"{a: number, b: r}")) nil?

# An infinite list under an unnamed key: WHNF is cheap, the spine is not.
# A spec that does not name it must not walk the spine.
` :suppress
inf-blk: block([[:a, 1], [:b, ints-from(1)]])
t9: (inf-blk validate(s"{a: number, ..}")) nil?

# A tuple spec that stops before a bomb element leaves it untouched.
` :suppress
bomb-list: [1, panic("SV3: list element beyond the prefix was forced")] take(1)
t10: (bomb-list validate(s"(number)")) nil?

RESULT: if([t1, t2, t3, t4, t5, t6, t7, t8, t9, t10] all-true?, :PASS, :FAIL)
```

Before committing, verify each bomb construction really stays lazy with the
*current* `validate`, and that flipping the spec to name the bomb genuinely
raises — i.e. that the bomb is live. Check the live-bomb direction in a scratch
file (not in the harness test, which must pass):

```bash
timeout 60 ./target/debug/eu --heap-limit-mib 2048 -L lib -e \
  'block([[:a, 1], [:b, panic("LIVE")]]) validate(s"{a: number, b: number}")'
```

Expected: `error: panic: LIVE`. If this does **not** raise, the bomb is inert and
the test gates nothing — fix the construction before proceeding.

If `t10`'s `take(1)` turns out to force the second element, drop `t10` rather
than weaken it; `t1`–`t9` already gate the record and list cases.

- [ ] **Step 2: Run it**

Run: `cargo test test_harness_193`
Expected: PASS.

- [ ] **Step 3: Fault-injection verify**

In `lib/contract.eu`, change `record-check` so that it forces every value —
e.g. add `all-forced: v values map(type-name)` to `record-check`'s block and
include it in the returned expression via
`(all-forced count) > -1 then(field-vs ++ extras, [])`. Rebuild and run
`cargo test test_harness_193`; expect a hard failure with
`panic: SV3: unmentioned subtree was forced`. Restore; expect PASS. Record this
for the PR body — it is the single most important fault injection in this PR.

- [ ] **Step 4: Commit**

```bash
git add tests/harness/193_sv3_forcing_discipline.eu tests/harness_test.rs
git commit -m "test(sv3): gate the spec-directed forcing discipline (eu-u9xj.1)

Co-Authored-By: Claude <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01E7q98Ubgo4WKJbS6CWNPFz"
```

---

## Task 5: `ContractViolation`, `CONTRACT_FAIL`, `ensure`

Spec §7. The strict wrapper and its error variant.

**Files:**
- Modify: `src/eval/intrinsics.rs`
- Create: `src/eval/stg/contract.rs`
- Modify: `src/eval/stg/mod.rs`, `src/eval/stg/support.rs`, `src/eval/error.rs`
- Modify: `src/driver/error_codes.rs`, `docs/reference/error-codes.md`
- Modify: `build.rs`, `xtask/src/main.rs`
- Modify: `lib/contract.eu`
- Test: `tests/harness/194_sv3_ensure.eu`,
  `tests/harness/errors/0NN_contract_violation.eu(.expect)`,
  `tests/harness/errors/0NN_contract_bad_spec.eu(.expect)`

**Interfaces:**
- Consumes: `validate` and `render-violation`/`headline` from Task 3.
- Produces: `ensure(spec, data) -> data` (unchanged) or raises.
- Produces: `ExecutionError::ContractViolation(Smid, Box<(String, Vec<String>)>)`
  with code `EU-EVAL-CONTRACT`.
- Produces: `__CONTRACT_FAIL(headline: string, lines: [string]) -> never`.

- [ ] **Step 1: Add the intrinsic table entry**

At the **end** of the `INTRINSICS` list in `src/eval/intrinsics.rs`, after
`ISZDT` (index 194):

```rust
    Intrinsic { // 195
            name: "CONTRACT_FAIL",
            ty: function(vec![str_(), list(), unk()]).unwrap(),
            strict: vec![0, 1],
    },
```

Appending keeps every existing index stable.

- [ ] **Step 2: Bump the bytecode wire-format version**

Adding an intrinsic changes `INTRINSIC_COUNT`, and `lib/prelude.blob` stores
global slots as `INTRINSIC_COUNT + prelude slot` (`src/eval/stg/blob.rs:48-51`).
The blob's freshness check is `SHA-256(lib/prelude.eu ‖ BYTECODE_WIRE_FORMAT_VERSION)`
— and this plan does **not** modify `lib/prelude.eu`, so without a version bump
an existing blob would be judged fresh while its slot numbering is off by one.
Bump it.

In `build.rs`, add to the doc comment above `BYTECODE_WIRE_FORMAT_VERSION` and
change the constant:

```rust
/// - v6: intrinsic table grew (`CONTRACT_FAIL`, eu-u9xj.1), shifting the
///   `INTRINSIC_COUNT + prelude slot` global numbering the blob's
///   `global_forms`/`Ref::G` values are baked against. Not a byte-layout
///   change, but a stale blob would otherwise pass the source-hash check with
///   an off-by-one slot map.
const BYTECODE_WIRE_FORMAT_VERSION: u32 = 6;
```

Make the identical change in `xtask/src/main.rs` — the two constants must match
(`grep -n "BYTECODE_WIRE_FORMAT_VERSION" xtask/src/main.rs`).

- [ ] **Step 3: Write the intrinsic**

Create `src/eval/stg/contract.rs`:

```rust
//! The CONTRACT_FAIL intrinsic — raises a structural-contract violation.
//!
//! `lib/contract.eu`'s `ensure` renders each violation to a line and calls
//! `__CONTRACT_FAIL(headline, lines)`. The marshalling boundary between the
//! eucalypt report and the Rust error variant is deliberately **strings**
//! (SV3 design §7.3): the presentation lives in eucalypt where it is
//! readable, testable by harness test, and changeable without touching Rust,
//! and the Rust side stays a dumb carrier. The structured report remains
//! available from `validate`.

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::intrinsic::{CallGlobal2, IntrinsicMachine, StgIntrinsic},
        memory::{mutator::MutatorHeapView, syntax::Ref},
    },
};

use super::{
    force::SeqStrList,
    support::{call, str_arg, str_list_arg},
    syntax::{
        dsl::{force, lambda, local, lref, unbox_str},
        LambdaForm,
    },
};

/// CONTRACT_FAIL(headline, lines)
///
/// Always raises `ExecutionError::ContractViolation`. The `Smid` is the
/// machine's current annotation; `to_diagnostic`'s location selection then
/// promotes the nearest user-file frame — the `ensure` call site — over this
/// library frame. A plain `lambda` (not `annotated_lambda`) is used
/// deliberately so the call-site annotation survives.
pub struct ContractFail;

impl StgIntrinsic for ContractFail {
    fn name(&self) -> &str {
        "CONTRACT_FAIL"
    }

    fn wrapper(&self, _annotation: Smid) -> LambdaForm {
        lambda(
            2, // [headline lines]
            force(
                SeqStrList.global(lref(1)),
                // [seqlines] [headline lines]
                unbox_str(
                    local(1),
                    // [unboxed-headline] [seqlines] [headline lines]
                    call::bif::contract_fail(lref(0), lref(1)),
                ),
            ),
        )
    }

    fn execute(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'_>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let headline = str_arg(machine, view, &args[0])?;
        let lines = str_list_arg(machine, view, args[1].clone())?;
        Err(ExecutionError::ContractViolation(
            machine.annotation(),
            Box::new((headline, lines)),
        ))
    }
}

impl CallGlobal2 for ContractFail {}
```

The wrapper mirrors `string::Join`'s (`src/eval/stg/string.rs:150-165`), which
is the established pattern for an intrinsic taking a string list: `SeqStrList`
forces and unboxes the spine, `unbox_str` handles the scalar. Index arithmetic:
inside `force`'s body the forced value is `local(0)` and the original arguments
shift by one, so `headline` is `local(1)`; inside `unbox_str`'s body the
unboxed string is `local(0)` and the seq'd list is `local(1)`.

Add to `src/eval/stg/support.rs`, inside `pub mod call { pub mod bif {`:

```rust
        pub fn contract_fail(headline: Ref, lines: Ref) -> Rc<StgSyn> {
            call_bif("CONTRACT_FAIL", &[headline, lines])
        }
```

Add to `src/eval/stg/mod.rs`: `pub mod contract;` alongside `pub mod constant;`,
and `rt.add(Box::new(contract::ContractFail));` next to
`rt.add(Box::new(panic::Panic));`.

- [ ] **Step 4: Add the error variant**

In `src/eval/error.rs`, add next to `UserPanic`:

```rust
    /// A structural contract (`ensure`) rejected its data.
    ///
    /// Carries the `ensure` call site, a headline (the type the data failed
    /// plus the violation count) and one rendered line per violation. The
    /// lines become diagnostic notes.
    #[error("{}", format_contract_violation(&.1.0, &.1.1))]
    ContractViolation(Smid, Box<(String, Vec<String>)>),
```

and the formatter next to the other `format_*` helpers:

```rust
/// Format a contract violation's headline for the diagnostic message. The
/// per-violation lines are attached separately, as notes.
fn format_contract_violation(headline: &str, violations: &[String]) -> String {
    let _ = violations;
    format!("contract violation: {headline}")
}
```

Add to the `smid()` match: `ExecutionError::ContractViolation(s, _) => *s,`.

Add to `code()`:

```rust
            ExecutionError::ContractViolation(..) => Some("EU-EVAL-CONTRACT"),
```

Add to the notes `match inner` in `to_diagnostic`:

```rust
            ExecutionError::ContractViolation(_, detail) => detail.1.clone(),
```

- [ ] **Step 5: Catalogue the code**

Add to `src/driver/error_codes.rs`'s `CATALOGUE` an entry for
`EU-EVAL-CONTRACT` in the same shape as the `EU-EVAL-TYPE` entry (read that
entry first and mirror its field names exactly).

Add to `docs/reference/error-codes.md`, after the `EU-EVAL-TYPE` section:

````markdown
### `EU-EVAL-CONTRACT`

**What it means:** data failed a structural contract applied with `ensure`.
The contract is an ordinary type spec written as an `s"…"` literal, and the
diagnostic's notes list every position that did not conform, each with a path
into the data.

**Example:**

```eucalypt
{ import: "contract.eu" }
schema: s"{ name: string, port: number }"
config: { name: "web", port: "8080" } ensure(schema)
```

```text
error[EU-EVAL-CONTRACT]: contract violation: 1 violation against {name: string, port: number}
  ┌─ example.eu:3:39
  │
3 │ config: { name: "web", port: "8080" } ensure(schema)
  │                                       ^^^^^^^^^^^^^^
  │
  = port: expected number, found string
```

**How to fix it:** correct the data at each path the notes name, or widen the
spec if the shape you are receiving is the shape you intended. To inspect the
violations as data rather than aborting, call `validate(spec, data)` — it
returns the same information as a list of blocks and never raises.
````

- [ ] **Step 6: Write `ensure` and the renderers in `lib/contract.eu`**

Append to `lib/contract.eu`:

```eu
##
## Rendering
##

` :internal
render-path(p): (p = "") then("(root)", p)

` :internal
render-keys(ks): ks map(str.of) str.join-on(", ")

` :internal
render-violation(v):
  { p: render-path(v.path)
  }.(cond[
       v.kind = :type-mismatch => "{p}: expected {v.expected}, found {v.found}"
     , v.kind = :missing       => "{p}: missing required field '{v.expected}'"
     , v.kind = :unexpected    => "{p}: unexpected keys: {render-keys(v.found)}"
     , v.kind = :length        => "{p}: expected {v.expected} elements, found {v.found}"
     , "{p}: does not conform"
     ])

` :internal
headline(td, vs):
  { n: vs count
    noun: (n = 1) then("violation", "violations")
  }."{n} {noun} against {td type-str}"

##
## ensure
##

` { doc: "`ensure(spec, data)` - return `data` unchanged if it conforms to type spec `spec`, otherwise raise a contract violation listing every non-conforming position."
    type: s"any → a → a" }
ensure(spec, data):
  { td: type-data?(spec) then(__TYPE_TO_DATA(spec), spec)
    violations: data validate(spec)
  }.((violations nil?)
       then(data,
            __CONTRACT_FAIL(headline(td, violations),
                            violations map(render-violation))))
```

`then` is lazy in its branches, so `__CONTRACT_FAIL` is not evaluated on the
success path: `ensure` on conforming data costs exactly one `validate`. The
parentheses around `violations nil?` are required.

- [ ] **Step 7: Write the `ensure` harness test**

Create `tests/harness/194_sv3_ensure.eu`:

```eu
{ import: "contract.eu"
  doc: "SV3 (eu-u9xj.1): ensure returns conforming data unchanged." }

schema: s"{ name: string, port: number, tags?: [string] }"

` :suppress
good: { name: "web", port: 8080 }

# `ensure` returns the data unchanged, so it drops into a pipeline.
t1: (good ensure(schema)) = good
t2: (good ensure(schema)).port = 8080

# Optional field absent is conformant.
t3: ({ name: "web", port: 1 } ensure(schema)).name = "web"

# Optional field present and correct is conformant.
t4: ({ name: "web", port: 1, tags: ["a"] } ensure(schema)).tags = ["a"]

# ensure inherits validate's forcing discipline: an unnamed subtree of an
# open spec is never forced, so this returns rather than raising.
` :suppress
bomb-blk: block([[:name, "web"], [:extra, panic("SV3: ensure forced an unnamed subtree")]])
t5: (bomb-blk ensure(s"{name: string, ..}")).name = "web"

# Rendering helpers produce the lines the diagnostic shows as notes.
t6: (render-violation({ path: "a.b", kind: :type-mismatch, expected: "number", found: "string" }))
      = "a.b: expected number, found string"
t7: (render-violation({ path: "", kind: :unexpected, expected: :closed, found: [:x, :y] }))
      = "(root): unexpected keys: x, y"

RESULT: if([t1, t2, t3, t4, t5, t6, t7] all-true?, :PASS, :FAIL)
```

`render-violation` is declared `` ` :internal ``, which makes it invisible to
importers. If `t6`/`t7` cannot see it, either drop the `:internal` marker from
`render-violation` (making it part of the library's surface — acceptable, it is
a useful function) or drop `t6`/`t7`. Prefer dropping `:internal`, and document
`render-violation` accordingly.

- [ ] **Step 8: Write the error tests**

Find the next free number: `ls tests/harness/errors/ | tail -5`.

`tests/harness/errors/0NN_contract_violation.eu`:

```eu
{ import: "contract.eu" }
schema: s"{ name: string, port: number }"
config: { name: "web", port: "8080" } ensure(schema)
```

`tests/harness/errors/0NN_contract_violation.eu.expect`:

```
exit: 1
stderr: "EU-EVAL-CONTRACT.*contract violation"
```

`tests/harness/errors/0NN_contract_bad_spec.eu`:

```eu
{ import: "contract.eu" }
result: { a: 1 } validate({ not: "a spec" })
```

`tests/harness/errors/0NN_contract_bad_spec.eu.expect`:

```
exit: 1
stderr: "validate: not a type spec"
```

The second test is what makes design decision 6 testable: a malformed **spec**
raises a `UserPanic`, a *different* variant from `ContractViolation`, so the two
are distinguishable from stderr and from a `.expect` sidecar.

Check whether `tests/harness_test.rs` needs per-file registration for error
tests too (`grep -n "test_error_0" tests/harness_test.rs | tail -3`) and add
entries in the established style.

- [ ] **Step 9: Build, run, and check the rendered diagnostic by eye**

```bash
cargo build
cargo test test_harness_194
cargo test test_error
timeout 60 ./target/debug/eu --heap-limit-mib 2048 -L lib tests/harness/errors/0NN_contract_violation.eu
```

Expected: the diagnostic carries `error[EU-EVAL-CONTRACT]`, points at the
`ensure` call site in the user's file (not into `contract.eu`), and lists each
violation as a `=` note. If it points into `contract.eu`, re-read
`to_diagnostic`'s location-selection comment (`src/eval/error.rs:1017+`) — the
fix is to leave the wrapper as a plain `lambda`, never `annotated_lambda`.

- [ ] **Step 10: Fault-injection verify**

1. Make `ensure` return `data` unconditionally → the error test
   `0NN_contract_violation` fails (exit 0).
2. Remove the `spec-shaped?` guard from `validate` → `0NN_contract_bad_spec`
   fails.
3. Change `code()` to return `None` for `ContractViolation` → the
   `EU-EVAL-CONTRACT` stderr regex fails.
4. Make `render-violation`'s `:unexpected` arm drop the keys → `t7` fails.

- [ ] **Step 11: Commit**

```bash
cargo fmt --all
cargo clippy --workspace --all-targets -- -D warnings
git add -A
git commit -m "feat(sv3): ensure + ExecutionError::ContractViolation (eu-u9xj.1)

Co-Authored-By: Claude <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01E7q98Ubgo4WKJbS6CWNPFz"
```

---

## Task 6: Documentation, dual-engine and GC validation, CHANGELOG

**Files:**
- Create: `docs/guide/contracts.md`
- Modify: `docs/reference/agent-reference.md`, `CHANGELOG.md`
- Regenerate: `lib/prelude.blob` (build artefact, gitignored — regenerated, not committed)

- [ ] **Step 1: Write the guide chapter**

Create `docs/guide/contracts.md`. Every eucalypt fence is ` ```eu,notest `
unless it genuinely runs standalone. Cover, in this order: what a contract is
and when to reach for one (the ingress boundary); `validate` and the shape of a
violation; the path grammar; `ensure` and what the raised error looks like; open
vs closed and why a spec literal means the same thing statically and at runtime;
optional fields; the forcing discipline and the three consequences users will
rely on (spec §6.6); and what is not validated in v1 (`Dict(T)`,
`NonEmpty([T])`, recursive `Mu` — accepted for `as-spec` parity).

Use the spec's §10 worked example (`servers.yaml` + the schema + the
three-entry report) verbatim as the chapter's centrepiece — it is already
correct and owner-reviewed.

Link the chapter from wherever the other guide chapters are indexed
(`grep -rn "navigating-nested-data" docs/ --include=*.md | grep -v guide/navigating`)
and add it in the same place, in the same style.

- [ ] **Step 2: Add a short reference entry**

In `docs/reference/agent-reference.md`, after §3.5a ("Type Specs — `as-spec`"),
add §3.5b:

````markdown
### 3.5b Structural contracts — `validate` and `ensure`

```eu,notest
{ import: "contract.eu" }

schema: s"{ name: string, port: number, tags?: [string] }"

# A report — a list of blocks. [] means conformant. Never raises.
report: { name: "web", port: "8080" } validate(schema)
# => [{ path: "port", kind: :type-mismatch, expected: "number", found: "string" }]

# Data unchanged on success, raises EU-EVAL-CONTRACT otherwise.
config: { name: "web", port: 8080 } ensure(schema)
```

| `kind` | `path` points at | `expected` | `found` |
|---|---|---|---|
| `:type-mismatch` | the value | rendered type | runtime type name |
| `:missing` | the containing record | the missing key | `:absent` |
| `:unexpected` | the record | `:closed` | list of surplus keys |
| `:length` | the list | required length | actual length |

Paths are strings: `servers[2].port`, `''` at the root, `'my key'` for a
non-identifier key. A **closed** spec (`{a: number}`) reports surplus keys; an
**open** one (`{a: number, ..}`) ignores them. Only the paths the spec names are
forced.
````

- [ ] **Step 3: Regenerate the prelude blob and confirm the version bump took**

```bash
cargo xtask prelude-compile
cargo build
cargo test
```

`lib/prelude.blob` is gitignored (`.gitignore:58`) — it is regenerated, not
committed. Confirm the build no longer warns "precompiled prelude not found",
and that the harness suite is green **with** the blob in place (the blob path is
the one that carries the shifted slot numbering, so this run is the real test of
the Task 5 Step 2 version bump).

- [ ] **Step 4: Dual-engine and GC validation**

```bash
cargo test
EU_HEAPSYN=1 cargo test
EU_GC_VERIFY=2 cargo test --test harness_test
```

All three must be green. Then confirm byte-identical output on the new tests:

```bash
for f in tests/harness/19{0,1,2,3,4}_*.eu; do
  a=$(timeout 60 ./target/debug/eu --heap-limit-mib 2048 -L lib "$f" 2>&1)
  b=$(EU_HEAPSYN=1 timeout 60 ./target/debug/eu --heap-limit-mib 2048 -L lib "$f" 2>&1)
  if [ "$a" = "$b" ]; then echo "SAME $f"; else echo "DIFFER $f"; fi
done
```

Expected: `SAME` for every file. Any `DIFFER` is a blocker — investigate before
proceeding, do not report it as a known difference.

- [ ] **Step 5: CHANGELOG**

Add under `## [Unreleased]` → `### Added`, in the established dense style, an
entry naming: `validate`/`ensure` in the new `lib/contract.eu` resource; the
violation entry shape and the four kinds; the additive `:t-record` closedness
element and its arity-tolerant read; the open-record and literal-string
`from-data` round-trip fixes that fall out of it; `reflect.type-str`; the new
`ExecutionError::ContractViolation` / `EU-EVAL-CONTRACT`; the spec-directed
forcing discipline and the harness test that gates it; and the
`BYTECODE_WIRE_FORMAT_VERSION` bump to v6 with its reason. Cite the spec path
and the bead `eu-u9xj.1`.

- [ ] **Step 6: Final gates and push**

```bash
cargo fmt --all
cargo clippy --workspace --all-targets -- -D warnings
cargo test
git status                 # confirm no probe.eu or stray artefacts
git add -A
git commit -m "docs(sv3): contracts guide, reference entry and changelog (eu-u9xj.1)

Co-Authored-By: Claude <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01E7q98Ubgo4WKJbS6CWNPFz"
git fetch origin master
git rebase origin/master
cargo test                 # re-run after rebase — pp-fix may have touched prelude.eu
git push -u origin feat/quill-sv3-contracts
```

- [ ] **Step 7: Open the PR**

Target `master`. The body must state:

- The bead (`eu-u9xj.1`) and the spec path.
- Every fault injection performed, per test, with the observed FAIL/PASS.
- Dual-engine results and the byte-identical confirmation.
- `EU_GC_VERIFY=2` result.
- That the `t-*` projection is a **versioned wire surface**, so **recorded
  review by someone other than the author is required before merge**, and that
  `BYTECODE_WIRE_FORMAT_VERSION` was bumped 5 → 6 because the intrinsic table
  grew.
- The departures listed below.

Then verify the actual full CI rollup and paste `gh pr checks <n>` output before
reporting done. A truncated or in-progress view is not a green rollup.

---

## Departures from the spec (must be reported in the PR)

1. **`BYTECODE_WIRE_FORMAT_VERSION` bump (Task 5 Step 2).** The spec does not
   mention `lib/prelude.blob`. Adding an intrinsic shifts the
   `INTRINSIC_COUNT + prelude slot` global numbering the blob bakes in, while
   the blob's freshness check hashes only `lib/prelude.eu` (unmodified here) and
   the wire-format version. Without a bump, a stale blob is judged fresh with an
   off-by-one slot map. Bumping is the least inventive fix.
2. **`:t-lit-str` rendering in `reflect`'s `to-str` (Task 1 Step 4).** Spec §5.2
   names only `record-str` as the behavioural fix. Without a `:t-lit-str` arm,
   literal string types render through the `"any"` fallthrough, making
   `expected:` wrong for any violation involving one and leaving `from-data`
   lossy for literal strings (verified on master:
   `c"\"prod\"" __TYPE_FROM_STRING to-data from-data` → `"any"`). Same class of
   fix as the `..` loss the spec does call out.
3. **Root path rendering in `render-violation`.** Spec §6.3 fixes the root
   *path* as `""`, but does not say how a violation *line* renders an empty
   path. `(root)` is used. Flagged, not silently assumed.
4. **`type-name` returns `"unknown"` for unclassifiable values.** Spec §6.2
   lists ten runtime type names; a value matching none of them needs a name.
   `"unknown"` is used.

## Underspecified points inherited from the spec (report, do not resolve)

- Spec §15.1/§15.2: `expected` is not uniformly typed across kinds (type string
  / key name / `:closed` / number), and `:missing` does not carry the missing
  field's type. Implemented exactly as §6.2 specifies; the owner flagged both
  for confirmation and this PR does not change them.
- Spec §13.7: `as-spec` has no `:t-lit-str`/`:t-lit-sym` arm, so
  `s"\"prod\"" as-spec` matches anything, while `validate` checks by equality.
  The two therefore disagree on literal types — deliberately, per §13.7, which
  calls the `as-spec` behaviour a probable defect. The `as-spec`-agreement
  harness corpus excludes literal types for this reason. A follow-up bead should
  be filed.
