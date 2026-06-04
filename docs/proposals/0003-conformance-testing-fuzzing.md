# 0003 — Testing for 1.0: the harness as conformance contract, property testing & fuzzing

- **Status:** Draft proposal for review
- **Track:** A — v1.0 readiness & process
- **Classification:** Whitespace
- **Suggested horizon:** 0.8–1.0
- **Related:** siblings [0001](0001-v1-charter.md) (1.0 charter / prelude-v1 freeze),
  [0002](0002-gradual-typing-boundary-policy.md) (boundary policy),
  [0013](0013-type-dsl-embedding.md) (type-DSL fuzzing), the forthcoming
  **alternative-backends** proposal (core→WASM and friends — the consumers of
  the conformance contract; see README backlog)

---

## Summary

Eucalypt 0.7.0 already has an unusually thorough, *dogfooded* test suite: 315+
harness tests written in eucalypt's own user-facing assertion operators
(`//=`/`//!`/`//=>`), doc-example extraction, AddressSanitizer and GC-stress CI
jobs, and a WASM smoke-test gate. The job here is **not** to build a parallel
suite — it is to (1) recognise the existing harness *as* eucalypt's conformance
corpus and document the thin contract that lets a *different backend* consume the
same files; (2) add a small **golden-output** layer (reusing the harness's
existing `.expect` sidecar mechanism) only where an external oracle is actually
needed — rendered-output bytes and the 0001 prelude-v1 freeze; (3) add
**property-based** tests for the invariants the example suite can't state; and
(4) add **fuzzing** of the parsers and the type-DSL, plus promotion of the
`EU_GC_VERIFY=2`/`EU_GC_STRESS=1` oracle from one CI job to the whole harness.
Nothing existing is removed or duplicated; the dogfooded `//=` corpus is the
asset everything else builds around.

---

## Motivation

### What the testing infrastructure already does well — and why that matters

The harness (`tests/harness_test.rs`) drives the functional tests, GC tests,
error tests, and type-check tests through `tester::test` /
`tester::error_test_with_binary`. Crucially, each `.eu` test asserts with the
**same operators users get** — `//=` (equality), `//!` (truth), `//=>`
(pass-through) — backed by `lib/test.eu` and surfaced as `eu test`. So the test
suite is *dogfooding the user-facing testing facility*: every harness run
exercises the tool eucalypt ships for its users. That property is an asset to
**protect**, not refactor away. Error and `eu check` tests additionally validate
against external **`.expect` sidecars** (`run_error_test`, `run_typecheck_test`)
— an external-oracle mechanism that already exists in the harness.

The CI matrix (`.github/workflows/build-rust.yaml`) is already ambitious: `lint`
(`check`/`rustfmt`/`clippy --all-targets -D warnings`), `audit` (`cargo audit`),
`test` (Ubuntu/Windows/macOS), `test-doc-examples`, `test-aarch64-sequential`
(release + `EU_GC_STRESS=1`), `asan` (nightly, `continue-on-error`), and `wasm`
(`wasm-pack build` + Node smoke tests). The gaps are structural, not "more of
the same."

### The gaps

**Gap 1 — the harness is an internal regression suite, not yet a portable
contract.** Two distinct limitations, both narrow:

- *The oracle is mostly internal.* A functional test passes iff `eu` evaluates
  the file's own `//=` assertions to true and exits 0. That is excellent
  dogfooding and a perfectly good *cross-backend* oracle (a backend that computes
  differently fails the same assertions) — but it does **not** externally pin two
  things: the **rendered output bytes** (does the backend emit the right
  YAML/JSON?), which `//=` tests of *values* don't fully exercise; and the
  **0001 prelude-v1 freeze** (byte-exact frozen behaviour), which needs a golden
  baseline the implementation can't move on its own.
- *It is wired to the reference runner.* Tests run through the Rust
  `tester::test` API. That is fine for `eu` itself, but a genuinely independent
  backend can't call into it. (Today's WASM build is *not* such a backend: it is
  the same Rust crate compiled to `wasm32`, so it cannot diverge on language
  semantics, only on driver-level concerns — step limits, error formatting, host
  I/O. The real independent backends are the **forthcoming core→WASM compiler
  and friends**, which is when cross-backend conformance starts to bite.)

The fix for both is small and is *not* "build a second corpus" — see Design §1.

**Gap 2 — no property-based testing.** The harness is example-based. Properties
like "render a value to JSON then parse it back yields the same value" or
"subtyping is reflexive" are never *stated*, only spot-checked where a developer
wrote a regression (`114_parse_as_roundtrip.eu` covers two formats, two keys, and
is not generated).

**Gap 3 — no fuzzer coverage.** The Rowan parser (`src/syntax/rowan/`, now the
sole front-end) and — more importantly — `src/core/typecheck/parse.rs`, the
hand-written recursive-descent parser over *untrusted type-annotation strings*
in user `.eu` files, have no fuzz target. This is precisely the code where odd
input must yield a clean diagnostic, not a panic (the CLAUDE.md panic policy).

**Gap 4 — GC verification confined to one CI job.** `EU_GC_VERIFY=2`
(four-checkpoint structural verification — `src/eval/memory/gc_verify.rs`) and
`EU_GC_STRESS=1` (force evacuation every cycle) are a rigorous in-process oracle
for the bug class most likely to corrupt silently — use-after-free, stale
forwarding pointers, block-list incoherence. They run in only one job
(`test-aarch64-sequential`) plus `asan` (`continue-on-error: true`). The
`transmute`-based fat-pointer evacuation path (`src/eval/memory/collect.rs`) is
exactly what warrants verification across the whole harness.

---

## Prior art & landscape

### Conformance corpora — the test files are the contract

**Dhall** is the directly relevant precedent, and it teaches the key point: the
shared asset is a directory of `(input, expected-output)` cases, and *each
implementation supplies its own runner* (Haskell, Rust, Go, Python, Java all run
the same files their own way). The corpus *is* the normative definition between
standard revisions; no implementation's runner is privileged. **WebAssembly spec
tests** (`.wast`) work the same way across V8/SpiderMonkey/Wasmtime. **Nickel**
takes the lowest-complexity form — `.ncl` files with `.expected` sidecars run by
a trivial script any implementation can reuse — which is almost exactly the
golden-output layer proposed below. The lesson for eucalypt: do **not**
parametrise the Rust runner with a backend; make the *files plus a documented
pass/fail convention* the contract, and let each backend bring its own runner.

### Property-based testing

Claessen & Hughes's QuickCheck (ICFP 2000): universally-quantified propositions
over generated inputs with a shrinker. For Rust, `proptest` is preferred (regex
string generation, deterministic seed replay). For compilers, CSmith (Yang et
al., PLDI 2011) and metamorphic/differential testing (Rigger & Su, OOPSLA 2020)
find deep bugs via semantics-preserving transformations — directly applicable to
the eucalypt pipeline, and a natural future cross-check *between backends*.

### Fuzzing

`cargo-fuzz` (libFuzzer) integrates cleanly with the Rust build and ASAN;
AFL++ has a richer mutation engine and can complement it. Standard playbook:
fuzz parsers and format loaders for panics/hangs on arbitrary bytes.

---

## Proposed design

### 1. The harness as a conformance contract (not a second corpus)

Keep the existing `tests/harness/` `.eu` files exactly as they are — dogfooded,
`//=`-based, doubling as language examples. Make them the conformance corpus by
adding two cheap things, neither of which removes or duplicates anything:

1. **Document the corpus contract.** Write down, once, what it means to run a
   harness test and what counts as conformance: a unit passes if it evaluates
   without error and its `//=`/`//!` assertions hold (signalled by exit 0), and —
   where a golden sidecar is present — if its rendered output matches. This
   one-page convention is what a *different* implementation's runner targets. The
   reference runner (`tester::test`) is unchanged; an alternative backend (the
   forthcoming core→WASM work) consumes the same files with its own runner.
2. **Add a golden-output layer where an external oracle is actually needed.**
   Extend the harness's existing `.expect` sidecar mechanism (already used by
   `run_error_test` and `run_typecheck_test`) to *rendered stdout* for a
   **curated subset** of functional tests — the cases where serialization bytes
   matter (each export format) and the cases that encode the **0001 prelude-v1
   freeze** (byte-exact). This is a sidecar on an *existing* test file, not a new
   test. It closes the two things `//=` value-assertions don't externally pin
   (output bytes; charter-frozen behaviour — the executable form of 0001's G3),
   and it is what would have caught the `cond` break before release.

The result: one corpus, dogfooded for `eu`, and a documented contract plus a
thin golden layer that any future backend can be held to. No `tests/conformance/`
duplicate, no `tester::test` backend parameter.

### 2. Property-based testing: `tests/property_tests.rs`

Add `proptest` to `[dev-dependencies]` and state the invariants the example
suite cannot:

- **Round-trip idempotence.** For each format, generate a structured value,
  render via `render-as`, parse via `parse-as`, check structural equality.
- **Render determinism.** A fixed source run twice yields byte-identical output
  (catches block-ordering / float-formatting non-determinism).
- **`eu fmt` idempotence.** `fmt(fmt(x)) = fmt(x)` (`src/export/eu.rs`) — a
  classic formatter property.
- **Type-checker metatheory.** Subtyping reflexivity/transitivity and the
  consistency-symmetry property of 0002, tested directly against
  `src/core/typecheck/` via a `proptest::Arbitrary` for `Type` (note: `Type` now
  includes `Con`/`App`/`Forall`/`Mu` — the generator must cover them and respect
  kinds).
- **GC invariants.** Run generated deeply-nested structures under
  `EU_GC_VERIFY=2`/`EU_GC_POISON=1`, exercising evacuation beyond the static files.

These are also the natural future **differential** oracle between the
interpreter and a core→WASM backend (same generator, two backends, compare).

### 3. Fuzzing: `fuzz/`

Three `cargo-fuzz` targets, pass criterion "no panic / no hang" (errors are fine):

- **`fuzz_type_dsl`** *(highest priority)* — arbitrary strings into
  `src/core/typecheck/parse.rs`, the hand-written parser over untrusted user type
  annotations (record/row/arrow/`forall`/kind/constraint grammar). Seed from the
  annotation strings in `lib/prelude.eu` and `tests/harness/typecheck/`.
- **`fuzz_parser`** — arbitrary bytes into the **Rowan** parser
  (`src/syntax/rowan/`). Seed from `tests/harness/*.eu`.
- **`fuzz_loader`** — arbitrary bytes into the import pipeline (`src/import/`:
  YAML/JSON/TOML/CSV/XML). Malformed input explicitly in-scope.

Every panic found becomes an `ExecutionError`/`CoreError` diagnostic with a
regression test in `tests/harness/errors/`. (Differential testing of the whole
pipeline is a medium-term target, best after the front-end work in
[0015](0015-parser-error-recovery.md) and once a second backend exists.)

### 4. VM correctness in CI: full harness under `EU_GC_VERIFY=2`

Promote GC verification from the single aarch64 job to a dedicated job over the
whole harness on Linux x86-64:

```yaml
gc-verified:
  name: GC verified harness (EU_GC_VERIFY=2 + EU_GC_STRESS=1)
  runs-on: ubuntu-latest
  steps:
    - ... (standard build) ...
    - run: cargo test --test harness_test
      env: { EU_GC_VERIFY: "2", EU_GC_STRESS: "1", EU_GC_POISON: "1" }
```

This turns every harness test into a structural-integrity check of the GC,
exercising the `transmute`-based evacuation path (`collect.rs`) across all
allocator paths rather than only the stress path on aarch64. Promote `asan` from
`continue-on-error: true` to a hard gate once the fuzz/property passes clear the
known crash backlog.

---

## Interaction with the existing roadmap

- **[0001](0001-v1-charter.md):** the golden-output subset *is* the executable
  form of the 1.0 stable surface and the prelude-v1 freeze (0001 G3). The
  contract document is what makes 0001's stability promise machine-checkable.
- **[0002](0002-gradual-typing-boundary-policy.md):** the type-metatheory
  properties operationalise 0002's stated invariants; the boundary policy decides
  what the checker is committed to and the golden/`.expect` cases encode it.
- **The forthcoming alternative-backends proposal (core→WASM, …):** this is the
  *reason* cross-backend conformance has teeth, and the consumer of the contract
  in §1. The conformance work is partly gated on it — the contract and golden
  layer are worth writing now (they serve 0001), but the multi-backend payoff
  arrives with backend #2.
- **[0013](0013-type-dsl-embedding.md):** `fuzz_type_dsl` is the safety net to
  have *before* touching the type-DSL grammar.

---

## Implementation sketch

**Phase 1 (0.8, low risk).** Write the one-page corpus contract; add golden-output
`.expect` sidecars to a curated subset of existing harness files (serialization
+ prelude-v1 freeze) — reusing the `.expect` plumbing already in
`tests/harness_test.rs`; add the `gc-verified` CI job (one YAML stanza). No new
corpus, no `src/` changes, nothing removed.

**Phase 2 (0.9).** Add `proptest` dev-dependency and `tests/property_tests.rs`
(round-trip, determinism, `fmt` idempotence); add `Arbitrary` for `Type` and the
metatheory properties.

**Phase 3 (0.9–1.0).** Add `cargo-fuzz` and the three targets with seed corpora;
run to exhaustion before the freeze; fix every panic.

**Component changes:** golden `.expect` sidecars under `tests/harness/` (data,
not new tests), `Cargo.toml` (`+proptest` dev-dep), `tests/property_tests.rs`
(new), `fuzz/` (new), `.github/workflows/build-rust.yaml` (new `gc-verified`
job). The reference runner and the existing test files are untouched.

---

## Alternatives considered

- **Build a separate `tests/conformance/` corpus.** Rejected: it would duplicate
  much of the harness and create two corpora to maintain, and it discards the
  dogfooding property. The harness already *is* the corpus; it needs a documented
  contract and a thin golden layer, not a parallel tree.
- **Parametrise `tester::test` with a backend.** Rejected: the Rust runner is
  implementation-specific. The portable asset is the test files plus the pass/fail
  convention; an independent backend brings its own runner (the Dhall/WASM model).
- **"The harness is good enough."** Half-right: it is an excellent dogfooded
  regression suite and a decent cross-backend value-oracle. It just doesn't yet
  externally pin rendered output or the charter freeze, isn't documented as a
  portable contract, and has no property or fuzz coverage.
- **`EU_GC_VERIFY=2` in debug builds by default.** Rejected: measurable overhead;
  CI-only is the right boundary.
- **AFL++ instead of cargo-fuzz.** Either works; `cargo-fuzz` integrates more
  naturally. Revisit once targets are live.

---

## Risks & what would kill this

- **GC-verified CI is too slow.** If `EU_GC_VERIFY=2` over the full harness
  exceeds ~5–10 min, move it to a daily scheduled run. Measure first.
- **Golden sidecars over-pin and churn.** If too many golden outputs are added,
  every intentional change touches many sidecars. Mitigation: keep the golden
  subset *small and deliberate* — serialization per format + the prelude-v1
  freeze — not a blanket snapshot of every test.
- **Fuzz findings near the freeze.** The fuzzer *will* find panics; that's the
  point. Start early so there's runway to fix rather than defer/delay 1.0.
- **Proptest flakiness.** Seed deterministically; pin any found counterexample.

---

## Success criteria

1. **Contract + golden layer:** a one-page corpus contract published; golden
   `.expect` sidecars cover every export format and the prelude-v1 frozen set;
   the contract is cited by the 0001 charter. The dogfooded `//=` harness is
   unchanged and still runs as `eu test`.
2. **Property tests:** round-trip idempotence (YAML/JSON/TOML), `fmt`
   idempotence, and subtyping reflexivity/transitivity pass.
3. **Fuzzer:** zero panics after a 24-hour run on each target; all campaign
   findings converted to diagnostics with regression tests.
4. **GC in CI:** `gc-verified` passes over the full harness on Linux x86-64;
   `asan` promoted to a hard gate.
5. **Backend-ready:** when the first alternative backend lands, it can be held to
   the *same* corpus + golden layer with its own runner — no change to the files.

---

## References

**Eucalypt files:** `tests/harness_test.rs` (`tester::test`, `run_error_test`,
`run_typecheck_test` with `.expect` sidecars); `tests/harness/gc/`;
`tests/harness/typecheck/`; `tests/harness/049_tester.eu`,
`125_expectations.eu` (`//=`/`//!` usage); `114_parse_as_roundtrip.eu`;
`lib/test.eu` (the user-facing test facility); `src/eval/memory/gc_verify.rs`,
`gc_debug.rs` (`EU_GC_VERIFY`/`POISON`); `src/eval/memory/heap.rs`
(`EU_GC_STRESS` evacuation forcing); `src/eval/memory/collect.rs`
(`transmute` fat-pointer evacuation); `src/core/typecheck/parse.rs` (type-DSL
parser); `src/syntax/rowan/` (the sole parser); `src/import/` (format loaders);
`src/export/eu.rs` (`fmt`); `src/wasm.rs` / `src/wasm_pipeline.rs` (same-core
WASM surface — shares semantics with `eu`); `.github/workflows/build-rust.yaml`.

**Papers & external:** Claessen & Hughes, *QuickCheck* (ICFP 2000); Yang et al.,
*Finding and Understanding Bugs in C Compilers* (PLDI 2011, CSmith); Rigger & Su,
*Finding Bugs in DBMSs* (OOPSLA 2020, metamorphic/differential); Dhall test suite
(`dhall-lang/dhall-lang/tree/master/tests`); WebAssembly spec tests
(`WebAssembly/spec/tree/main/test/core`); Nickel `.expected` sidecars;
`proptest` (`proptest-rs/proptest`); `cargo-fuzz` (`rust-fuzz/cargo-fuzz`).
