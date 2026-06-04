# 0003 — Conformance suite, property testing & fuzzing; VM correctness-in-CI

- **Status:** Draft proposal for review
- **Track:** A — v1.0 readiness & process
- **Classification:** Whitespace
- **Suggested horizon:** 0.8–1.0
- **Related:** sibling proposals [0001](0001-v1-charter.md) (1.0 charter),
  [0002](0002-gradual-typing-boundary-policy.md) (boundary policy),
  [0019](0019-host-language-interop.md) (host-language interop / alternative surfaces)

---

## Summary

Eucalypt 0.7.0 already has an unusually thorough test suite for a language of
its size: 315+ numbered harness tests, 11 GC-specific tests, 24 type-checker
tests, doc-example extraction, AddressSanitizer and GC-stress CI jobs, and a
WASM smoke-test gate. The gaps that matter for a 1.0 stability commitment are
different in character: there is no implementation-independent conformance
corpus, no property-based testing of language invariants (idempotence,
round-trips, type-system metatheory), no systematic fuzzing of the parsers or
type-DSL, and the excellent `EU_GC_VERIFY=2`/`EU_GC_STRESS=1` infrastructure
runs in only one CI job (aarch64) rather than across the whole harness. This
proposal argues for closing each gap as a prerequisite for the 1.0 stability
charter defined in 0001: the conformance suite becomes the *executable
definition* of the stable surface, and the other three pillars systematically
convert the remaining panic-class bugs into diagnostics before the freeze.

---

## Motivation

### What the testing infrastructure already does well

The harness (`tests/harness_test.rs`) drives 152 numbered functional tests, 11
GC tests (`tests/harness/gc/`), 266 error tests, and 24 type-check tests
through a common `tester::test` / `tester::error_test_with_binary` interface.
Each `.eu` test file uses in-language assertion operators (`//=` for equality,
`//!` for truth, `//=>` for pass-through assertion) so tests are simultaneously
runnable by the harness and readable as language examples. The
`scripts/test-doc-examples.py` pipeline extracts and runs every `` ```eu ``
block in `docs/` on Linux CI, ensuring documentation does not rot. The CI
matrix (`.github/workflows/build-rust.yaml`) is already ambitious:

| Job | What it validates |
|-----|-------------------|
| lint | `cargo check`, `rustfmt --all`, `clippy --all-targets -D warnings` |
| audit | `cargo audit` (CVE scan) |
| test | `cargo test` on Ubuntu, Windows, macOS-on-master |
| test-doc-examples | Python extraction + `eu` run, Linux only |
| test-aarch64-sequential | Release binary + `EU_GC_STRESS=1`, aarch64 |
| asan | ASAN nightly build + `EU_GC_STRESS=1` harness, `continue-on-error: true` |
| wasm | `wasm-pack build`, WASM clippy, 4 Node.js smoke tests |

This is strong. The gaps are structural, not a matter of adding more of the
same.

### The gaps

**Gap 1 — No implementation-independent conformance corpus.** Every test
today is executed *only* against the `eu` reference implementation. The WASM
build is a second execution surface — `src/wasm.rs` exposes `evaluate()` and
`evaluate_expr()` backed by `src/wasm_pipeline.rs`, a deliberately separate
pipeline path — but CI only runs four hand-written smoke tests against it
(`src/wasm.rs:116–161`). If the WASM pipeline diverges from `eu` on a corner
case, no current test will catch it. If a future embedding or codegen surface
(see [0019](0019-host-language-interop.md)) is added, the problem compounds.
There is no machine-readable document that says "these are the behaviours that
eucalypt 1.0 is defined to have."

**Gap 2 — No property-based testing.** The harness is example-based:
specific inputs, specific outputs. Properties like "rendering a value to JSON
then parsing it back yields the same value" or "the type subtyping relation is
reflexive" are never stated as properties and never verified beyond the handful
of cases where a developer happened to write a regression. The
import-export-import round-trip tests in `114_parse_as_roundtrip.eu` are
valuable but cover only two formats and two keys; they are not generated.

**Gap 3 — No fuzzer coverage.** The LALRPOP-generated lexer/parser
(`src/syntax/`) and the Rowan-based incremental parser share no fuzz corpus.
More critically, `src/core/typecheck/parse.rs` is a hand-written
recursive-descent parser over untrusted metadata strings (type annotation
strings in user `.eu` files) — precisely the kind of code where unexpected
input should produce a clean diagnostic, not a panic. There is currently no
fuzz target for any of these.

**Gap 4 — GC verification confined to one CI job.** `EU_GC_VERIFY=2`
(four-checkpoint structural verification: post-mark, post-evacuate,
post-update, post-sweep — `src/eval/memory/gc_verify.rs`) and
`EU_GC_STRESS=1` (force SelectiveEvacuation on every GC cycle —
`src/eval/memory/heap.rs:1786`) together constitute a rigorous in-process
correctness oracle. They catch the class of bug most likely to cause silent
data corruption in the STG VM: use-after-free, stale forwarding pointers, and
block-list incoherence. Yet they currently run in exactly one CI job
(`test-aarch64-sequential`) and `asan` (`continue-on-error: true`). The
`transmute`-based fat-pointer reconstruction in `src/eval/memory/collect.rs:576`
is exactly the kind of code that warrants systematic verification across all
platforms and test cases.

---

## Prior art & landscape

### Conformance suites

**Dhall** is the most directly relevant precedent. Dhall's
`dhall-lang/dhall-lang` repository contains a `tests/` directory of
implementation-independent normalisation and type-inference acceptance tests;
every Dhall implementation (Haskell, Rust `serde-dhall`, Go, Python, Java) runs
the same suite. Cases are `(input.dhall, expected-output)` pairs in a versioned
directory tree. The Dhall approach is explicit that the test corpus *is* the
normative definition of language behaviour between standard revisions.

**WebAssembly spec tests** (`WebAssembly/spec/test/core/`) distribute `.wast`
text-format test scripts that every compliant engine (V8, SpiderMonkey,
Wasmtime, wasmer) must pass. The format is compact, language-independent, and
machine-checkable. Crucially, the spec and the tests are co-evolved — a
behaviour not covered by a test is considered unspecified.

**test262** (ECMAScript) and **CSS WPT** demonstrate the approach at scale, but
they are relevant as existence proofs that conformance suites work across
wildly divergent implementations. Eucalypt's situation is closer to Dhall's:
one reference implementation plus a WASM surface now, with the possibility of
more.

**Nickel** takes a simpler approach: `.ncl` files with `.expected` sidecars
run by a shell-script runner any implementation can re-use — a reasonable
lower-complexity model for eucalypt.

### Property-based testing

Claessen and Hughes's QuickCheck (ICFP 2000) established the idiom: universally
quantified propositions over generated inputs with a shrinker to isolate minimal
counterexamples. For Rust, `proptest` is preferred over the `quickcheck` port
for its deterministic seed replay and regex-based string generation. MacIver et
al.'s Hypothesis (ECOOP 2020) extended the model with database-backed example
persistence; the shrinking ideas apply to `proptest` strategies directly.

For compiler testing, CSmith (Yang et al., PLDI 2011) found hundreds of GCC and
Clang bugs via random program generation. The key technique is *metamorphic
testing* — semantics-preserving input transformations must produce identical
output. Rigger and Su (OOPSLA 2020) generalise this to differential testing,
directly applicable to eucalypt's compiler pipeline.

### Fuzzing

`cargo-fuzz` (libFuzzer for Rust) and AFL++ are the main options. `cargo-fuzz`
integrates cleanly with the Rust build and ASAN without a separate compilation
step; AFL++ has a richer mutation engine and can complement it. The standard
playbook: fuzz the parser with random byte sequences for panics/hangs, fuzz
format loaders for assertion failures on malformed input.

---

## Proposed design

### 1. A conformance corpus: `tests/conformance/`

Create a versioned, implementation-independent corpus at
`tests/conformance/VERSION/CATEGORY/NNN-slug/`:

```
tests/conformance/
  v1.0/
    core-language/
      001-arithmetic/
        input.eu
        expected.json
      002-lazy-evaluation/
        input.eu
        expected.yaml
    format-io/
      001-yaml-roundtrip/
        input.eu
        expected.yaml
    error-behaviour/
      001-free-variable/
        input.eu
        expected.exit   # exit code
        expected.stderr # substring match
    type-checking/
      001-annotation-mismatch/
        input.eu
        expected.exit
        expected.stderr
```

The `input.eu` files are self-contained; no prelude dependency unless the
category explicitly states one. `expected.*` files are the sole
implementation-independent assertion. A conformance runner is a simple shell
script or Python wrapper that runs `eu input.eu` (or the WASM API) and diffs
against expected output.

**Seeding the corpus.** Extract the well-specified `(input, expected-output)`
cases from the 315+ harness files — a curation exercise, not a code-writing one.
Cover arithmetic, booleans, block/list operations, strings, prelude, each import
and export format, round-trip identity, and the type-checker error cases in
`tests/harness/typecheck/`. Aim for 200–300 versioned cases by 1.0.

**Coupling to the 1.0 charter (0001).** The conformance corpus becomes the
machine-readable definition of the stable surface: what is covered is committed
to; what is not covered is explicitly unspecified. The WASM CI job should run
the conformance corpus, not just four smoke tests.

**Versioning.** The corpus lives under `v1.0/` (or `v0.8/` for a preview).
When a behaviour is changed by a future edition, the old corpus directory is
kept and the new version adds or modifies cases. This gives the maintainer
precise visibility into what changed.

### 2. Property-based testing: `tests/property_tests.rs`

Add `proptest = "1"` to `[dev-dependencies]` in `Cargo.toml` and create a
`tests/property_tests.rs` with the following property groups:

**Import-export round-trip idempotence.** For each format pair `(F_in, F_out)`,
a generator builds a simple structured value (block of scalars, list of
scalars, nested block), renders it to `F_out` via `render-as`, parses it back
via `parse-as`, and checks structural equality. The existing `render-as` /
`parse-as` intrinsics make this expressible within eucalypt. The Rust
property test invokes the pipeline directly via `wasm_pipeline::evaluate_unit`.

**Render determinism.** Given a fixed eucalypt source string, running it twice
through the pipeline must produce byte-for-byte identical output. This catches
any residual non-determinism in block ordering or float formatting.

**`eu fmt` idempotence.** The formatter (`src/export/eu.rs`) must satisfy
`fmt(fmt(x)) = fmt(x)` — a classical property that has historically caught
many bugs in code formatters.

**Type-checker metatheory.** Subtyping reflexivity (`T <: T` for all `T`),
transitivity (`T <: U ∧ U <: V ⟹ T <: V`), and consistency symmetry (holds for
the current bidirectional system; may break under blame tracking — 0002 should
flag this). These are tested at the `src/core/typecheck/` level without running
the full pipeline, using a proptest `Arbitrary` instance for `Type` and calling
the subtyping/consistency predicates directly.

**GC invariants under verification.** Run proptest-generated deeply-nested
structures through the pipeline with `EU_GC_VERIFY=2` and `EU_GC_POISON=1`,
stressing the evacuation path beyond the static harness files.

### 3. Fuzzing: `fuzz/`

Introduce `cargo-fuzz` targets. Three targets in the initial corpus:

**`fuzz_parser`** — feed arbitrary byte sequences to the LALRPOP
(`src/syntax/`) and Rowan (`src/syntax/rowan/`) parsers. The pass criterion is
"no panic, no crash"; error results are acceptable. This directly implements
the CLAUDE.md panic policy: every panic in user-reachable code should become a
diagnostic.

**`fuzz_loader`** — feed arbitrary YAML, JSON, TOML, CSV, and XML byte
sequences to the import pipeline (`src/import/`). The pass criterion is "no
panic". Malformed input is explicitly in-scope.

**`fuzz_type_dsl`** — feed arbitrary strings to
`src/core/typecheck/parse.rs` (the hand-written recursive-descent parser).
This is the highest-priority target: it is a parser over untrusted user input
(type annotations in metadata strings) with a non-trivial grammar including
record types, row variables, and function arrows. The parser already has
`ParseError` return types — the question is whether every reachable input
produces one or panics.

A seed corpus for `fuzz_parser` is derived automatically from the `.eu` files
in `tests/harness/`. For `fuzz_type_dsl`, seed from the annotation strings in
`lib/prelude.eu` and `tests/harness/typecheck/`.

Differential testing of the compiler pipeline (semantics-preserving input
transformations must produce identical output) is a medium-term target, best
implemented after the Rowan parser is the sole front-end
([0015](0015-parser-error-recovery.md)).

### 4. VM correctness in CI: full harness under `EU_GC_VERIFY=2`

Promote the GC verification harness from a single aarch64 CI job to a
dedicated CI job that runs against all platforms (at minimum Linux x86\_64) and
uses `EU_GC_VERIFY=2` rather than `EU_GC_STRESS=1` alone:

```yaml
gc-verified:
  name: GC verified harness (EU_GC_VERIFY=2 + EU_GC_STRESS=1)
  runs-on: ubuntu-latest
  steps:
    - ... (standard build) ...
    - name: Run full harness with GC verification
      run: cargo test --test harness_test
      env:
        EU_GC_VERIFY: "2"
        EU_GC_STRESS: "1"
        EU_GC_POISON: "1"
```

This turns every harness test into a structural-integrity check of the
GC. The `transmute`-based fat-pointer evacuation path in
`src/eval/memory/collect.rs:576` and the backing-array forwarding fix
(addressed in the `SentinelArrayBacking` type, `collect.rs:76–85`) are the
precise code paths that need exercising across all allocator paths, not only
the SelectiveEvacuation path triggered by stress mode on aarch64.

The ASAN job should be changed from `continue-on-error: true` to a hard gate
once the fuzz and property-testing passes have cleared the known crash
backlog.

---

## Interaction with the existing roadmap

This proposal is a prerequisite for the 1.0 stability commitment in
[0001](0001-v1-charter.md): the conformance corpus *is* the executable
definition of what is committed to. Without it, the 1.0 charter is a prose
document with no machine-checkable content.

It depends on and reinforces [0002](0002-gradual-typing-boundary-policy.md):
the boundary policy defines what the type-checker is committed to, and the
conformance corpus encodes that commitment as test cases. The property-based
type-system tests (subtyping reflexivity, consistency symmetry) are a direct
operationalisation of 0002's metatheory.

The fuzzer for `fuzz_type_dsl` is an enabler for
[0013](0013-type-dsl-embedding.md): before changing the type-DSL syntax, you
want a fuzz corpus that confirms the current parser has no panic paths.

The WASM conformance gate directly enables [0019](0019-host-language-interop.md)
by establishing a shared correctness oracle that any future embedding must pass.

---

## Implementation sketch

**Phase 1 (0.8, low risk, ~2 person-weeks).** Curation of the conformance
corpus: script-extract the well-specified harness cases into
`tests/conformance/v0.8/`, write the shell/Python conformance runner, update
the WASM CI job to run the corpus. No Rust code changes required. Simultaneously,
add the GC-verified CI job (one YAML stanza).

**Phase 2 (0.9, medium risk, ~1 person-week).** Add `proptest` dev-dependency,
write `tests/property_tests.rs` with the round-trip, determinism, and fmt
idempotence properties. Add `proptest::Arbitrary` for `Type`
(`src/core/typecheck/types.rs`) and write the type-metatheory properties.

**Phase 3 (0.9–1.0, medium risk, ~1 person-week).** Add `cargo-fuzz`
infrastructure, three fuzz targets, seed corpora. Run locally to exhaustion
before the 1.0 freeze; fix every panic found.

**Component changes:** `tests/conformance/` (new directory), `Cargo.toml` (`+proptest`
dev-dep), `tests/property_tests.rs` (new file), `fuzz/` (new directory),
`.github/workflows/build-rust.yaml` (new `gc-verified` job, ASAN hardened).
No changes to `src/`.

---

## Alternatives considered

**"The harness is good enough."** It is good for regression testing of the
reference implementation. It is not a conformance oracle, not a property
checker, and not a fuzzer. The gap matters precisely because 1.0 is a stability
commitment, not just a version bump.

**Embedding `proptest` inside the existing harness tests.** This would make
the test files harder to reason about and mix example-based and property-based
evidence. Keeping them separate is cleaner and allows the property tests to run
independently.

**AFL++ instead of cargo-fuzz.** AFL++ has a richer mutation engine and better
coverage on some targets, but `cargo-fuzz` integrates more naturally with the
Rust build and does not require a separate build step. Either would work; the
choice can be revisited once initial fuzz targets are live.

**Running `EU_GC_VERIFY=2` in debug builds by default.** The verification adds
measurable overhead (it traverses every block's line map at each checkpoint) and
would slow incremental development. CI-only is the right boundary.

---

## Risks & what would kill this

**Performance.** `EU_GC_VERIFY=2` across the full harness may add significant
wall-clock time to CI. If it exceeds 5–10 minutes, it will be skipped. Measure
before committing to the CI stanza; if slow, restrict to a daily scheduled run.

**Proptest flakiness.** Property tests with random generators can exhibit
time-dependent failures on slow CI runners. Seed every run deterministically
(`PROPTEST_CASES` and `proptest::Config::with_cases`) and add explicit seeds for
any failing case found.

**Fuzz findings block the release.** The fuzzer *will* find panics; that is the
point. If the fuzz campaign finds many panics close to the 1.0 freeze, the
maintainer must decide whether to fix, defer (with a `//notest` note in the
corpus), or delay 1.0. The earlier fuzzing starts, the more runway there is.

**WASM pipeline divergence.** If the conformance corpus exposes differences
between `eu` and the WASM pipeline, those differences must be resolved before
1.0. The WASM pipeline (`src/wasm_pipeline.rs`) is intentionally separate from
the CLI driver path; subtle differences in step-limit handling or error
formatting are plausible.

---

## Success criteria

1. **Conformance corpus:** ≥200 versioned cases in `tests/conformance/v1.0/`;
   WASM CI job runs and passes the corpus; the corpus is cited in the 1.0
   charter as the normative test set.
2. **Property tests:** Round-trip idempotence passes for YAML, JSON, TOML;
   `eu fmt` idempotence passes; subtyping reflexivity/transitivity pass for all
   types representable in the DSL.
3. **Fuzzer:** Zero panics found after a 24-hour fuzz run against each of the
   three targets on a standard CI runner; all panics found during the campaign
   are converted to `ExecutionError`/`CoreError` diagnostics with regression
   test cases added to `tests/harness/errors/`.
4. **GC in CI:** `gc-verified` job passes on Linux x86\_64 across the full
   harness; ASAN job promoted from `continue-on-error: true` to a hard gate.
5. **Operational:** The maintainer can state, in the 1.0 release announcement,
   that the conformance corpus defines the stable surface and that all three
   fuzz targets passed a 24-hour campaign.

---

## References

**Eucalypt files:**
- `tests/harness_test.rs` — harness driver, 315 tests across all categories
- `tests/harness/gc/` — 11 GC-specific tests
- `tests/harness/typecheck/` — 24 type-checker message tests
- `tests/harness/049_tester.eu` — `//=` operator usage
- `tests/harness/125_expectations.eu` — `//=`, `//!` semantics
- `tests/harness/114_parse_as_roundtrip.eu` — existing round-trip test
- `src/eval/memory/gc_debug.rs` — `EU_GC_VERIFY`, `EU_GC_POISON` flags
- `src/eval/memory/gc_verify.rs` — four-checkpoint structural verifier
- `src/eval/memory/heap.rs:1786` — `EU_GC_STRESS` SelectiveEvacuation forcing
- `src/eval/memory/collect.rs:576` — `transmute`-based fat-pointer reconstruction
- `src/core/typecheck/parse.rs` — hand-written type-DSL recursive-descent parser
- `src/wasm.rs:116–161` — four existing WASM smoke tests
- `src/wasm_pipeline.rs` — separate WASM evaluation pipeline
- `scripts/test-doc-examples.py` — doc-example extraction and runner
- `.github/workflows/build-rust.yaml` — full CI matrix

**Papers and external references:**
- Claessen, K. and Hughes, J. (2000). "QuickCheck: a lightweight tool for
  random testing of Haskell programs." *ICFP 2000.*
- MacIver, D., Donaldson, A., and Sherrat, Z. (2020). "Test-Case Reduction
  via Test-Case Generation." *ECOOP 2020.*
- Yang, X. et al. (2011). "Finding and Understanding Bugs in C Compilers."
  *PLDI 2011.* (CSmith)
- Rigger, M. and Su, Z. (2020). "Finding and Understanding Bugs in Database
  Management Systems." *OOPSLA 2020.* (metamorphic/differential testing)
- Dhall standard test suite: `github.com/dhall-lang/dhall-lang/tree/master/tests`
- WebAssembly spec tests: `github.com/WebAssembly/spec/tree/main/test/core`
- `proptest` crate: `github.com/proptest-rs/proptest`
- `cargo-fuzz`: `github.com/rust-fuzz/cargo-fuzz`
