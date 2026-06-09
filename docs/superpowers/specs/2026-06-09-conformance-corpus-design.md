# W5: Conformance Corpus, Property Tests & Fuzzing

**Bead:** eu-yhk0.3
**Target:** 0.8.0 (begin), 1.0 (bar)
**Roadmap ref:** W5 (Section 7, ROADMAP.md)

## Problem

The 315+ harness tests are good — dogfooded, written in eucalypt's own
assertion operators, with `.expect` sidecars for error tests. But the
harness is an internal regression suite, not yet:

1. A **portable contract** an independent backend could run.
2. A pin on **rendered output bytes** or the **prelude-v1 freeze**.
3. Backed by **property-based testing** or **fuzzing** of parsers and
   the type-DSL.

GC verification runs in only one CI job.

## Design (Three Phases)

### Phase 1 (0.8): Contract doc, golden sidecars, GC CI job

**Document the corpus contract.** Write
`tests/harness/CONFORMANCE.md` specifying the pass/fail convention:
the `.eu` files plus their sidecars (`.expect` for error tests,
`.golden` for output tests) are the contract. Each implementation
brings its own runner, never privileging one. (Dhall and WebAssembly
spec tests model.)

**Golden-output `.golden` sidecars.** Add a `.golden` file to a
curated subset of harness tests — every export format and the
prelude-v1 frozen set. The harness runner compares rendered output
byte-for-byte against the `.golden` file. This is the executable form
of the stability freeze and "what would have caught the `cond` break".

**Sidecar plumbing.** Extend `tests/harness_test.rs` to:
- Look for `<test>.golden` alongside each test file.
- If present, run `eu <test> -x <format>` and compare output.
- Fail on mismatch with a diff.
- A `--bless` mode regenerates `.golden` files (for intentional
  changes).

**GC-verified CI job.** Promote `EU_GC_VERIFY=2` + `EU_GC_STRESS=1` +
`EU_GC_POISON=1` to a **full-harness CI job** on x86-64. Currently
these flags are available but not run in CI on the full suite.

### Phase 2 (0.9): Property tests

Add `proptest` property tests for:

1. **Round-trip idempotence**: render → parse produces equivalent
   structure.
2. **Render determinism**: same input renders identically across runs.
3. **`eu fmt` idempotence**: formatting a formatted file is a no-op.
4. **Subtyping reflexivity/transitivity** and the consistency-symmetry
   property from Section 4.2 of ROADMAP.md (needs `Arbitrary` for
   `Type` covering `Con`/`App`/`Forall`/`Mu`).
5. **GC invariants** over generated structures.

### Phase 3 (0.9–1.0): Fuzz targets

Three `cargo-fuzz` targets:

1. **`fuzz_type_dsl`** (highest priority) — the hand-written type
   parser over untrusted annotation strings
   (`src/core/typecheck/parse.rs`).
2. **`fuzz_parser`** — the Rowan parser over arbitrary byte input.
3. **`fuzz_loader`** — YAML/JSON/TOML/CSV/XML import paths.

Seed corpora from the existing harness tests. Run to exhaustion before
the 1.0 freeze. Every panic becomes a diagnostic with a regression
test.

## Testing

Phase 1 is itself a testing deliverable — success is:

- Golden sidecars cover every export format and the frozen prelude set
- The GC-verified CI job runs the full harness green under
  `EU_GC_VERIFY=2` + `EU_GC_POISON=1`
- A future breaking change to prelude-v1 output is caught by a
  `.golden` mismatch

Phase 2: property suite passes in CI.

Phase 3: **zero panics after a 24-hour fuzz run** on each target.

Ultimate bar (1.0): when the first alternative backend lands it can be
held to the same corpus with its own runner.

## Scope Exclusions

- No new test *language* features (the `//=`/`//!` operators are
  sufficient)
- No cross-backend runner in this deliverable (that arrives with the
  alternative backend candidate)
- ASAN as a hard gate is a CI infrastructure decision, not a code
  change — include if feasible but not gating
