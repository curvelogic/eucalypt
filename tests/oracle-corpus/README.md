# Oracle corpus (eu-2sa6.14)

Frozen dual-engine golden output for the `tests/harness/*.eu` sources,
captured while the bytecode engine's output was confirmed byte-identical to
HeapSyn's. See the design doc for full context:
`docs/superpowers/specs/2026-07-16-oracle-corpus-replacement-plan.md`.

## What's here

`golden/<name>.stdout`, `golden/<name>.stderr`, `golden/<name>.exit` — one
triple per captured source, plus `golden/MANIFEST.tsv` (name, exit code,
stdout byte count).

This is the **prototype** (Phase 1) capture: a whole-file, non-test-mode
`eu <file>` evaluation per source, produced by `scripts/capture-oracle-corpus.sh`.
174 of the 175 `tests/harness/*.eu` sources captured cleanly (bytecode and
HeapSyn byte-identical on stdout, stderr, and exit code).

**Known exclusion:** `049_tester.eu` is not present. Plain whole-file
evaluation of that file (which defines three named test targets and no
default) runs outside test mode, where one target's intentionally-failing
`//=` assertion (`failures.α`, `verify: [:all-values-false]`) panics instead
of returning `false` — and the two engines report a different call-site trace
for that panic. This is a **capture-methodology artifact, not an engine
correctness bug**: driving the file the way `cargo test` actually does (`eu
test tests/harness/049_tester.eu`, exercising each named target through the
test-mode machinery) is byte-identical between engines. See the design doc
§4.2 for the full root-cause and the production tool's fix (drive captures
per-target through the same mechanism as `cargo test`, not naive whole-file
eval).

## Regenerating

```
cargo build --release
scripts/capture-oracle-corpus.sh target/release/eu tests/oracle-corpus/golden
```

Regenerating and committing new golden output is a **reviewed, deliberate
act** (it changes what future runs are checked against) — never a side effect
of an unrelated change. See the design doc's update policy (§7).
