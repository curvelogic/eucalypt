# Phase-4 oracle-replacement plan — frozen dual-engine golden corpus

- **Bead:** eu-2sa6.14 (0.13.1, parent epic eu-ntwg)
- **Author:** Furnace
- **Date:** 2026-07-16
- **Status:** DESIGN NOTE + PROTOTYPE CAPTURE. No CI wiring, no conformance
  test suite yet — this is the plan for owner sign-off, plus a working
  capture prototype run against the real corpus to ground the plan in
  measured numbers rather than projections. Implementation (the production
  capture tool and the gating conformance-test suite) is follow-up work,
  filed as a new bead (§8).
- **Relates to:** transition review §6 non-perf criterion 1
  (`docs/superpowers/specs/2026-07-12-bytecode-transition-review.md`) — "Oracle
  replacement in place and green" is one of the two non-performance blockers
  on Phase 4 (HeapSyn retirement / eu-oufc). eu-dy96 (this session, PR #1029)
  fixed a latent caller-state-restoration bug in the HeapSyn engine itself,
  so the oracle this plan freezes is trustworthy at capture time.

## 0. Executive summary

HeapSyn is not just the performance baseline — it is the **only** independent
implementation eucalypt's correctness has ever been checked against. Deleting
it (Phase 4) without a replacement removes that check permanently: a future
bytecode-only regression that happens to keep the affected harness test's own
`RESULT` computation looking plausible (or that touches output the test
doesn't assert on at all) would have no second implementation left to disagree
with it.

This plan replaces "compare against a live second engine" with "compare
against a frozen record of what the second engine (and the first) agreed was
correct, captured while both existed and matched." Concretely: for every
`tests/harness/*.eu` source, capture the bytecode engine's full output
(stdout, stderr, exit code) at a point where it is confirmed byte-identical to
HeapSyn's own output for the same source, and freeze that output as a
permanent conformance fixture. Future engine changes (bytecode-only, or
between whatever bytecode variants exist post-Phase-4 — byte-path vs.
pre-decoded, see §6) are validated by re-running against the frozen corpus and
diffing, not by re-running a second engine that no longer exists.

I built and ran the capture mechanism against the real corpus rather than
just specifying it (§3): 174 of 175 `tests/harness/*.eu` sources captured
cleanly, byte-identical between engines on stdout, stderr, and exit code. The
one apparent mismatch was root-caused to a capture-methodology artifact, not
an engine bug (§4.2) — a finding that directly shapes the production tool's
design. Measured corpus size (~67 KB of content across 174 sources, §5) settles
the checked-in-vs-generated trade-off the dispatch asked me to make explicit:
check it in.

## 1. What is actually lost at HeapSyn deletion

Today, three distinct things happen to depend on HeapSyn existing:

1. **`cargo test` runs the full harness corpus twice** (default bytecode, and
   `EU_HEAPSYN=1`) — CI fails if either engine's own `RESULT`/exit-code
   criterion fails. This catches divergences *from the test's own
   specification*, on both engines independently. It does **not** directly
   compare bytecode's output to HeapSyn's output — two engines each passing
   their own spec is strong but indirect evidence of agreement, not a byte
   diff.
2. **`assert_three_way`** (`src/eval/bytecode/differential.rs`, eu-ntwg.3, PR
   #1023) directly compares byte-path bytecode, pre-decoded bytecode, and
   HeapSyn on ~26 hand-written *synthetic* STG programs (small, engine-team
   authored, not real `.eu` source). This is the actual "compare engines to
   each other" mechanism today, but its coverage is deliberately narrow (unit
   tests for specific opcodes/paths), not the language surface.
3. **A handful of hand-written differential tests** compare bytecode vs.
   HeapSyn end-to-end on specific real programs — e.g.
   `tests/bytecode_io_differential_test.rs` (IO driver, ~9 expressions).

None of these amount to "every real-world-shaped `.eu` program's engine output
is checked against a second implementation." That coverage exists only
*implicitly*, as a side effect of running the same 175 harness sources through
two engines and having both happen to pass. When HeapSyn is deleted, (1)
degrades to "bytecode passes its own spec" only — still real coverage, but
no longer independently checked by a second implementation — and (2)/(3)
continue working (they don't depend on HeapSyn *existing after Phase 4*, only
on it existing *now*, to have been the reference during their construction).

**What this plan adds**: a fourth mechanism, filling the gap (1) leaves —
frozen full-output snapshots (not just pass/fail) for the entire harness
corpus, diffed on every future run, with no second engine required at diff
time.

## 2. Corpus scope

**In scope (this plan):** `tests/harness/*.eu` — the 175 sources the project
already treats as its representative language-surface corpus, each already a
`cargo test` target (`tests/harness_test.rs`). Using the existing corpus means
zero new source material to design or maintain a taxonomy for; the corpus
inherits `tests/harness/`'s existing curation (edge cases, regression tests
named after their beads, feature coverage) for free.

**Explicitly out of scope for Phase 1** (noted as future expansion, not
blocking):
- `tests/harness/errors/*.eu` — these already have their own frozen-expectation
  mechanism (`.expect` sidecars: `exit: <code>` + `stderr: "<regex>"`), which
  is itself a form of golden-output testing, just regex-based rather than
  exact-match. Folding these into the same corpus format is a natural Phase 2
  extension (§8) but the sidecar mechanism already provides *some* oracle-
  independent coverage today, so it is not urgent.
- AoC solutions (`aoc/`), doc examples (`scripts/test-doc-examples.py`) — real
  programs, but not currently part of the "cargo test gates on this" set, so
  adding them changes what CI enforces, a separate decision from this plan.
- IO-differential coverage (`tests/bytecode_io_differential_test.rs`) — already
  has its own working mechanism; folding it into the corpus format is possible
  later but the 9 cases there are already both engine-checked and frozen in
  the sense that they're explicit `assert_eq!` pairs in a checked-in test.

## 3. Capture mechanism

### 3.1 What I built and ran

`scripts/capture-oracle-corpus.sh` (prototype, throwaway — see §4 for why it's
not the production tool): for each `tests/harness/*.eu` source, runs
`eu --heap-limit-mib 2048 --lib-path tests/harness <file>` under both the
default (bytecode) engine and `EU_HEAPSYN=1`, each wrapped in
`timeout 60`. If stdout, stderr, and exit code are identical between the two
runs, the bytecode triple is written to `tests/oracle-corpus/golden/<name>.{stdout,stderr,exit}`
and recorded in `MANIFEST.tsv`; otherwise the source is logged to
`MISMATCHES.txt` and excluded.

Run against the real corpus (release binary, this worktree, 2026-07-16):

```
total=175 captured=174 mismatched=1
```

The captured data is checked in at `tests/oracle-corpus/golden/` (§5 justifies
checking it in rather than treating it as a build artifact).

### 3.2 The one mismatch, root-caused

`049_tester.eu` was flagged as a mismatch: bytecode and HeapSyn both exit 1,
but report a **different call-site trace** in the panic diagnostic (bytecode:
`α at 049_tester.eu:12:8`; HeapSyn: `beta-specifically-is-true at
049_tester.eu:19:1`).

This is a capture-methodology artifact, not an engine bug. `049_tester.eu`
defines three named test targets (`test-⊕`, `test-fail`,
`test-default-expectation`) and no default/main target
(`eu list-targets` confirms this). Naive `eu <file>` — no `-t` target, no test
mode — evaluates the file's top-level block as ordinary data for rendering,
which is not how the real test suite exercises this file:
`tests/harness_test.rs:470` calls `run_test(&opts("049_tester.eu"))`, which
goes through `tester::test()` — it discovers all three targets and drives
each one *in test mode*. In test mode, a failing `//=` returns `false` and
execution continues (`src/eval/stg/expect.rs`); outside test mode, the same
failing `//=` panics. `049_tester.eu`'s `failures` block
(`verify: [:all-values-false]`) is *designed* to contain assertions that fail
— that's the point of the target — and only survives being evaluated because
test mode catches the failure gracefully. My prototype's naive whole-file eval
bypasses test mode entirely, hits `failures.α`'s intentional failure as a raw
panic, and the two engines' otherwise-harmless difference in *which call site
gets blamed for an uncaught panic reached via an unusual, never-in-production
evaluation path* becomes visible.

Verification: driving the file the way `cargo test` actually does —
`eu test tests/harness/049_tester.eu` vs.
`EU_HEAPSYN=1 eu test tests/harness/049_tester.eu` — produces byte-identical
output on both engines (modulo an expected non-deterministic report UUID in
the "Report generated at ..." line, unrelated to engine choice). So the file
that actually ships to `cargo test` is fine on both engines; only my
prototype's simplified capture strategy tripped over it.

**This directly shapes the production tool's design (§8):** capture must
drive each named target through the same mechanism `cargo test` uses
(`tester::test()`, or equivalently `eu test <file> -t <target>` per target),
not a single whole-file `eu <file>` eval. `049_tester.eu` is excluded from
the Phase-1 prototype corpus rather than worked around, with this
explanation recorded in `tests/oracle-corpus/README.md` — it is the
production tool's job to capture it correctly, not the prototype's job to
paper over the gap.

### 3.3 Non-determinism hazards for the production tool

Beyond the target-discovery issue above, the production tool needs to handle
(known from the codebase, not yet hit in the prototype run, since the
78/103_random-style sources use explicit seeds):
- Timestamps/UUIDs in output (as seen in `eu test`'s report path above) —
  the production tool should capture *evidence* (`exit`/`stdout`/`stderr`/
  `result`, per `docs/guide/testing.md`'s custom-validator evidence keys),
  not raw report-generation side effects, and should exclude or normalise any
  field known to be run-identifying rather than program-identifying.
- Any source relying on wall-clock time or unseeded randomness (none found in
  a spot check of the 174 captured, but the production tool should assert-fail
  loudly on a source it can't capture deterministically across two consecutive
  same-engine runs, rather than silently freezing a value that will drift).

## 4. Why not just keep comparing two engines?

Because there will not be two independently-implemented engines after Phase
4. Byte-path bytecode and pre-decoded bytecode (the two dispatch modes
`assert_three_way` already compares, per eu-ntwg.3) are the **same**
implementation lineage — a decode-time transformation of the same compiled
instruction stream, not an independent re-implementation of the semantics.
Agreement between byte-path and pre-decoded bytecode is a valuable
regression net for the pre-decode transformation specifically (and
`assert_three_way` should keep doing exactly that, indefinitely — see §6),
but it cannot catch a bug present in the *shared* semantics both dispatch
modes execute identically. Only a frozen record of what an independently-
implemented second engine (HeapSyn) once agreed was correct can catch that
class of bug going forward.

## 5. Storage: check it in

Measured (§3.1's run): 174 captured sources, ~67 KB of actual content
(56,948 bytes stdout + 10,098 bytes stderr, `wc -c` across all
`.stdout`/`.stderr` files), in 522 files (174 × 3: stdout/stderr/exit, plus
the manifest). The largest single captured entry is `179_cg3_strict_recurse.stdout`
at 8,175 bytes; the rest are mostly well under 1 KB. Apparent disk usage is
1.4 MB due to filesystem block-size rounding on many small files — not
representative of git's actual (content-addressed, compressed) storage cost,
which will track the ~67 KB figure closely.

This settles the trade-off explicitly: **check the golden output in.**
Generation-on-demand (re-deriving expected output from source at test time)
is not actually available here in the way it would be for, say, a compiled
binary — there is no longer a HeapSyn to re-derive *from* post-Phase-4; the
entire point is that the frozen values must be captured and persisted *now*,
while HeapSyn still exists to have validated them. The only real alternative
to checking in the outputs would be checking in nothing and accepting the
oracle gap — not checking in the *sources* instead, since the sources
(`tests/harness/*.eu`) are already checked in and are not what's at risk.

**Escalation policy for future growth:** if the corpus scope expands (§2's
"explicitly out of scope" items — AoC solutions, larger data-processing
fixtures) and any single captured output exceeds roughly 100 KB, that entry
should be reconsidered individually (e.g. hash-and-length rather than full
content, or excluded with a documented reason) rather than changing the
default policy for the whole corpus. At the current scale this is a
non-issue: the largest single captured file today is 8,175 bytes
(`179_cg3_strict_recurse.stdout`) — well under the proposed 100 KB threshold.

## 6. What validates future engine changes, after Phase 4

Four independent mechanisms, each covering a different failure class, replace
"run it on HeapSyn too":

1. **This corpus** (frozen full-output diff) — catches any output change,
   whether or not the affected harness test's own `RESULT` computation would
   have noticed. The primary deliverable of this plan.
2. **Each harness test's own `RESULT`/`.expect` criterion** — unaffected by
   Phase 4, continues to assert the *objectively correct* value independent
   of engine identity. Already exists; not new coverage, but not lost either.
3. **`assert_three_way`** (`differential.rs`) — continues comparing byte-path
   vs. pre-decoded bytecode dispatch on its ~26 synthetic programs. Doesn't
   depend on HeapSyn's continued existence (HeapSyn is one of its three
   modes today, but the mechanism and its two bytecode-only modes survive
   its removal as a two-way check). This is engine-*construction*
   correctness (does a dispatch-mode transformation preserve semantics), a
   different and complementary concern to this plan's engine-*semantics*
   correctness (does the shared semantics stay correct over time).
4. **Retained property/fuzz targets** (`tests/property_test.rs`'s
   `gc_properties::*`, `fuzz/`) — structural invariants (GC safety, parse/
   render round-trips, subtyping properties) that were never engine-comparison
   based and are unaffected by Phase 4. The transition review's gate
   explicitly calls for retaining these; this plan does not touch them, and
   they should not be pruned as part of the Phase-4 collapse.

## 7. Update policy

Regenerating the frozen corpus changes what every future run is checked
against — it must never be a side effect of an unrelated change. Policy for
the production tool and for whoever operates it:

- A corpus update is its own reviewed commit, with a stated reason (e.g. "PR
  #NNNN legitimately changes rendering of X; regenerating N of the 174
  affected entries").
- A corpus *diff* surfacing in a PR that isn't explicitly about updating the
  corpus is a signal to stop and investigate, not to regenerate and move on —
  this is the entire point of freezing it.
- The regeneration command must show its diff before committing (this is not
  a new constraint on the tool the plan needs to design further — `git diff`
  on the checked-in `tests/oracle-corpus/golden/` already does this for free,
  which is part of why checking the output in, rather than hiding it behind a
  build step, is the right call structurally, not just size-wise).

## 8. Follow-up work (new bead, not part of this PR)

The production implementation, gated on owner sign-off of this plan:

1. **Production capture tool** (`cargo xtask oracle-corpus-capture` or
   similar), replacing `scripts/capture-oracle-corpus.sh`: drive every named
   target in every `tests/harness/*.eu` source through `tester::test()`
   (matching `cargo test`'s own invocation, per §3.2's finding), capturing the
   evidence keys `docs/guide/testing.md` already defines (`exit`, `stdout`,
   `stderr`, `result`) rather than a raw whole-file eval; add `049_tester.eu`
   correctly under this mechanism.
2. **Conformance test suite** (e.g. `tests/oracle_conformance_test.rs`):
   iterates `tests/oracle-corpus/golden/`, re-runs each entry the same way it
   was captured, diffs against the frozen record, fails on any divergence.
   Wired into `cargo test` so it runs on every CI build, independent of engine
   flag.
3. **Fold in `tests/harness/errors/*.eu`** (§2) using the same corpus format,
   once (1)/(2) exist — subsumes the `.expect` sidecar mechanism or runs
   alongside it.
4. **eu-4zhi gate linkage**: the transition review's Phase-4 gate also
   requires "the eu-4zhi lookup-class decision (written waiver naming cause +
   post-retirement plan, OR an index fix)". eu-4zhi's own bead notes already
   contain the substance of a waiver (measured ~0 realised upside, explicit
   defer-until-real-evidence reasoning, "Parked" status in the review §7);
   formalising that into the explicit written-waiver form the gate names is
   a small, separate task, out of this plan's scope but worth doing
   alongside the follow-up bead so Phase 4's non-performance blockers are
   fully closed together.

I will file this as a new bead (child of eu-ntwg or eu-2sa6, owner's call)
immediately after this PR is up, referencing this design doc.

## 9. Open questions for owner sign-off

- Does the corpus scope (§2: `tests/harness/*.eu` only, Phase 1) match intent,
  or should `errors/*.eu` be pulled into Phase 1 rather than deferred?
- Is checking the golden output into `tests/` (rather than, say,
  `docs/superpowers/`) the right location? I chose `tests/` because it is
  primarily test fixture data consumed by the future conformance suite, not
  documentation, but this is a naming/placement call, not a substantive one.
- Should the follow-up implementation (§8) be a single bead or split (capture
  tool / conformance suite / errors-folding / eu-4zhi waiver as four
  separately-sequenced items)? I lean toward one bead with four tracked
  sub-tasks, mirroring how eu-ntwg's own children are structured, but defer to
  the owner's preferred granularity.
