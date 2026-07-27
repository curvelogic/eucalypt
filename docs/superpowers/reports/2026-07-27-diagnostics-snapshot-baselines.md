# Diagnostic snapshot baselines: 0.13.2 → 0.14-dev

**Date**: 2026-07-27
**Task**: eu-1tkk.7.4 (golden snapshots + `--bless` + prelude-source suppression)
**Epic**: eu-1tkk.7 — 0.14 Diagnostics Overhaul

This report accompanies the baselines in
`docs/superpowers/diagnostics-baselines/` and interprets them. It exists because
the 0.14 release claim is that diagnostics *greatly improved*, and a claim of
improvement needs a "before".

## What was captured, and why now

Four diagnostics fixes were approved but unmerged when this was written
(**#1077**, **#1079**, **#1080**, and eu-og3u6 in progress). Once they land,
"before" becomes unreconstructable in practice. Two captures were therefore
frozen first:

| capture | ref | what it is |
|---|---|---|
| `0.13.2.snapshots.txt` | tag **0.13.2**, released 2026-07-22 | the last thing users actually installed |
| `master-a1af1f0b.snapshots.txt` | master **a1af1f0b**, 2026-07-27 | 0.14-dev *before* the pending fixes |

Both were taken against the **same corpus** (195 fixtures from this checkout)
with the **same invocation**; only the binary differs. An older `eu` embeds its
own prelude at its own compile time, so this measures that release's
diagnostics rather than a hybrid. Debug and release builds were verified to
produce byte-identical diagnostics across all 195 fixtures, so the release-built
baselines are comparable with the debug-built goldens `cargo test` produces.

## Headline numbers

Over the **93 fixtures whose output changed** (102 of 195 are unchanged):

| property | 0.13.2 | master a1af1f0b |
|---|---|---|
| prelude source lines excerpted into diagnostics | **198** | **0** |
| fixtures whose primary label points into the prelude | **21** | **0** |
| trace frames pointing into the prelude | **99** | **9** |
| trace frames pointing into a user file | **9** | **83** |
| total trace frames | 108 | 92 |
| fixtures carrying a stable error code | 0 | 17 |
| fixtures with a primary label in a user file | 72 | 84 |
| fixtures with no primary label at all | 0 | **9** |
| fixtures ending in a Rust panic | 0 | 0 |

The trace numbers are the substance of the epic's Phase 2 claim: traces used to
be 92% prelude frames and are now 90% user frames, while getting *shorter*.

## What that looks like

`tests/diagnostics/corpus/nth_out_of_range.eu` — a two-line program,
`result: xs nth(10)` on a three-element list.

**0.13.2**

```text
error: tail of empty list
     ┌─ [prelude]
     <4 prelude source line(s) suppressed>
     = guard against empty lists with 'nil?', e.g. 'if(xs nil?, default, xs head)'
     = 'head' and 'tail' are only defined on non-empty lists; use 'nth(0, xs)' or pattern matching if the list may be empty
     = stack trace:
       - nth at [prelude]
       - drop at [prelude]
       - drop at [prelude]
       - drop at [prelude]
         ... 10 frames elided (6× repetition)
       - drop at [prelude]
```

**master a1af1f0b**

```text
error: panic: index 10 out of range for list of length 3
  ┌─ tests/diagnostics/corpus/nth_out_of_range.eu:2:12
  │
2 │ result: xs nth(10)
  │            ^^^
  │
  = stack trace:
    - result at nth_out_of_range.eu:2:12
    - in 'nth' at [prelude]
```

Not merely better-located: 0.13.2 reported the *wrong error* ("tail of empty
list" — an internal consequence of `nth`'s implementation), pointed at prelude
source, and spent its entire trace inside `drop`.

`tests/harness/errors/109_unknown_arg.eu` is the extreme case: a 15-frame
all-prelude trace with a 17-line prelude source excerpt becomes a one-frame
user-anchored trace pointing at the user's own `parse-args` call.

The other 91 changed fixtures are in
`docs/superpowers/diagnostics-baselines/0.13.2-vs-master-a1af1f0b.md`, each with
both renderings in full.

## What did not improve

**Nine fixtures traded a meaningless location for no location.** `047`, `072`,
`094`, `106`, `115`, `124`, `125`, `142` and `168` moved from `primary: prelude`
to `primary: none`. That satisfies objective invariant (i) — never blame the
prelude — but only by deleting the label. Mostly IO-permission and
render-format errors raised outside evaluation, where no user Smid is threaded
through. Filed as **eu-1tkk.7.21**; it should not be counted as a win.

**One fixture ends in a raw Rust panic.** `171_yaml_large_uint` aborts with
`thread '<unnamed>' panicked at src/export/yaml.rs`, a `RUST_BACKTRACE` note and
`main thread panicked: Any { .. }`. It has been green all along because its
`.expect` sidecar asserts only `exit: 101` and a substring the panic happens to
contain. Filed as **eu-1tkk.7.20** (P1). This is the clearest single
demonstration of why full-output snapshots beat regex sidecars.

**Blob and source prelude still disagree on 13 of 195 fixtures.** See
`tests/diagnostics/DIVERGENCE.md`. Some of these are the eu-7x0r work in flight
(#1079, unmerged at capture time); `errors/143_bitwise_float` is a case where
the *blob* path — the one released binaries use — has no source location while
`--source-prelude` does.

**The many-declaration fixtures are red on purpose.** In blob mode
`many_decls_late_nth` blames `pad0470` and names three unrelated `padNNNN`
declarations in its trace for a failure in `result`; `many_decls_early_nth`
shows that the "correct in a small file" property does not survive into blob
mode at all. These are eu-og3u6 and the eu-7x0r aliasing class, recorded rather
than asserted, so that the snapshot diff when they are fixed is the proof.

## What these snapshots will and will not prove

**Will**: that a diagnostic changed, exactly how, and how its objective
properties moved — where the primary label points, how many trace frames are in
user files versus the prelude, whether prelude source is being excerpted,
whether a stable error code is present, whether the process panicked. All of it
mechanically derived and reproducible from checked-in data.

**Will not**: that a diagnostic is *good*, or that any given change is an
improvement. Three specific caveats:

1. Some deltas are **features, not quality**. Seventeen fixtures gained a stable
   error code because the structured diagnostic model did not exist at 0.13.2 —
   not because those messages got clearer.
2. Some deltas are **ambiguous**, as the nine lost locations show. Direction has
   to be read, not counted.
3. The corpus is **our fixtures, not real user programs**, and success is
   measured against invariants we chose. It says nothing about whether a person
   hitting one of these errors can now fix their program faster. The design
   spec's honesty guardrail is explicit that no validated automatic quality
   metric exists and that LLM-as-judge scores specifically failed to predict
   human outcomes (Santos & Becker, *Not the Silver Bullet*, UKICER 2024). Under
   that guardrail these snapshots are evidence for review, not a score.

The right reading of this report is: **the mechanical properties the epic set
out to fix moved decisively in the intended direction, three named defects
remain open with beads against them, and the per-fixture diffs are checked in so
the claim can be audited rather than taken on trust.**

## Reproducing

```bash
# The comparison in this report
cargo xtask diag-snapshot --compare \
    docs/superpowers/diagnostics-baselines/0.13.2.snapshots.txt \
    docs/superpowers/diagnostics-baselines/master-a1af1f0b.snapshots.txt \
    --mode source --out /tmp/report.md

# The same, against the current tree rather than a frozen baseline —
# this is the command that shows what the pending fixes bought
cargo xtask diag-snapshot --compare \
    docs/superpowers/diagnostics-baselines/master-a1af1f0b.snapshots.txt \
    tests/diagnostics/snapshots \
    --mode source --out /tmp/since-baseline.md
```

Mechanism and format: `tests/diagnostics/SNAPSHOTS.md`.
