# Argument blame: warning linkage (eu-1tkk.7.40) and the wrong-sibling caret (eu-1tkk.7.39)

2026-08-01 · Clarion

Two beads paired on the theory that the type checker's argument-level
knowledge could supply what runtime blame lacks. 7.40 landed. 7.39 did not,
and this records why, with the measurements.

## Are the warnings in hand at error-render time?

No. They are not merely dropped after emission — they are produced in a
**different `SourceLoader` entirely**.

`src/bin/eu.rs` runs `run_type_checker` / `run_type_checker_from_blob_core`,
each of which builds its own `SourceLoader`, and therefore its own `SourceMap`
and its own `SimpleFiles`. `PipelineCheckResult` is consumed inline to render
warnings and then falls out of scope; the evaluator that follows is built from
the *other* loader.

The consequence that shapes the whole design: **a `Smid` from the check pass
is meaningless to the evaluator, and so is a file id.** Matching had to key on
something stable across both, which is the source text — hence `(file name,
byte span)`, resolved on the check side while its `SourceMap` is still alive.
The bead's suggestion of "same Smid" matching is not available.

`--suppress-type-warnings` turned out to be simpler than feared: on the
evaluate path the whole check block is inside `if !opt.suppress_type_warnings()`,
so the checker does not run at all and there is nothing to cite. (This differs
from CLAUDE.md's "the checker still runs", which holds for `eu check`.)

## What 7.40 built

`TypeWarning` gains an optional `CallSite { head, app }`, recorded only by
argument- and overload-mismatch emission. `driver::warning_link` resolves those
to source coordinates; the `Executor` holds them for the run and, when
rendering an error, adds a secondary label on each flagged argument carrying
the checker's own `expected …, found …` text.

Shape (b) from the bead only. Shape (a) — a note saying "see the type warning
above" — was not implemented: notes and hints are outside Clarion's remit, and
(b) makes (a) unnecessary, because the error carries the finding itself rather
than pointing at output that may never have been printed.

Matching is byte-identical span, same file, against either the error's primary
label or a frame of the curated stack trace. The trace frames matter: they are
what carries the finding across a call boundary. In a four-file chain the error
is raised in the leaf and the finding is promoted onto the call in the root
file, three imports away.

Corpus effect: 11 of 214 fixtures gained a caret on the argument responsible;
none lost anything. `errors/error_175` is representative — `"not_a_list"
sort-nums` blamed `sort-nums` and now also marks the receiver that supplied the
string.

## Why 7.39 did not land

### The specimen produces no type warning

`eu check` on the four-file specimen is silent. The string arrives from *data*
(`width: "8"`), which gradual typing cannot see. So the pairing hypothesis
fails on the bead's own specimen: 7.40 cannot unlock it. (It does help the
adjacent case where the mistake is a swapped parameter order, which the checker
does catch.)

### Root cause is a stale annotation register, not argument identity

The error's own Smid is `machine.annotation()` — a single "where am I"
register. `EU_ERROR_TRACE_DUMP` shows `error smid: smid=63 … span=71..76`,
which is `shape` in `shape.units`: the sibling argument, because it was the
**last thing forced** before the multiplication ran.

Confirmed by deleting the use of `unit` from the callee, so nothing forces
`shape.units`: the caret immediately moves off the innocent sibling. The bead's
hypothesis (blame picks the App node's only explicit argument) is not what
happens; blame picks whatever was evaluated most recently.

The mechanism: `BcContinuation::Branch`/`BranchPredecoded` capture
`annotation: state.annotation` when the `case` is pushed and **never restore
it** on return. `SeqBind` and `ApplyTo` both do. So after a `case` forces a
scrutinee, the register holds the scrutinee's location. Same omission in the
HeapSyn engine (`vm.rs`), which is why 0.13.2 behaves identically.

### The obvious fix trades one wrong blame for another

Restoring the case-site annotation on return (all six arms, guarded on the
annotation being real) was implemented and measured against the 214-fixture
corpus. **7 fixtures moved: 3 better, 2 clearly worse, 2 marginal.**

Better — `errors/080_array_oob` and `errors/082_array_reshape_mismatch` stop
blaming `arr.from-flat` and blame `arr.get` / `arr.reshape`, the call that
actually failed. The 7.39 specimen stops blaming `shape.units`.

Worse — `provocations/metadata_span`:

```eu,notest
scale(factor, xs): xs map(* factor)
doubled(xs): scale("2", xs)
```

blame moves from line 3 (which contains the mistake, `"2"`) to line 2 (the
callee's definition). And `errors/149_not_value_source_loc` — a fixture whose
whole purpose is pinning the call-site location — degrades from `str.of` to a
six-line caret over the entire file.

The two directions are the same mechanism seen twice. The register holds the
annotation of the **last-forced thunk**. In `metadata_span` the last-forced
thunk *is* the offending value, so stale blame is excellent. In the 7.39
specimen the last-forced thunk is an innocent sibling, so stale blame is
terrible. It is a lottery, and restoring the case site does not win it — it
just biases blame inward (toward the callee) instead of outward.

### Verdict

Not tractable as a local change. What is actually wanted is "blame the
expression that produced the offending *value*", which is value provenance —
eu-1tkk.7.13/.14 — and the bead explicitly distinguishes itself from those on
the grounds that the defect is present even when provenance is irrelevant. The
measurement says otherwise: provenance is exactly what is missing, because
without it there is no way to tell the guilty last-forced thunk from the
innocent one.

The experiment was reverted. The one durable artefact is the diagnosis above,
in particular the `Branch` restore gap, which is a real asymmetry with
`SeqBind`/`ApplyTo` and worth revisiting once provenance exists to arbitrate.
