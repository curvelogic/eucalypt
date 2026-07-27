# Diagnostic snapshot baselines

Frozen captures of eucalypt's full diagnostic output at specific refs, plus the
comparisons between them. This is the evidence base for any claim that
diagnostics improved between two releases.

## The files

| file | what it is |
|---|---|
| `0.13.2.snapshots.txt` | Every corpus fixture as rendered by `eu` built from tag **0.13.2** (released 2026-07-22). |
| `master-a1af1f0b.snapshots.txt` | The same corpus as rendered by **master `a1af1f0b`** (0.14.0-dev, 2026-07-27) — the state *before* the pending 0.14 diagnostics fixes landed. |
| `0.13.2-vs-master-a1af1f0b.md` | Generated comparison of the two. |

**These captures are immutable historical records. Do not regenerate them.**
Add a new file for a new ref instead. The live goldens under
`tests/diagnostics/snapshots/` are the moving copy; these are the fixed points
a release claim is measured against.

## Reading a baseline

Both files use the same format as a `.snap` golden, concatenated one fixture per
section and sorted by fixture id, so the whole history question reduces to one
`diff`:

```bash
diff -u docs/superpowers/diagnostics-baselines/0.13.2.snapshots.txt \
        docs/superpowers/diagnostics-baselines/master-a1af1f0b.snapshots.txt
```

For the tabulated form, with per-fixture fact deltas and the two renderings side
by side:

```bash
cargo xtask diag-snapshot --compare \
    docs/superpowers/diagnostics-baselines/0.13.2.snapshots.txt \
    docs/superpowers/diagnostics-baselines/master-a1af1f0b.snapshots.txt \
    --mode source --out /tmp/report.md
```

## Capturing a new baseline

```bash
# From a git ref — builds `eu` at that ref in a throwaway worktree under target/
cargo xtask diag-snapshot --capture --from-ref 0.14.0 \
    --out docs/superpowers/diagnostics-baselines/0.14.0.snapshots.txt

# Or from a binary you already have
cargo xtask diag-snapshot --capture --binary ./target/release/eu \
    --label "master abc1234" --out /tmp/capture.txt
```

The **corpus** always comes from the current checkout; only the **binary** comes
from the ref. That is what makes a retroactive capture possible: an older `eu`
embeds its own prelude at its own compile time, so running it against today's
fixtures measures that release's diagnostics, not a hybrid.

`--from-ref` builds in a **separate worktree** at `target/diag-baseline/<ref>`,
never a checkout or a stash of the current one. `refs/stash` lives in the common
git directory and is shared by every worktree of the repository, so stashing to
move between refs can restore another worktree's work into the tree the baseline
is captured from — silently, and with no error. The worktree is kept between
runs so a repeat capture does not pay for a full release build; remove it with
`git worktree remove target/diag-baseline/<ref>` when you are finished.

To confirm a committed baseline was not captured from a contaminated tree,
re-capture it and compare: the bundles are deterministic and must be
byte-identical.

```bash
cargo xtask diag-snapshot --capture --from-ref 0.13.2 --out /tmp/recheck.txt
cmp /tmp/recheck.txt docs/superpowers/diagnostics-baselines/0.13.2.snapshots.txt
```

Captures taken with an external binary are recorded `--source-prelude` only.
Whether an arbitrary binary embeds a verified prelude blob is not knowable from
outside it, and recording a second source-mode run as if it were the blob mode
would be a falsehood in the historical record. Compare baselines with
`--mode source` accordingly.

## What a comparison does and does not establish

**Does**: that the rendered output changed, exactly how, and how the objective
properties (primary label in a user file vs the prelude vs absent, trace frames
in user files vs the prelude, prelude source lines excerpted, presence of a
stable error code, Rust panics) moved.

**Does not**: that the diagnostics are *good*, or that a given change is an
improvement. Several deltas across a release boundary are new features rather
than better diagnostics — a stable error code appears because the structured
diagnostic model did not exist at 0.13.2, not because that message got clearer.
Some are ambiguous: nine fixtures traded a meaningless prelude primary for *no*
primary, which satisfies the "never blame the prelude" invariant while leaving
the user with no location at all.

There is no validated automatic metric for diagnostic quality, and LLM-as-judge
specifically failed to predict human outcomes (Santos & Becker, *Not the Silver
Bullet*, UKICER 2024). The comparison tables are counts; the per-fixture diffs
are the evidence; a human reading them is the verdict.
