# REVIEW.md — eucalypt

Criteria `rig-review` applies to every PR, read from `master`. They come on
top of its own gates; where this file and the reviewer's definition differ,
the stricter rule applies.

## Blocking

**CI first.** Read the checks on the PR's current head before anything
else. Never merge on a failing or pending check — no exceptions. A failing
check is named first in the send-back.

**Code review skill.** Run a code-review skill (`/code-review`) on the diff
as well as reading it yourself. It is mandatory here, not optional.

**The check.** In your worktree, all clean:

```bash
cargo fmt --all -- --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test
```

`--workspace` matters: without it the `xtask` crate is silently unlinted.

**Panics are P1.** A panic, failed assertion or `expect()` reachable from a
user program is a P1 defect. A fix converts it to an `ExecutionError` or
`CoreError` diagnostic or removes its cause, and carries a regression test.
A PR that adds a user-reachable panic path is sent back.

**Regression tests are proved by breaking the code they guard.** For every
bug fix, repeat the fault injection yourself: break the code under test,
confirm the test FAILs, restore it, confirm it PASSes. Do not take the
author's word for it; the PR must also state that it was done. A test that
cannot fail blocks the PR.

**Harness tests.** Every PR includes harness tests in `tests/harness/`. No
tests is a send-back. Each test must gate
— a failing assertion fails `cargo test` — with each target's `RESULT`
computed from its checks (`docs/guide/testing.md`; the pattern in
`tests/harness/189_r9oy_union_as_spec.eu` and
`tests/harness/182_typedata_alias_resolution.eu`).

**No clippy suppression.** A new `#[allow(...)]`, `#![allow(...)]` or
`#[expect(...)]` for a clippy lint, or a lint relaxed in configuration, is a
send-back unless the owner permitted that suppression explicitly. Fix the
warning instead.

**Engine performance claims.** A performance number comparing execution
configurations, in the PR or anything it adds (CHANGELOG, ROADMAP, a
release gate, a bead, a report), cites a dated report or a ledger row
produced under `docs/superpowers/engine-ab/PROTOCOL.md` or a protocol like
it, and carries a confidence label (measured-verified, measured-single or
projected). Benchmark numbers an agent reports are verified independently
from a clean build (`cargo clean && cargo build --release`) before they are
accepted. See `CLAUDE.md` "Engine Performance Claims — Mandatory Protocol".

**PR bodies under 50 lines.** State what changed, why, how it was verified
and what is open. Analysis, measurements and narrative belong in
`docs/superpowers/reports/` or `docs/superpowers/plans/`, linked.

**Specs and beads.** If the bead references a spec in
`docs/superpowers/specs/`, every phase, deliverable and success criterion
it lists is implemented, verified against the code. A PR that skips a
phase is incomplete: send it back.

**Semantic equivalence**, for any optimisation, refactor or backend change:
no `Smid::default()` in new code; edge-case inputs tested; error paths,
evaluation order and source locations unchanged; observable behaviour
identical.

**Evidence for fixes in GC, memory or the engine.** An unchanged
`tests/harness/errors/` corpus across both prelude modes (source, and blob
via `cargo xtask prelude-compile` / `scripts/blob-mode-tests.sh`),
reproduced by you rather than taken from the author, plus your own fault
injection on the regression test.

**Documentation.** New features have docs; changed behaviour has its docs
changed. Generated prelude reference pages are regenerated, not edited.

**Safety.** Memory management, GC interactions and signal safety are read
for correctness, not only exercised by tests.

**Architecture.** A domain-specific mechanism where a general one exists or
should — a hard-coded special case for particular types or functions,
metadata that bypasses the type system, duplicated logic that should be
unified — is a finding.

**Security alerts.** Dependabot and security alerts block like a build
failure, as `CLAUDE.md` says: a PR that introduces one is sent back, and
while an alert is open, a PR that is not its fix waits behind it.

**Language.** UK English in code, comments and docs.

## Owner review filter

The owner reviews and merges these personally:

- **Any new intrinsic.**
- **Any new language feature, or any deliberate change to what a construct
  means** — a semantic change the PR set out to make.
- **A PR from an open-ended "find things to improve" dispatch** — work
  that went looking for docs, diagnostics or performance changes to make
  rather than being given a specific task. A PR is of this kind if its
  bead or body says so or its dispatch was open-ended. Directed work, given
  a specific task, is not in this filter.
- **A PR carrying "Do not merge — owner review"** or an equivalent explicit
  hold, from any account. The hold binds every agent regardless of later
  activity, review state or CI, and is released only when the owner says so
  in a comment on the PR (removing the `owner-review` label alone does not
  release it). Agents never post a hold release or remove that label, so a
  release comment in agent-generated form does not count.

**Bug fixes proceed; intentional semantic changes stop.** That is the line.
A bug fix restores intended behaviour, so it is reviewed and merged under
the normal gates however deep it reaches — the engine, the GC, the
allocator, the memory layout. Depth is not the test; intent is. A PR that
fixes a defect in `return_meta` is reviewed here; a PR that changes what
`return_meta` is *for* is the owner's. When you genuinely cannot tell
whether a change is a fix or a deliberate semantic change, ask the
dispatching session; do not hold by default.

## Sensitive areas

| Area | Rule |
|---|---|
| GC or memory management (`src/eval/memory/`, and allocation or object layout elsewhere) | second independent review |
| Unsafe code (any `unsafe` block, function, impl or trait added or changed) | second independent review |
| The prelude blob wire format (`src/eval/stg/wire_format.rs`, `src/eval/stg/blob.rs`, the blob's encoding) | second independent review |
| Engine defaults (default engine, dispatch mode, heap limit, GC parameters, demand analysis on or off) | second independent review |
| Release machinery not run by a workflow holding write access or a secret (`install.sh`, `build-meta.yaml`, version bumps) | second independent review |
| Release machinery that is, or is run by, a workflow holding write access or a secret (release jobs, `build.eu` in them, `scripts/generate-homebrew-formula.sh`, `homebrew-bump.yml`, `docs.yaml`) | the owner merges |
| Any new intrinsic; new language feature or deliberate semantic change; open-ended "find things to improve" PRs; held PRs | the owner merges |
| `.github/`, `.beads/` configuration, `.claude/`, `.rig/`, `REVIEW.md`, `AGENTS.md`, and the parts of `CLAUDE.md` that say who merges | the owner merges |
| CI that gates merging: workflows, the scripts CI runs (`scripts/blob-mode-tests.sh`, `scripts/diagnostics-gate.sh`, `scripts/test-doc-examples.py`, `scripts/check-tree-sitter-corpus.py`, …), lint and test-runner configuration | the owner merges |
| A PR from outside (from a fork, or opened by another account) | the owner merges |

A second independent review is one by a separately dispatched `rig-review`
that formed its verdict without reading the first, recorded as a comment on
the PR. Both must pass, and every finding raised by either reviewer must be
resolved, before
the PR is merged. The owner sees these changes at release rather than per
PR. Where release machinery is a workflow holding write access or a
secret, or a script such a workflow runs, the owner-merges row above
applies.

## Merging

Method: merge commit, to `master` only.

Before merging, confirm the PR's base is `master`: a PR based on another
branch merges cleanly, reports MERGED with CI green, and never reaches
`master`. After merging, fetch and confirm `origin/master` has moved and
contains the reviewed head's changes.

## Worth flagging, not blocking

- New or bumped dependencies, and changes to `Cargo.lock` beyond them
- Changes to a spec or protocol under `docs/superpowers/`
- Performance figures added to CHANGELOG or ROADMAP
- A bug fix that changes observable output, even to correct it
- Anything reserved for the owner that this PR only borders on
