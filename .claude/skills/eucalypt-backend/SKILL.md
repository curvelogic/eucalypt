---
name: eucalypt-backend
description: Working on eucalypt's backend — the STG compiler, the bytecode engine, the garbage collector and memory management, or intrinsics (src/eval/, src/driver/ evaluation and io-run loop). Use before changing, reviewing or debugging code there, including any GC, heap, allocator or memory-layout change.
---

# eucalypt backend: STG, bytecode engine, GC, intrinsics

## Scope

- `src/eval/stg/` — STG compiler (`compiler.rs`), syntax, intrinsic
  implementations, the prelude blob wire format (`wire_format.rs`, `blob.rs`)
- `src/eval/bytecode/` — the bytecode engine, the sole execution engine
  (`machine.rs`, `opcode.rs`, `predecode.rs`)
- `src/eval/memory/` — heap, GC, allocation, object layout
- `src/eval/intrinsics.rs`, `src/eval/error.rs` — intrinsic catalogue and
  execution errors
- `src/driver/` — evaluation driver and io-run loop

## Read first

- `CLAUDE.md` — pre-commit checklist, debug environment variables, `eu dump`
  phases, "Panics Are Critical", "Review and merge"
- `docs/development/gc-implementation.md` and
  `docs/development/gc-verification-spec.md` — before any change under
  `src/eval/memory/`. The GC is subtle: read the code carefully as well.
- `docs/development/prelude-blob.md` — before touching the blob or its
  wire format
- `src/eval/stg/compiler.rs`, `src/eval/bytecode/machine.rs`
- The bead (`bd show <id>`) and any spec it references in
  `docs/superpowers/specs/`: every phase and success criterion must be met
  before the work is called complete

## Rules

- Validate every backend change under `EU_GC_VERIFY=2` and `EU_GC_STRESS=1`
  (`cargo build --release`, then
  `EU_GC_VERIFY=2 EU_GC_STRESS=1 timeout 600 ./target/release/eu test --allow-io tests/harness`),
  as CI's GC-verification job does.
- A bug fix touching GC, memory or the engine shows the `tests/harness/errors/`
  corpus unchanged in **both** prelude modes (source, and blob via
  `cargo xtask prelude-compile` / `scripts/blob-mode-tests.sh`).
- Semantic equivalence for any optimisation or refactor: no `Smid::default()`
  in new code; edge-case inputs tested; error paths, evaluation order and
  source locations unchanged; observable behaviour identical.
- A new intrinsic is a change the owner reviews (`REVIEW.md`); a fix to an
  existing one is not. Bug fixes proceed; intentional semantic changes stop.
- GC or memory management, unsafe code, the blob wire format and engine
  defaults are sensitive areas with a review rule of their own (`REVIEW.md`).
- Convert user-reachable `panic!`/`expect()` into `ExecutionError`
  diagnostics; every panic fix carries a regression test.
- Harness tests must gate and be fault-injection verified (`CLAUDE.md`
  "Writing harness tests that gate"); say in the PR that you did it.
- Use `eu dump` phases to investigate, not temporary debug prints; wrap
  every `eu` run in `timeout`.
- Work in a worktree branched from `master`, one bead per PR, PR to
  `master`.
- Review and merge: a fresh `rig-review` against `REVIEW.md` on `master`.
