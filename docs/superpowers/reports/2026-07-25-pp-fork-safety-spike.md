# PP fork-safety feasibility spike — report

**Date:** 2026-07-25 · **Bead:** eu-u9xj.6 (spec §9 gate) · **Agent:** furnace
**Spec:** `docs/superpowers/specs/2026-07-25-pp-parallelism-design.md` (branch `docs/pp-parallelism-spec`, PR #1071)
**Prototype:** `examples/pp_fork_spike.rs` on branch `spike/pp-fork-safety` (THROWAWAY — not for merge)

## Verdict

**GO-WITH-RISKS** — the spec §4 COW-fork mechanism is sound on the evidence:
the managed Immix heap, the HeapSyn `Machine`, and the GC (including forced
evacuation under `EU_GC_STRESS=1`) survive `fork()` unchanged in child and
parent, values cross an anonymous `MAP_SHARED` arena correctly, and four
concurrent COW workers reproduce the exact par-sum shape deterministically.
The risks (register below) are all implementation rules or scoped follow-ups,
none structural. The one genuine correctness hazard found is **live lazy
producers** (streaming imports) sharing an fd offset across forked workers —
v1 must refuse or drain them at the fork boundary.

## What was built and run

`examples/pp_fork_spike.rs` (unix-only example binary, ~600 lines, throwaway),
four phases, each its own `fork()`:

- **Phase A — heap-level COW fork.** A live `Heap` with 50 000 rooted number
  atoms plus garbage; one pre-fork collection (baseline green); fork; the
  child allocates 5 × 100 000 further objects into the inherited COW heap,
  runs 5 more full collections (each with `EU_GC_VERIFY=2` structural
  checkpoints), checksums the inherited roots and writes checksum + string
  through the arena; the parent waits, verifies the checksum, then collects
  and re-checksums its own copy to prove the child's GCs did not perturb the
  parent heap.
- **Phase B — full VM COW fork.** A `Machine` built via `standard_machine`
  with the complete standard runtime globals loaded, run to termination
  (quiescent point per spec §4); fork; the child re-enters the inherited
  machine 20 000 times (load fresh STG → evaluate → numeric result), with a
  4 MiB heap limit so the in-run GC policy fires constantly against real
  machine roots; result sum + GC count returned via the arena; parent
  verifies the sum arithmetically and then runs 5 000 further evaluations on
  its own machine copy post-join.
- **Phase D — W=4 concurrent workers (the actual par-sum shape).** One
  inherited machine, four simultaneous forked workers, each evaluating a
  contiguous 5 000-item chunk in its own COW heap and writing exactly one
  partial into its index-addressed arena slot; parent joins all four,
  combines partials in worker-index order, asserts equality with the
  sequential expectation, then proves its own machine still works.
- **Phase C — costs.** Median fork+waitpid round trip (20 samples) and
  ns/record for 1 000 000 × 16-byte length-prefixed writes into the shared
  arena, read back by the parent.

The process mimics the real `eu` binary's thread shape before forking:
`install_crash_handler()` (the unconditional SIGSEGV/SIGBUS handler),
a `ctrlc` handler thread, and a parked thread standing in for the
join-blocked initial thread (`src/bin/eu.rs` spawns `run` on a 64 MiB
stack and blocks in `join`).

A `--nofork` mode runs the *identical* workloads inline in one process — the
control that separates fork-induced GC effects from artefacts of the workload
or verification machinery.

### Commands and observed output

Build: `cargo build --release --example pp_fork_spike` (clean master +
spike example only; `cargo fmt --all` and
`cargo clippy --workspace --all-targets -- -D warnings` clean;
baseline `cargo test --release` green before any work).

**Run 1 — verify+poison, forked (the §9 minimal case, extended):**

```text
$ EU_GC_VERIFY=2 EU_GC_POISON=1 timeout 300 ./target/release/examples/pp_fork_spike
pp_fork_spike: EU_GC_VERIFY=2 EU_GC_POISON=true
== Phase A: heap-level COW fork (fork=true) ==
[parent] pre-fork: 50000 roots, checksum ok, 1 collections, verify level 2
[parent] phase A OK: child ran 6 collections (verify level 2), arena round-trip ok, parent heap intact post-join
== Phase B: full VM COW fork (fork=true) ==
[parent] pre-fork: machine live, sanity compute ok, 0 collections
[parent] phase B OK (fork): child ran 20000 iters / 200011 ticks / 17476 machine GCs (parent had 0), correct sum through arena, parent machine intact post-join
== Phase D: W=4 concurrent COW workers (par-sum shape) ==
[parent] phase D OK: 4 workers x 5000 items in 3.603219917s, worker GC counts [2475, 2475, 2475, 2475], partials combine to expected, parent machine intact
== Phase C: fork + serialisation cost ==
[parent] fork+waitpid round-trip median: 646.25µs (min 577.167µs, max 876.792µs)
[parent] arena write: 1000000 x 16B records in 1.180375ms (1.2 ns/record); read back in 436.25µs (0.5 ns/record)
pp_fork_spike: ALL PHASES PASSED   (exit 0)
```

**Run 2 — no-fork control, same env:** identical GC behaviour in every
number that matters — 6 collections phase A, 17 476 machine GCs phase B,
identical sums, exit 0. Crucially the `GC HOLE WARNING` count is **623 in
both runs** (see finding 3): the warnings are workload artefacts, not
fork effects. Fork changes nothing observable in the GC.

**Run 3 — verify+poison+stress (`EU_GC_STRESS=1`, forced SelectiveEvacuation
on every collection — the hardest COW case, since evacuation rewrites objects
and forwarding pointers all over the inherited pages):**

```text
$ EU_GC_VERIFY=2 EU_GC_POISON=1 EU_GC_STRESS=1 PP_SPIKE_ITERS=6000 timeout 590 ./target/release/examples/pp_fork_spike
[parent] phase A OK: child ran 6 collections ...
[parent] phase B OK (fork): child ran 6000 iters / 60011 ticks / 3476 machine GCs ...
[parent] phase D OK: 4 workers x 5000 items in 21.6s, worker GC counts [2475, 2475, 2475, 2475] ...
pp_fork_spike: ALL PHASES PASSED   (exit 0)
```

(`PP_SPIKE_ITERS` scales phase B down because verify-2 + evacuate on *every*
collection is orders of magnitude slower; 3 476 evacuating, fully verified
collections in a forked child is ample evidence.)

All runs on macOS (Darwin 25.5.0, Apple Silicon). Zero verification panics,
zero poison hits, zero forwarding-pointer or block-integrity failures, in any
child or any parent, across every configuration.

## Spec §9 checklist

| §9 requirement | Result |
|---|---|
| 1. Managed heap survives COW fork, no corruption child or parent | **Proven** — phases A/B/D under `EU_GC_VERIFY=2` + `EU_GC_POISON=1` + (run 3) `EU_GC_STRESS=1`; ~20k verified collections in forked children total; parent heap re-verified post-join every time |
| 2. mmap arena shared and readable across fork/join | **Proven** — u64s, strings, and per-worker partial slots all round-trip; 4 concurrent writers to disjoint slots, no atomics needed (spec §6a layout) |
| 3. FD / signal / crash-handler inheritance sane | **Proven in the mimicked config** — crash handler + ctrlc thread + parked thread all present at fork; children allocate heavily and run to `_exit` cleanly (hundreds of forks). Notes in risk register (R3, R4) |
| 4. Rough per-fork + per-value costs | **Measured** — fork+waitpid median ~0.65 ms (up to ~1.3 ms with the heavier stress-config heap); arena serialisation ~1.2 ns per 16-byte record write, ~0.5 ns read |

A key incidental result for §2 of the spec: the heap's blocks come from the
**global Rust allocator** (`std::alloc` in `src/eval/memory/block.rs`), not
raw private mmap — plain COW-inherited process memory, no mmap-flag
subtleties at fork.

## Findings

1. **The COW-fork mechanism works exactly as the spec assumes.** Fork at a
   quiescent point, child evaluates in the inherited heap (including
   thousands of collections, with evacuation), writes forced results to the
   shared arena, `_exit`s; parent joins and reads. Fork-vs-no-fork GC
   behaviour is bit-for-bit identical in every counter we compared. The
   parent's heap and machine remain fully functional after joining 1 child or
   4 concurrent children.

2. **Cost model inputs (spec §7):** ~0.65–1.3 ms per fork+join on this
   machine, ~ns-scale per small value across the arena. For the motivating
   workloads (day09-p1 ~3 min, day10-p2 ~80 s) the overhead is noise; the
   N-threshold can be generous. A W=4 par-sum of 20 000 items ran end-to-end
   (fork → evaluate → partial-combine → join) correctly with per-worker GC.

3. **Pre-existing, fork-neutral: `GC HOLE WARNING` spam under
   `EU_GC_POISON=1`.** Phase A's direct heap/`collect` usage produces 623
   warnings from `verify_hole_is_dead` (`src/eval/memory/bump.rs`) —
   *identically with and without fork*, and zero under stress mode or in the
   machine-driven phase B/D (17k+ collections warning-free). The checker
   documents its own imprecision ("We check both mark states since we can't
   easily access Heap's current mark_state"): objects marked under the stale
   parity from an earlier cycle and since dead legitimately sit in holes and
   trip it. Warning-only, no panic, no failure. Follow-up filed as
   **eu-rkw2f** (make hole verification parity-aware); not a PP blocker.

4. **One real correctness hazard: live lazy producers.** `LazyProducer`
   (`src/eval/stg/stream.rs`) advances "the underlying source (file cursor,
   CSV parser…)" at evaluation time. Fork shares the open file description —
   the offset — so two workers forcing values from the same non-exhausted
   impure producer would interleave reads nondeterministically. The trait
   already anticipates this (`is_pure()`: "Import and IO producers are not
   pure. Pure producers can safely be forked/shared in future"). See R2.

## Risk register

| # | Risk | Severity | Evidence / mitigation | Spec § |
|---|---|---|---|---|
| R1 | POSIX formally allows only async-signal-safe calls in the child of a multithreaded parent (malloc locks). The real `eu` has a join-blocked main thread + ctrlc thread at eval time. | Low | Empirically clean here with exactly that thread shape (both macOS libmalloc and glibc register atfork handlers; Rust uses the system allocator). Mitigation: fork only from the evaluation thread at the quiescent point (already the design); the portable escape hatch is the spawn model (eu-9udwg). Validate once on Linux CI (R7). | §4 |
| R2 | **Live impure lazy producers share fd offsets across workers** → interleaved reads, nondeterminism/corruption. | **Medium — must handle in v1** | At `par-*` entry, check the `ProducerTable`: if any non-exhausted `!is_pure()` producer is reachable, either drain it before forking or raise the §6 boundary error. Cheap check, aligns with the existing §6 "non-serialisable → runtime error" decision. | §3, §6 |
| R3 | Child-side process hygiene: unwinding out of the child, running parent `Drop`s/atexit, flushing inherited stdio buffers, writing to stdout. | Low (implementation rule) | Spike pattern is the rule: `catch_unwind` + `libc::_exit`, never `exit()`, never unwind past the fork frame; workers touch only the arena and (optionally) stderr. Rendering/emitters stay parent-only. | §4 |
| R4 | Signals: workers inherit the ctrlc disposition but have no handler thread, so they won't observe interrupts; the parent's `INTERRUPTED` flag is per-process. | Low | Parent's interrupt path must kill (SIGKILL) and reap workers before propagating; optionally reset SIGINT to `SIG_DFL` in the child immediately after fork. The crash handler's inheritance is benign — arguably useful (child crash diagnostics to shared stderr). | §4 |
| R5 | Spike drove the HeapSyn `Machine`; the default engine is the bytecode VM. | Low | `BytecodeMachine` uses the identical `memory::heap::Heap` / `collect` subsystem (verified imports in `src/eval/bytecode/machine.rs`), so the heap result transfers directly; still, the PP implementation must repeat the phase-B/D equivalence test on the bytecode engine (its GC entry points differ). | §4, §8 |
| R6 | PRNG (`Native::Prng`) state duplicates into every worker — identical streams. | Negligible | `f` is pure by contract and the PRNG is deterministic/splittable; identical to the sequential semantics for the §3 contract. Document it. | §3 |
| R7 | All evidence is single-platform (macOS/aarch64). | Low | Run the spike binary once under Linux CI (a one-job addition) before or during implementation; nothing in the mechanism is Darwin-specific. | §9 |
| R8 | Arena sizing/overflow and the general value serialiser are unproven — the spike moved u64s and strings by hand-rolled length-prefix. | Expected (scoped work, not risk to the mechanism) | §6 serialiser and §6a per-worker segment accounting with overflow → error are ordinary implementation work; nothing found that constrains them. | §6, §6a |

## Recommendation

The spec's v1 mechanism (§4 COW-fork, §6a anon-shared arena, results-only
transport, index-ordered reassembly) **holds as written** — no revision
needed. Two additions the spec should absorb from this spike:

1. **§6 boundary check must also cover live impure producers** (R2), not
   just non-serialisable results — same error-at-the-boundary philosophy.
2. **Worker process hygiene rules** (R3/R4: `_exit` only, no stdout, parent
   kills workers on interrupt) belong in the §4 mechanism text.

Implementation can proceed against spec + this register. Suggested first
implementation checkpoints: repeat phase B/D on the bytecode engine (R5)
and add a Linux CI run of the spike (R7).

## Reproduction

```bash
git fetch origin spike/pp-fork-safety
git checkout spike/pp-fork-safety
cargo build --release --example pp_fork_spike
EU_GC_VERIFY=2 EU_GC_POISON=1 timeout 300 ./target/release/examples/pp_fork_spike
EU_GC_VERIFY=2 EU_GC_POISON=1 timeout 300 ./target/release/examples/pp_fork_spike --nofork
EU_GC_VERIFY=2 EU_GC_POISON=1 EU_GC_STRESS=1 PP_SPIKE_ITERS=6000 \
  timeout 590 ./target/release/examples/pp_fork_spike
```
