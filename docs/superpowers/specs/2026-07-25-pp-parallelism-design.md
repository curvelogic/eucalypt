# PP — Process-level parallelism (par-map + safe reductions) — design

**Status:** draft for owner review · **Date:** 2026-07-25 · **Bead:** eu-u9xj.6 (PP) · **Epic:** eu-1tkk (0.14)

> The eucalypt-facing surface (combinators) is shown in real catenation syntax.
> The transport (fork / mmap arena / value serialisation) is native Rust and is
> described at the design level, not as final APIs. Combinator member names are
> proposed. The whole design is gated on the §9 feasibility spike.

## 1. Purpose & scope

Speed up the **embarrassingly-parallel** slice of real workloads by evaluating
independent computations in separate OS processes, without surrendering the
single-threaded lazy-pure heap (Principle 4).

**Motivation is measured, not assumed.** A survey of the 12 AoC 2025 puzzles
(examples/aoc25/) found PP genuinely helps exactly the shape
`data map(heavy-independent-fn) reduce(assoc-op)`:

| Candidate | Now | Shape |
|---|---|---|
| day09 p1 — all-pairs area max | ~3 min | `points par-max(best-from)` |
| day10 p2 — N independent ILP solvers | ~80 s | `machines par-sum(solve)` |
| day08 — ~250k independent pair distances | ~17 s+ | par-map over pairs (combine is sequential) |
| day05 p1 (moderate), day11 p2 (~4-wide) | — | secondary |

The two **slowest** puzzles (day04 p2 CA-fixpoint, day09 p2 branch-and-bound)
are **inherently sequential** — PP cannot help them. This is a real speedup on a
real slice, **not** a blanket "make AoC fast".

**In scope (v1):**
- `par-map` + a **safe, provably-associative reduction vocabulary** (`par-sum`,
  `par-max`, `par-min`, `par-concat`).
- **Unix COW-fork** transport; results-only across an mmap arena.
- A **value serialiser** (data only) — v1 is *not* gated on BV5.

**Explicitly out of scope:**
- **General `par-fold(op, z)`** — associativity is uncheckable; a non-associative
  op would silently violate "never semantically observable". Deferred behind an
  explicit opt-in if ever needed.
- **Inherently sequential algorithms** — state-threading folds, CA-style
  fixpoints, branch-and-bound sharing a prune bound. Named, not attempted.
- **Shared-memory / `Send`+`Sync` heap (Model B)** — rejected by Principle 4.
- **Windows / portable spawn model** — needs BV5; tracked separately as
  **eu-9udwg** (depends on eu-lb0r).

## 2. Constraints this builds on (verified)

1. The heap is a **single-threaded `UnsafeCell`**; GC is **stop-the-world, no
   background thread** (ROADMAP §Principle 4, GC notes). So `fork()` happens at a
   quiescent, single-threaded point — the precondition for a safe COW fork.
2. Evaluation is **pure**; the only effect is IO-action *values* interpreted by
   the driver. So a pure `f` has no side effects to race, and a parallel merge is
   order-independent.
3. Parallelism is **process-level and isolated** — never shared-memory mutation.

## 3. Surface — combinators (real catenation syntax)

All follow the prelude idiom (receiver is the **last** argument; define
`f(args…, list)`, call `list f(args…)`). Every combinator is **semantically
identical to its sequential form** — a pure performance advisory.

```
# parallel map — returns the mapped list IN ORDER (≡ `xs map(f)`)
result: points par-map(expensive-score)

# fused parallel map-then-associative-reduce — only W partials cross the boundary
best:   points par-max(best-from)      # ≡ points map(best-from) max
total:  machines par-sum(solve)        # ≡ machines map(solve) sum
lo:     items par-min(cost)
all:    chunks par-concat(expand)      # associative list concat
```

- `par-map(f, xs)` → list of `f(xs[i])` in index order. Use when you need the
  mapped list itself.
- `par-sum(f, xs)` / `par-max(f, xs)` / `par-min(f, xs)` / `par-concat(f, xs)` →
  **fused** map + associative reduce. Each worker reduces its own chunk locally,
  so only one partial per worker crosses — the efficient form, and what the AoC
  candidates want. (To reduce an existing list rather than fuse a map, pass
  `identity`; but the whole point is that the *heavy* work is `f`.)

**Contract on `f`:** pure (no IO), and its results (and `xs`'s elements) must be
**serialisable data** (§6). `par-*` is **strict** — it forces `xs`'s spine to
partition, and forces each result to normal form to serialise it. For the finite,
fully-consumed lists these workloads use, that matches the sequential result
exactly.

## 4. Mechanism — Unix COW-fork, results-only

`par-map(f, xs)`:
1. **Force the spine of `xs`**; get N and index access. Choose worker count W
   (§7). If W ≤ 1 or N below threshold → **transparent sequential fallback**
   (just run `xs map(f)`); no fork, identical result.
2. **`fork()` W workers.** Each child inherits the *entire* heap **copy-on-write**
   — `f`, `xs`, prelude, program state are all present for free. **No code or
   input is serialised.**
3. Each worker evaluates `f(xs[i])` for its **contiguous index chunk** in its own
   COW heap (forcing its own elements — no cross-process sharing, no duplicated
   work), and writes each **fully-forced result** into a shared
   **`mmap(MAP_SHARED)` arena** at its index-addressed slot.
4. Parent **`waitpid`s (join)**, reads results back from the arena into its heap,
   reassembles **in index order** → deterministic regardless of worker timing.

`par-sum`/`par-max`/… are identical except step 3 is a **worker-local partial
reduction** (one partial per worker), and the parent combines the W partials with
the associative op — so only W values cross, not N.

**Determinism:** index-ordered slots for `par-map`; worker-index-ordered partial
combine (with an associative op) for the reductions. Both are byte-for-byte equal
to the sequential result.

## 5. Why not gated on BV5

Because fork inherits the *code*, the only thing that crosses the boundary is
**result values**. So v1 needs a **value serialiser** (§6), not serialisable
*bytecode*. BV5 (eu-lb0r) is only needed by the **spawn** model (fresh workers
loading a serialised program), which is the Windows/portable path — **eu-9udwg**,
deferred.

## 6. Value serialisation scope

Serialisable (can cross the arena): numbers, strings, symbols, booleans, null,
lists of serialisable, blocks of serialisable. **Not** serialisable: functions /
closures, IO-action values, anything carrying an un-flattenable captured
environment. A non-serialisable result (or `xs` element) is a **runtime error at
the boundary** (decided) — `par-map` returning closures/IO-actions is a
programmer mistake worth surfacing, not silently papering over. (The error names
the offending value's kind and the combinator.)

## 6a. Arena — sizing & disposal

Results are variable-size and unknown ahead of time, but in the target workloads
they are **small** (a number / small tuple per element) even when the compute is
heavy. So:

**Sizing — over-provision *virtual*, pay only *physical*.** The arena is a
`MAP_SHARED | MAP_ANONYMOUS` region created **before** the fork (children inherit
it; MAP_SHARED makes writes mutually visible), sized to a generous **configurable
cap** (default derived from N with a ceiling). Anonymous pages are **demand-zero**
— physical memory equals only the bytes actually written, so a large virtual cap
costs address space, not RAM. The arena is split into **W per-worker segments**:
worker *w* owns a contiguous index chunk and writes its results length-prefixed
into segment *w*, in order. The parent reads segments in index order → global
index order, deterministic. **No cross-process atomics, no growth/remap.** A
worker overflowing its segment → error (rare, given small results).

**Disposal — trivial and crash-safe.** Because the mapping is **anonymous** (no
backing file, no named kernel resource), the parent `munmap`s the arena after
`waitpid` + reading results back, and each worker's mapping is reclaimed by the OS
when that worker exits (at join). Nothing to `unlink`; no leftover temp files or
state even if the parent dies mid-op — process death frees all mappings.

## 7. Cost model (advisory)

Because it is semantically ≡ sequential, PP can be conservative:
- **Worker count** W = `min(cores - 1, N)` by default.
- **Fork only above a threshold** (small default N, tunable) — below it, run
  sequentially. eucalypt can't cheaply estimate `f`'s cost, so the gate is on N
  (plus an optional explicit hint later, e.g. a `{ workers: … }` option block).
- Fork + arena + result-serialisation has real per-op overhead; the win requires
  heavy per-element `f`. When it doesn't pay, the fallback makes it a no-op cost
  beyond the spine force.

## 8. Testing

- **Determinism / equivalence:** every `par-*` result is asserted **byte-identical
  to its sequential equivalent** across worker counts (W=1,2,4,…) and input sizes
  — the core "never semantically observable" guarantee.
- **Speedup:** ≥ 1.5× wall on a real data-parallel case (a scaled day09-p1 /
  day10-p2 shape) on 4+ cores, per the ROADMAP success bar.
- **Fallback:** small-N and W≤1 produce identical results with no fork.
- **GC integrity:** `EU_GC_VERIFY=2` clean in parent and workers.
- Each harness test computes RESULT from its checks; fault-injection verified.

## 9. Feasibility spike (the gate — before any real build)

Forking a process whose heap is our custom managed GC heap (Immix mmap blocks,
`UnsafeCell`) is *probably* fork-safe but must be **proven** first (this session's
discipline). The spike: fork the VM at a quiescent point, have a child force a
trivial `f(x)`, serialise **one value** through an `mmap(MAP_SHARED)` arena back
to the parent, and run **`EU_GC_VERIFY=2` on both sides**. It must establish:

1. The managed heap survives COW fork with no GC corruption in child or parent
   (block mmap flags, allocator/GC invariants across the fork point).
2. The mmap arena is correctly shared and readable across fork/join.
3. FD / signal-handler / crash-handler (the unconditional SIGSEGV handler in
   `main()`) inheritance is sane in the child.
4. Rough per-fork + per-value-serialise cost (feeds the §7 threshold).

If any of these fail or prove costly, revisit before speccing the combinators
further — the whole COW-fork model rests on this.

**Resolved:** non-serialisable → runtime error (§6); worker/threshold heuristic
`W = min(cores-1, N)` + N-threshold (§7); arena over-provisioned anon-shared with
per-worker segments, munmap-on-join disposal (§6a).

**Remaining minor:** the default arena virtual-cap value and its N-scaling (§6a);
whether reductions expose an explicit worker-count hint in v1 (leaning no —
advisory default is enough).

## 10. Summary

`par-map` + a safe reduction vocabulary (`par-sum`/`par-max`/`par-min`/
`par-concat`), each semantically ≡ its sequential form. Implemented by Unix
COW-fork: workers inherit code+inputs for free, only results (or worker-local
partials) cross an mmap arena, reassembled deterministically in index order.
Needs a value serialiser, **not** BV5; Windows/spawn is deferred (eu-9udwg).
Serves a measured slice of real workloads (day09-p1, day10-p2, day08-gen); does
not touch inherently-sequential work. Gated on a fork-safety feasibility spike.
