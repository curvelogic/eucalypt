# 0005 — Generational nursery & GC trigger/Immix-defrag completion

- **Status:** Draft proposal for review
- **Track:** B — performance, runtime & concurrency
- **Classification:** Whitespace
- **Suggested horizon:** 0.9
- **Related:** H4 (runtime performance, type-system-evolution.md §5 Q4), ADR-001, sibling proposals [0006 — strictness analysis](0006-strictness-analysis.md), [0020 — persistent blocks & GC finalisation](0020-persistent-blocks-gc-finalisation.md), [0008 — parallel evaluation](0008-parallel-evaluation.md)

## Summary

Eucalypt's garbage collector is the single largest consumer of runtime on
real workloads: GC mark time is reported at over 95% of VM time on
traversal-heavy programs, and the mark phase is proportional to the **live**
set, which it re-scans in full on every cycle. The collector is already
Immix-inspired with working opportunistic evacuation and lazy (deferred) sweep
— better than its own documentation claims — but it is **non-generational**,
its collection trigger is crude (a 500-step countdown gated on a block-count
limit that is *disabled entirely when the heap is unbounded*), and its
highest-fragmentation defragmentation tier is selected but unwired. The
highest-leverage memory work before 1.0 is a **generational nursery** (most
cheaply realised as a sticky-mark-bit variant of the existing collector), a
write barrier on thunk update, an allocation-rate-aware trigger, and finishing
the stubbed `DefragmentationSweep`. The hard part is not the nursery: it is the
write barrier a lazy heap forces on us, which this document treats honestly.

## Motivation

### What the collector actually is today (and a correction)

`docs/development/gc-implementation.md` is **stale**. It states "No evacuation:
Objects are not moved during collection" and "Eager sweeping — all blocks swept
immediately after marking". Both are false. The code implements:

- **Opportunistic evacuation.** `collect_with_evacuation`
  (`src/eval/memory/collect.rs:465`) copies live objects out of fragmented
  candidate blocks (`evacuate`, `collect.rs:298`), installs forwarding pointers
  in the old header (`AllocHeader::set_forwarded`, `src/eval/memory/header.rs:106`),
  and rewrites references in an update phase via `scan_and_update`
  (`collect.rs:530`).
- **Lazy sweep.** After marking, `defer_sweep` (`src/eval/memory/heap.rs:674`)
  moves blocks to an `unswept` queue; they are reclaimed one at a time by
  `lazy_sweep_next` (`heap.rs:680`) at allocation time, not eagerly.
- **Fragmentation-driven strategy.** `analyze_collection_strategy`
  (`heap.rs:1782`) classifies every block as Sparse / Fragmented / Dense
  (`BumpBlock::analyze_density`, `src/eval/memory/bump.rs:457`) and picks
  `MarkInPlace` below 10% fragmentation, `SelectiveEvacuation` from 10–30%, and
  `DefragmentationSweep` above 30% (`heap.rs:1885`).
- **Emergency collection.** `find_space` (`heap.rs:2354`) falls back to an
  emergency collection on allocation failure; the `.expect("aargh")` panic the
  stale doc cites no longer exists.

The doc should be rewritten to match: the collector is, in fact, "Immix without
the *generational* dimension".

### Mark cost dominates, and it is wasted on the old generation

Marking resets every block's line map (`reset_region_marks`, `heap.rs:2593`;
`LineMap::reset`, `bump.rs:77`) and re-traces the entire reachable graph from
roots on **every** collection (`collect.rs:414`). The header carries a single
mark bit interpreted against a per-heap mark state that flips after each cycle
(`flip_mark_state`, `heap.rs:1716`; `is_marked`, `heap.rs:2657`), so "live"
means "mark bit equals current state" — there is no notion of age, no
remembered set, and no way to *skip* an object known to be long-lived.

The cost of this is visible directly: the `Clock` already partitions runtime
into `Mutator`, `CollectorMark`, and `CollectorSweep`
(`src/eval/machine/metrics.rs:13`), surfaced through `-S/--statistics`. On
traversal-heavy programs mark time exceeds 95% of VM time. The benchmark
`tests/harness/bench/008_long_lived_graph.eu` is the canonical case: it builds a
200-cell persistent cons-list that survives the whole run, then churns
temporary garbage around it. Every collection re-marks all 200 cells (plus
their backing) even though none of them has changed since allocation. That is
the generational hypothesis stated as a bug: **we pay, repeatedly, to discover
that the old generation is still alive.**

### The trigger is crude

The VM checks for collection on a countdown of 500 steps (`gc_check_freq`,
`src/eval/machine/vm.rs:1760`), then calls `policy_requires_collection`
(`heap.rs:1771`). That predicate returns **`false` whenever no `--heap-limit-mib`
is set** — and the CLI default is a 32 GiB managed limit, so in practice short
programs never collect until termination, and long ones collect on a
fixed-step cadence unrelated to how fast they are allocating. A program that
allocates megabytes between two checks and one that allocates kilobytes are
treated identically. There is no allocation-rate signal, despite
`AllocationStats` already tracking `allocation_rate_bps` and burst size
(`heap.rs:167`).

### The top defrag tier is unwired

`analyze_collection_strategy` returns `DefragmentationSweep` above 30%
fragmentation, but `CollectionStrategy::candidates()` for that variant returns
an empty `Vec` with a literal `// TODO: all blocks` (`heap.rs:87`). Because
`collect` (`collect.rs:388`) treats an empty candidate list as "mark in place",
**the worst-fragmentation case silently degrades to a non-compacting
collection** — exactly the case Immix evacuation exists to fix.

## Prior art & landscape

**Immix** (Blackburn & McKinley, *Immix: A Mark-Region Garbage Collector with
Space Efficiency, Fast Collection, and Mutator Performance*, PLDI 2008) is the
direct ancestor of eucalypt's heap: 32 KiB blocks, 128 B lines, conservative
line marking requiring two free lines to recognise a hole, and opportunistic
evacuation of fragmented blocks in the same pass as marking. Eucalypt has all of
this. Crucially, the Immix paper reports that *"as the mature space in a
generational collector, Immix matches or beats a highly tuned generational
collector"* — i.e. the design was always intended to sit underneath a nursery,
not to run ungenerational.

**Sticky Immix / the sticky-mark-bit algorithm** is the cheap generational
upgrade, due to Demers et al. (*Combining Generational and Conservative Garbage
Collection*, POPL 1990) and well summarised by Wingo ("the sticky mark-bit
algorithm", wingolog 2022): *"flip the mark bit only on major collections"* and
*"include a remembered set in the roots for minor collections"*, so that *"old
objects are not traced during minor collections, as they are already marked"* —
*"marking an object is tenuring, in-place."* No copying, no second semi-space.
This matters because **eucalypt already flips a single mark bit per collection**
(`flip_mark_state`, `heap.rs:1716`): sticky-mark-bit is a near-minimal delta on
the collector we have. A minor collection becomes "do not flip; trace only from
roots + remembered set; sweep only nursery blocks"; a major collection is
today's full mark-sweep.

**The generational hypothesis** — most objects die young — is strongly true for
eucalypt. Lazy evaluation manufactures vast numbers of short-lived thunks and
intermediate cons-cells (`007_short_lived.eu` builds and discards hundreds of
temporary ranges), while genuinely long-lived data is the small result block
being rendered. This is the textbook profile for a nursery.

**The laziness complication, and how GHC solves it.** A generational collector
needs a write barrier to catch **old → young** pointers: a minor collection that
skips the old generation would otherwise miss young objects reachable only from
old ones. In an imperative language the barrier fires on field stores. In a
*pure lazy* language the principal mutation is **thunk update** — when a thunk is
forced, its black-hole placeholder is overwritten in place with the computed
value, which may be a freshly allocated young object pointed at by an old thunk.
GHC handles exactly this. As its RTS notes put it, *"Haskell laziness means that
we have to update a value when it gets evaluated"*; GHC couples a write barrier
with a per-generation *mutable list* (remembered set) and uses **eager
promotion** — *"if old generation ends up pointing to young generation (due to a
write-once), promote the new object to the old generation."* The reassuring half
is that the traffic is rare: Haskell mutates far less than an imperative
language. Eucalypt's profile is narrower still — thunk update is essentially the
*only* in-place heap write — making the barrier both unavoidable and cheap.

Among the data-language peers (Jsonnet, Dhall, CUE, Nickel, Pkl, KCL,
Starlark), none offers a useful GC blueprint: Starlark and Jsonnet ride host GCs
(Go, C++/V8-derived), Dhall is Haskell-on-GHC. The relevant literature is the
runtime-systems line above. Eucalypt should borrow Sticky Immix's *mechanism*
and GHC's *barrier discipline*, and nothing from the peers here.

## Proposed design

This is a runtime-internal change; it adds **no surface syntax** and honours
syntactic conservatism trivially. Four parts, sequenced so each lands value
independently.

### 1. Allocation-rate-aware trigger (no GC algorithm change)

Decouple "should we collect?" from the heap limit. Maintain bytes allocated
since the last collection (already available via `AllocationStats`,
`heap.rs:167`) and trigger when it crosses an adaptive nursery budget, even when
the heap is unbounded. Keep the 500-step countdown only as the *polling*
interval, not the decision. Concretely, `policy_requires_collection`
(`heap.rs:1771`) gains an allocation-volume branch that fires independently of
`self.limit`. This alone makes unbounded runs collect on a sane cadence and is a
prerequisite for a nursery (whose whole point is a small, frequently collected
region).

### 2. Sticky-mark-bit generational collection

Introduce a **minor collection** alongside the existing full collection:

- A **nursery**: designate freshly allocated blocks as nursery blocks. The
  existing `head`/`overflow` active blocks (`HeapState`, `heap.rs:358`) are the
  natural nursery; blocks demoted to `rest` after surviving a minor collection
  become the mature space.
- **Tenuring by marking, in place.** A minor collection traces from roots plus
  the remembered set *without flipping the mark state*. Any nursery object it
  marks survives and is thereby tenured (its mark bit now matches the persistent
  major-collection state). This reuses `mark_object`/`is_marked` unchanged
  (`heap.rs:2668`, `heap.rs:2657`); only the *flip discipline* changes.
- **Major collection** is today's behaviour: flip the mark state
  (`heap.rs:1716`) and trace everything.

No copying, no second space, minimal new code in `collect.rs`. The header needs
**no new bit** for the basic scheme — the sticky mark bit *is* the
old/young distinction once minor collections stop flipping it. (An explicit
"is-nursery" line attribute may help bound the minor sweep; see Alternatives.)

### 3. The write barrier on thunk update

The single mutation site is `EnvironmentFrame::update`
(`src/eval/machine/env.rs:361`), reached from `MachineState::update`
(`vm.rs:540`) when an `Update` continuation (`src/eval/machine/cont.rs:53`)
fires and overwrites an environment slot — replacing the black hole
(`HeapSyn::BlackHole`, written at `vm.rs:438`) with the computed closure. This is
where an old-generation frame can come to hold a young pointer.

Two viable disciplines, mirroring GHC:

- **Eager promotion (preferred to start).** At `update` time, if the frame being
  updated is mature and the value being written is in the nursery, promote
  (tenure) the value immediately. For eucalypt this is attractive because the
  write is "write-once" (a thunk updates exactly once), so there is no repeated
  barrier cost, and promotion needs only to mark the young object with the major
  mark state. No remembered-set growth in the common case.
- **Remembered set (fallback / for cycles).** Where eager promotion is awkward
  (e.g. the value is itself a large young sub-graph), record the mature frame in
  a remembered set scanned as an extra root during minor collections.

Honesty about cost: the barrier adds a branch (and occasionally a promotion) to
*every* thunk update, and thunk updates are extremely frequent in a lazy
language — `002_thunk_updates.eu` exists to stress this path. The bet is that the
branch is predictable (the "mature frame holding young value" case is the
minority) and that cutting the mark set more than pays for it. **This must be
measured, not assumed** (see Risks). [0006 — strictness analysis](0006-strictness-analysis.md)
is a strong complement: fewer thunks means both fewer updates (less barrier
traffic) and a smaller live set (less to mark).

### 4. Finish `DefragmentationSweep`

Replace the stubbed `candidates()` for `DefragmentationSweep`
(`heap.rs:87`, the `// TODO: all blocks`) with the full set of unpinned
fragmented block indices — the enumeration already exists for the stress path in
`analyze_collection_strategy` (`heap.rs:1790`). With pinning already respected
(`is_block_pinned`, `heap.rs:1738`) and the evacuation machinery proven by the
existing tests (`collect.rs:858` onward), this is a small, well-bounded change
that closes the worst-fragmentation regression (`009_fragmentation.eu`).

### Preserving the safety invariant

Every part above runs **inside the existing stop-the-world, single-threaded
collector**. The soundness of the `UnsafeCell` heap depends on exactly that
(`src/eval/memory/heap.rs:8`: "`Heap` is not `Sync`… mutator and collector never
run concurrently"). Minor collection is still stop-the-world; the write barrier
runs on the mutator thread with no sharing. Nothing here introduces concurrency
or weakens the invariant — that is deliberately deferred to
[0008 — parallel evaluation](0008-parallel-evaluation.md).

## Interaction with the existing roadmap

This is **Track B / Whitespace**: there is no type-system dependency and it
touches none of the H-hypotheses except H4 (open question 4, "how much do we
care about runtime performance?", `type-system-evolution.md §5`). It should land
after [0004 — compiled-unit caching](0004-compiled-unit-caching.md) (which
attacks the more *visible* ~500–700 ms compile latency) and ideally alongside or
just after [0006 — strictness analysis](0006-strictness-analysis.md), with which
it compounds.

The sharpest interaction is with [0020 — persistent blocks & GC finalisation](0020-persistent-blocks-gc-finalisation.md).
ADR-001 records that persistent `im_rc::OrdMap` blocks leaked because the GC's
bump allocator overwrites recycled cells **without running `Drop`**, stranding
`Rc` nodes on the Rust heap and causing 220–580% regressions. 0020 needs a
*finaliser mechanism in the GC*; this proposal needs a *nursery and barrier*.
Both rewrite GC core, compete for the same scarce expertise and fragile code,
and **should be sequenced, not interleaved.** A synergy worth flagging: a
generational collector already maintaining a remembered set and per-region sweep
bookkeeping is a more natural host for a finaliser table than the current flat
sweep — so doing 0005 first may *lower* the cost of 0020.

## Implementation sketch

| Phase | Change | Files | Size/risk |
|-------|--------|-------|-----------|
| 0 | Rewrite the stale GC doc to describe evacuation + lazy sweep as implemented | `docs/development/gc-implementation.md` | trivial / none |
| 1 | Allocation-rate trigger | `heap.rs:1771`, `vm.rs:1739` | small / low |
| 2 | Finish `DefragmentationSweep` candidates | `heap.rs:87`, `collect.rs` | small / low |
| 3 | Minor collection (sticky mark bit) + nursery designation | `collect.rs`, `heap.rs` (`HeapState`, flip discipline) | medium / **medium-high** |
| 4 | Write barrier on thunk update (eager promotion) | `env.rs:361`, `vm.rs:526`, `cont.rs` | medium / **high** |
| 5 | Remembered set fallback + minor/major scheduling policy | `collect.rs`, `heap.rs`, `vm.rs` | medium / medium |

Phases 0–2 are independently shippable wins with little risk and should land
first regardless of whether the maintainer accepts the nursery. Phases 3–5 are
the strategic core and the genuine risk. The existing debug infrastructure is a
major asset here: `EU_GC_VERIFY=2` (full structural verification),
`EU_GC_POISON=1` (use-after-free detection), and `EU_GC_STRESS=1` (forces
evacuation every cycle) — see CLAUDE.md — give exactly the safety net a
generational rewrite needs. A new `EU_GC_VERIFY` checkpoint should assert the
**no-untracked-old→young-edge** invariant after every minor collection.

## Alternatives considered

- **Explicit copying nursery.** A dedicated bump-allocated young space with
  Cheney copying gives bump-pointer young allocation and automatic young
  compaction. Rejected *as the first step* because it is a second allocator and
  object lifecycle bolted onto a heap whose copying machinery (evacuation) is
  already battle-scarred (the suite around `collect.rs:1312` documents real
  evacuation corruption bugs). Sticky-mark-bit reaches most of the benefit by
  reusing the collector we have; a copying nursery is a reasonable *phase 6* if
  the in-place nursery's sweep cost proves to dominate.
- **Do nothing / rely on 0006.** Strictness analysis cuts thunk churn but does
  not change the fact that the *surviving* graph is re-marked every cycle; on
  `008` the persistent structure is strict already. Generational collection is
  the only thing that stops re-scanning the old generation. The two are
  complementary, not substitutes.
- **Per-object age counter + classic tenuring threshold.** More precise than a
  single sticky bit but needs header space (`AllocHeader` is a tight 16 bytes,
  `header.rs:141`) and more bookkeeping. Deferred; the sticky bit's "survive one
  minor collection ⇒ tenured" policy is coarse but adequate to start.

## Risks & what would kill this

- **The write barrier costs more than the nursery saves.** This is the central
  risk. If barrier overhead on the hot thunk-update path (`002_thunk_updates.eu`,
  `001_naive_fib.eu`) exceeds the mark-time saved on `008`, the bet fails. A
  net **regression on `002`/`001` that is not recovered by gains on `008`/`007`**
  would falsify it. Mitigation: eager promotion (write-once ⇒ no repeated cost)
  and gating the barrier behind a single predictable branch.
- **Generational rewrites are where GCs go to crash.** Old→young edge bugs are
  subtle and platform-dependent (the suite already records aarch64-specific
  evacuation crashes, `collect.rs:1515`). Mitigation: the `EU_GC_VERIFY`/
  `POISON`/`STRESS` harness, a new post-minor-collection edge-invariant check,
  and conformance fuzzing per [0003 — conformance & fuzzing](0003-conformance-testing-fuzzing.md).
- **Single-threaded invariant.** Any accidental introduction of sharing would be
  unsound (`heap.rs:8`). Mitigation: keep everything stop-the-world; review the
  barrier for any non-mutator-thread access.
- **Effort collides with 0020.** Both rewrite GC core. Mitigation: explicit
  sequencing (do the cheap wins and the nursery first; fold finalisation in
  after).

## Success criteria

- **Doc parity (phase 0):** `gc-implementation.md` no longer claims "no
  evacuation / eager sweep".
- **Mark-time share falls.** On `008_long_lived_graph.eu`, with `-S`, the
  `CollectorMark` fraction of VM time drops substantially (target: well below
  the current >95% regime) because minor collections stop re-marking the
  persistent structure.
- **Throughput on churn.** Net wall-clock improvement on `007_short_lived.eu`
  and `004_generations.eu` (independently verified via `cargo bench` on a clean
  `--release` build, per CLAUDE.md).
- **No fragmentation cliff.** `009_fragmentation.eu` no longer degrades to a
  non-compacting collection at high fragmentation (DefragmentationSweep actually
  evacuates).
- **No correctness regressions.** Full harness passes under `EU_GC_VERIFY=2`
  and `EU_GC_POISON=1`; the new old→young invariant holds across the suite.
- **Hot-path neutrality.** `001_naive_fib.eu` / `002_thunk_updates.eu` show no
  significant barrier-induced regression.

## References

**Eucalypt source.** `heap.rs` (8 — single-threaded invariant; 87 — stubbed
DefragmentationSweep TODO; 358 — `HeapState`; 674/680 — defer/lazy sweep; 1716 —
`flip_mark_state`; 1771 — `policy_requires_collection`; 1782/1885 — strategy
selection; 2354 — emergency `find_space`; 2593 — `reset_region_marks`;
2657/2668 — is_marked/mark_object); `collect.rs` (298 — `evacuate`; 388 — `collect`;
465 — `collect_with_evacuation`; 536 — update phase; 1323/1515 — evacuation-bug
tests); `bump.rs` (77 — `LineMap::reset`; 457 — `analyze_density`); `header.rs`
(106 — `set_forwarded`; 141 — 16-byte header); `vm.rs` (438 — black-hole write;
540 — `update`; 1760 — GC countdown); `env.rs` (361 — the barrier site);
`cont.rs` (53 — `Update` continuation); `metrics.rs` (13 — `ThreadOccupation`);
all under `src/eval/`. Also `docs/development/architectural-decisions.md`
(ADR-001); `docs/development/gc-implementation.md` (stale); benches
`tests/harness/bench/{004_generations,007_short_lived,008_long_lived_graph,009_fragmentation}.eu`.

**Papers & systems.** Blackburn & McKinley, *Immix: A Mark-Region Garbage
Collector*, PLDI 2008. Demers et al., *Combining Generational and Conservative
Garbage Collection: Framework and Implementations*, POPL 1990. A. Wingo, *the
sticky mark-bit algorithm*, wingolog, 2022. Sansom & Peyton Jones, *Generational
Garbage Collection for Haskell* / GHC RTS notes on thunk-update write barriers
and the mutable list.

**Peer languages.** Considered and found inapplicable as GC blueprints (host-GC
or GHC-hosted): Jsonnet, Dhall, CUE, Nickel, Pkl, KCL, Starlark.
