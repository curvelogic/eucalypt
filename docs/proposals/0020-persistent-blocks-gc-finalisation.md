# 0020 — Persistent O(log n) blocks & resolving the GC-finalisation wall (ADR-001)

- **Status:** Draft proposal for review
- **Track:** E — ecosystem, interop & safety
- **Classification:** Whitespace
- **Suggested horizon:** 0.9
- **Related:** ADR-001 (`docs/development/architectural-decisions.md`), sibling proposals [0005 — generational GC](0005-generational-gc.md), [0008 — parallel evaluation](0008-parallel-evaluation.md)

## Summary

Blocks — eucalypt's core data structure, the thing it exists to produce — are
cons-lists of key-value pairs with **O(n) lookup**. A persistent O(log n)
representation (`im_rc::OrdMap`) was built across two phases and then
**reverted** (ADR-001), not because it was slow but because it leaked: the GC's
bump allocator recycles memory blocks **by overwrite and never runs `Drop`**, so
the `Rc`-counted map nodes — which live on the Rust heap, outside GC management —
were never freed, producing 220–580% regressions on GC-intensive benchmarks even
for programs that barely use blocks. This proposal argues the right fix is to
attack the **underlying GC-finalisation problem** rather than the block
representation, evaluates ADR-001's four candidate paths against the collector as
it actually exists, and recommends storing a **CHAMP** (an improved HAMT) map
**inline in the GC heap** as ordinary scannable objects via the existing `Array`
backing-store machinery — sidestepping finalisation entirely — with a finaliser
table as the pragmatic fallback. It is deep GC work that competes with
[0005](0005-generational-gc.md) for the same expertise and must be sequenced
against it.

## Motivation

### Blocks are cons-lists, and lookup is linear

A block literal compiles to a `Block(list, index)` data constructor whose `list`
is a cons-list of `BlockPair`/`BlockKvList` cells (`Block::wrapper`,
`src/eval/stg/block.rs:79`; the constructor is built at `block.rs:87`). Lookup
walks that list pair by pair: the `find` loop in `LookupOr`
(`src/eval/stg/block.rs:495`) recurses down `ListCons` cells calling
`MATCHES_KEY` until it hits the key or `ListNil`. That is O(n) per lookup, and
the prelude builds heavily on it — `lookup`, `lookup-or`, `has`, `lookup-path`,
`lookup-alts`, `merge-at`, and the `~` safe-navigation operator are all defined
in terms of it (`lib/prelude.eu:408–441`).

A partial mitigation already exists. On a lookup against a block of at least
`BLOCK_INDEX_THRESHOLD = 16` entries (`block.rs:469`), `LookupOr::execute` builds
a `HashMap<SymbolId, usize>` index and caches it inside the block's constructor
by mutating the index slot (`store_index_in_block`, `block.rs:854`). This is
revealing in two ways. First, it concedes that O(n) lookup is a real problem
worth a special case. Second — and this is the crux of the whole proposal — that
cached index is an **`Rc<BlockIndex>`** stored in `Native::Index`
(`src/eval/memory/syntax.rs:33,47`), an `Rc` on the Rust heap that the GC also
never finalises. We tolerate *that* leak because the index is built rarely (only
for large blocks, once, lazily) and is a single allocation. The reverted
persistent-block work failed because it made the very same leak happen O(log n)
times per *insert*, on *every* block. The difference between an acceptable leak
and a fatal one is purely frequency — which tells us exactly where the fix must
land.

### Merge is catenation, and it is everywhere

The index helps lookup but does nothing for the other half of the problem.
Block merge — `merge`, `deep-merge`, and the `<<` operator (`lib/prelude.eu:359`,
`365`, `372`) — is implemented by walking **both** operand lists into an
`IndexMap` and re-emitting a fresh cons-list (`Merge::execute`,
`block.rs:1453`; `MergeWith::execute`, `block.rs:1560`). Every merge is O(n+m)
and allocates a whole new spine; the cached index on the inputs is discarded and
must be rebuilt on the output. `deep-merge` recurses this at every level of
nesting. Configuration workloads are merge-heavy by nature — layering a base
over an environment override over a local patch is the canonical eucalypt task —
so the O(n) spine-copy is not an edge case but the main line. A persistent map
with structural sharing would make merge *share* unchanged sub-trees instead of
copying them.

The user-visible ceiling shows up in `deep-find` (`lib/prelude.eu:461`), which
generically walks an entire block tree collecting values for a key at any depth.
On a large uniform configuration this is O(total nodes) with an O(n) lookup at
each block — quadratic in practice. These are the workloads where O(n) blocks
bite (quantified in §"Honest cost/benefit").

### The root cause, precisely (ADR-001)

ADR-001 records the failure. The persistent implementation used
`im_rc::OrdMap<SymbolId, BlockEntry>`, whose internal nodes are reference-counted
(`Rc`) and live on the **Rust** heap, not in the GC-managed block heap. The
collector's reclamation path is the bump allocator's `recycle`
(`src/eval/memory/bump.rs:371`): it finds a hole via the line map and resets the
cursor, and allocation simply **overwrites** the bytes (`bump`, `bump.rs:294`;
in debug builds the hole is filled, `bump.rs:311`, but no destructor runs). No
`Drop::drop` is ever called on a reclaimed eucalypt object. So when a block
holding an `OrdMap` was reclaimed, the `OrdMap`'s `Rc` nodes were stranded: their
refcount never reached zero, and they leaked. The leak was proportional to the
number of blocks created, which is why GC-churn benchmarks regressed 220–580%
*even when they did not use blocks heavily* — the leaked nodes inflated the live
heap and forced more frequent, larger collections. ADR-001 also notes that a
first finalisation attempt (tracking `HeapBlock` pointers and dropping them
during marking) reduced but did not eliminate the overhead.

This is the wall. It is not a block-design problem; it is a **GC-finalisation**
problem that any non-trivial `Drop`-bearing payload would hit.

## Prior art & landscape

**HAMT (Bagwell, *Ideal Hash Trees*, 2001).** The 32-way bitmap-indexed hash
trie underpinning Clojure's and Scala's immutable maps. It gives O(log₃₂ n)
lookup/insert/delete — effectively constant for realistic sizes — with
structural sharing on update, packing present children densely via a `popcount`
over a bitmap. The standard answer to "persistent map with cheap copies".

**CHAMP (Steindorfer & Vinju, OOPSLA 2015).** The modern refinement.
CHAMP ("Compressed Hash-Array Mapped Prefix-tree") keeps the branching but
**splits each node into two compact arrays** — inline data entries, and sub-node
references — instead of interleaving them, and keeps nodes canonically compact
after deletes. This yields **1.3–6.7× faster iteration** and **3–25.4× faster
equality** than a classic HAMT at a smaller footprint; Scala adopted it for
`immutable.HashMap` in 2.13. The iteration win matters here: rendering to
YAML/JSON and `deep-find`/`deep-merge` are all iteration-bound.

**The `im` / `im_rc` crates (bodil).** The reverted work used `im_rc::OrdMap`,
which is a **B-tree**, not a HAMT — and the crate's own docs report it runs
"2–3× slower than std on basic operations" *before* any leak. Both `im` (`Arc`)
and `im_rc` (`Rc`) build from Rust-heap, refcounted nodes — exactly the shape the
GC cannot finalise. So an off-the-shelf crate both forces the leak and was not
even the fastest choice; a purpose-built CHAMP of GC objects avoids both.

**GC finalisation and its hazards.** Running cleanup code when an object becomes
unreachable is notoriously treacherous: **resurrection** (a finaliser revives a
dying object, forcing the collector to re-check all garbage before reclaiming —
"a simple implementation re-checks all garbage if even a single object has a
finaliser"; *Object resurrection*, Wikipedia; SEI CERT MET12-J); **ordering**
(finalisers run in no guaranteed order and may reference already-finalised
objects); and **non-determinism and cost** (they may be delayed or never run, and
inflate construction/destruction). Java **deprecated `finalize()`** over exactly
these, replacing it with the weaker `Cleaner`. For Rust specifically, Hughes &
Tratt, *Garbage Collection for Rust: The Finalizer Frontier* (2024), treats
bolting `Drop`-style finalisation onto a tracing GC and shows it raises real
soundness questions (premature finalisation while a value is still reachable;
finaliser safety; elision) — the territory ADR-001's "track `HeapBlock` pointers
and drop them during marking" attempt wandered into.

The takeaway: a general finaliser is a heavy, hazard-rich hammer. Where the
payload is itself just more heap data, the principled move is to make it
**GC-managed data in the first place** so no finaliser is ever needed.

## Proposed design: evaluate ADR-001's four options against the collector

ADR-001 lists four ways forward. The collector today is more capable than its
own documentation admits (per the [0005](0005-generational-gc.md) recon, and
confirmed in code): it has **opportunistic evacuation** with forwarding pointers
(`collect_with_evacuation`/`evacuate`, `src/eval/memory/collect.rs:465,298`) and
**lazy deferred sweep** (`defer_sweep`/`lazy_sweep_next`,
`src/eval/memory/heap.rs:674,680`). Crucially, it already manages **raw byte
backing stores** as first-class heap objects: an `Array<T>`'s buffer is allocated
with `alloc_bytes` (`src/eval/memory/array.rs:131`), marked via
`OpaqueHeapBytes`/`mark_raw_bytes` (`collect.rs:90,207`), evacuated, and has its
owning pointer rewritten by `set_backing_ptr` in the update phase (`array.rs:435`;
driven from `HeapSyn::scan_and_update`, `src/eval/memory/syntax.rs:574–606`).
This machinery is the key asset, and it decides the evaluation.

### (a) General finaliser table scanned during sweep

Maintain a side table of `(pointer, drop-fn)` for every object carrying a
non-trivial `Drop`. During sweep, for each block about to be reclaimed,
enumerate its dead finalisable objects and invoke their drop functions before
the bytes are overwritten.

*Against the code:* the sweep this must hook is lazy and block-granular
(`lazy_sweep_next`, `heap.rs:680`) — blocks reclaimed at allocation time, not in
one pass — so the table is consulted incrementally, per block, on the allocation
hot path. It must also be correct under **evacuation**: an evacuated object
(`evacuate`, `collect.rs:298`) is copied byte-for-byte and its *old* copy
abandoned in the candidate block, so the table must distinguish "dead, finalise"
from "moved, do not finalise" or it runs `Drop` on a live, forwarded object — the
resurrection hazard, concretely. This is the improved version of what ADR-001
tried, and inherits exactly the weakness ADR-001 measured: maintaining and
scanning the table is ongoing overhead on the busiest paths, threaded through
evacuation, lazy sweep, and the level-2 verifier. The pragmatic fallback, not the
principled answer.

### (b) Arena-allocated nodes instead of `Rc`

Replace `im_rc`'s `Rc` nodes with nodes bump-allocated from a side arena, freed
en masse. *Against the code:* this trades a leak for a correctness problem.
Persistent maps share nodes across versions; a per-block arena cannot free a node
that a *later* block still shares, and a global arena that never frees is just
the leak again. Lifetime-tracking the arena reintroduces reference counting by
hand. Rejected.

### (c) Store the persistent map inline in the GC heap *(recommended)*

Make CHAMP nodes **ordinary GC-managed heap objects**, allocated and scanned
exactly like the STG objects the collector already handles. No `Rc`, no Rust-heap
nodes, therefore **no finaliser ever required** — a dead node is reclaimed by the
same mark/sweep that reclaims a dead cons-cell.

Concretely, a CHAMP node is laid out as a header-prefixed heap object — the same
`AllocHeader` + payload shape as every STG allocation — containing:

- a `u32` **datamap** bitmap and a `u32` **nodemap** bitmap (CHAMP's two-array
  discriminator);
- an inline **data array** of `(SymbolId, value-ref)` pairs for keys resolved at
  this node, and an inline **node array** of references to child CHAMP nodes —
  both realised with the existing `Array<T>` whose backing buffer is already a
  scannable, evacuable GC object (`array.rs`, `collect.rs:220` `mark_array`).

Scanning is a new `impl GcScannable for ChampNode` modelled directly on the
existing `HeapSyn` arms (`syntax.rs:396` onward): `scan` marks the backing buffer
via `mark_array`, pushes it as `OpaqueHeapBytes`, and pushes each child-node
reference and each value-ref as grey objects; `scan_and_update` calls
`set_backing_ptr` after the backing is evacuated and rewrites any child/value
pointers the update phase has forwarded — line-for-line the pattern already in
`HeapSyn::scan_and_update` at `syntax.rs:574–606`. Evacuation needs **no new
code**: a CHAMP node is a header-prefixed object, so `evacuate` copies it and
installs a forwarding pointer like any other (`collect.rs:298`); its parent's
`scan_and_update` fixes the pointer. The value payloads stay as the same
`Ref`/closure pointers blocks already hold, so laziness and metadata are
unaffected.

The `Block` constructor changes from `Block(cons-list, index)` to
`Block(champ-root, meta)`; `LOOKUP`/`MERGE`/`ELEMENTS` reimplement against the
trie. Iteration for rendering walks the node arrays in order — the CHAMP
two-array split is what makes that iteration fast. This is the principled fix
because it **reuses evacuation and scanning that already exist**, introduces no
finalisation hazard, and fits Immix unchanged: nodes are normal small objects
that mark, evacuate, and sweep like everything else.

### (d) Switch to a `Drop`-supporting GC strategy

Replace the mark-region collector with one doing per-object deallocation so
`Drop` runs naturally. *Against the code:* this discards the Immix line-map
reclamation, evacuation, and lazy sweep the project has invested heavily in
(and debugged hard — see the aarch64 evacuation-corruption suite at
`collect.rs:1312` onward). It is a far larger and riskier rewrite than (c), to
buy a `Drop` we do not need if blocks are GC-native. Rejected.

### Recommendation

**Adopt (c), the inline-GC-heap CHAMP, as the solution; hold (a), the finaliser
table, as the fallback** if a future payload genuinely cannot be expressed as GC
data. (c) wins because it removes the problem rather than managing it: there is
nothing to finalise, so none of the finalisation hazards (resurrection,
ordering, non-determinism, the Rust `Drop`-safety questions) can arise, and the
collector needs no new phase — only new `GcScannable` impls slotting into the
existing mark/evacuate/update/sweep cycle.

## Interaction with the existing roadmap

This is **Track E / Whitespace** with no type-system dependency. Its sharpest
interaction is with [0005 — generational GC](0005-generational-gc.md), which
states the relationship from its side: both rewrite GC core, draw on the same
scarce expertise, and touch the same fragile, platform-sensitive code, so they
**must be sequenced, not interleaved** — **0005 first, then 0020.** 0005's cheap
early wins (allocation-rate trigger, `DefragmentationSweep`) and its nursery land
independently of blocks; and the nursery directly benefits the CHAMP design,
since persistent-map updates produce many short-lived intermediate nodes a
nursery collects cheaply (a generational collector is also a better host for the
fallback (a)). Doing 0005 first lowers 0020's risk and raises its payoff. 0020
shares 0005's hard constraint: everything runs **inside the stop-the-world,
single-threaded collector** (`src/eval/memory/heap.rs` safety preamble) — CHAMP
nodes are mutated only by the mutator and traced only by the collector, never
concurrently; concurrency stays deferred to [0008](0008-parallel-evaluation.md).

## Implementation sketch

| Phase | Change | Files | Size/risk |
|-------|--------|-------|-----------|
| 1 | `ChampNode` heap type: header layout, datamap/nodemap, two `Array`s | `src/eval/memory/` (new), `array.rs` | medium / medium |
| 2 | `GcScannable` for `ChampNode` (`scan`, `scan_and_update`) mirroring `HeapSyn` | `src/eval/memory/syntax.rs`/new | medium / **high** (GC correctness) |
| 3 | Trie ops: lookup, insert, structural-sharing merge, ordered iteration | `src/eval/stg/block.rs` | medium / medium |
| 4 | Switch `Block` ctor + `LOOKUP`/`MERGE`/`DEEPMERGE`/`ELEMENTS`; drop the `Rc` index | `block.rs`, `syntax.rs:47` | medium / medium |
| 5 | Validation under `EU_GC_VERIFY=2`, `EU_GC_POISON=1`, `EU_GC_STRESS=1`; benches | tests, benches | small / low |

Phase 2 is the genuine risk and where the existing GC debug harness (CLAUDE.md)
is indispensable: `EU_GC_STRESS=1` forces evacuation every cycle, exercising
CHAMP-node forwarding immediately; `EU_GC_POISON=1` catches any node whose lines
were mis-marked; `EU_GC_VERIFY=2` re-traces to prove no reachable node is missed.
A new verifier checkpoint should assert CHAMP child/value pointers are valid
post-update. Removing the `Rc<BlockIndex>` index slot (`syntax.rs:47`) is a small
ancillary win: it deletes the one leak we currently tolerate.

## Alternatives considered

- **Keep cons-lists; widen the cached index.** Build the index below threshold
  16 and preserve it across merges. Cheaper, but the index is still an
  unfinalised `Rc` (more of today's leak), merge still copies the whole spine
  with no structural sharing, and `deep-find`/iteration stay linear. A patch, not
  a fix.
- **`im`/`im_rc` with the finaliser table (a).** The literal ADR-001 path with a
  better finaliser. Viable as fallback, but inherits a B-tree 2–3× slower than
  std and pays ongoing table-scan overhead on the hot allocation path — the very
  cost ADR-001 measured. Held in reserve only.
- **Defer indefinitely (status quo).** Legitimate if blocks really are "usually
  small". The honest counter is below.

## Honest cost/benefit: when do O(n) blocks actually bite?

ADR-001's closing argument is "for most eucalypt programs O(n) is acceptable as
blocks are typically small", and for *small* blocks that is simply true: at
n ≤ 16 a linear scan beats a trie's pointer-chasing on cache effects alone, and
the index never even builds. The honest case for 0020 is therefore narrow.
O(n) blocks bite when:

1. **Large uniform configurations** — a generated block with hundreds or
   thousands of keys (Kubernetes manifests, large `values.yaml`-style inputs,
   data tables ingested via CSV/JSON). Here lookup crosses the index threshold
   and the `Rc` index leak is already incurred; CHAMP removes both the leak and
   the rebuild-on-merge.
2. **Merge-heavy pipelines** — repeated `<<`/`deep-merge` layering, `merge-all`
   over a list of blocks (`lib/prelude.eu:1662`). Each merge is O(n+m) spine-copy
   today (`Merge::execute`, `block.rs:1453`); structural sharing makes unchanged
   sub-trees free.
3. **Big `lookup`/`deep-find` loops** — `deep-find` (`prelude.eu:461`) over a
   large tree is O(nodes) × O(n) per block, the quadratic blow-up that sets the
   practical depth/size limit on generic block queries.

For the dominant eucalypt workload — small-to-medium config with shallow merges —
0020 buys little and costs real GC risk. That is the genuine tension the
maintainer must weigh, and it is why the horizon is 0.9 (after the type-system
work and after 0005), not sooner. The decision hinges on whether large/merge-heavy
configs are a target class for 1.0. If they are, 0020 removes a hard scaling wall
*and* deletes the one leak in the heap today; if they are not, deferring remains
defensible.

## Risks & what would kill this

- **CHAMP-node tracing bugs.** Getting `scan`/`scan_and_update`/evacuation right
  for a new pointer-bearing heap type is the same class of subtle, sometimes
  platform-specific bug the existing evacuation suite documents
  (`collect.rs:1312`+). Mitigation: the `EU_GC_VERIFY`/`POISON`/`STRESS` harness
  and a dedicated CHAMP-pointer checkpoint. **A use-after-free under
  `EU_GC_POISON=1` falsifies the implementation.**
- **No net win on real configs.** If benchmarks on representative large/merge
  configs show CHAMP failing to beat the indexed cons-list end-to-end (rendering
  included), the value case collapses. Mitigation: gate acceptance on
  independently verified benches (CLAUDE.md) over large-block and merge-heavy
  inputs, not microbenchmarks.
- **Collides with 0005.** Both rewrite GC core. Mitigation: strict sequencing —
  0005 first.
- **Pointer-chasing regresses small blocks.** A trie can lose to a linear scan
  for tiny n. Mitigation: keep a small inline-leaf fast path (CHAMP's inline data
  array already gives this for the root) and benchmark the small-block case as a
  guardrail.

## Success criteria

- **Lookup complexity.** A 1,000-key block shows lookup cost growing
  ~logarithmically, not linearly, with key count.
- **Merge shares structure.** `<<`/`deep-merge` of two large blocks differing in
  a few keys allocates proportional to the *difference*, not the total size.
- **`deep-find` scales.** `deep-find` over a large deep block drops from
  quadratic toward near-linear in node count.
- **No leak.** GC-churn benches (`002_thunk_updates`, `007_short_lived`) show
  **no regression** — the 220–580% ADR-001 regression does not recur — and the
  `Rc<BlockIndex>` index slot is gone.
- **Correctness.** Full harness passes under `EU_GC_VERIFY=2` and
  `EU_GC_POISON=1`; the new CHAMP-pointer checkpoint holds across the suite.
- **Small-block neutrality.** No significant regression on small-block-heavy
  programs.

## References

**Eucalypt source.** `src/eval/stg/block.rs` (79/87 — block constructor; 469 —
index threshold; 495 — linear `find`; 854 — `store_index_in_block`; 1453/1560 —
`Merge`/`MergeWith` spine-copy); `src/eval/memory/syntax.rs` (33/47 —
`BlockIndex`/`Native::Index(Rc<…>)`; 396/574–606 — `HeapSyn` scan and
`scan_and_update`); `src/eval/memory/bump.rs` (294/311/371 — bump/fill/recycle,
no `Drop`); `src/eval/memory/heap.rs` (safety preamble — single-threaded
invariant; 674/680 — defer/lazy sweep); `src/eval/memory/collect.rs` (90/207 —
`OpaqueHeapBytes`/`mark_raw_bytes`; 220 — `mark_array`; 298 — `evacuate`;
465 — `collect_with_evacuation`; 1312+ — evacuation-bug suite);
`src/eval/memory/array.rs` (131 — `alloc_bytes`; 419/435 —
`allocated_data`/`set_backing_ptr`); `lib/prelude.eu` (359/365/372 —
merge/`<<`; 408–441 — lookup family; 461 — `deep-find`; 1662 — `merge-all`).
Also `docs/development/architectural-decisions.md` (ADR-001) and branch
`feat/furnace-persistent-blocks-v2`.

**Papers & systems.** P. Bagwell, *Ideal Hash Trees*, EPFL TR, 2001.
M. J. Steindorfer & J. J. Vinju, *Optimizing Hash-Array Mapped Tries for Fast and
Lean Immutable JVM Collections*, OOPSLA 2015 (CHAMP; 1.3–6.7× iteration,
3–25.4× equality; adopted by Scala 2.13). E. J. Hughes & L. Tratt, *Garbage
Collection for Rust: The Finalizer Frontier*, 2024. *Object resurrection* and
*Finalizer* (Wikipedia); SEI CERT MET12-J, *Do not use finalizers*; Java
`Cleaner`/`finalize()` deprecation. The `im` / `im_rc` crates (bodil),
docs.rs (OrdMap = B-tree, `Rc` nodes, 2–3× slower than std).

**Peer languages.** Clojure and Scala ship HAMT/CHAMP immutable maps as standard
collections; their experience is the evidence that a persistent O(log n) map is
the right representation once the host can reclaim its nodes. Among the data
peers, none resolves the GC-finalisation issue for us — the answer is internal.
