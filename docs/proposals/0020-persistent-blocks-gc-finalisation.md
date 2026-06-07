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
for programs that barely use blocks. This proposal argues the right fix attacks
the **underlying GC-finalisation problem**, not the block representation. It
evaluates ADR-001's four candidate paths against the collector as it actually
exists, and recommends holding the map **inline in the GC heap** as ordinary
scannable objects via the existing `Array` backing-store machinery — sidestepping
finalisation entirely — as a **CHAMP** (an improved HAMT) key-index paired with a
persistent-vector *order sequence* that keeps a block's defining insertion order,
and applied only above the existing small-block threshold so plain cons-lists
still serve the common case. A finaliser table is the pragmatic fallback. It is deep GC work that competes with
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

### Blocks are ordered, and the order is part of the value

A block is not merely a map: it is **insertion-ordered**, and that order is
observable output. `{ a: 1 b: 2 }` renders `a` before `b`; merging preserves it —
overriding an existing key updates it **in place**, keeping its position, while a
genuinely *new* key is **appended** (all three behaviours verified directly
against `./target/release/eu`). Any representation must reproduce this exactly,
and it is the one requirement a hash trie cannot meet on its own: a CHAMP orders
entries by hash, so a bare CHAMP would silently reorder every rendered document.
Order preservation is thus a hard constraint on the design, not a nicety — it is
what forces the two-structure shape recommended in (c).

### Merge is catenation, and it is everywhere

The index helps lookup but does nothing for the other half of the problem.
Block merge — `merge`, `deep-merge`, and `<<` (`lib/prelude.eu:359`, `365`,
`372`) — walks **both** operand lists into an `IndexMap` and re-emits a fresh
cons-list (`Merge::execute`, `block.rs:1453`; `MergeWith::execute`,
`block.rs:1560`). Every merge is O(n+m), allocates a whole new spine, and
discards the inputs' cached index; `deep-merge` recurses this at every level.
Configuration workloads are merge-heavy by nature — layering a base over an
override over a local patch is the canonical eucalypt task — so this spine-copy
is the main line, not an edge case. A persistent map with structural sharing
would make merge *share* unchanged sub-trees instead of copying them. The same
linearity sets the practical ceiling on `deep-find` (`lib/prelude.eu:461`), which
walks a whole block tree for a key at any depth: O(nodes) × O(n) per block,
quadratic in practice (quantified in §"Honest cost/benefit").

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

**Persistent vectors (Bagwell; RRB).** Clojure's `PersistentVector` is a 32-way
trie giving O(log₃₂ n) append/update/index and O(n) in-order iteration with
structural sharing — the vector analogue of a HAMT. Relaxed Radix-Balanced (RRB)
trees (Bagwell & Rompf, 2011) add O(log n) concatenation and slicing on top. This
is the structure that can carry a block's **insertion order** persistently:
appends share the existing spine, and updating one slot rewrites only that
root-to-leaf path.

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

Replace `im_rc`'s `Rc` nodes with arena-allocated nodes freed en masse.
*Against the code:* persistent maps share nodes across versions, so a per-block
arena cannot free a node a *later* block still shares, and a global arena that
never frees is just the leak again. Lifetime-tracking the arena reintroduces
reference counting by hand. Rejected.

### (c) Store the persistent structures inline in the GC heap *(recommended)*

Make the persistent structures' nodes **ordinary GC-managed heap objects**,
allocated and scanned exactly like the STG objects the collector already handles. No `Rc`, no Rust-heap
nodes, therefore **no finaliser ever required** — a dead node is reclaimed by the
same mark/sweep that reclaims a dead cons-cell.

Concretely, a CHAMP node is laid out as a header-prefixed heap object — the same
`AllocHeader` + payload shape as every STG allocation — containing:

- a `u32` **datamap** bitmap and a `u32` **nodemap** bitmap (CHAMP's two-array
  discriminator);
- an inline **data array** of `(SymbolId, order-slot)` entries for keys resolved
  at this node — the `order-slot` indexing the *order sequence* introduced below,
  where the values themselves live — and an inline **node array** of references to
  child CHAMP nodes; both are the existing `Array<T>` whose backing buffer is
  already a scannable, evacuable GC object (`array.rs`, `collect.rs:220`
  `mark_array`).

Scanning is a new `impl GcScannable for ChampNode` modelled directly on the
existing `HeapSyn` arms (`syntax.rs:396` onward): `scan` marks the backing buffer
via `mark_array`, pushes it as `OpaqueHeapBytes`, and pushes each child-node
reference as a grey object (the `order-slot`s are plain indices, not pointers);
`scan_and_update` calls `set_backing_ptr` after the backing is evacuated and
rewrites any child pointers the update phase has forwarded — line-for-line the
pattern already in `HeapSyn::scan_and_update` at `syntax.rs:574–606`. Evacuation
needs **no new code**: a CHAMP node is a header-prefixed object, so `evacuate`
copies it and installs a forwarding pointer like any other (`collect.rs:298`); its
parent's `scan_and_update` fixes the pointer. The value payloads stay as the same
`Ref`/closure pointers blocks already hold — now carried by the order sequence
below — so laziness and metadata are unaffected.

**Preserving order: a key-index plus an order sequence.** A CHAMP answers lookup
but, being ordered by hash, cannot answer *render* — so a large block is **two
GC-native structures, not one**: the CHAMP as a key-index, and a persistent
**order sequence** (a Bagwell/RRB persistent vector — §"Prior art & landscape")
holding the `(key, value)` pairs in insertion order. The CHAMP maps
`SymbolId → order-slot`; the order sequence is the single source of truth for
values and for iteration. This reproduces the verified block semantics exactly:

- **lookup** descends the CHAMP to a slot, then indexes the order sequence — both
  O(log n);
- **render / `ELEMENTS`** walks the order sequence front to back — O(n), values in
  hand, no per-key lookup (this is what an earlier draft got wrong: the CHAMP's
  own node-array order is *hash* order, useless for rendering);
- **override** of an existing key updates that one slot in the order sequence,
  leaving its position — and the CHAMP — untouched, so only the order sequence
  allocates;
- a **new key** is appended to the order sequence and inserted into the CHAMP.

Slot positions stay stable because blocks only ever append or update in place
(deletion rebuilds a fresh block, exactly as the prelude filters do today), so
the index never dangles. Both structures share unchanged sub-trees across a
merge, so `<<`/`deep-merge` allocates in proportion to the **changed** keys, not
the total size. (Lookup can be cut to a single descent by also caching the
value-ref in the CHAMP slot, at the cost of keeping two copies in step on
override — a tuning choice, not a semantic one.)

**Both structures are the same machinery twice.** The order sequence's nodes are
header-prefixed heap objects backed by the very same scannable, evacuable
`Array<Ref>` as the CHAMP nodes, with a `GcScannable` impl of the identical shape
(it is the order-sequence nodes that push the **value-refs** as grey objects).
Adding the second structure is therefore not double the GC risk: it is one
backing-store discipline, instantiated for two node types. Order preservation
costs design clarity, not new collector surface.

**Keep cons-lists below the threshold (the hybrid).** None of this should touch
small blocks. Below `BLOCK_INDEX_THRESHOLD` (the existing 16, `block.rs:469`) a
block stays a plain **cons-list** — ordered, allocation-light, and faster than a
trie at small *n* on cache effects alone — exactly as today; the paired
CHAMP-plus-order-sequence form is built only at or above the threshold, chosen at
construction or merge time when the result size is known, never by in-place
mutation. This mirrors both the existing index threshold and Clojure's own
array-map → hash-map transition, and keeps the change **purely additive** for the
dominant small-block workload.

The `Block` constructor accordingly changes from `Block(cons-list, index)` to
`Block(repr, meta)`, where `repr` is the cons-list below threshold and the
`(champ-index, order-sequence)` pair above it, and `meta` replaces the `Rc`
`Native::Index` slot (`syntax.rs:47`) — deleting the one leak we tolerate today.
`LOOKUP`/`MERGE`/`DEEPMERGE`/`ELEMENTS` dispatch on `repr`. This is the principled
fix: it **reuses evacuation and scanning that already exist**, adds no
finalisation hazard, and fits Immix unchanged — every node is a normal small
object that marks, evacuates, and sweeps like everything else.

**A note on `vec`.** The primitive `vec` (`HeapVec { elements: Vec<Primitive> }`,
`vec.rs:14`) is a *flat* array with **O(1)** indexed access — deliberately not a
trie — and must stay that way; it should **not** be folded into the order
sequence's persistent vector, which would regress its defining property. What
`vec` and the new structures genuinely share is, again, the GC-managed scannable
`Array`. Lifting `vec`'s primitive-only restriction so it can hold arbitrary
values is a small, **separate** cleanup riding on that machinery — the backing
becomes an `Array<Closure>` (*not* `Array<Ref>`: a `Ref` cannot name a
block/list/lambda, so the precedent is the env frame's `Array<C>`,
`env.rs:196`). It is worked up as **0000 F6** — a generalisation, not a structure
merge.

### (d) Switch to a `Drop`-supporting GC strategy

Replace the mark-region collector with one doing per-object deallocation so
`Drop` runs naturally. *Against the code:* this discards the Immix line-map
reclamation, evacuation, and lazy sweep the project has invested heavily in
(and debugged hard — see the aarch64 evacuation-corruption suite at
`collect.rs:1312` onward). It is a far larger and riskier rewrite than (c), to
buy a `Drop` we do not need if blocks are GC-native. Rejected.

### Recommendation

**Adopt (c), the inline-GC-heap CHAMP-plus-order-sequence, as the solution; hold
(a), the finaliser table, as the fallback** if a future payload genuinely cannot be expressed as GC
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
since persistent-structure updates produce many short-lived intermediate nodes a
nursery collects cheaply (a generational collector is also a better host for the
fallback (a)). Doing 0005 first lowers 0020's risk and raises its payoff. 0020
shares 0005's hard constraint: everything runs **inside the stop-the-world,
single-threaded collector** (`src/eval/memory/heap.rs` safety preamble) — CHAMP
nodes are mutated only by the mutator and traced only by the collector, never
concurrently; concurrency stays deferred to [0008](0008-parallel-evaluation.md).

## Implementation sketch

| Phase | Change | Files | Size/risk |
|-------|--------|-------|-----------|
| 1 | `ChampNode` **and order-sequence (persistent-vector) node** heap types: header layout, datamap/nodemap, `Array<Ref>` backing | `src/eval/memory/` (new), `array.rs` | medium / medium |
| 2 | `GcScannable` for both node types (`scan`, `scan_and_update`) mirroring `HeapSyn` | `src/eval/memory/syntax.rs`/new | medium / **high** (GC correctness) |
| 3 | Trie + order-sequence ops: lookup, append, update-in-place, structural-sharing merge, in-order iteration | `src/eval/stg/block.rs` | medium / medium |
| 4 | Dual `Block` repr behind the threshold; dispatch `LOOKUP`/`MERGE`/`DEEPMERGE`/`ELEMENTS`; drop the `Rc` index | `block.rs`, `syntax.rs:47` | medium / medium |
| 5 | Validation under `EU_GC_VERIFY=2`, `EU_GC_POISON=1`, `EU_GC_STRESS=1`; benches | tests, benches | small / low |

Phase 2 is the genuine risk, and the existing GC debug harness (CLAUDE.md) is
indispensable there: `EU_GC_STRESS=1` forces evacuation every cycle (exercising
CHAMP-node forwarding immediately), `EU_GC_POISON=1` catches mis-marked node
lines, and `EU_GC_VERIFY=2` re-traces to prove no reachable node is missed; a new
checkpoint should assert CHAMP child/value pointers are valid post-update.
Removing the `Rc<BlockIndex>` slot (`syntax.rs:47`) deletes the one leak we
currently tolerate.

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
0020 buys little and costs real GC risk; hence the 0.9 horizon, after the
type-system work and after 0005. The decision hinges on whether large/merge-heavy
configs are a target class for 1.0: if they are, 0020 removes a hard scaling wall
*and* deletes the one leak in the heap today; if not, deferring remains
defensible.

## Risks & what would kill this

- **CHAMP-node tracing bugs.** Getting `scan`/`scan_and_update`/evacuation right
  for a new pointer-bearing heap type is the same class of subtle, platform-specific
  bug the existing evacuation suite documents (`collect.rs:1312`+). Mitigation:
  the `EU_GC_VERIFY`/`POISON`/`STRESS` harness plus a CHAMP-pointer checkpoint.
  **A use-after-free under `EU_GC_POISON=1` falsifies the implementation.**
- **No net win on real configs.** If benches on representative large/merge configs
  show CHAMP failing to beat the indexed cons-list end-to-end (rendering included),
  the value case collapses. Mitigation: gate acceptance on independently verified
  benches (CLAUDE.md), not microbenchmarks.
- **Collides with 0005.** Both rewrite GC core. Mitigation: strict sequencing.
- **Pointer-chasing regresses small blocks.** A trie can lose to a linear scan at
  tiny *n*. Mitigation: the threshold hybrid keeps sub-16 blocks as cons-lists, so
  small blocks never touch the trie at all; a small-block guardrail benchmark
  confirms neutrality.

## Success criteria

- **Lookup complexity.** A 1,000-key block shows lookup cost growing
  ~logarithmically, not linearly, with key count.
- **Merge shares structure.** `<<`/`deep-merge` of two large blocks differing in
  a few keys allocates proportional to the *difference*, not the total size.
- **`deep-find` scales.** `deep-find` over a large deep block drops from
  quadratic toward near-linear in node count.
- **Order preserved.** Rendered key order is byte-identical to the cons-list
  implementation across the whole harness — overrides keep position, new keys
  append — checked by golden output, not merely by complexity bounds.
- **No leak.** GC-churn benches (`002_thunk_updates`, `007_short_lived`) show
  **no regression** — the 220–580% ADR-001 regression does not recur — and the
  `Rc<BlockIndex>` index slot is gone.
- **Correctness.** Full harness passes under `EU_GC_VERIFY=2` and
  `EU_GC_POISON=1`; the new CHAMP-pointer checkpoint holds across the suite.
- **Small-block neutrality.** No significant regression on small-block-heavy
  programs.

## References

**Eucalypt source** (specific lines cited inline in the body): `src/eval/stg/block.rs`
(block constructor, index threshold, linear `find`, `store_index_in_block`,
`Merge`/`MergeWith`); `src/eval/memory/syntax.rs` (`BlockIndex`/`Native::Index`,
`HeapSyn` scan/`scan_and_update`); `src/eval/memory/bump.rs` (bump/recycle, no
`Drop`); `src/eval/memory/heap.rs` (single-threaded invariant, defer/lazy sweep);
`src/eval/memory/collect.rs` (`OpaqueHeapBytes`, `mark_array`, `evacuate`,
`collect_with_evacuation`, evacuation-bug suite); `src/eval/memory/array.rs`
(`alloc_bytes`, `set_backing_ptr`); `lib/prelude.eu` (merge/`<<`, lookup family,
`deep-find`, `merge-all`). Also `docs/development/architectural-decisions.md`
(ADR-001) and branch `feat/furnace-persistent-blocks-v2`.

**Papers & systems.** P. Bagwell, *Ideal Hash Trees*, EPFL TR, 2001.
M. J. Steindorfer & J. J. Vinju, *Optimizing Hash-Array Mapped Tries for Fast and
Lean Immutable JVM Collections*, OOPSLA 2015 (CHAMP; 1.3–6.7× iteration,
3–25.4× equality; adopted by Scala 2.13). P. Bagwell & T. Rompf, *RRB-Trees:
Efficient Immutable Vectors*, EPFL TR, 2011 (relaxed radix-balanced persistent
vectors); Clojure's 32-way `PersistentVector`. E. J. Hughes & L. Tratt, *Garbage
Collection for Rust: The Finalizer Frontier*, 2024. *Object resurrection* and
*Finalizer* (Wikipedia); SEI CERT MET12-J, *Do not use finalizers*; Java
`Cleaner`/`finalize()` deprecation. The `im` / `im_rc` crates (bodil),
docs.rs (OrdMap = B-tree, `Rc` nodes, 2–3× slower than std).

**Peer languages.** Clojure and Scala ship HAMT/CHAMP immutable maps as standard
collections — evidence that a persistent O(log n) map is the right representation
once the host can reclaim its nodes. None of the data peers resolves the
GC-finalisation issue for us; the answer is internal.
