# Env-Frame Allocation Profiling Report

**Version:** eucalypt 0.9.2  
**Date:** 2026-06-19  
**Beads:** eu-1gfj (env-frame allocation hot path), eu-fwlt (O(depth) env traversal)  
**Purpose:** Pre-0.10.0 baseline — establish allocation and traversal costs across all AoC 2025 examples

---

## 1. Methodology

All measurements use the release binary (`cargo build --release`). Tools used:

- `--statistics-file` for allocation and timing data
- `EU_STACK_DIAG=1` for continuation stack depth distribution
- `eu dump stg` for de Bruijn index distribution (static proxy for env-chain traversal depth)
- Source analysis for object sizes and allocator hot-path review

**Note on `machine_allocs`:** This counter records the *sum of binding counts* across every `Let`/`LetRec` instruction executed, not the count of heap objects allocated. It is a lower bound on the number of closures written into env-frame backing arrays.

---

## 2. Object Sizes

| Object | Size |
|--------|------|
| `AllocHeader` | 16 bytes |
| `EnvFrame` (`EnvironmentFrame<SynClosure>`) | 48 bytes |
| Total per `EnvFrame` allocation | **64 bytes** |
| `SynClosure` | 16 bytes |
| `Array<SynClosure>` header | 16 bytes |
| Per-binding overhead in backing array | 16 bytes/binding |

A frame with *n* bindings therefore costs: 64 + 16 + 16*n bytes (header + array header + n closures). For a typical frame with 4 bindings that is **144 bytes total**, or **36 bytes per binding amortised**.

---

## 3. Allocation Hot Path

The bump allocator (`src/eval/memory/bump.rs`, `BumpBlock::bump()`) is the common path:

```
cursor -= size   // subtract from high-water mark
return block[cursor]
```

Fast path is two arithmetic ops plus a pointer return — no lock, no syscall. Slow path (crossing a hole or block boundary) calls `find_hole()` and potentially `replace_head_targeted()`. GC fires only when the heap cannot satisfy an allocation after an emergency sweep attempt.

**Per-call overhead is negligible.** The bottleneck is *volume*, not the allocator itself.

---

## 4. Allocation Volume by Example

| Example | Binds (M) | Blocks (K) | Heap (GB) | VM (s) | Ticks/bind |
|---------|-----------|-----------|-----------|--------|-----------|
| day10-part-1 | 228.3 | 944.2 | 30.9 | 22.1 | 2.3 |
| day08-part-1 | 117.6 | 404.3 | 13.2 | 10.9 | 2.0 |
| day09-part-1 | 80.4 | 279.8 | 9.2 | 8.3 | 2.5 |
| day03-part-2 | 25.2 | 95.0 | 3.1 | 2.3 | 2.2 |
| day04-part-1 | 19.7 | 72.3 | 2.4 | 2.5 | 2.9 |
| day07-part-1 | 16.4 | 52.2 | 1.7 | 1.8 | 2.7 |
| day05-part-1 | 14.1 | 49.7 | 1.6 | 2.2 | 4.4 |
| day03-part-1 | 11.6 | 43.6 | 1.4 | 1.1 | 2.3 |
| day06-part-2 | 10.6 | 33.2 | 1.1 | 4.8 | 11.9 |
| day07-part-2 | 10.4 | 33.1 | 1.1 | 2.5 | 2.8 |
| day11-part-2 | 7.4 | 311.5 | 10.2 | 144.9 | **415.3** |
| day01-part-2 | 4.8 | 14.9 | 0.5 | 9.7 | 39.2 |
| day12-part-1 | 3.5 | 9.4 | 0.3 | 0.3 | 1.9 |
| day01-part-1 | 2.7 | 6.2 | 0.2 | 3.5 | 17.8 |
| day06-part-1 | 1.6 | 5.4 | 0.2 | 0.6 | 10.4 |
| day05-part-2 | 0.7 | 2.1 | 0.1 | 0.05 | 1.7 |
| day02-part-2 | 0.6 | 2.5 | 0.1 | 0.06 | 2.2 |
| day11-part-1 | 0.5 | 8.3 | 0.3 | 1.5 | 125.5 |
| day02-part-1 | 0.1 | 0.6 | 0.02 | 0.01 | 2.3 |

**Key observations:**

- **day10, day08, day09** are the three most allocation-intensive examples, each consuming 9–31 GB of heap. They are allocation-bound (2–2.5 ticks/bind), meaning the CPU barely pauses between Let executions.
- **day11-part-2** (415 ticks/bind) and **day11-part-1** (125 ticks/bind) are computation-bound, not allocation-bound. Large work is done between allocations.
- **GC barely fires**: every measured example triggers exactly 2 collections with `blocks_recycled = 0`. The GC cannot reclaim anything — objects are all live at collection time. This suggests the working set grows monotonically (lazy spine construction) and the GC adds overhead without benefit.

### Bytes per binding

With roughly 36 bytes/binding amortised (frame + array + closures), the expected heap usage for day10 is:

```
228M bindings × 36 B = 8.2 GB (binding store only)
944K blocks × 32 KB = 30.9 GB (actual)
```

The factor of ~3.8× overshoot relative to pure binding bytes indicates significant additional allocation for `HeapSyn` nodes (code objects): `App`, `Case`, `Cons`, `Let`, `Ann`, etc. These are allocated once per evaluation site but multiply as thunks are forced and update frames rewrite them.

---

## 5. Environment Traversal — O(depth) Analysis

### 5.1 The traversal code

`EnvironmentFrame::cell()` (`src/eval/machine/env.rs:334`):

```rust
fn cell(&self, guard: &dyn MutatorScope, idx: usize) -> Option<(Array<C>, usize)> {
    let len = self.logical_len();
    if idx < len {
        Some((self.bindings.clone(), self.physical_index(idx)))
    } else {
        match self.next {
            Some(ref env) => (*ScopedPtr::from_non_null(guard, *env)).cell(guard, idx - len),
            None => None,
        }
    }
}
```

For a lookup of `Ref::L(i)`, this dereferences one `RefPtr<EnvFrame>` per frame in the chain until it finds the frame containing binding `i`. With *k* frames of equal size *b*, the worst case is *k = i / b* dereferences — each potentially a cache miss given the heap's scattered allocation pattern.

### 5.2 De Bruijn index distribution (static proxy for traversal depth)

The de Bruijn index `i` in `Ref::L(i)` is the exact number of bindings that `cell()` must skip before finding the target. Index statistics from `eu dump stg`:

| File | Refs | Max | P95 | Mean |
|------|------|-----|-----|------|
| day01 | 20,177 | **9,550** | 9,032 | **4,507** |
| day05 | 5,753 | 2,394 | 2,234 | 983 |
| day12 | 2,835 | 2,089 | 1,919 | 749 |
| day08 | 5,219 | 2,079 | 1,870 | 769 |
| day11 | 3,158 | 1,239 | 1,100 | 442 |
| day09 | 5,784 | 1,023 | 849 | 173 |
| day10 | 8,748 | 428 | 152 | 18 |
| day03 | 1,421 | 414 | 365 | 114 |
| day04 | 1,243 | 334 | 240 | 65 |
| day07 | 1,722 | 349 | 242 | 52 |
| day06 | 976 | 81 | 39 | 5 |
| day02 | 1,964 | 76 | 19 | 3 |

### 5.3 The prelude dominates — the key finding

**day01** has max de Bruijn index **9,550** from a very simple 5-line program. Investigation of the STG dump reveals the high-index references are in `Cons` cells within the prelude's compiled LetRec:

```
[9535] Cons(✳9534, ✳9533)
```

The prelude is compiled as a **monolithic LetRec** of approximately 9,500 bindings. Functions within the prelude that reference each other across the binding list carry de Bruijn indices proportional to their distance in the list.

The ratio of local to global refs in day01's dump:
- Local (`✳`) refs: **20,177** — all traversed via `cell()` (O(depth))
- Global (`⊗`) refs: **81** — looked up via flat globals array (O(1))

The prelude's internal cross-references account for the extreme mean (4,507). Every call from a late-defined prelude function to an early-defined one must traverse thousands of env-chain links.

In contrast, **day02** — which uses fewer prelude functions and has simpler user code — has max 76, mean 3.2. The impact is programme-dependent: programmes that stress-test the prelude heavily suffer most.

### 5.4 User-code O(depth) — day08

**day08** has mean de Bruijn index **769** and max **2,079** entirely from user-code scope nesting (`prepare`'s deeply nested block expressions, `solve-n`'s cascading let-bindings). This is independent of the prelude effect.

With 117M bindings at 2 ticks/bind, and ~769/4 ≈ 192 frame pointer dereferences per lookup, the env-traversal overhead for day08 is substantial even at the per-lookup level.

---

## 6. Continuation Stack Depth

| Example | Max depth | Branch | Update | ApplyTo | Pattern |
|---------|-----------|--------|--------|---------|---------|
| day08-part-1 | **138,752** | 138,711 | 40 | 1 | List spine (lazy) |
| day06-part-2 | 18,878 | 7,553 | 11,324 | 1 | Mixed |
| day05-part-2 | 2,631 | 1,307 | 1,323 | 1 | Balanced |
| day09-part-1 | 2,997 | 1,494 | 1,500 | 3 | Balanced |
| day06-part-1 | 2,025 | 1,008 | 1,014 | 3 | Balanced |
| day07-part-2 | 2,006 | 1,001 | 1,004 | 1 | Balanced |
| day01-part-1 | 2,360 | 1,179 | 1,180 | 1 | Balanced |
| day12-part-1 | 1,082 | 540 | 541 | 1 | Balanced |
| day05-part-1 | 1,116 | 557 | 558 | 1 | Balanced |
| day03-part-1 | 1,031 | 513 | 517 | 1 | Balanced |
| day07-part-1 | 727 | 291 | 435 | 1 | Update-heavy |
| day10-part-1 | 566 | 275 | 290 | 1 | Balanced |
| day04-part-1 | 576 | 283 | 292 | 1 | Balanced |
| day02-part-1 | 122 | 57 | 62 | 3 | Balanced |

**day08** is the extreme outlier: 138,711 Branch frames with only 40 Update frames. This pattern — vastly more Branch than Update — indicates a deeply lazy list spine is being constructed: each `case xs of { Cons h t -> ... }` pushes a Branch continuation but the forced thunks update quickly. The `concat` of window-pair lists creates a chain of 138K nested case expressions that must be traversed end-to-end when the head of the concatenated list is demanded.

**Balanced Branch/Update patterns** (most other examples) indicate balanced thunk-forcing traversal over a list — typical `map`/`filter`/`scanl` patterns.

The continuation stack is a **separate concern from env-chain depth**: day08 has both a very deep continuation stack (138K) and deep env chains (mean 769), while day10 has a shallow continuation stack (566) but massive allocation volume (228M bindings, 30 GB).

---

## 7. GC Behaviour

All measured examples trigger exactly **2 GC collections** with **0 blocks recycled**. This means:

1. The GC fires at two heap checkpoints but cannot reclaim any blocks.
2. The entire heap is live at collection time — objects are retained by the lazy thunk spine.
3. GC overhead is ~30ms total (negligible vs multi-second VM time).
4. A generational or nursery scheme would **not help** unless objects are short-lived, which they are not — the lazy spine forces all thunks to remain live until the final result is forced.

**Implication for nursery sizing:** A small nursery would cause very frequent promotions (all objects promoted on first collection) without any benefit in dead-object elimination. If a nursery is implemented, it should be sized to at least cover the working set of the innermost evaluation loop — but the primary benefit would be locality (cache warmth), not GC throughput.

---

## 8. Hypotheses and Recommendations

### H1: Prelude LetRec depth causes systematic O(depth) overhead

**Confidence: High.**  
The prelude compiled as a monolithic LetRec of ~9,500 bindings gives every prelude-to-prelude cross-reference a de Bruijn index in the thousands. Moving prelude bindings to the globals array (flat, O(1) lookup) would eliminate this overhead entirely. This is the highest-impact change available.

**Affected programmes:** Any programme that calls many prelude functions (most real programmes). day01 shows mean index 4,507; the overhead is proportional to how often high-index prelude functions are called.

**Action:** Compile prelude as pre-linked globals (see branch `furnace/precompiled-prelude`). Validate with day01-part-2 as the regression target (39 ticks/bind, high ratio of prelude calls).

### H2: User-code block nesting creates O(depth) env chains in day08

**Confidence: High.**  
day08's `prepare` function builds deeply nested block scopes. Mean de Bruijn index 769 from user code (not prelude) means each binding lookup traverses ~192 frame pointers on average. The demand analysis work (W11) that eliminates redundant bindings would reduce the env chain depth and thus traversal cost.

**Action:** Demand analysis to eliminate dead bindings, reducing env chain length. Alternatively, flatten block scopes in the compiler where bindings are independent.

### H3: Lazy spine accumulation drives continuation stack depth in day08

**Confidence: High.**  
day08's 138K Branch frames come from `concat` of a large list-of-lists being evaluated lazily. This is correct behaviour (lazy evaluation), but the O(n) stack depth is a memory concern. Tail-call optimisation or strictness analysis for `concat` could reduce this.

**Action:** Investigate whether `concat` in the prelude can be compiled with an accumulator to avoid O(n) continuation stack depth.

### H4: GC nursery sizing for allocation-bound programmes

**Confidence: Medium.**  
For day10 (228M bindings, 30 GB), 2 GC collections produce 0 recycled blocks — the entire heap is live. A small nursery adds GC overhead without reclaiming anything. If a nursery is introduced:
- Size it at ≥ 1 GB for large workloads (to match the working set of a single outer iteration)
- Or use a "sticky mark bits" strategy where the nursery only contains objects allocated since the last GC

**Action:** Consider nursery only after prelude O(depth) fix (H1) is in, to correctly measure the benefit.

### H5: day11 is computation-bound, not allocation-bound

**Confidence: High.**  
day11-part-2 has 415 ticks/bind and 145s runtime. This is not an allocation or env-traversal problem — the computation between allocations is expensive. Likely a branch-and-bound search or a matrix algorithm that does significant arithmetic.

**Action:** Profile day11-part-2 separately as a computation hotspot for W11 (demand analysis won't help here).

---

## 9. Priority Order for 0.10.0

| Priority | Action | Expected Impact | Target Example |
|----------|--------|----------------|----------------|
| P1 | Fix prelude O(depth): move prelude to globals array | ~40% reduction for day01-part-2 (39 ticks/bind → ~10) | day01 |
| P2 | Demand analysis: eliminate dead bindings | Reduce heap, env depth, GC pressure | day08, day09 |
| P3 | Investigate `concat` continuation depth | Reduce 138K Branch stack in day08 | day08 |
| P4 | GC nursery (post H1) | Improved locality; unclear throughput benefit | day10, day08 |
| P5 | day11 computation profiling | Understand 415 ticks/bind | day11 |

---

*Raw data: see `allocation-data.json` in this directory.*
