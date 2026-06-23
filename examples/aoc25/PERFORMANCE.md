# AoC 2025 Performance Analysis

Performance characteristics of the AoC 2025 eucalypt example programs, benchmarked on the 0.10.0 release binary on ARM64 (Apple Silicon). All timings are 5-run means using user CPU time. Tick counts and allocation figures come from eucalypt's built-in statistics output. The 0.10.0 release introduced strict eager evaluation of prelude globals (blob mode), which eliminated the uniform 2-GC-collection overhead present in all 0.9.x runs.

---

## Summary

| Example | Wall time | Ticks | Allocs | Primary bottleneck | 0.10.0 improvement |
|---------|-----------|-------|--------|--------------------|--------------------|
| day01-p1 | ~0.41s | 13,015,410 | 1,879,963 | VM dispatch | −89% wall time |
| day01-p2 | ~0.36s | 13,423,535 | 1,923,144 | VM dispatch | −96% wall time |
| day02-p1 | ~0.04s | 295,133 | 52,469 | Trivial | −61% wall time |
| day02-p2 | ~0.05s | 1,319,154 | 175,899 | Trivial | −58% wall time |
| day03-p1 | ~1.03s | 27,813,225 | 11,553,241 | VM dispatch | −4% wall time (GC eliminated) |
| day03-p2 | ~2.31s | 57,806,510 | 25,204,145 | VM dispatch | ≈0% (GC eliminated) |
| day04-p1 | ~1.10s | 53,908,706 | 3,215,923 | VM dispatch + env walk | −53% wall time |
| day04-p2 | >600s (timeout) | — | — | VM dispatch (loop-bound) | first analysed at 0.10.0 |
| day05-p1 | ~1.19s | 60,865,199 | 2,275,240 | VM dispatch + env walk | −46% wall time |
| day05-p2 | ~0.06s | 1,252,458 | 151,165 | Trivial | −54% wall time |
| day06-p1 | ~0.44s | 16,706,391 | 232,193 | VM dispatch | −27% wall time |
| day06-p2 | ~4.97s | 126,966,700 | 10,646,539 | Env walk (dominant) | ≈0% (GC eliminated) |
| day07-p1 | ~0.87s | 42,911,737 | 2,174,510 | VM dispatch | −44% wall time |
| day07-p2 | ~1.46s | 28,483,917 | 1,437,304 | Handle-instruction (54%) | −40% wall time |
| day08-p1 | ~4.64s | 228,966,325 | 18,660,114 | Alloc + VM dispatch | −57% wall time |
| day08-p2 | >600s (timeout) | — | — | VM dispatch + alloc (pair gen) | first analysed at 0.10.0 |
| day09-p1 | ~3.87s | 195,078,936 | 12,342,225 | VM dispatch + alloc | −53% wall time |
| day09-p2 | >600s (timeout) | — | — | Continuation overhead + env walk | first analysed at 0.10.0 |
| day10-p1 | ~21.9s | 544,089,317 | 228,950,942 | Alloc-intensive | −1% wall time (GC eliminated) |
| day10-p2 | >600s (timeout) | — | — | Env walk (dominant, 39.3%) | first analysed at 0.10.0 |
| day11-p1 | ~1.32s | 66,475,331 | 357,770 | VM dispatch loop | −10% wall time |
| day11-p2 | ~129s | 3,068,130,793 | 4,264,268 | Tick-bound algorithm | −11% wall time |
| day12-p1 | ~0.19s | 6,647,460 | 1,527,523 | Trivial | −35% wall time |

---

## 0.10.0: Strict Prelude Globals

Every 0.9.x run showed exactly 2 GC collections, regardless of program complexity. This was not the programs themselves — it was the prelude being evaluated lazily on first use, forcing GC during the program's initial computation. The 0.10.0 strict global evaluation (blob mode) pre-evaluates all prelude globals at startup, eliminating this overhead entirely.

**Impact summary:**
- All 19 programs: GC collections dropped from 2 → 0
- Allocation reductions: 31%–90% (median ~84%), because thunk allocation for prelude bindings is eliminated
- Tick reductions: 0%–93% depending on how heavily the program used prelude functions
- Wall-time reductions: 10%–96%

Programs where ticks barely changed (day11, day12, day06-p2) were already dominated by application logic. Programs with dramatic tick reductions (day01-p2: −93%, day01-p1: −73%) were spending a large fraction of their time re-entering prelude thunks. The 0.9.x numbers were inflated by prelude overhead; the 0.10.0 figures represent the true cost of each program.

---

## day01 — part 1

Single-pass fold over a sequence of 13 million instructions. Accumulates a position modulo 100, counting how many steps land on a multiple of 100. The fold is inherently sequential with no branching or data structure construction beyond the accumulator.

**Stats (0.10.0):** 13,015,410 ticks | 1,879,963 allocs | 0 GC | ~0.41s

*No CPU profile (< 0.5s threshold, borderline — profile not captured).*

**Characteristics:** Compute-bound with moderate allocation (1 alloc per 7 ticks). No GC pressure. The 1.9M allocations are from closure construction during the fold. Lazy evaluation of each fold step creates short-lived thunks for intermediate accumulators.

**0.10.0 changes:** Ticks −73% (48.3M → 13.0M), allocs −31% (2.72M → 1.88M), wall time −89% (3.54s → 0.38s), GC 2 → 0. The dramatic improvement reflects how much time 0.9.x spent evaluating prelude thunks on a program that calls prelude functions on every one of 13M steps.

**Bottleneck:** VM dispatch loop. Each of the 13M steps touches several prelude functions; the instruction count is proportional to input size.

**Potential improvements:**
- *Source-level:* The fold processes a flat list of instructions. If the input were chunked into blocks and processed with arithmetic shortcuts (runs of identical instructions), a significant fraction of steps could be collapsed before the fold.
- *VM-level:* The prelude's `foldl` is lazy: the accumulator `op(i, l head)` is passed unevaluated at each step, building a chain of O(n) Update frames when the result is finally forced (`EU_STACK_DIAG=1` confirms linear stack growth to depth 28,626 for this input). The STG machine already handles the tail-recursive call correctly — the issue is the unevaluated accumulator. A strict fold variant (forcing the accumulator at each step) would reduce peak stack depth to O(1) and may reduce allocation.
- *Engine-level:* List fusion (deforestation) could eliminate intermediate list allocations if the instruction list is generated rather than read from input.

---

## day01 — part 2

Single-pass fold tracking position, detecting when the position crosses a multiple of 100 via zero-crossings (checking whether `pos mod 100` has wrapped). Structurally identical to part 1 but with a slightly different predicate.

**Stats (0.10.0):** 13,423,535 ticks | 1,923,144 allocs | 0 GC | ~0.36s

**Characteristics:** Same profile as part 1. Tick count is nearly identical (13.4M vs 13.0M), confirming the algorithm is the same shape. Marginally fewer wall-time seconds despite slightly more ticks — measurement noise.

**0.10.0 changes:** Ticks −93% (188.4M → 13.4M), allocs −60% (4.80M → 1.92M), wall time −96% (9.74s → 0.38s), GC 2 → 0. The 0.9.x tick count was 14× higher than 0.10.0, making this the most dramatic improvement in the suite. The zero-crossing predicate called more prelude functions per step than part 1's modulo check, so the lazy prelude overhead was proportionally larger.

**Bottleneck:** VM dispatch. Same as part 1 — inherently O(n) in instruction count.

**Potential improvements:** Same as part 1. The crossing detection could also be reformulated as arithmetic (find the smallest k such that `cumsum[k] ≥ 100`) using a prefix-sum scan, but this would require the input to be all positive, which may not hold.

---

## day02 — part 1

Closed-form arithmetic: multiplies a pattern multiplier by the sum of a valid range using the arithmetic series formula. No iteration over large collections.

**Stats (0.10.0):** 295,133 ticks | 52,469 allocs | 0 GC | ~0.04s

**Characteristics:** Trivially fast. The low tick and alloc counts reflect a near-direct computation. This is parsing overhead and a handful of arithmetic operations.

**0.10.0 changes:** Ticks −7% (318K → 295K), allocs −61% (135K → 52K), wall time −61% (0.013s → 0.005s), GC 2 → 0. The alloc reduction is almost entirely prelude thunk elimination; the tick count barely changed because this program calls very few prelude functions.

**Bottleneck:** None of significance. Parse and startup dominate at this scale.

**Potential improvements:** None warranted. Already negligible.

---

## day02 — part 2

Inclusion-exclusion over prime factor subsets. Generates all subsets of prime factors, computes an arithmetic series sum per subset, alternates sign by subset size. Modestly more computation than part 1 but still fast.

**Stats (0.10.0):** 1,319,154 ticks | 175,899 allocs | 0 GC | ~0.05s

**Characteristics:** Subset generation allocates proportionally to 2^(number of prime factors). For typical AoC inputs this is small. Moderate allocation density (1 alloc per 7.5 ticks).

**0.10.0 changes:** Ticks −1% (1.33M → 1.32M), allocs −71% (611K → 176K), wall time −58% (0.058s → 0.024s), GC 2 → 0. Tick count is essentially unchanged — this program's logic doesn't heavily use prelude functions in its hot path. The alloc and wall-time reductions come entirely from eliminating the 2-GC overhead (GC stalls dominated a 0.058s runtime).

**Bottleneck:** None of significance.

**Potential improvements:** None warranted.

---

## day03 — part 1

Greedy digit selection via windowed recursion. Finds the largest digit in a valid window, then recurses on the remaining sequence. Processes a moderately sized input with O(n×w) comparisons where w is the window size.

**Stats (0.10.0):** 27,813,225 ticks | 11,553,241 allocs | 0 GC | ~1.03s

**Characteristics:** Compute-bound with high allocation (1 alloc per 2.4 ticks). No GC pressure. Each recursive `cons(best, pick(k-1,...))` call creates a thunk for the tail, allocating proportionally to output size (~1,209 digits).

**0.10.0 changes:** Ticks +4% (26.6M → 27.8M), allocs −0.6% (11.6M → 11.6M), wall time −4% (1.07s → 1.03s), GC 2 → 0. Tick and allocation counts are essentially unchanged from 0.9.x — the lazy cons evaluation pattern was always present. The dramatic figures in the initial 0.10.0 rebaseline were an artefact of incorrect cons strictness (fixed in PR #895), which had suppressed thunk allocation by forcing both cons args eagerly. The only genuine improvement is GC elimination.

**Bottleneck:** VM dispatch for the recursive windowed search.

**Potential improvements:**
- *Source-level:* The greedy algorithm currently uses functional windowing (`take`/`drop`). A fold-based approach that maintains a sliding window index without list slicing would reduce allocation significantly.
- *VM-level:* The STG machine handles the recursive descent correctly — `cons(best, pick(k-1,...))` creates a thunk rather than pushing a continuation for the recursive call. Stack depth is bounded by k (output size, ~1,209 at most), not input size. No tail-call issue exists here.

---

## day03 — part 2

Same greedy algorithm extended to 12-digit selection. Higher constant factor but identical structure.

**Stats (0.10.0):** 57,806,510 ticks | 25,204,145 allocs | 0 GC | ~2.31s

**Characteristics:** Structurally identical to part 1 with a larger selection target. Tick and alloc counts scale roughly with the additional selection rounds. 1 alloc per 2.3 ticks — same ratio as part 1.

**0.10.0 changes:** Ticks +5% (55.1M → 57.8M), allocs −0.2% (25.2M → 25.2M), wall time ≈0% (2.31s → 2.31s), GC 2 → 0. Same situation as part 1: lazy cons evaluation restores 0.9.x-level ticks and allocs; the only measurable improvement is GC elimination.

**Bottleneck:** Same as part 1. The larger selection target multiplies the constant but doesn't change the algorithm's structure.

**Potential improvements:** Same as part 1. The alloc-per-tick ratio is identical to part 1 (1 per 2.3 ticks vs 1 per 2.4 ticks), confirming the same allocation pattern.

---

## day04 — part 1

2D convolution via horizontal windowing and vertical stacking to compute 3×3 block sums across a grid; counts blocks where the sum is below threshold. Involves constructing and traversing a grid of partial sums.

**Stats (0.10.0):** 53,908,706 ticks | 3,215,923 allocs | 0 GC | ~1.10s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 351 | 38.7% |
| Machine::run | 221 | 24.4% |
| EnvironmentFrame::get | 117 | 12.9% |
| create_arg_array | 27 | 3.0% |
| Array::push | 23 | 2.5% |

**Characteristics:** VM dispatch-bound (handle_instruction 38.7% + Machine::run 24.4% = 63%). Env-walk is significant at 12.9% — the convolution builds nested closures over grid rows, creating deeper-than-average env chains. Allocation overhead is moderate (try_allocate + BumpBlock::bump = ~5%).

**0.10.0 changes:** Ticks −6% (57.5M → 53.9M), allocs −84% (19.7M → 3.2M), wall time −53% (2.47s → 1.16s), GC 2 → 0. Tick reduction is small (the hot loop doesn't call many prelude functions repeatedly), but the alloc reduction is large — the 2D windowing used prelude list functions that created many short-lived closures.

**Bottleneck:** VM dispatch loop. The 3×3 windowing creates O(rows × cols) intermediate block structures, each requiring multiple evaluations.

**Potential improvements:**
- *Source-level:* The convolution currently materialises intermediate sums as lists of lists. A single-pass imperative scan (prefix sums) would require a fundamentally different algorithmic structure, but could reduce tick count significantly.
- *Engine-level:* Persistent blocks would reduce allocation cost for the intermediate grid blocks. Estimated ~30% wall-time improvement.
- *VM-level:* Flat closures would address the 12.9% env-walk cost.

---

## day04 — part 2

Tail-recursive removal loop: each step calls `survivals(grid)` (the same 3×3 block-sum convolution as part 1), removes accessible rolls from the grid, and accumulates the count. Repeats until no accessible rolls remain.

**Stats (0.10.0):** did not complete — terminated at 600s (timeout).

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 2,930 | 35.2% |
| Machine::run | 2,232 | 26.8% |
| EnvironmentFrame::get | 944 | 11.4% |
| create_arg_array | 271 | 3.3% |
| try_allocate | 270 | 3.2% |

**Characteristics:** Profile is structurally identical to part 1 (VM dispatch dominant: 35.2% + 26.8% = 62%, env walk 11.4%). The bottleneck is not algorithmic complexity per iteration but iteration count: for the actual puzzle input (135×135 grid) the removal loop requires hundreds of steps, each costing the same ~1.1s as part 1. Allocation rate (try_allocate + BumpBlock::bump + create_arg_array + Array::push ≈ 12%) reflects each `survivals(grid)` call materialising new intermediate grid structures (h-sums, block-sums) that accumulate until GC. Peak heap footprint during profiling reached 12.6 GB, suggesting a steady accumulation of intermediate convolution structures between GC cycles. `env_from_data_args (209 samples, 2.5%)` reflects argument-copying overhead in the tight convolution inner loop.

**Bottleneck:** Iteration count × per-iteration cost. The `step` function is properly tail-recursive and executes without stack growth, but the underlying convolution work per iteration is unchanged from part 1.

**Potential improvements:**
- *Source-level (highest impact):* The total removal count can be computed analytically from the initial block-sum grid without iterating. Because each cell is removed at most once, the total is bounded by the initial accessible count. A smarter structure that identifies the removal order without re-convolving on every step would collapse the O(iterations) loop to O(1) convolution passes.
- *Source-level:* The tail-recursive `step` allocates a new grid on every iteration via `zip-with(zip-with(-), grid, removed)`. Sharing structure between iterations (only updating changed cells) would cut per-iteration allocation significantly.
- *VM-level:* Flat closures for the 11.4% env-walk cost.
- *Engine-level:* Persistent blocks for the intermediate list structures.

---

## day05 — part 1

Filters an ID list against range predicates using functional filtering. Essentially a series of predicate tests across an input list.

**Stats (0.10.0):** 60,865,199 ticks | 2,275,240 allocs | 0 GC | ~1.19s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 303 | 35.4% |
| Machine::run | 226 | 26.4% |
| EnvironmentFrame::get | 165 | 19.3% |
| create_arg_array | 34 | 4.0% |
| try_allocate | 25 | 2.9% |

**Characteristics:** Notably env-walk-bound at 19.3% — the highest env-walk ratio of programs with fewer than 3.4s runtime. The range predicates are closures that capture outer bindings, and filtering over a large ID list walks these env chains on every element. VM dispatch is still dominant (61.8% combined), but env-walk is significant.

**0.10.0 changes:** Ticks −3% (62.4M → 60.9M), allocs −84% (14.1M → 2.3M), wall time −46% (2.21s → 1.18s), GC 2 → 0. Negligible tick reduction confirms this program's hot path is the filtering loop itself, not prelude overhead. The 46% wall-time improvement comes from GC elimination.

**Bottleneck:** VM dispatch + env chain walking. The predicate closures are evaluated for each of a large number of IDs.

**Potential improvements:**
- *Source-level:* Range predicates could be compiled into interval arithmetic early (is ID in range [a,b]?), reducing the number of closure evaluations per element.
- *VM-level:* Flat closures would directly address the 19.3% env-walk cost. This is the clearest single-program case for that optimisation outside day06-p2.
- *Compiler-level:* Inline the predicate at call sites if it's used only once.

---

## day05 — part 2

Sorts ranges, folds with merge logic to combine overlapping intervals, sums merged range lengths.

**Stats (0.10.0):** 1,252,458 ticks | 151,165 allocs | 0 GC | ~0.06s

**Characteristics:** Fast. The merge fold is O(n log n) due to sorting, but n is small. Low allocation density.

**0.10.0 changes:** Ticks +3% (1.22M → 1.25M), allocs −79% (731K → 151K), wall time −54% (0.052s → 0.024s), GC 2 → 0. Tick count increased marginally (noise), allocs and wall time improved significantly from GC elimination.

**Bottleneck:** None of significance at this scale.

**Potential improvements:** None warranted.

---

## day06 — part 1

Transposes a row-oriented list of numbers into column problems, applies per-column operators. Involves a grid transpose and per-column fold.

**Stats (0.10.0):** 16,706,391 ticks | 232,193 allocs | 0 GC | ~0.44s

**Characteristics:** Moderate ticks but very low allocation (1 alloc per 72 ticks) — the second-lowest alloc ratio in the suite. The work is computation-heavy with little data structure construction. This likely reflects the per-column operator applications being arithmetic-heavy with minimal intermediate lists.

**0.10.0 changes:** Ticks +0% (16.6M → 16.7M), allocs −85% (1.60M → 232K), wall time −27% (0.646s → 0.475s), GC 2 → 0. Ticks unchanged confirms the prelude wasn't in the hot path. The alloc and wall-time improvements are from GC elimination only.

**Bottleneck:** Pure VM dispatch for the per-column arithmetic. The 0.10.0 numbers show this is genuinely compute-bound in the application logic.

**Potential improvements:**
- *VM-level:* Numeric intrinsics for batch column operations could help, but the current approach is already low-allocation.
- *Source-level:* No obvious algorithmic improvement available.

---

## day06 — part 2

Transposes a character grid, reverses columns, folds right-to-left grouping digits by operator boundaries. Structurally similar to part 1 but with character classification and more complex grouping logic.

**Stats (0.10.0):** 126,966,700 ticks | 10,646,539 allocs | 0 GC | ~4.97s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| EnvironmentFrame::get | 910 | 33.4% |
| handle_instruction | 885 | 32.4% |
| Machine::run | 808 | 29.6% |
| create_arg_array | 28 | 1.0% |
| try_allocate | 23 | 0.8% |

**Characteristics:** The most env-walk-bound program in the suite. `EnvironmentFrame::get` at 33.4% is the dominant cost — surpassing `handle_instruction` (32.4%). The grouping fold captures many outer bindings (column index, operator type, accumulator) creating deep env chains that are walked on every element of every column. Allocation is moderate (1 alloc per 12 ticks) — the cons-fix restored lazy cons args in the fold accumulator, raising allocs from the pre-fix 1.4M back to 10.6M (consistent with 0.9.x at 10.6M). Even so, the dominant runtime cost is env chain walking, not allocation.

**0.10.0 changes:** Ticks +0.8% (125.9M → 127.0M), allocs +0.8% (10.6M → 10.6M), wall time +2.5% (4.85s → 4.97s), GC 2 → 0. Ticks, allocs, and wall time are essentially unchanged from 0.9.x. The pre-cons-fix rebaseline had artificially suppressed allocs (1.4M), which made this appear to be a 30% wall-time winner. The only structural change is GC elimination.

**Bottleneck:** Env chain walking. This is the single best candidate in the entire suite for flat closures optimisation. If `EnvironmentFrame::get` were O(1) instead of O(depth), this program would likely run in under 2s.

**Potential improvements:**
- *VM-level (high impact):* Flat closures / display-based closure representation. The 33.4% env-walk self-time would drop to near zero. Estimated >30% wall-time improvement for this program specifically.
- *Compiler-level:* Closure analysis to detect free variable sets and pre-project them at closure creation time, trading creation cost for access cost.
- *Source-level:* Refactor the fold to carry an explicit tuple accumulator rather than capturing many outer bindings, reducing env chain depth.

---

## day07 — part 1

Bit-vector beam propagation. Per-row fold detects splitter hits, emits left/right bit shifts, merges beam states with bitwise max. Pure bit manipulation over a grid.

**Stats (0.10.0):** 42,911,737 ticks | 2,174,510 allocs | 0 GC | ~0.87s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 209 | 36.9% |
| Machine::run | 144 | 25.4% |
| EnvironmentFrame::get | 61 | 10.8% |
| try_allocate | 23 | 4.1% |
| create_arg_array | 21 | 3.7% |

**Characteristics:** Classic VM dispatch profile: handle_instruction (36.9%) + Machine::run (25.4%) = 62.3%. Env walk at 10.8% is moderate. Allocation overhead is low-moderate (try_allocate + BumpBlock::bump ≈ 7%).

**0.10.0 changes:** Ticks −3% (44.1M → 42.9M), allocs −87% (16.4M → 2.2M), wall time −44% (1.82s → 1.02s), GC 2 → 0.

**Bottleneck:** VM dispatch. The bit-vector operations are each individually cheap but there are many of them (43M ticks total).

**Potential improvements:**
- *Source-level (high impact):* The beam is a list of 0/1 integers operated on element-wise via `zip-with`. Bitwise intrinsics `&`, `|`, `<<`, `>>` already exist. Replacing the list representation with a packed integer bitmask (one integer per row) would reduce each row-step from O(width) `zip-with` calls to O(1) bit operations: `h = beam & sp`, `p = beam & ~sp`, `emitted = (h << 1) | (h >> 1)`, `new_beam = p | emitted`. This eliminates the dominant allocation source and most of the 43M ticks.
- *Engine-level:* Persistent blocks for beam state accumulation — already largely addressed by 0.10.0's 87% alloc reduction; diminishing returns remain.

---

## day07 — part 2

Same bit-vector beam structure as part 1 but sums beam counts rather than taking the max. This seemingly minor change results in a much higher `handle_instruction` self-time.

**Stats (0.10.0):** 28,483,917 ticks | 1,437,304 allocs | 0 GC | ~1.46s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 592 | 54.5% |
| EnvironmentFrame::get | 200 | 18.4% |
| Machine::run | 200 | 18.4% |
| create_arg_array | 20 | 1.8% |
| try_allocate | 18 | 1.7% |

**Characteristics:** Anomalously high `handle_instruction` self-time at 54.5% — the highest in the suite. The summation path through the VM exercises different instruction patterns than the bitwise max path (part 1), spending proportionally more time in dispatch than in continuations. Note that part 2 has *fewer* ticks than part 1 (28.5M vs 42.9M) but takes *longer* (1.46s vs 0.87s) — the instruction mix is more expensive on average. Env walk at 18.4% is also higher than part 1 (10.8%), suggesting the summation accumulator captures more closures.

**0.10.0 changes:** Ticks −1% (28.8M → 28.5M), allocs −86% (10.4M → 1.44M), wall time −40% (2.45s → 1.46s), GC 2 → 0.

**Bottleneck:** VM instruction dispatch (54.5%) — specifically the summation path. The instruction mix is the root cause, not absolute tick count.

**Potential improvements:**
- *Source-level (primary):* Part 2 counts timelines with `zip-with(+, p, emitted)` — still a list of integers, not packed. For part 2, counts can exceed 1, so bitwise packing does not directly apply. However, the same structural improvement applies: rather than list-of-counts-of-width-w, a direct fold over beam state without materialising per-element lists would reduce the 28.5M ticks substantially.
- *Source-level:* The `solve2` final step `foldl(row-step2, b0) sum` forces the final beam vector to a single sum. With a packed representation, `sum` becomes `popcount`. The divergence in instruction cost between parts 1 and 2 (same ticks, but part 2 is 1.68× slower) is likely because the `+` combiner in the summation path creates more thunks than the `max` combiner in part 1.
- *Compiler-level:* The `handle_instruction` self-time of 54.5% (vs 36.9% in part 1) and near-identical tick counts suggest the summation combiner generates different STG instructions that are individually more expensive. `eu dump stg day07.eu -t part-2` vs `-t part-1` would reveal the difference.

---

## day08 — part 1

X-sorted windowing to reduce pairs to O(n×w); union-find on k closest edges, with the product of the three largest component sizes as output. Implements a subset of Kruskal's algorithm.

**Stats (0.10.0):** 228,966,325 ticks | 18,660,114 allocs | 0 GC | ~4.64s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 1,116 | 32.7% |
| Machine::run | 542 | 15.9% |
| EnvironmentFrame::get | 378 | 11.1% |
| create_arg_array | 230 | 6.7% |
| try_allocate | 218 | 6.4% |

**Characteristics:** Allocation-intensive. `create_arg_array` (6.7%) + `try_allocate` (6.4%) + `BumpBlock::bump` (3.8%) + `Array::push` (3.3%) = 20.2% of CPU in allocation machinery. With 18.7M allocs in 4.64s, this is allocating at ~4M allocs/sec. Note: `graph.union-find` is a Rust intrinsic — the allocations come from the windowed pair generation pipeline: `window-pairs(150, vertices)` generates ~75,000 encoded pairs via nested `tails`, `zip-with`, `iota`, and `concat` operations, all of which are forced when `sort-nums` materialises the full list. Env walk at 11.1% is moderate.

**0.10.0 changes:** Ticks −4% (239.0M → 229.0M), allocs −84% (117.6M → 18.7M), wall time −57% (10.93s → 4.64s), GC 2 → 0. Despite large alloc reduction, 18.7M allocs remain — the windowed pair generation genuinely materialises ~75,000 cons cells before the union-find step.

**Bottleneck:** VM dispatch + allocation overhead from the pair generation pipeline. The ~75,000 encoded pairs must all be materialised for `sort-nums` before the union-find step.

**Potential improvements:**
- *Source-level (high impact):* `window-pairs` generates all pairs eagerly when `sort-nums` is applied. The pipeline `window-pairs(150, vertices) sort-nums map(decode-edge) take(1000) graph.union-find(n)` forces all ~75,000 cons cells. Rewriting `window-pairs` to generate pairs in a form that feeds directly into `sort-nums` without intermediate cons allocation would be the largest win.
- *Source-level:* The `tails`-based windowing materialises `n` tails each of decreasing length. A direct fold over index pairs `(i, j)` where `i < j <= i+w` with a mutable encoding accumulator would generate the same encoded values without any intermediate list structure.
- *VM-level:* Flat closures for the 11.1% env walk.

---

## day08 — part 2

MST via Kruskal on the full candidate set. `solve2-n(999, data)` uses window size w=999 (vs w=150 for part 1), expanding candidate pairs from ~75,000 to n×(n−1)/2 ≈ 499,500 for n=1,000 points. All pairs are materialised, sorted by `sort-nums`, and passed to `graph.kruskal-edges` to build the complete MST. The bridge is identified by `last-edge-idx`: `graph.kruskal-edges(n) (reverse ; nth(1))`.

**Stats (0.10.0):** did not complete — terminated at 600s (timeout).

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 3,146 | 37.7% |
| Machine::run | 1,929 | 23.1% |
| EnvironmentFrame::get | 1,107 | 13.3% |
| try_allocate | 284 | 3.4% |
| create_arg_array | 270 | 3.2% |

**Characteristics:** Profile matches day08-p1 in structure (VM dispatch 37.7% + 23.1% = 60.8%, env walk 13.3%), scaled up by 6.6× more candidate pairs. The eucalypt-side pair generation (`tails` + `zip-with` + `iota` + `concat` for 499,500 pairs) dominates: allocation machinery (try_allocate + BumpBlock::bump + Array::push + find_space) totals ~12%. Two minor signals absent from day08-p1 appear: `serde_json::de::Deserializer::parse_integer (37 samples, 0.4%)` and `Pow::execute (29 samples, 0.3%)`, reflecting the `dist2` squared-distance computation (power intrinsic) being sampled in the tight pair-generation inner loop. The `graph.kruskal-edges` Rust intrinsic processes only (n−1) MST edges and is not visible in the profile, confirming the bottleneck is the eucalypt-side generation of the full 499,500-pair candidate set.

**Bottleneck:** Pair generation pipeline (≈6.6× more pairs than part 1). The `window-pairs(999, vertices)` call materialises ~499,500 cons cells before `sort-nums` can sort them.

**Potential improvements:**
- *Source-level (highest impact):* Same as part 1: eliminate the cons-cell materialisation in `window-pairs`. With 499,500 pairs, this would reduce allocation by the same 84% seen in total allocs for part 1.
- *Source-level:* The `prepare` function is called once and returns a block that is then interrogated. If `window-pairs` were replaced by a fold that feeds directly into a sort-by-accumulator, the pair list would never need to be materialised as a cons sequence.
- *VM-level:* Flat closures for the 13.3% env-walk cost.

---

## day09 — part 1

Pairwise brute-force via `tails` with `foldl`. Computes area for each head-to-tail pair, finds the maximum. O(n²) in the number of points.

**Stats (0.10.0):** 195,078,936 ticks | 12,342,225 allocs | 0 GC | ~3.87s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 933 | 32.5% |
| Machine::run | 525 | 18.3% |
| EnvironmentFrame::get | 453 | 15.8% |
| try_allocate | 147 | 5.1% |
| create_arg_array | 146 | 5.1% |

**Characteristics:** High env walk (15.8%) combined with significant allocation (try_allocate + BumpBlock::bump + Array::push ≈ 11%). The pairwise structure means every pair evaluation creates a closure capturing both head and tail context, deepening env chains. With 12.3M allocs at ~3.2M allocs/sec, allocation is a real cost.

**0.10.0 changes:** Ticks −4% (202.2M → 195.1M), allocs −85% (80.4M → 12.3M), wall time −53% (8.32s → 3.87s), GC 2 → 0.

**Bottleneck:** Env walk + allocation for the O(n²) pairwise fold. Both costs are intrinsic to the algorithm structure.

**Potential improvements:**
- *Source-level:* The brute-force pairwise scan is O(n²) by design (part 1). If the problem admits a smarter algorithm, this would be the most impactful change. Even a spatial index (grid-based bucketing) could reduce comparisons.
- *VM-level:* Flat closures would cut the 15.8% env walk.
- *Engine-level:* Persistent blocks for intermediate accumulated structures.

---

## day09 — part 2

Largest-inscribed-rectangle sweep over a rectilinear polygon. `levels.from-points` partitions the 496-point polygon into cross-sections (levels) at each distinct y-coordinate. `search.find-max` sweeps all level pairs as (start, end) candidates: an outer fold iterates start levels, an inner `foldl` sweeps from start downward narrowing the corridor at each step. Pruning skips pairs where the maximum possible area cannot beat the current best.

**Stats (0.10.0):** did not complete — terminated at 600s (timeout).

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| Machine::run | 2,804 | 33.6% |
| handle_instruction | 2,580 | 30.9% |
| EnvironmentFrame::get | 1,414 | 16.9% |
| try_allocate | 217 | 2.6% |
| create_arg_array | 210 | 2.5% |

**Characteristics:** The profile shows an anomalous Machine::run (33.6%) > handle_instruction (30.9%) ratio — the only program in the suite where Machine::run dominates. This indicates a high proportion of continuation-management overhead: the nested `outer-step` / `inner-step` folds generate many function applications, returns, and resume-continuation frames per level pair. EnvironmentFrame::get at 16.9% is significant: `outer-step` captures `ymax`, `maxw`; `inner-step` captures `sy`, `start-tiles`, `ymax`; `max-rect` captures `sy`, `st`, `ey`, `et`, `cl`, `cr` — each level-pair step walks a moderately deep chain. `serde_json::de::Deserializer::parse_integer (38 samples, 0.5%)` and `NumParse::execute (20 samples, 0.2%)` appearing in the steady-state profile indicates that coordinate parsing is still being forced lazily during level construction (the 496-point polygon's coordinates are being converted from strings to numbers as the level sweep forces each `pts` element for the first time).

**Bottleneck:** Continuation overhead (Machine::run 33.6%) combined with env walk (16.9%) and the O(levels²) worst-case sweep. For the actual puzzle input, the pruning may not be reducing the search space sufficiently.

**Potential improvements:**
- *Source-level:* Pre-force the entire `pts` list before level construction to eliminate the lazy parsing overhead (the `serde_json` signal in the profile).
- *Source-level:* The outer fold iterates over `tails(lvs)`, passing each suffix to `outer-step`. If the pruning condition `max-possible <= best` is satisfied early, earlier termination would help, but the data access pattern (each level is visited once as a start) limits this.
- *VM-level (primary):* The Machine::run dominance suggests continuation management is the binding cost. Inlining small tail calls or improving continuation allocation would benefit this program most.
- *VM-level:* Flat closures for the 16.9% env-walk cost.

---

## day10 — part 1

DFS subset enumeration over GF(2). Finds minimum k-size subsets via XOR-based target matching. Highly recursive with extensive backtracking.

**Stats (0.10.0):** 544,089,317 ticks | 228,950,942 allocs | 0 GC | ~21.9s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 1,548 | 29.6% |
| Machine::run | 641 | 12.2% |
| try_allocate | 453 | 8.7% |
| EnvironmentFrame::get | 437 | 8.4% |
| create_arg_array | 354 | 6.8% |

**Characteristics:** The most allocation-intensive program in the suite. With 229M allocs in 21.9s, allocation rate is ~10.5M allocs/sec. The CPU profile below was captured on the initial 0.10.0 binary (pre-cons-fix, 43M allocs); with the correct post-fix 229M allocs, allocation machinery overhead will be proportionally higher. The DFS enumeration creates a closure per candidate subset via recursive `cons`, and each backtracking step must rebuild env frames. Env walk at 8.4% is moderate but secondary to allocation.

**0.10.0 changes:** Ticks +3% (527.6M → 544.1M), allocs +0.3% (228.3M → 229.0M), wall time −1% (22.16s → 21.9s), GC 2 → 0. Tick and allocation counts are essentially unchanged from 0.9.x. The dramatic improvement shown in the initial 0.10.0 rebaseline (−68% wall time) was an artefact of incorrect cons strictness (fixed in PR #895): the DFS uses recursive cons construction throughout the backtracking loop, and forcing those args eagerly had artificially suppressed allocations and ticks. The only genuine improvement is GC elimination.

**Bottleneck:** Allocation machinery (CPU profile from pre-cons-fix pass; see note in Characteristics). This program remains the strongest case in the suite for persistent blocks and arena allocation improvements.

**Potential improvements:**
- *Engine-level (highest impact in suite):* Persistent blocks would directly reduce the allocation overhead. Given 28% of CPU is in allocation infrastructure, a 2× improvement in allocator throughput would yield ~14% wall-time improvement.
- *Source-level:* The DFS uses functional subset accumulation. An iterative representation with explicit stack would reduce closure creation, but would require significant algorithmic rewrite.
- *Compiler-level:* Examine whether `from_let` (3.6%) and `from_closure` (2.4%) bindings in the DFS body can be lifted or shared across iterations.

---

## day10 — part 2

Branch-and-bound integer linear programme solver. For each machine `[joltage, buttons]`, `solve-machine2` performs: (1) float Gaussian elimination (`gauss.forward`) to find pivot/free columns and compute slopes and bounds; (2) integer Bareiss elimination (`gauss-int.forward`) for exact back-substitution; (3) branch-and-bound search over free variables with cost-based pruning (`search(0, [], 0, INF)`). For machines with many free variables, the search is exponential.

**Stats (0.10.0):** did not complete — terminated at 600s (timeout).

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| EnvironmentFrame::get | 3,237 | 39.3% |
| handle_instruction | 2,378 | 28.9% |
| Machine::run | 750 | 9.1% |
| create_arg_array | 260 | 3.2% |
| try_allocate | 203 | 2.5% |

**Characteristics:** EnvironmentFrame::get at 39.3% is the highest env-walk ratio in the entire suite — surpassing day11-p2 (27.1%) and day06-p2 (33.4%). The `solve-machine2` block captures an exceptionally large number of outer bindings (`nc`, `nb`, `INF`, `result`, `reduced`, `pivots`, `rank`, `nfree`, `frees`, `solution-vec`, `base-cost`, `lowers`, `uppers`, `slopes`, `result-int`, `iref`) all accessed by deeply nested closures (`search`, `inner-solve`, `eval-free`, `adjusted-lower-bound`, `adjusted-upper-bound`, `try`). Every env frame access in the branch-and-bound inner loop walks past all these enclosing bindings. handle_instruction at 28.9% and Machine::run at 9.1% are the inverse of the usual ratio, confirming env walk dominates dispatch here. Allocation machinery (create_arg_array + try_allocate + BumpBlock::bump + Array::push + find_space) totals ~14%, with `from_let (134 samples, 1.6%)` and `from_letrec (20 samples, 0.2%)` reflecting Gaussian matrix row construction. `BlockListIterator::next (16 samples, 0.2%)` and `hashbrown::raw::RawTable::reserve_rehash (9 samples, 0.1%)` reflect block iteration and hashmap growth in the matrix state.

**Bottleneck:** Env-walk (39.3%) and the exponential search over free variables. Flat closures would be the single largest structural improvement; beyond that, the search complexity is input-dependent.

**Potential improvements:**
- *VM-level (highest impact in suite):* Flat closures would directly address the 39.3% env-walk. Even a conservative 50% reduction in env-walk cost (extrapolating from day06-p2 estimates) would yield ~20% wall-time improvement in non-search-bound runs.
- *Source-level:* Pre-project the captured bindings used in the inner loop. The `search` function accesses `slopes`, `uppers`, `lowers`, `frees`, `nfree`, `base-cost` on every recursive call. Packing these into a single tuple argument would reduce the env chain depth by eliminating the outer block's binding layer.
- *Source-level:* The `eval-free` function rebuilds the back-substitution from scratch on every call to `inner-solve`. Memoising partial back-substitution results for previously fixed prefix assignments would reduce redundant work in the inner exhaustive loop.
- *Compiler-level:* The `solve-machine2` block creates closures for `free-lower-bound`, `free-upper-bound`, `free-slope`, `adjusted-lower-bound`, `adjusted-upper-bound`, `inner-solve`, and `eval-free` — all computed once per machine but allocated freshly. Static analysis could detect that these are machine-local constants and lift them out of the block as frozen closures.

---

## day11 — part 1

DFS topological sort on a DAG followed by DP path counting in reverse topological order using block lookups.

**Stats (0.10.0):** 66,475,331 ticks | 357,770 allocs | 0 GC | ~1.32s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 406 | 42.2% |
| Machine::run | 377 | 39.2% |
| EnvironmentFrame::get | 156 | 16.2% |
| BumpBlock::bump | 7 | 0.7% |
| create_arg_array | 6 | 0.6% |

**Characteristics:** Pure compute — 1 alloc per 186 ticks, the lowest allocation ratio in the suite. The DP path counting runs many arithmetic evaluations with minimal data structure construction. handle_instruction (42.2%) + Machine::run (39.2%) = 81.4% of CPU in the dispatch loop itself. This program is limited by raw VM throughput; there is nothing to tune at the algorithmic or allocation level. Env walk at 16.2% is significant relative to the near-zero allocation cost.

**0.10.0 changes:** Ticks +0% (66.5M → 66.5M), allocs −33% (530K → 358K), wall time −10% (1.47s → 1.32s), GC 2 → 0. No tick change confirms this program was already not touching prelude functions in its hot path. The 10% wall-time improvement is GC elimination only.

**Bottleneck:** VM dispatch loop. With 81.4% in dispatch and near-zero allocation, this is a pure VM throughput problem.

**Potential improvements:**
- *VM-level (primary):* Any improvement to the dispatch loop itself (better branch prediction hints, instruction fusion, register allocation in the interpreter) would benefit this program most. It is the cleanest benchmark for measuring raw VM dispatch speed.
- *VM-level:* Flat closures for the 16.2% env walk.
- *Compiler-level:* The block lookups in the DP step may be generating many `LOOKUP` instructions. Compile-time key resolution where the lookup key is statically known would save instruction count.
- *Source-level:* No algorithmic improvement available — the DP is already optimal for this structure.

---

## day11 — part 2

Inclusion-exclusion with graph variant generation. Four independent DP passes computing path counts via required nodes: `total - no_dac - no_fft + neither`, each on a graph variant with one or two required nodes removed.

**Stats (0.10.0):** 3,068,130,793 ticks | 4,264,268 allocs | 0 GC | ~129s

**CPU profile (top 5):**

| Function | Self-time | % |
|----------|-----------|---|
| handle_instruction | 37,727 | 50.3% |
| EnvironmentFrame::get | 20,326 | 27.1% |
| Machine::run | 14,807 | 19.7% |
| sip Hasher::write (block key hashing) | 265 | 0.4% |
| create_arg_array | 264 | 0.4% |

**Characteristics:** VM dispatch and env-walk bound. `EnvironmentFrame::get` at 27.1% is the highest env-walk ratio in the suite — matching day01-p1 (29%). Allocation is negligible (1 alloc per 720 ticks; try_allocate is only 0.4%). The 3.07B ticks come from the four DP passes, each of which is structurally identical to part 1 but starts from "svr" (a node with a much larger reachable subgraph than "you"). The deep env chains arise from `dp-step` closures that capture `g` (the graph block) and `table` (the current DP accumulator block), both large objects deep in the env frame — every `lookup-count(table, name)` call inside `map(lookup-count(table))` walks the chain to find `table`. Notable: `sip Hasher::write` at 0.4% (0.52s) reflects the cost of hashing symbol keys for block lookups in the hot path.

**0.10.0 changes:** Ticks +0% (3.07B → 3.07B), allocs −42% (7.39M → 4.26M), wall time −11% (144.9s → 129.1s), GC 2 → 0. Zero tick change — the hot path (the DP loop itself) does not call prelude thunks. The 11% wall-time improvement is GC elimination only.

**Bottleneck:** The DP passes execute 3.07B instructions. The bottleneck has two components: (1) raw VM dispatch (50.3% + 19.7% = 70%), addressable only by VM throughput improvements; (2) env-walk (27.1%), addressable by flat closures. The inclusion-exclusion structure (4 DP passes) is already optimal for this problem formulation — the dominant cost is the graph size reachable from "svr", not the number of required nodes.

**Potential improvements:**
- *VM-level (highest impact):* Flat closures would directly address the 27.1% env-walk. Conservatively applying the reduction seen on day01 (−33% in EnvironmentFrame::get) yields ~9% wall-time savings — roughly 12s off 129s. This is the strongest single-program case in the suite for flat closures.
- *Source-level (highest algorithmic impact):* The `dp-step` function calls `merge(table, kv-block(sym(node), c))` at each step. `sym(node)` interns a string to a symbol on every DP step (visible as `SymbolPool::intern` in the profile). Pre-interning all node names once before the DP loop and storing them in a block would eliminate this repeated hashing overhead.
- *Source-level:* The four DP passes each rebuild the entire DP table from scratch. Graph variants (with one node removed) share most of their structure; the tables could be computed incrementally starting from a shared base, reducing redundant work.
- *VM-level:* Any improvement to raw VM dispatch speed (see day11-p1 notes) scales proportionally across 3B ticks. A 2× dispatch improvement would halve wall time to ~65s.
- *Engine-level:* Persistent blocks would not help here (allocs are negligible); the hashing cost is for block lookup, not allocation.

---

## day12 — part 1

Area check and box dimension filter. Counts feasible regions passing geometric constraints.

**Stats (0.10.0):** 6,647,460 ticks | 1,527,523 allocs | 0 GC | ~0.19s

**Characteristics:** Fast. Moderate allocation density (1 alloc per 4.4 ticks — relatively high, suggesting the feasibility check allocates per-region structures). No GC pressure.

**0.10.0 changes:** Ticks +0% (6.66M → 6.65M), allocs −56% (3.48M → 1.53M), wall time −35% (0.287s → 0.186s), GC 2 → 0.

**Bottleneck:** None of significance at 0.19s. The relatively high alloc density (1 per 4.4 ticks) is mildly unusual but not a problem at this scale.

**Potential improvements:** None warranted.

---

## Cross-cutting Observations

### Flat closures (env walk)

`EnvironmentFrame::get` is a measurable cost in every program > 0.5s. The extreme cases are day06-p2 (33.4%) and day11-p2 (27.1%), with significant cost also in day05-p1 (19.3%), day11-p1 (16.2%), day09-p1 (15.8%), and day07-p2 (18.4%). A flat closure representation would eliminate this cost across the board. Conservative estimate: 10–20% wall-time improvement for most programs, 25–35% for day06-p2 and day11-p2.

### Persistent blocks

day10-p1 is the clearest case: 28% of CPU in allocation machinery (CPU profile from pre-cons-fix pass), 229M allocs. day08-p1 (20% allocation overhead), day09-p1 (11%), and day04-p1 (5%) would also benefit. Persistent blocks reduce per-element allocation for block construction, which would lower these costs.

### VM dispatch loop

day11-p1 and day11-p2 are the cleanest benchmarks for VM dispatch speed: near-zero allocation, near-zero GC, no other bottleneck. day11-p1's 81.4% in the dispatch loop makes it the ideal microbenchmark for any VM dispatch optimisation.

### Algorithmic limits

day11-p2 (3B ticks, 129s) has a large algorithmic component, but is not purely algorithmic: its 27.1% env-walk overhead means flat closures could save ~12s. The remaining ~117s reflects the genuine cost of four large-graph DP passes. Source-level improvements (reducing redundant computation across graph variants, pre-interning node symbols) are the highest-leverage path to a materially faster result.
