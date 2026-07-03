# A/B engine performance study: bytecode vs HeapSyn (eu-mhjz)

**Date:** 2026-07-03
**Branch base:** `integration/0.12.0` @ 185dda4a (includes #952 dual-form blob, #954 DirectApp fix)
**Toolchain:** stable-aarch64-apple-darwin (rustc 1.96.0), clean release build, embedded prelude blob regenerated (`cfg(prelude_blob_ok)`).
**Author:** Stopwatch. **Status:** measurement + analysis only — no code changes.

Both engines run the same binary and the same embedded blob; the only variable
is the engine (`EU_HEAPSYN=1` selects HeapSyn). All `eu` runs used
`--heap-limit-mib 12288` under `timeout`.

---

## 0. Headline

**With rigorous 5-run wall-clock medians, the bytecode engine is at parity on
pure-dispatch code and 1.3–2.1× *slower* on allocation-touching compute — the
opposite of the intended BV1 dispatch win.** The single dominant cause is
**per-instruction transient heap churn through the system allocator**
(`malloc`/`free` = ~19–21 % of samples on allocation-heavy programs), which
HeapSyn does not incur (it allocates everything in the bump/GC heap).

The one large exception runs the other way: on the highest-allocation
benchmark (**day08-p1**) HeapSyn is GC-bound and does **not finish in 500 s**,
while bytecode completes in **9.6 s** (>52× bytecode advantage).

---

## 1. Methodology and three measurement traps (important)

1. **The in-process `-S` "Total:" is NOT comparable across engines.** The banner
   `Total:` sums pipeline + VM occupancy. On HeapSyn the eval work is counted
   **twice** (pipeline `stg-eval` phase *and* the `VM` occupation), so the banner
   over-reports HeapSyn by ~2×. On bytecode there is no `VM` occupation timing
   (`bytecode-eval` counted once). The machine-readable trailing `Total:` line is
   a *per-section subtotal* and differs again. **Only external wall-clock is
   valid for the A/B.** All ratios below are external `perf_counter` wall medians.
2. **The bytecode path reports zero Machine/Heap/GC stats.** Ticks, Allocs,
   Blocks, Collections all read 0 under bytecode (the counters are wired to the
   HeapSyn `Machine`). Tick/alloc/GC columns below are **HeapSyn-only**; they
   characterise the workload, not the bytecode engine.
3. **Startup differs by ~3 ms** (bc ~9 ms vs hs ~12 ms for `eu -e 1`). Any
   benchmark under ~50 ms is startup-dominated and its ratio is noise, **not** an
   engine result. Those rows are excluded from analysis.

Run counts: 5 medians for the sweep (7–9 for spot re-checks); CPU profiles via
macOS `sample` (idle watchdog-thread frames excluded).

---

## 2. Phase 1 — CPU profiles (top leaf functions, % of non-idle `eu` samples)

### 2a. Pure dispatch, ~zero allocation — day11-p1 (the BV0 reference)
Wall: **bc 1.58 s / hs 1.63 s → 0.97** (parity; bytecode marginally ahead).

| Bytecode | % | HeapSyn | % |
|---|--:|---|--:|
| `handle_op` | 30.6 | `handle_instruction` | 49.1 |
| `enter_local` | 25.2 | `Machine::run` | 37.0 |
| `dispatch` | 11.8 | `EnvironmentFrame::get` (env-walk) | 12.6 |
| `read_ref` (operand decode) | 11.2 | — | |
| `drop_in_place ExecutionError` | 6.7 | | |
| `step` | 5.2 | | |

**Reading:** bytecode's threaded opcode dispatch (`handle_op`+`dispatch` ≈ 42 %)
is genuinely leaner than HeapSyn's pointer tree-walk (`handle_instruction`+`run`
≈ 86 %). That entire gain is consumed by three bytecode-only costs:
`enter_local` (frame entry, 25 %), `read_ref`/instruction-stream decoding
(11 %), and **`drop_in_place ExecutionError` (6.7 %)** — the hot loop returns
`Result<_, ExecutionError>` and drops a large error enum every instruction.
Net = parity.

### 2b. Dispatch + allocation — naive fib(30)
Wall: **bc 4.05 s / hs 1.89 s → 2.11** (bytecode 2.1× slower).

| Bytecode | % | HeapSyn | % |
|---|--:|---|--:|
| `handle_op` | 20.0 | `handle_instruction` | 20.8 |
| **`_xzm_free` + `malloc_zone_malloc` + `_free` + `xzm_xzone_malloc`** | **≈18.8** | `Machine::run` | 13.1 |
| `dispatch` | 5.0 | `Heap::try_allocate` | 10.0 |
| `try_allocate` | 4.4 | `return_native` | 6.9 |
| `Map::fold` (arg-array iterate) | 3.9 | `BumpBlock::bump` | 5.9 |
| `read_ref` / `step` / `bump` | ~2.8 ea | `from_let` / `env_from_data_args` | ~5.7 ea |
| `drop_in_place ExecutionError` | 2.7 | | |

**Reading:** the difference between 2a and 2b is the **system allocator**.
Bytecode spends ~19 % of CPU in `malloc`/`free` that is entirely absent on
HeapSyn (whose allocation is all bump/`try_allocate` inside the managed heap).

### 2c. Block-lookup + allocation — 40-key block, 3 lookups × 1M tail-recursive iters
Wall: **bc 4.53 s / hs 2.09 s → 2.17.**

Bytecode: `handle_op` 14.5, **malloc/free ≈ 20.7**, `enter_local` 6.5,
`read_ref` 5.0, **`bc_lookup_in_block` 4.0** (the O(n) linear find — index gated
off), `Map::fold` 5.3, `Vec::from_iter` 2.8.
HeapSyn: `handle_instruction` 19.6, `run` 18.2, `return_native` 9.1,
`EnvironmentFrame::get` 8.5, managed-heap alloc (`try_allocate`+`bump`+
`find_space`+`alloc`) ≈ 17, **no system malloc**.

**Reading:** the gated-off block index shows up as `bc_lookup_in_block` but at
only ~4 % on a 40-key block — a real but *secondary* tax. Malloc churn (~21 %)
dominates. (See §5 on why the block-index caveat is minor in practice.)

### 2d. Extreme allocation — day08-p1 (247 M ticks / 118 M HeapSyn allocs)
Wall: **bc 9.6 s / hs >500 s** (killed; >52×).

Bytecode: `handle_op` 23.7, `enter_local` 8.0, malloc/free ≈13, decode
(`read_ref`+`read_form_header`+`step`) ≈10.4, `drop ExecutionError` 2.4 — **no GC
frames**.
HeapSyn: `handle_instruction` 27.6, `run` 11.9, `EnvironmentFrame::get` 6.8,
`Array::push` 6.4, **`mark_region_in_block` 6.1 (GC MARK)**, `try_allocate`
5.6, `bump` 3.8, `find_space` 3.2 — **GC is active and grows as the managed
heap fills**. Both engines produce the correct answer (`day08 -t test` PASSes on
both).

### Mutator vs GC attribution
At the production heap (12 GiB) **every compute benchmark does zero GC
collections** — the compute A/B is 100 % mutator (matches ROADMAP "collections
dropped to zero"). GC only appears (a) under heap constraint — naive fib at
**512 MiB times out (>60 s) on *both* engines** (shared GC-thrash vulnerability),
or (b) at extreme managed-heap allocation, where it cripples HeapSyn (day08).

---

## 3. Phase 2 — full sweep (external wall medians, 5 runs)

### Harness micro-benchmarks (`tests/harness/bench/`)
| bench | bc med (s) | hs med (s) | bc/hs | hs ticks | hs allocs | gc |
|---|--:|--:|--:|--:|--:|--:|
| 001_naive_fib | 4.041 | 1.888 | **2.14** | 115,779,125 | 18,847,763 | 0 |
| 002_thunk_updates | 0.876 | 0.860 | 1.02 | 26,175,009 | 593,881 | 0 |
| 003_smarter_fib* | 0.017 | 0.016 | 1.07 | 216,323 | 156,693 | 0 |
| 004_generations | 0.109 | 0.116 | 0.94 | 3,596,235 | 580,695 | 0 |
| 005_drop_cons | 0.143 | 0.067 | **2.13** | 3,340,149 | 1,380,046 | 0 |
| 006_block_lookup* | 0.011 | 0.014 | 0.77 | 1,669 | 419 | 0 |
| 007_short_lived | 0.227 | 0.122 | **1.87** | 6,031,616 | 2,545,703 | 0 |
| 008_long_lived_graph | 0.170 | 0.115 | 1.48 | 4,461,670 | 1,982,054 | 0 |
| 009_fragmentation | 0.137 | 0.088 | 1.55 | 3,887,932 | 1,856,973 | 0 |
| 010_deep_find_perf | 0.065 | 0.048 | 1.36 | 1,347,036 | 756,385 | 0 |
| 011_string_split_join* | 0.010 | 0.012 | 0.77 | 3,204 | 5,181 | 0 |
| 012_regex_match* | 0.010 | 0.012 | 0.79 | 6,372 | 4,584 | 0 |
| 013_interpolation* | 0.010 | 0.013 | 0.79 | 4,561 | 2,457 | 0 |
| 014_text_transform* | 0.011 | 0.012 | 0.89 | 21,942 | 12,439 | 0 |

`*` = <50 ms, startup-dominated → ratio is noise, not an engine result.

### AoC 2025 (real puzzle inputs; run from `examples/aoc25/`)
| target | bc med (s) | hs med (s) | bc/hs | hs ticks | hs allocs | gc |
|---|--:|--:|--:|--:|--:|--:|
| day01-p1 | 0.698 | 0.524 | 1.33 | 12,915,277 | 2,242,216 | 0 |
| day01-p2 | 0.697 | 0.554 | 1.26 | 13,382,554 | 2,363,110 | 0 |
| day02-p1* | 0.025 | 0.024 | 1.07 | 322,348 | 136,431 | 0 |
| day02-p2 | 0.064 | 0.044 | 1.47 | 1,263,752 | 613,702 | 0 |
| day03-p1 | 0.933 | 0.619 | 1.51 | 27,382,786 | 11,533,441 | 0 |
| day03-p2 | 2.034 | 1.217 | 1.67 | 56,836,818 | 25,184,345 | 0 |
| day04-p1 | 2.064 | 1.707 | 1.21 | 59,230,889 | 19,862,289 | 0 |
| day05-p1 | 1.873 | 1.477 | 1.27 | 62,895,143 | 14,067,503 | 0 |
| day05-p2 | 0.067 | 0.062 | 1.08 | 1,260,793 | 732,587 | 0 |
| day06-p1 | 0.636 | 0.595 | 1.07 | 16,727,001 | 1,588,612 | 0 |
| day06-p2 | 4.203 | 4.089 | 1.03 | 126,918,682 | 10,627,704 | 0 |
| day07-p1 | 1.711 | 1.224 | 1.40 | 45,470,121 | 16,334,385 | 0 |
| day07-p2 | 2.345 | 2.486 | 0.94 | 29,729,386 | 10,349,367 | 0 |
| day08-p1 | 9.589 | **>500 (TO)** | **>52** | (n/a) | (n/a) | — |
| day09-p1 | 6.979 | 5.142 | 1.36 | 208,578,165 | 81,203,159 | 0 |
| day11-p1 | 1.579 | 1.634 | 0.97 | 66,410,797 | 488,779 | 0 |
| day12-p1 | 0.288 | 0.196 | 1.47 | 6,745,268 | 2,361,925 | 0 |

Timed out on **both** engines (excluded): day04-p2, day08-p2, day09-p2,
day10-p1/p2, day11-p2 (~144 s, tick-bound).

### Higher-order fold `range(0,N) foldl((_+_),0)` (ROADMAP §7 env-walk case)
| N | bc wall | hs wall | bc/hs | hs ticks |
|---|--:|--:|--:|--:|
| 5,000 | 0.642 | 0.695 | 0.92 | 13,732,702 |
| 10,000 | 5.005 (min 2.05, high variance) | 2.195 | 2.28 | 52,465,202 |

**Both engines are O(n²)** here (ticks ≈ 3.8× for 2× N; wall ≈ 6× for 2× N).
This is the STG-level lazy-accumulator / cactus-environment env-walk — it is
**engine-independent** and confirms the BV3 register-frame motivation applies to
both. Note bytecode's large run-to-run variance at N=10k (system-allocator
pressure). More broadly, all lazy-list ops are O(n²): `range(0,10000) count` =
2.2 s, `range(0,20000) count` = 13.3 s on both engines.

### `cargo bench` (`benches/`)
- **`parse`** — frontend only, engine-independent (report once).
- **`alloc`, `gc`** — construct `HeapSyn`/`EnvFrame` and exercise the bump/GC
  layer *directly*; **not engine-selectable** (both engines share this memory
  layer). Not part of the A/B.
- **`text`** (`text_pipelines`, files 011–014) — only bench that runs full
  execution, engine-selectable. Result: **parity** (split-join bc 32.7 ms / hs
  32.6 ms; regex bc 31.4 / hs 30.7). Each iteration re-parses and re-compiles, so
  the ~32 ms is frontend-dominated and execution is a small slice — consistent
  with the startup-bound harness rows 011–014.

---

## 4. Analysis by workload group

**Dispatch-heavy, low allocation (day11-p1, day06-p2, 002_thunk_updates):**
parity (0.97–1.03). Bytecode's leaner dispatch exactly offsets its decode +
`enter_local` + `Result`-drop overhead. *This is the true engine comparison and
it is a wash.*

**Allocation-heavy (fib, drop_cons, short_lived, fragmentation, long_lived,
day01/03/04/05/07/09/12):** bytecode **1.3–2.1× slower**, driven by system
`malloc`/`free` churn (~19–21 % of CPU) absent on HeapSyn.
`alloc/tick`-ratio vs slowdown has Pearson r = 0.41 (n=21) — a real but partial
driver. **fib is the worst outlier** (2.14× at only 0.16 alloc/tick): it is
call-dense (each call = App+Cons+Case), and each such instruction allocates a
transient `Vec` (§6), so *App/instruction density* amplifies the tax beyond what
raw alloc/tick predicts.

**Extreme allocation (day08-p1):** reverses hard — HeapSyn is GC-bound (>500 s,
`mark_region_in_block` present and growing) while bytecode finishes in 9.6 s. The
bytecode design that hurts elsewhere (transients on the C heap) here keeps the
*managed* heap small enough to avoid the GC death-spiral.

**Startup-dominated (<50 ms: 003, 006, 011–014, day02-p1):** ignore; measures
the ~3 ms startup edge bytecode has, not execution.

---

## 5. On the block-index caveat (as briefed)

The `LookupOr`/`SafeLookup` symbol-position index is gated **off** on bytecode
(`machine.rs:2865 block_index_enabled → false`), so bytecode block lookups are
O(n) linear scans (`bc_lookup_in_block`) vs O(1) on HeapSyn. **Measured impact is
small:** on a 40-key block it is ~4 % of samples, dwarfed by malloc churn; and
day11-p1 — genuinely sym-keyed-lookup-heavy — is at **parity (0.97)**, so its
block lookups are not large enough for the O(n) tax to bite. The gating is a real
apples-to-oranges factor to keep in mind for *very large, lookup-dominated,
low-allocation* blocks, but it is **not** a material contributor to any measured
regression here. The malloc-churn regression is the story, not the index gating.

---

## 6. Root cause (code)

`src/eval/bytecode/machine.rs` allocates transient `Vec`s on the system heap on
the hot path, **once per reduction step**:
- `read_arg_offsets` (`:139`) → `Vec<CodeRef>` per App/Cons.
- `arg_refs: Vec<DecodedRef> = …collect()` (`:1326`, `:1494`, App/Cons handlers).
- `read_let` (`:1211`) → `Vec<FormHeader>` per Let; `read_branch_table` (`:170`)
  → `Vec<Option<CodeRef>>` per Case.
- `make_arg_array` (`:1169`); `merged … to_vec()` (`:788`).

These are the `malloc_zone_malloc` / `xzm_free` / `Vec::from_iter` / `Map::fold`
/ `Array::push` frames in the profiles. HeapSyn avoids them because its execution
IR is materialised once into the GC heap and walked by pointer. Secondary: the
hot loop threads `Result<_, ExecutionError>` and drops a large error enum every
instruction (`drop_in_place ExecutionError`, 2.4–6.7 %).

---

## 7. Recommendations (for owner — not implemented)

1. **Eliminate per-instruction transient `Vec` allocation** (highest value). Read
   arg offsets/branch tables directly from the code slice into a reusable
   scratch buffer or stack-resident smallvec; stop `collect()`-ing per App/Case.
   This targets the ~19–21 % malloc cost that makes bytecode lose on all
   allocation-heavy code — likely turns the 1.3–2.1× losses into parity-or-win.
2. **Shrink the hot-path `Result`/`ExecutionError`** (e.g. box the error payload
   so the `Ok` path drop is trivial) — retires the 2.4–6.7 % `drop_in_place`
   cost.
3. **Reinstate a bytecode-compatible block index** — real but low priority; only
   matters for large lookup-dominated blocks and is dominated by (1).
4. **BV3 register frames / strict-fold** — the O(n²) env-walk
   (`range foldl`, `EnvironmentFrame::get` 8–13 %) hits **both** engines and is
   the single biggest algorithmic ceiling; independent of the engine choice.
5. **On the Phase-4 delete-HeapSyn question:** do **not** retire HeapSyn until
   (1) lands and the day08-class GC pathology is understood. Today bytecode is a
   net regression on the majority of allocation-heavy programs; its only decisive
   win (day08) is a HeapSyn GC failure, not a bytecode dispatch win. Fix (1)
   first, then re-run this A/B.
