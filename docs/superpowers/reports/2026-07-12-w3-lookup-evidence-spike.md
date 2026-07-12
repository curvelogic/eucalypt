# W3 lookup-evidence spike — is the bytecode block index (eu-4zhi) worth its ABI-migration cost?

- **Bead:** eu-cj1h.4 (spike, report-only) · parent eu-cj1h (0.12.1 — close the engine gap) · subject eu-4zhi
- **Date:** 2026-07-12
- **Author:** Stopwatch
- **Branch/build:** `spike/lookup-evidence` off `origin/master` @ `0bac13a3`; clean `cargo build --release`; rustc 1.97.0; all runs `timeout … --heap-limit-mib 12288` (default heap), Apple aarch64.
- **Status:** profiling + analysis only. No engine changes.

## TL;DR — recommendation: **DEFER (leaning DROP) out of 0.12.1**

Reinstating the bytecode block index (eu-4zhi) buys a large speed-up **only** for a
workload pattern that does not occur in the real eucalypt corpus: a **stable,
static-literal, large block** hit by **saturated repeated `.key` lookups**. The
synthetic upside there is real and big (up to ~87× bc/hs at 1000 keys). But:

1. **No real code hits that pattern.** Every large *literal* block in the corpus is
   a namespace (prelude = 291 keys), a `test:` container, a synthetic bench table, or
   a let-scope module — none are dynamically keyed in a loop. The *only* large blocks
   under hot lookups are **runtime-built, input-sized tables** (day11, ~589 nodes).
2. **On that one real large-block workload, bc and hs are already at PARITY**
   (day11-p1: bc 1.50–1.60 s vs hs 1.47–1.55 s). The existing HeapSyn index delivers
   **no measurable benefit** there, because those tables are dynamic (boxed-symbol
   `sym(name)` keys) and incrementally rebuilt (a new block instance per fold step, so
   a stored index is never reused). Reinstating the same index on bytecode would
   therefore also deliver ~nothing on day11.
3. **The realistic ceiling for a *dynamic* stable table is ~1.2–1.5×**, not 87× — and
   even that is the generous case (built once, never rebuilt), which the corpus does
   not contain either.

Against a large, GC-risky SynClosure-ABI migration (the index build/store/lookup
subsystem panics on `BcBifContext`; static literals bake into the immutable arena; a
pointer-keyed side-table is GC-unsafe), the realised upside on code that exists today
is approximately zero. **Defer it out of 0.12.1.** Re-trigger only on concrete
evidence of a stable, large, hot-queried table *and* a cheap indexing path for
boxed-symbol keys (see "When to revisit").

---

## 1. Mechanism (what is actually gated off, and what the "index" really does)

`.key` lookups (static and dynamic) compile to `LookupOr`/`SafeLookup`
(`src/eval/stg/compiler.rs:1846`, `src/eval/stg/block.rs`). Each first tries an
index BIF, then falls back to a linear STG **find loop** that walks the block's
cons-list comparing keys.

- **HeapSyn engine:** `block_index_enabled()` defaults `true`
  (`src/eval/machine/intrinsic.rs:487`). For a block with ≥ `BLOCK_INDEX_THRESHOLD`
  = **16** keys, the BIF lazily builds a `SymbolId→position` map and stores it in the
  block by mutating its heap node (`store_index_in_block`, `block.rs:858`).
- **Bytecode engine:** `BcBifContext::block_index_enabled()` returns **false**
  (`src/eval/bytecode/machine.rs:3463`). The index build/store path names
  `SynClosure`/`EnvFrame`/`nav`/`set_closure`, which the bytecode `BcBifContext`
  cannot supply — those methods `panic!` (`machine.rs:3474`). So bytecode always
  takes the linear find loop. **This is the whole of eu-4zhi's scope: porting that
  subsystem onto a neutral ABI.**

**Important nuance — the "O(1) index" is not O(1).** On an index *hit*, the code still
calls `walk_list_to_position` (`block.rs:620,1170`), which walks the cons-list to
`position` via `BlockListIterator::nth(position)` — **O(position) ≈ O(N/2)**. The
index removes the per-cell *key comparison*, not the walk. What actually makes HeapSyn
fast is that this walk is **native Rust iteration**, whereas the bytecode find loop is
**interpreted** (one `dispatch`/`enter_local` round-trip and a closure allocation per
cons cell). So the win an index would give bytecode is "native position-walk" vs
"interpreted find-loop", not "constant time" vs "linear".

**Profile confirmation.** `sample` of an 800-key saturated bc run (leaf frames,
`prof.txt`):

```
handle_op        845
enter_local      727
BytecodeMachine::dispatch  468
read_ref         325
step             126
… env_builder / make_arg_array / BumpBlock::bump / Heap::try_allocate (per-cell alloc)
```

The O(n) cost lives in the **interpreted find loop** (dispatch/enter_local/read_ref
plus per-cell allocation), **not** in `bc_lookup_in_block` — which early-returns
`ListNil` on bytecode. The per-cell allocation is why the bc curve is *super*-linear
at large N (allocation → GC amplification), see §2.

---

## 2. Block-size scaling curve (synthetic worst case)

**Workload:** a static-literal block `{ k0:0 … k(N-1):N-1 }`; a hot loop performing
saturated dynamic `lookup` across 16 keys spread over the block (`gen2.py`), so the
per-cell key comparison is exercised at average depth ~N/2. Two densities:

### 2a. Dense (16 lookups/iter × M=500 = 8000 lookups; minimal list/GC harness)

| N (keys) | bc (s) | hs (s) | bc/hs | index built on hs? |
|---:|---:|---:|---:|:--|
| 8   | 0.15 | 0.12 | 1.25 | no (< 16) |
| 12  | 0.22 | 0.16 | 1.38 | no (< 16) |
| 15  | 0.28 | 0.23 | 1.22 | no (< 16) |
| 16  | 0.25 | 0.15 | 1.67 | yes |
| 40  | 0.40 | 0.21 | 1.90 | yes |
| 100 | 0.99 | 0.19 | 5.21 | yes |
| 250 | 3.31 | 0.37 | 8.95 | yes |
| 500 | 15.07 | 0.41 | 36.76 | yes |
| 1000 | 72.73 | 0.83 | 87.63 | yes |

Reproducibility (independent re-runs): N=100 bc 0.86 / 0.99; N=500 bc 14.19 / 14.65 —
~10 % variance, well inside the trend.

### 2b. Single lookup/iter × M=5000 (list/`map`/`sum` harness present)

| N | bc (s) | hs (s) | bc/hs |
|---:|---:|---:|---:|
| 10  | 1.16 | 1.00 | 1.16 |
| 40  | 1.37 | 0.99 | 1.38 |
| 100 | 1.70 | 1.06 | 1.60 |
| 250 | 3.26 | 1.11 | 2.94 |
| 500 | 11.25 | 1.22 | 9.22 |

### Interpretation

- **Below the 16-key threshold the index does nothing** — bc ≈ hs (ratio ~1.2–1.4,
  which is generic bytecode-vs-HeapSyn overhead on this loop, *not* lookups; neither
  engine has an index there). Small records — the overwhelmingly common block shape —
  get **zero** benefit from eu-4zhi.
- **HeapSyn is near-flat**; bytecode grows and, past ~250 keys, *super*-linearly
  (N 500→1000 is 4.8×, not 2×) because each interpreted cons step allocates, so larger
  blocks amplify GC. This is why the headline ratios look huge.
- **The huge ratios (9×–87×) require both large N *and* saturated repeated lookups on
  the *same stable* block.** That is the synthetic worst case; §4 shows it is absent
  from real code.
- Prior evidence (eu-mhjz: ~4 % on a 40-key block) is consistent: at realistic
  *density* (a handful of lookups, not 8000), even 40 keys is single-digit-percent.
  The 1.9× at N=40 here is the *saturated* figure.

---

## 3. Static-literal vs dynamic (boxed-symbol) blocks

eu-4zhi notes different fixes for literal vs dynamic blocks. But the more decisive
finding is that **the index's payoff itself differs sharply by block kind**, even on
HeapSyn where it is enabled. Same workload as §2a, block built as a static literal vs
built once at runtime via `block([[:k0,0]…])` (boxed `sym` keys, stable instance):

| N | static bc | static hs | static bc/hs | dyn bc | dyn hs | dyn bc/hs |
|---:|---:|---:|---:|---:|---:|---:|
| 100 | 1.08 | 0.23 | **4.7×** | 2.09 | 1.38 | **1.5×** |
| 250 | 3.58 | 0.33 | **10.8×** | 9.28 | 7.48 | **1.24×** |

(N=500 dynamic did not complete inside the 2-minute wall — dynamic blocks are costly
on both engines.)

**Reading:** for a *dynamic* boxed-symbol block, even HeapSyn's index barely helps
(hs 7.48 s at N=250 vs 0.33 s for the same-size static block — 22× slower). Boxed keys
force per-lookup unboxing and a slower comparison/extraction path, so the index's
advantage largely evaporates. **This matters enormously because every large
hot-lookup block in real code is dynamic and boxed** (§4). The realistic upside for
reinstating the index is the *dynamic* column (~1.2–1.5×), not the static one.

---

## 4. Real-code evidence (the crux)

Surveyed `examples/aoc25/*.eu` (12 files, AoC-2025), `lib/*.eu` (prelude et al.), and
`tests/harness/**` for block sizes and lookup density.

### 4a. Largest blocks are never hot-lookup targets

| Keys | Location | Kind |
|---:|:--|:--|
| **291** | `lib/prelude.eu` top-level unit | module namespace (resolved by name, not `.key`) |
| 51 | `tests/harness/175_sv2_to_spec.eu` `test:` | test container (each key evaluated once) |
| 50 | `tests/harness/bench/006_block_lookup.eu` `big-block` | synthetic bench table |
| 35 | `tests/harness/134_match_predicate.eu` `test:` | test container |
| 33 | `lib/prelude.eu` `str:` namespace | library namespace |
| 32 | `examples/aoc25/day10.eu` `solve-machine2` | let-scope module (`{…}.(expr)`) |

Every large *literal* block is a namespace, a `test:` container, a synthetic table, or
a let-scope block whose keys are bound in scope and referenced **by name** (not via
runtime `.key` in a loop). None is a hot-lookup target.

### 4b. The only large hot-lookup blocks are runtime-built, input-sized tables

`examples/aoc25/day11.eu` is the strongest real case:
- `dests-of(g,name): lookup-or(sym(name), [], g)` inside recursion (`dfs-topo`) and a
  fold; `lookup-count(table,name): lookup-or(sym(name), 0, table)` inside a `map`/fold
  (`day11.eu:44,63,69,76`).
- `g` and the DP `table` are **built at runtime** via `block`/`kv-block`/`merge`, sized
  by input: measured `inputs/day11.txt` = **589 nodes**, so both tables reach ~589
  keys — far above the 16-key threshold. The author annotates "O(log n) lookup" four
  times, assuming an index path exists.

**Measured, day11-p1 (589-node tables, hot lookups in recursion + fold):**

| | bc (s) | hs (s) |
|:--|---:|---:|
| rep 1 | 1.50 | 1.47 |
| rep 2 | 1.60 | 1.55 |

**Parity.** The HeapSyn index — enabled, threshold met — delivers **no measurable
benefit**. Two compounding reasons: (i) the tables are dynamic **boxed-symbol** blocks
(§3: index barely helps those); (ii) the DP `table` is **incrementally rebuilt**
(`foldl(dp-step(g), {}, order)` produces a *new* block instance each step), so a
lazily-built in-place index is thrown away before it is ever reused. Reinstating the
same index on bytecode would inherit both problems → ~no benefit on day11.
(day11-p2 runs the same dynamic-table pattern four times over; part-1 parity is
representative — no reason to expect part-2 to differ.)

### 4c. Real hot-loop `.key` lookups otherwise target *small* blocks

e.g. `day02.eu:76` `include-exclude` = **4 keys**, dot-looked-up inside a nested `map`
— below threshold, always linear, but N is tiny so it is cheap. `006_block_lookup.eu`
(50-key static) does only **~20 one-shot lookups** (not a hot loop) → bc = hs = 0.04 s;
this is the eu-mhjz ~4 % case.

**Bottom line:** the intersection of "large block" AND "repeated hot lookup" occurs in
the corpus **only** for day11-style runtime tables — which are exactly the dynamic,
boxed, rebuilt case where even the existing index yields nothing.

---

## 5. Projected upside range

| Scenario | Occurs in corpus? | Projected bc speed-up from an O(position) index |
|:--|:--|:--|
| Small record (< 16 keys), any density | ubiquitous | **0 %** (index never built) |
| Static-literal large block, saturated hot lookups | **no** | 5×–87× (synthetic only) |
| Stable *dynamic* large table, hot lookups | **no** (tables are rebuilt) | ~1.2–1.5× |
| Rebuilt dynamic table, hot lookups (day11) | **yes** | ~1.0× (measured parity) |

Realistic upside **on code that exists today: ≈ 0** (day11 parity; everything else is
small blocks or cold lookups). The eye-catching 9×–87× figures are reachable only by a
stable/static/large/saturated pattern that the corpus does not contain.

---

## 6. Cost side (from eu-4zhi / furnace-stats #961)

- Index build/store/lookup is written against the HeapSyn ABI; `nav`/`set_closure`/
  `root_env` **panic on `BcBifContext`** — a full port onto the neutral ABI is required
  even for a "dynamic-blocks-only" subset.
- Static block literals bake their template into the **immutable code arena** — no
  per-instance mutable index slot exists; giving them one is a codegen change.
- A pointer-keyed side-table is **GC-unsafe across evacuation**.
- Any of these lands new mutable state on the hot lookup path → GC-safety gate
  mandatory (the plan already requires this).

High, GC-risky effort for ≈ 0 realised upside on current code.

---

## 7. Recommendation & when to revisit

**DEFER eu-4zhi out of 0.12.1 (leaning DROP).** Do not spend the SynClosure-ABI
migration budget now. The measured benefit on the only real large-block hot-lookup
workload is nil, and the large synthetic wins correspond to a code shape (stable +
static + large + saturated) that does not appear in the corpus.

**Re-trigger conditions — revisit only if _all_ hold:**
1. A real workload appears with a **stable** (not rebuilt) large block queried in a hot
   loop, and
2. that block's keys are cheap to index — i.e. the boxed-symbol lookup/extraction path
   is first made competitive (else even a correct index yields the ~1.2× dynamic
   ceiling), and
3. ideally, `walk_list_to_position` is replaced by a true O(1) value store (the current
   design is O(position), capping the ceiling at native-walk speed).

If instead the goal is "fast large lookup tables" generally, a better-value line of
work than eu-4zhi is a **native map/dict value backed by `im-rc`** (persistent blocks
already depend on it) with O(log n) keyed access independent of engine — sidestepping
the ABI migration and the boxed-symbol penalty entirely. Worth a separate spike before
committing to eu-4zhi in any future cycle.

### Uncertainty / caveats (candid)

- The synthetic bc ratios are inflated by GC/allocation amplification in the
  interpreted find loop, so 87× overstates the *algorithmic* O(n) gap; the *direction*
  and the real-code parity conclusion are robust regardless.
- The synthetic hot loops use a `range/map/sum` harness that is itself super-linear in
  the iteration count (an unrelated list/GC effect); I controlled for it by holding the
  iteration count fixed and varying only block size, and by subtracting a no-lookup
  baseline — but absolute seconds should be read as trend, not micro-benchmark truth.
- day11-p2 was not separately timed bc/hs (part-1 parity taken as representative of the
  same dynamic-table pattern).

## Appendix — reproduction

Generators and workloads live on `spike/lookup-evidence` (`gen.py`, `gen2.py`,
`w_*.eu`, `prof.txt`) under the worktree; not committed to `master`. Key commands:

```
# dense static sweep
python3 gen2.py <N> 500 16 > w.eu
/usr/bin/time -p ./target/release/eu w.eu -e main            # bytecode (default)
/usr/bin/time -p env EU_HEAPSYN=1 ./target/release/eu w.eu -e main   # HeapSyn baseline

# real workload
cd examples/aoc25
/usr/bin/time -p ./eu day11.eu -t part-1                     # (path to release eu)
EU_HEAPSYN=1 /usr/bin/time -p eu day11.eu -t part-1
```
