# Allocation-gap profiling spike — why bytecode trails HeapSyn on alloc-bound cases

- **Bead:** eu-cj1h.2 (epic eu-cj1h, 0.12.1 "Close the engine gap")
- **Author:** Stopwatch
- **Date:** 2026-07-12
- **Branch base:** `master` @ `5670a16e` (PR #982 fused-primop-inline / Option C merged)
- **Toolchain:** rustc 1.97.0 (2d8144b78), clean `cargo build --release`
- **Host:** macOS 26.5.1, aarch64 (M-series)
- **Status:** PROFILING + ANALYSIS ONLY — no VM/source changes. Recommends one lever for owner approval.

> Every `eu` run below used `timeout 600 --heap-limit-mib 12288`. All numbers are
> pasted from real command output. bc = default bytecode engine; hs = `EU_HEAPSYN=1`.
> Timing is the `VM Mutator` figure from `-S` (eval-only, excludes parse/compile/render),
> taken as the median of 9 interleaved bc/hs runs (drift-robust harness).

---

## 1. Executive summary

The bytecode↔HeapSyn gap on alloc-bound programs is **not** caused by the bytecode
engine performing *more* managed allocations — allocation **counts are identical**
(bc ≤ hs on every case measured). The gap is entirely **per-operation overhead in the
bytecode drive loop**, and it splits into two fusion-immune buckets that scale with the
number of `LET`/data-construction ops an alloc-heavy program executes:

1. **Bytecode re-decode** (reading form headers / arg-offsets / refs out of the byte
   stream on every op): **~14.5 % of bc mutator time on `drop_cons`, with no HeapSyn
   analogue** (HeapSyn walks a pre-materialised pointer graph). Largest single contributor.
2. **Transient system-heap traffic** (`malloc`/`free` on the C heap, *outside* the GC):
   **7.0 % on `drop_cons`, 7.2 % on `short_lived`, 5.4 % on aoc25 day09 — versus 0.2 % on
   HeapSyn.**

**Recommended lever (lowest-risk, evidence-grounded, in-repo precedent):** finish the
job eu-w2oy started. eu-w2oy (CLOSED, PR #955) converted the *arg-offset*, *arg-ref* and
*Case branch-table* paths off the system heap, but **left two hot paths still allocating
a transient `Vec` per op**: `read_let` → `Vec<FormHeader>` (fires on **every** `LET`/`LETREC`,
i.e. every thunk/closure allocation) and `materialize_bif_args` → `Vec<Ref>` (every BIF
call). Reading form headers straight into the managed env-frame array — exactly as the
`Op::Case` handler already reads its branch table (machine.rs:1739 comment: *"buffering it
in a transient `Vec`/`SmallVec` first would … malloc per `Case`"*) — removes this residual.

- **Projected upside:** recovers most of the residual 5–7 % system-malloc/free on
  alloc-bound cases → `drop_cons` bc/hs from **1.77× toward ~1.68×**; proportional smaller
  wins elsewhere. **Partial, not gap-closing.**
- **Biggest uncertainty:** the exact split of the 7 % between `read_let` and
  `materialise_bif_args` (and `drop_glue`) can only be nailed by implementing and re-profiling;
  the *decode* bucket (14.5 %, the larger prize) is architectural and out of scope for a
  surgical lever.
- **W3 / eu-4zhi note:** the block index is **irrelevant to every alloc-bound case here** —
  zero block-lookup samples in `drop_cons`, `short_lived`, or day09. It addresses a
  different (lookup-heavy) workload and must not be expected to move the alloc gap.

---

## 2. Per-case classification

Median of 9 interleaved runs. `allocs` and `ticks` from `-S` (`Allocs`, `Ticks`).

| Case | bc mut (s) | hs mut (s) | **bc/hs** | bc ticks | allocs (bc = hs*) | allocs/tick | Class |
|------|-----------:|-----------:|----------:|---------:|------------------:|------------:|-------|
| `005_drop_cons`   | 0.0893 | 0.0506 | **1.765×** |   2,900,118 |   440,094 | 0.152 | **alloc-bound (extreme)** |
| `007_short_lived` | 0.1220 | 0.0856 | **1.425×** |   4,860,375 |   424,359 | 0.087 | **alloc-bound** |
| aoc25 day03 `-t part-2` | 0.6677 | 0.4752 | **1.405×** |  26,689,572 | 2,461,441 | 0.092 | **alloc-bound** |
| `004_generations` | 0.0827 | 0.0601 | 1.377× |   3,360,835 |    95,315 | 0.028 | mixed (thunk chains) |
| aoc25 day09 `-t part-1` | 4.6158 | 4.0253 | 1.147× | 188,897,784 | 12,339,745 | 0.065 | decode-bound (long-running) |
| `001_naive_fib` (fused) | 2.8676 | 2.2489 | 1.275× |  98,277,638 | 18,847,776 | 0.192 | decode-bound — **fusion-territory, already addressed** |

\* Allocation counts are identical or bc-lower on every case:
`drop_cons` 440,094 = 440,094; `short_lived` 424,359 = 424,359; `generations` 95,315 = 95,315;
day03 bc 2,461,441 **<** hs 2,481,841; day09 bc 12,339,745 **<** hs 12,341,729; fib 18,847,776 = 18,847,776.
**The bytecode engine never allocates more than HeapSyn.** The gap is not an allocation-*count* problem.

**Reading the table.** `drop_cons` is the extreme: highest allocs/tick (0.152), tiny tick
count, worst ratio (1.77×). It is the clean alloc-bound probe. `fib` is the contrast — high
allocs/tick *post-fusion* only because PR #982 collapsed the non-alloc Case+primop ticks;
its residual 1.275× is decode-bound and is what fusion targeted. So allocs/tick alone is a
noisy classifier — the profiles below are the real evidence.

**Per-tick cost is the invariant.** On `drop_cons`, bc = 0.0893 s / 2.90 M ticks = **30.6 ns/tick**
vs hs = 0.0506 s / 2.98 M ticks = **17.0 ns/tick** (bc 1.80× per tick). On fib, bc = 29.2 ns/tick
vs hs = 18.0 ns/tick (bc 1.62×) — and note fib's bc **tick count is already 21 % lower than hs**
(98.3 M vs 125.2 M, fusion's doing) yet bc is still slower in wall time. **Each bytecode tick is
~1.6–1.8× more expensive than a HeapSyn step, universally.** The alloc-bound cases simply execute a
higher proportion of the ticks (`LET` + data construction) that fusion cannot merge.

---

## 3. Allocation-path profiles: bytecode vs HeapSyn

Sampling profiler: macOS `/usr/bin/sample` at 1 kHz (samply 0.13.1 crashes on macOS 26.5;
dtrace needs privileges under SIP). Workload: `drop_cons` scaled to `take(400000)`
(~1.6 s eval) so the steady-state loop dominates. `1 sample ≈ 1 ms` main-thread CPU.
(The `__ulock_wait`/`semaphore_wait_trap` counts belong to a parked signal-handler thread
and are excluded — all figures are main-thread self-time.)

### 3.1 Top self-time (leaf) symbols

**Bytecode — main-thread total 3803 samples:**

| self ms | symbol |
|--------:|--------|
| 1070 | `bytecode::machine::handle_op` (all opcodes inlined here) |
|  281 | `BytecodeMachine::dispatch` |
|  205 | `bytecode::machine::read_ref` — *decode* |
|  201 | `bytecode::machine::enter_local` — local resolve |
|  128 | `memory::heap::Heap::try_allocate` — **managed alloc** |
|  125 | `bytecode::machine::read_arg_offsets` — *decode* |
|  107 | `bytecode::machine::step` |
|   98 | `Array<BcValue>::push` — env-frame build |
|   94 | `bytecode::machine::make_arg_array` — env-frame build |
|   92 | `bytecode::machine::return_data` |
|   91 | `memory::bump::BumpBlock::bump` — **managed alloc** |
|   83 | `Array<Option<u64>>::push` |
|   83 | `bytecode::machine::return_meta` |

**HeapSyn — main-thread total 2979 samples:**

| self ms | symbol |
|--------:|--------|
| 786 | `machine::vm::MachineState::handle_instruction` |
| 317 | `machine::vm::Machine::run` |
| 235 | `memory::heap::Heap::try_allocate` — **managed alloc** |
| 214 | `MachineState::return_data` |
| 191 | `EnvBuilder::create_arg_array` — env-frame build |
| 153 | `memory::bump::BumpBlock::bump` — **managed alloc** |
| 128 | `EnvBuilder::from_let` — env-frame build |
| 126 | `EnvironmentFrame::get` — env resolve |
| 100 | `Heap::find_space` — **managed alloc** |
|  95 | `MachineState::return_fun` |

### 3.2 Bucketed self-time (absolute ms; 1 sample ≈ 1 ms)

Buckets are approximate (some symbols span roles — e.g. `make_arg_array` both decodes
offsets and builds the managed array). Directionally robust.

| Bucket | bc ms | bc % | hs ms | hs % | **bc − hs** |
|--------|------:|-----:|------:|-----:|------------:|
| dispatch / resolve loop (`handle_op`,`dispatch`,`step`,`run`,`enter_local`,`HeapNavigator`) | 1802 | 47.4 | 1245 | 41.8 | **+557** |
| **bc re-decode** (`read_ref`,`read_arg_offsets`,`read_form_header`,`read_let`,`make_arg_array`,`resolve_ref`) | 553 | 14.5 | 0 | 0.0 | **+553** |
| **system malloc/free/drop** (`*malloc*`,`_free`,`drop_glue`,`memset`) | 266 | 7.0 | 5 | 0.2 | **+261** |
| transient `Vec`/`SmallVec` build (`spec_from_iter`,`try_process`,`materialize_bif_args`) | 59 | 1.6 | 0 | 0.0 | **+59** |
| env-frame build (`Array::push`,`env_builder`,`from_saturation`,`env_from_data_args`) | 352 | 9.3 | 690 | 23.2 | −338 |
| managed alloc (`try_allocate`,`bump`,`find_space`) | 280 | 7.4 | 534 | 17.9 | −254 |
| return dispatch (`return_data/fun/meta/native`) | 280 | 7.4 | 405 | 13.6 | −125 |
| intrinsics | 22 | 0.6 | 5 | 0.2 | +17 |

### 3.3 The per-allocation cost diff (this IS the lever)

The inversion in §3.2 is the finding. **HeapSyn spends the *majority* of its time on
productive work** — env-frame construction (23.2 %) + managed allocation (17.9 %) +
return dispatch (13.6 %) = **54.7 %**. **Bytecode spends only 24.1 %** on those same three,
and burns the difference on a decode/dispatch envelope wrapped around each allocation:

- Bytecode does the **same managed allocations** but reaches each one by first
  **re-decoding the byte stream** (form header, arg-offsets, refs) — a cost HeapSyn paid
  *once* when it materialised its IR into the GC heap, and never repeats.
- Wrapped inside that decode, two paths still **malloc/free on the C heap per op**
  (§4), which HeapSyn never touches (0.2 %).

Same allocation graph, ~1.8× the per-tick cost — because every allocation in bytecode is
delivered through a heavier decode + transient-malloc envelope.

### 3.4 Corroboration on other cases

| Case (bc) | main total | system malloc/free/drop | block-lookup |
|-----------|-----------:|------------------------:|-------------:|
| `drop_cons` take(400000) | 3803 | **7.0 %** | **0 %** |
| `short_lived` churn(100,4000) | 2228 | **7.2 %** | **0 %** |
| aoc25 day09 `part-1` | 3038 | **5.4 %** | **0 %** |

The transient-malloc tax generalises across every alloc-bound case; **block-lookup cost
is zero in all of them** (see §5).

---

## 4. Root cause of the residual system-heap traffic

eu-w2oy (CLOSED, PR #955) was *exactly* this issue: *"the bytecode hot loop allocates
transient Vecs on the SYSTEM heap once per reduction step — read_arg_offsets,
arg_refs.collect(), read_branch_table, make_arg_array … malloc/free = 19–21 % CPU."*
It **partially** landed. Verified against `master` @ `5670a16e`:

- ✅ **Fixed.** `Op::Cons`/data return now uses **inline `SmallVec<[_;8]>`**
  (`type ArgOffsets = SmallVec<[CodeRef; 8]>`, `ArgRefs = SmallVec<[DecodedRef; 8]>`,
  machine.rs:60–61) — ≤ 8 args stay on the stack, no malloc.
- ✅ **Fixed.** `Op::Case` reads its densified branch table **straight into the managed
  `Array`** (machine.rs:1739–1746), with a comment explicitly explaining the anti-malloc
  intent.
- ❌ **Missed — still mallocs per op:**
  - **`read_let` → `Vec<FormHeader>`** (machine.rs:1591–1596), consumed by `Op::Let`
    (1808–1819) and `Op::LetRec` (1821–1841). Fires on **every `LET`/`LETREC`** — i.e.
    every thunk/closure allocation in alloc-heavy code. The handler already rebuilds a
    managed `Array<BcValue>` immediately afterwards, so the `Vec<FormHeader>` is pure
    transient waste (malloc → iterate → free). Call-graph confirms:
    `read_let` → `spec_from_iter_nested::<Vec<FormHeader>>` → `__rustc::__rdl_alloc`.
  - **`materialize_bif_args` → `Vec<Ref>`** (machine.rs:436–445), one fresh heap `Vec`
    per BIF dispatch (1730 not this — that is the fixed SmallVec path; 2253 / 3361 are the
    BIF path). Leaf profile shows its `spec_from_iter::<Vec<Reference,Native>>`.

**Why it was missed:** eu-w2oy converted the *argument* and *branch* collections (which are
what the A/B study's flamegraph surfaced on call-dense fib) but the *`LET`-form-header* and
*BIF-args* collections sit on a different decode path that the fib-centric study under-weighted.
`drop_cons` (LET-per-cons) makes them dominant.

---

## 5. Candidate levers, scored against the evidence

### (c) Transient-allocation reduction — **RECOMMENDED, pursue first**
Finish eu-w2oy: take `read_let` and `materialize_bif_args` off the system heap.
- **Two clean options, both precedented in-repo:**
  - **Direct-read** the form headers into the managed env-frame `Array` inside `Op::Let`
    (mirrors `Op::Case` at machine.rs:1740). `Op::LetRec` needs the existing two-pass
    (pre-size dangling → alloc frame → fill), reading each `FormHeader` (12 bytes, `Copy`)
    from `code` in the fill pass. Eliminates the `Vec` entirely.
  - **`SmallVec<[FormHeader; 8]>`** as `read_let`'s return type — smallest diff, directly
    parallels the arg-offset fix; most `LET` blocks bind 1–4 forms so inline-8 avoids the
    malloc in the common case.
- **Evidence:** direct — §3.2/§4, 5–7 % system-malloc/free on every alloc-bound case,
  with the exact allocating call sites identified.
- **Projected upside:** recovers most of the 7.0 % on `drop_cons` → bc/hs **1.77× → ~1.68×**;
  ~5–7 % mutator on `short_lived`/day03; ~5 % on day09. **Real but partial** — does not
  close the gap.
- **Risk:** LOW. Local to two functions + their handlers; the Case handler is a working
  template; GC-safety unchanged (fewer, not more, off-heap objects). Standard clippy/test gate.

### (a) Reduce per-allocation env-frame / `Array<BcValue>` overhead
- **Evidence:** WEAK as a *gap* lever. Bytecode's env-frame build is already **cheaper**
  than HeapSyn's in absolute self-time (352 ms vs 690 ms) — HeapSyn spends *more* here.
  There is no bytecode-specific excess to remove; this is shared, already-lean machinery.
- **Verdict:** not the gap. Deprioritise.

### (b) Block index reinstatement (eu-4zhi / W3)
- **Evidence:** **zero** block-lookup samples in `drop_cons`, `short_lived`, or day09
  (§3.4). None of the alloc-bound cases exercise block lookup.
- **Verdict:** **orthogonal to the alloc gap.** It targets lookup-heavy blocks only, where
  the prior A/B measured ~4 % on a 40-key block (day11-p1 already at parity). It should be
  pursued on its own merits (W3), **not** expected to move any case in this report.
  Interaction with the recommended lever: none — disjoint code paths, disjoint workloads.

### (d) The larger, harder prize the profile surfaces — bytecode re-decode (out of scope)
- The **+553 ms decode bucket (14.5 %) is the single biggest contributor** and is
  **fusion-immune** (it is `LET`/data decode, not Case+primop). Closing it means not
  re-parsing the byte stream every execution — e.g. caching decoded `FormHeader`s per code
  offset, a threaded/pre-decoded instruction representation, or hoisting decode out of the
  drive loop. **High value, high risk, architectural** — a spike of its own, not a surgical
  0.12.1 lever. Flagged for the epic; recommend a separate dispatch if pursued.

---

## 6. Recommendation

**Pursue lever (c) first: finish eu-w2oy by removing the `read_let` `Vec<FormHeader>` and
`materialize_bif_args` `Vec<Ref>` system-heap allocations** (SmallVec or direct-read into
the managed array, mirroring the shipped `Op::Case` treatment). It is the only lever that is
(i) grounded in a measured 5–7 % cost on *every* alloc-bound case, (ii) low-risk with a
working in-repo precedent, and (iii) a clean completion of already-approved work.

Be candid about the ceiling: this is a **partial** win (`drop_cons` ~1.77× → ~1.68×). The
majority of the residual gap is the fusion-immune **decode + dispatch envelope** (§3.3),
which is architectural and needs its own spike. eu-4zhi (W3) is disjoint from all of this
and should be scheduled on its own lookup-heavy evidence, not against these cases.

**Only implementation + re-profile can confirm:** the exact `read_let` vs
`materialize_bif_args` vs `drop_glue` split inside the 7 %, and therefore the precise
realised delta. State the before/after `-S` mutator medians and a fresh `sample` malloc/free
share when the lever lands.

---

## Appendix — reproduction

```
# Build (direct toolchain PATH avoids a rustup-proxy quirk on this host):
export PATH="$HOME/.rustup/toolchains/stable-aarch64-apple-darwin/bin:$PATH"
cargo clean && cargo build --release

# Interleaved bc/hs mutator medians + ticks/allocs (harness in worktree):
bash measure.sh 005_drop_cons bench-drop-cons tests/harness/bench/005_drop_cons.eu

# Sampling profile (1 kHz, main-thread self-time):
EU=./target/release/eu
$EU --heap-limit-mib 12288 -t bench-drop-cons big_drop.eu >/dev/null 2>&1 & PID=$!
sample $PID 5 1 -f /tmp/sample_bc_drop.txt -mayDie
EU_HEAPSYN=1 $EU --heap-limit-mib 12288 -t bench-drop-cons big_drop.eu >/dev/null 2>&1 & PID=$!
sample $PID 6 1 -f /tmp/sample_hs_drop.txt -mayDie
# big_drop.eu = 005_drop_cons with take(400000); big_short.eu = churn(100,4000).
```

Hot allocating sites (all `src/eval/bytecode/machine.rs` @ `5670a16e`):
`read_let` 1591–1596; `Op::Let` 1808–1819; `Op::LetRec` 1821–1841;
`materialize_bif_args` 436–445; fixed precedent `Op::Case` 1733–1746; inline SmallVec
types 60–61.
