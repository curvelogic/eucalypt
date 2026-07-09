# W0 spike: bytecode decode-cost profiling + lever recommendation (eu-9mvh.1)

**Date:** 2026-07-09
**Branch:** `spike/bytecode-decode-cost` @ cb9acce4 (`origin/master`)
**Toolchain:** stable-aarch64-apple-darwin (rustc 1.97.0), clean release build
(`cargo clean && cargo build --release`), embedded prelude blob regenerated
(`cargo xtask prelude-compile`, `276486` bytes / `2634` constants / `548`
global forms).
**Machine:** Apple M3 Max, macOS 26.5.1 (Darwin 25.5.0).
**Author:** Stopwatch. **Status:** measurement + analysis only — no VM/source
changes.

This is the W0 profiling spike gating W1 (eu-9mvh, part 2) per
`docs/superpowers/specs/2026-07-09-0.12.1-delivery-design.md` §4 and
`docs/superpowers/plans/2026-07-09-0.12.1-close-the-engine-gap.md` Task 1. It
supersedes nothing in the 2026-07-03 A/B report
(`docs/superpowers/reports/2026-07-03-bytecode-vs-heapsyn-ab.md`) — it
re-measures on this machine, profiles four cases at opcode-name granularity,
and traces the dominant cost to a specific, reproducible STG-compilation
pattern with a concrete, low-risk fix.

---

## 0. Headline

**The single largest cost in every profiled case is the VM's own
dispatch/decode loop (`handle_op` + `dispatch` + `read_*` + `enter_local`),
at 46–65% of active CPU samples — not system `malloc`/`free` (which is much
smaller here, 1.9–6.2%, than the 2026-07-03 report's ~18.8% on fib; see §1.3
on drift).** Tracing this back to the STG, a **concrete, reproducible, and
fully explained root cause** was found: every strict binary primop call in
the language (`<=`, `-`, `+`, `*`, `÷`, `=`, …) compiles to a **fully
expanded 5×5 = 25-leaf `Case`-of-`Case` decision tree over the five `Native`
tags (Num/Str/Sym/Zdt/other)**, purely to force both operands to WHNF before
calling a single, internally-polymorphic intrinsic — **every one of the 25
leaves calls the identical primop.** This pattern recurs identically across
all four profiled programs (fib, day03-p2, day09-p1, day08-p1) — it is not an
artifact of one program.

Bytecode's `Op::Case` is expensive relative to the already-existing, cheap
`Op::Seq`: it decodes a full branch table (`min_tag`, `len`, up to `len`
`u32` body offsets) **and heap-allocates a managed `Array` sized to the
branch count** on every evaluation, whereas `Op::Seq` reads two `u32`s and
allocates nothing beyond the continuation frame both forms need anyway.
Recompiling a **uniform-branch** `Case` (all alternatives — and, where
present, the fallback — target the identical body) as `Op::Seq` instead
removes the branch-table decode and the per-Case `Array` allocation for
what the evidence below shows is the majority of `Case` traffic in
arithmetic/comparison-dense code.

**Recommendation: pursue lever (c), "reduced per-reduction decode work,"
first — specifically, detect uniform-branch `Case` nodes at bytecode-encode
time (`src/eval/bytecode/encode.rs::encode_node`, the `StgSyn::Case` arm) and
emit `Op::Seq` instead of `Op::Case`.** This is scoped entirely to the
bytecode *encoder* (it does not touch the shared STG syntax tree, so HeapSyn
is untouched and blob/source-prelude parity is a non-issue beyond the usual
regeneration step). It is low-risk, requires no new opcode, and — per §3 —
plausibly removes a double-digit percentage of the dominant cost bucket in
every arithmetic-touching program, including the worst regression-set case
(fib, today's 2.14×/1.96× outlier). True superinstruction fusion (lever a)
is recommended as a **second, complementary** step for the genuinely
non-uniform `Case` traffic (real data-constructor pattern matches) that
remains after the free win above. Denser operand encoding (lever b) is
recommended **last**, only if (c)+(a) fall short of the bar — its evidenced
share of cost is smaller and it does not touch the dominant `handle_op`/
`dispatch`/branch-table-allocation costs.

---

## 1. Baseline: bytecode vs HeapSyn, regression set (this machine, today)

### 1.1 Method

Clean release build; `--heap-limit-mib 12288` under `timeout 600` on every
run; external wall-clock median of 7 runs (increased from the specified 5 —
see §1.2 on machine noise); `EU_HEAPSYN=1` selects HeapSyn. AoC cases run
from `examples/aoc25/`.

```
BIN=./target/release/eu
for i in 1..7: timeout 600 $BIN --heap-limit-mib 12288 <case>        # bytecode
for i in 1..7: EU_HEAPSYN=1 timeout 600 $BIN --heap-limit-mib 12288 <case>  # HeapSyn
```

### 1.2 A note on machine noise

This is a shared development machine; `uptime` load average fluctuated
between **1.9 and 9.9** over the course of this spike (other agents/processes
active). An early 5-run pass at load ≈6–10 gave visibly noisier, lower
ratios than a later 7-run pass at load ≈2–4 (both included below for
transparency). All figures quoted as "final" in §1.3 are from the low-load
7-run pass. No code changed between the two passes — the difference is
purely environmental.

### 1.3 Results — today's bc/hs ratios vs the 2026-07-03 report

| Case | bc med (s) | hs med (s) | bc/hs (today) | bc/hs (2026-07-03 report) |
|---|--:|--:|--:|--:|
| `001_naive_fib` | 3.8313 | 1.9564 | **1.96×** | 2.14× |
| `005_drop_cons` | 0.1129 | 0.0659 | **1.71×** | 2.13× |
| `007_short_lived` | 0.1569 | 0.1304 | **1.20×** | 1.87× |
| aoc25 `day03-p2` | 1.4710 | 1.0036 | **1.47×** | 1.67× |
| aoc25 `day09-p1` | 5.3251 | 4.3930 | **1.21×** | 1.36× |

(Earlier, higher-load 5-run pass, for transparency: fib 2.041×/1.941×,
005 1.505×/1.298×, 007 1.160×/1.284×, day03-p2 1.566×, day09-p1 1.202× —
broadly consistent with the low-load pass; 005 and 007 show the most
run-to-run spread, expected given their sub-200ms wall times.)

**Drift observed: every ratio is lower today than the 2026-07-03 figures,
by a consistent 10–35% relative margin, direction unchanged (bytecode
trails HeapSyn on every regression-set case; fib remains the worst).** No
code has changed on this path since 0.12.0 shipped (this branch is
`origin/master` unmodified). The most likely contributor, per the profiling
in §2: **system `malloc`/`free` is a much smaller share of bytecode's CPU on
this run (1.9–6.2%) than the ~18.8–20.7% reported on 2026-07-03 for
fib/block-lookup** — plausibly a newer macOS malloc-zone implementation
(`_xzm_*` symbols throughout, vs plain `malloc_zone_*` framing in the
2026-07-03 report) with a cheaper fast path on this OS build
(macOS 26.5.1), plus general machine-noise variance on a shared host. This
does not change the qualitative conclusion — bytecode still trails HeapSyn
materially on every regression-set case — but the acceptance-bar owner
should be aware the 2.14×/2.13×/1.87×/1.67×/1.36× figures are this
machine-and-moment's numbers, not a fixed constant, and W1's final
before/after measurement should be taken on the same machine under the same
load discipline as this baseline, not compared against the 07-03 figures
directly.

`day08-p1` (bc only; HeapSyn is known to time out >500s, not re-run here):
**7.02s wall** (`time` build-in, single run) — consistent with "extreme
allocation, bytecode wins hugely" from the 07-03 report; used below purely
as a profiling exemplar, not a ratio target, per the delivery spec.

---

## 2. Per-symbol CPU profiles (bytecode engine)

### 2.1 Method

macOS `/usr/bin/sample` (same tool as the 2026-07-03 report), attached to
the actual `eu` child process (found via `pgrep -P` on the `timeout`
wrapper's PID — `sample`'ing the `timeout` process itself just captures its
`sigsuspend` wait loop, a methodology trap worth flagging for future
spikes), sampling interval 1ms, for 5–12s depending on case runtime (long
enough to cover the whole run). `__ulock_wait`/`semaphore_wait_trap` frames
(idle GC/worker threads blocked on a semaphore, not doing work) are excluded
from all percentages below, matching the 07-03 report's "idle
watchdog-thread frames excluded" convention. "Active samples" = total
top-of-stack samples minus these idle frames.

Raw `sample` output and the parsing/categorisation scripts used are not
committed (ephemeral profiling artefacts); the leaf-symbol tables below are
transcribed directly from `sample`'s "Sort by top of stack, same collapsed"
section — every percentage in this report is computed from that real,
unedited output.

### 2.2 Per-symbol leaf breakdown (top entries, % of active samples)

**`001_naive_fib`** (active=2574 samples, 8s window):

| Symbol | % |
|---|--:|
| `handle_op` | 24.20 |
| `BytecodeMachine::dispatch` | 8.16 |
| `Heap::try_allocate` | 5.05 |
| `return_native` | 4.55 |
| `Array<Option<Native>>::push` (branch table build) | 4.43 |
| `Array<BcValue>::push` (arg array build) | 3.42 |
| `BumpBlock::bump` | 3.38 |
| `read_ref` | 3.15 |
| `step` | 3.11 |
| `Heap::find_space` | 2.91 |
| `enter_local` | 2.64 |
| `drop_glue<ExecutionError>` | 2.45 |
| `read_arg_offsets` | 2.41 |
| `_xzm_free` (malloc) | 2.41 |
| `resolve_native` | 2.02 |
| `Vec::from_iter` (materialize_bif_args) | 1.83 |
| `env_from_data_args` | 1.67 |
| `native_from_value` | 1.63 |
| `BytecodeMachine::run` | 1.32 |
| `read_let` | 1.13 |
| `read_form_header` | 1.09 |
| `arith::Sub::execute` | 1.09 |
| `drop_glue<BcValue>` | 1.05 |
| (remainder, each <1%) | ~18.9 (combined) |

**`day03-p2`** (active=802, 5s window): `handle_op` 25.94, `dispatch` 10.97,
`read_ref` 7.11, `enter_local` 6.61, `read_arg_offsets` 4.86,
`make_arg_array` 4.11, `drop_glue<ExecutionError>` 3.87, `step` 3.37,
`try_allocate` 2.99, `Array<BcValue>::push` 2.24, `BumpBlock::bump` 2.00,
`return_data` 2.00, `return_fun` 1.75, `run` 1.62, remainder <1.5% each.

**`day09-p1`** (active=3895, 10s window): `handle_op` 25.70, `dispatch`
12.20, `enter_local` 10.09, `read_ref` 7.27, `read_arg_offsets` 4.42,
`drop_glue<ExecutionError>` 4.08, `step` 4.03, `try_allocate` 2.57,
`return_data` 2.11, `make_arg_array` 2.08, `BumpBlock::bump` 1.95, `run`
1.82, `saturate_with_array` 1.39, `find_space` 1.34, `env_from_data_args`
1.28, remainder <1.2% each.

**`day08-p1`** (active=5258, 12s window — extreme-allocation exemplar):
`handle_op` 26.97, `enter_local` 9.26, `dispatch` 8.46, `read_ref` 5.67,
`read_arg_offsets` 4.75, `drop_glue<ExecutionError>` 3.46, `step` 3.25,
`try_allocate` 2.76, `return_data` 2.59, `BumpBlock::bump` 2.23,
`make_arg_array` 2.03, `saturate_with_array` 1.94, `Array<BcValue>::push`
1.71, `run` 1.65, `env_from_data_args` 1.52, `return_fun` 1.48,
`find_space` 1.46, remainder <1.4% each. **No GC/mark frames present** —
confirms the 07-03 finding that bytecode keeps the managed heap small enough
to avoid HeapSyn's day08 GC death-spiral.

### 2.3 Category roll-up (all four cases; % of active samples)

| Category | fib | day03-p2 | day09-p1 | day08-p1 |
|---|--:|--:|--:|--:|
| `handle_op` | 24.20 | 25.94 | 25.70 | 26.97 |
| `dispatch` | 8.16 | 10.97 | 12.20 | 8.46 |
| decode (`read_ref`/`read_arg_offsets`/`read_form_header`/`read_let`/`step`) | 10.88 | 16.08 | 16.64 | 15.04 |
| `enter_local` | 2.64 | 6.61 | 10.09 | 9.26 |
| **core dispatch/decode loop (sum of above 4)** | **45.88** | **59.60** | **64.63** | **59.73** |
| arg/env build (`make_arg_array`/`env_from_data_args`/`saturate_with_array`/`Array::push`/`Vec::from_iter`/`try_process`) | 14.84 | 14.34 | 9.99 | 11.66 |
| managed-heap alloc (`try_allocate`/`find_space`/`BumpBlock::bump`) | 11.34 | 5.99 | 5.85 | 6.45 |
| `ExecutionError`/`BcValue` drop_glue | 3.50 | 5.37 | 4.62 | 4.05 |
| system malloc/free (`libsystem_malloc`) | 6.18 | 1.87 | 2.85 | 2.97 |
| return/resolve/enter/apply (dispatch support) | 9.29 | 6.36 | 6.91 | 8.22 |
| `BytecodeMachine::run` (outer loop) | 1.32 | 1.62 | 1.82 | 1.65 |
| intrinsic `execute` (arith/num_arg/etc.) | 2.95 | 0.00* | 0.26 | 1.08 |
| other (<0.15% each, long tail) | 4.70 | 4.86 | 3.08 | 4.18 |

`*` day03-p2's arith-intrinsic leaf frames fell just under the ≥0.15%
inclusion cut in this window; they are present in the raw table (`arith::Sub`
et al. sub-1% entries were dropped by the display cutoff, not absent from
execution).

**Reading:** the "core dispatch/decode loop" — `handle_op` + `dispatch` +
decode + `enter_local` — is the largest bucket in **every** case, at
**46–65%** of active CPU, dwarfing system malloc/free (1.9–6.2%, much
smaller than 07-03's ~18.8% on this run — see §1.3) and even the managed-heap
allocator (6–11%). `ExecutionError`/`BcValue` drop_glue sits at **3.5–5.4%**
combined, consistent with — slightly above — the delivery spec's W2 estimate
of 2.4–3.5%, confirming eu-adnu's target cost is real. The "arg/env build"
bucket (10–15%) is the direct-hit target for the 2026-07-03 report's
Recommendation #1 (transient `Vec` allocation per App/Bif); `materialize_bif_args`
(`fn materialize_bif_args(state) -> Vec<Ref>`, `machine.rs:433`) and `read_let`
(`fn read_let(...) -> (Vec<FormHeader>, CodeRef)`, `machine.rs:1377`) both
still heap-allocate a fresh `Vec` per call/Let — real, but a smaller share of
total cost on this run than 07-03 measured (their malloc/free figure folded
in more of the system-allocator cost that this run doesn't reproduce at the
same scale).

---

## 3. Fusible hot sequences — the dominant, well-evidenced finding

### 3.1 The pattern: uniform-branch `Case` used purely to force operands

`eu dump stg tests/harness/bench/001_naive_fib.eu` shows the compiled STG for
`fib(n): if(n <= 1, 1, fib(n - 1) + fib(n - 2))`. The `n <= 1` comparison
alone compiles to (excerpted, full nesting in the dump):

```
case ✳1 of                    # outer: tag-dispatch on operand 1 (n)
  Num → case ✳0 of             #   (forces n to WHNF as a side effect)
    _ → case ✳2 of              # inner: tag-dispatch on operand 0 (1)
      Num → case ✳0 of _ → LTE(✳2, ✳0)
      Str → case ✳0 of _ → LTE(✳2, ✳0)
      Sym → case ✳0 of _ → LTE(✳2, ✳0)
      Zdt → case ✳0 of _ → LTE(✳2, ✳0)
      _   → case ✳0 of _ → LTE(✳2, ✳0)
  Str → case ✳0 of  ... (same 5-way inner cascade, same LTE(✳2, ✳0)) ...
  Sym → ... (same) ...
  Zdt → ... (same) ...
  _   → ... (same, 5 more LTE leaves) ...
```

**Every one of the 25 leaves calls the textually identical `LTE(✳2, ✳0)`.**
This is not a display artefact: `grep -c` on the dumped STG confirms exactly
**25 occurrences of `LTE(`** for this single source-level `<=`, and exactly
**25×2 = 50 occurrences of `SUB(`** for the two `-` operations (`n - 1`,
`n - 2`), and 25 of `ADD(` for the one `+`. The type dispatch is not actually
happening in the `Case` structure — `LTE`/`SUB`/`ADD` are generic,
internally-polymorphic intrinsics (the profile's `arith::Lte::execute` /
`ordered_cmp` / `arith::Sub::execute` frames do the real per-type work). The
`Case`-of-`Case` exists **only to force both operands to WHNF** before the
single intrinsic call — the tag actually matched is discarded.

This is not fib-specific. The same shape, with the same 25-leaf-per-binop
signature, appears in every other profiled program:

| Case | Arithmetic op counts in `eu dump stg` (each = one 5×5 uniform-branch tree) |
|---|---|
| `001_naive_fib` | `LTE(` ×25, `SUB(` ×50, `ADD(` ×25 |
| `day03-p2` | `SUB(` ×50, `ADD(` ×50, `MUL(` ×25, `EQ(` ×11* |
| `day09-p1` | `SUB(` ×25, `ADD(` ×25, `GT(` ×25 |
| `day08-p1` | `SUB(` ×25, `ADD(` ×75, `MUL(` ×50, `DIV(` ×50 |

`*` `EQ(` ×11 rather than a multiple of 25 — `=` has a different STG shape
for at least one call site in day03 (mixed-type equality, not purely
numeric); not investigated further here, out of scope for the spike.

### 3.2 Why this is expensive: `Op::Case` vs `Op::Seq` at the bytecode level

`src/eval/bytecode/machine.rs`, `handle_op`'s `Op::Case` arm (~line 1518):
reads `min_tag` (u8) + `len` (u8) + up to `len` `u32` body offsets (a
densified branch table, up to 5 entries here), **then heap-allocates a
managed `Array` sized to `len` and pushes each entry** (`Array::with_capacity`
+ per-entry `push`, both visible in the profile as
`Array<Option<Native>>::push` at 4.43% on fib alone), before pushing a
`Branch` continuation. `Op::Seq` (~line 1548) reads two `u32`s (scrutinee
offset, body offset) and pushes a cheap `SeqBind` continuation — **no branch
table, no array allocation.** A uniform-branch `Case` (all alternatives, and
any fallback, target the identical body) is semantically equivalent to `Seq`
*provided the branch set is exhaustive* — the only externally-observable
difference `Case` provides over `Seq` in that situation is raising
`NoBranchForDataTag` on a genuinely unmatched tag, which cannot happen if
every tag in the (densified, full-range) table is already covered.

The bytecode encoder (`src/eval/bytecode/encode.rs::encode_node`, the
`StgSyn::Case` arm, ~line 150) currently encodes every `Case` node
uniformly — it does not check whether all branch bodies are identical.
**This is the concrete, scoped fix location.** It is purely a
bytecode-*emission* decision: the shared `StgSyn::Case` node (interpreted by
HeapSyn via direct tree-walk, unaffected either way) is untouched; only how
the bytecode *encoder* chooses between `Op::Case` and `Op::Seq` for a given
`StgSyn::Case` shape changes.

### 3.3 Quantifying the share

The "core dispatch/decode loop" bucket (§2.3, 46–65%) includes `handle_op`
+ `dispatch` + decode (`read_ref`/`read_arg_offsets`/`read_form_header`/
`read_let`/`step`) — **all of which run twice per binop** under the current
encoding (once for the outer `Case`, once for the inner `Case`) where a
`Seq`-based encoding would run them **twice but far more cheaply** (no
branch-table decode, no per-Case `Array` allocation). The "managed-heap
alloc" bucket (6–11%) is also partly attributable: `Array<Option<Native>>::push`
at up to 4.43% (fib) is the branch-table `Array` build specifically, which a
`Seq` encoding removes entirely. **Concretely, for fib alone, the two
per-op `Case` evaluations at the top of every `<=`/`-`/`+` account for a
lower bound of `read_arg_offsets`(2.41%, not directly used by Case but
indicative of decode-fn cost) + a share of `handle_op`/`dispatch`/`step`
proportional to Case-vs-Seq call-site frequency, plus the ~4.43% branch-table
`Array::push` directly attributable** — this spike does not have
instruction-level call-site attribution (no VM changes were made to add
per-opcode counters, per the profiling-only mandate), so an exact percentage
of the 46% "core loop" bucket attributable to `Case`-that-could-be-`Seq`
specifically is not claimed; the qualitative case is nonetheless strong: `Case`
sites vastly outnumber `Seq` sites in every dumped STG (fib: 137 `case `
occurrences for 4 logical arithmetic ops + 1 conditional + recursive calls),
and every arithmetic/comparison op — the operation fib, day03, day09 and
day08 are all dense in — pays the expensive form where the cheap form would
do.

---

## 4. Per-lever projection against the 1.25×/1.3× bar

### (c) Reduced per-reduction decode work — RECOMMENDED FIRST

**Concrete instance: uniform-branch `Case` → `Op::Seq` at bytecode-encode
time (§3).** Evidence: directly targets the dominant cost bucket (46–65% of
active CPU across all four profiled cases) with a specific, reproducible,
exhaustively-confirmed STG pattern (25-leaf identical-body decision trees on
every strict binop, in every profiled program). Removes: branch-table decode
(up to 2+20 bytes per `Case`), a managed-heap `Array` allocation per `Case`
(directly ~4.4% on fib from `Array<Option<Native>>::push` alone), and
downgrades `handle_op`'s work for these sites to the much cheaper `Seq` path.
**Risk: low.** No new opcode; `Op::Seq` already exists, is tested, and its
semantics (force scrutinee, evaluate one body) are exactly what a uniform,
exhaustive `Case` needs. The one correctness subtlety — the branch set must
be genuinely exhaustive over the full tag range before the swap is safe,
otherwise a would-be `NoBranchForDataTag` error silently stops being raised
— is checkable statically at encode time (the STG builder already emits
dense, full-range branch tables for these sites; W1 must verify exhaustiveness,
not just literal-body-equality, before swapping). Fib is arithmetic-dense
(4 binops/call, each paying the 2×`Case` tax) and is today's worst
regression-set case (1.96–2.14×) — this lever's upside is concentrated
exactly where the bar is hardest to hit.

**Also in scope for (c):** `materialize_bif_args`
(`Vec<Ref>` per Bif call, `machine.rs:433`) and `read_let`
(`Vec<FormHeader>` per Let, `machine.rs:1377`) — both still heap-allocate a
fresh `Vec` on every reduction (the 2026-07-03 report's Recommendation #1).
Evidenced at ~2–5% combined here (smaller than 07-03's headline figure, see
§1.3 on drift) — worth doing as a second (c)-lever pass (switch to a
reusable scratch buffer or `SmallVec`, mirroring `read_arg_offsets`'s
existing `SmallVec<[CodeRef; 8]>`), but secondary to the `Case`→`Seq` win.

### (a) Superinstruction fusion — RECOMMENDED SECOND

Once uniform-branch `Case`s are downgraded to `Seq` (above), the remaining
`Case` traffic is genuine data-constructor pattern matching (list
cons/nil, block lookup, user ADT-style matches) where branches *do* differ —
real fusion candidates here are "force scrutinee then immediately branch on
a *small*, *already-known-shape* tag set" (e.g. list cons/nil in `foldl`/
`map`-style recursion) and "alloc-thunk then immediate first-use update"
(the `enter_local` black-holing dance, 2.6–10.1% across the four cases,
already a single function but itself pushing an `Update` continuation
separately from the initial dispatch). Evidence is present (`enter_local`'s
share grows with call-density: 2.64% on fib, 10.09% on day09-p1) but this
spike did not isolate a single dominant fusible *non-uniform* sequence with
the same clarity as §3's finding — recommend W1 write a short design note
identifying 2–3 concrete fusion targets from the *post-(c)* profile (re-profile
after (c) lands, since (c) will materially change the shape of what remains
hot) rather than fusing blind now. Risk: medium (new opcodes, byte-identical
harness invariant, blob/source-prelude parity all apply squarely here per the
plan's W1 acceptance criteria).

### (b) Denser operand encoding — RECOMMENDED LAST

Evidence: the "decode" sub-bucket alone (`read_ref`+`read_arg_offsets`+
`read_form_header`+`read_let`+`step`, excluding `handle_op`/`dispatch`/
`enter_local`) is 10.9–16.6% — real, but roughly a third of the "core loop"
total and it does not touch the two largest contributors within that bucket
(`handle_op`'s dispatch-switch cost itself, and the branch-table `Array`
allocation §3 identifies). Denser encoding (e.g. variable-length integers
for small offsets, or packing `min_tag`/`len` more tightly) would shave
bytes read per `read_*` call but leaves the `Case`-vs-`Seq` structural
redundancy and the fusion opportunities in (a) untouched. Recommend treating
this as a fallback/stacking lever only if (c)+(a) together fall short of the
1.25×/1.3× bar — the evidence here does not support it as a first move.

### Summary against the bar

The dominant, best-evidenced lever is (c)'s `Case`→`Seq` downgrade: it is
the cheapest to implement (no new opcode), lowest-risk (encoder-only, HeapSyn
untouched, semantics-preserving given an exhaustiveness check), and targets
the single largest cost bucket (46–65%) via a pattern confirmed present, with
identical signature, in **every one of the four profiled programs** —
including fib, today's worst case. Whether it alone reaches 1.25× cannot be
claimed without implementing and measuring (per the two-phase workflow, this
report recommends, it does not implement) — the plan's own risk mitigation
(§9: "if the top lever falls short, stack a second lever … before loosening
the bar") is the right posture: implement (c) first, re-profile, then decide
whether (a) is needed to close the remaining gap.

---

## 5. Interaction risk with W2 and W3

**W2 (eu-adnu, `ExecutionError` boxing):** independent, as the plan states.
This spike's `ExecutionError`/`BcValue` `drop_glue` figure (3.5–5.4%
combined across the four cases) is consistent with — slightly above — the
delivery spec's 2.4–3.5% estimate, confirming the target cost is real and
worth banking regardless of W1's outcome. **No overlap** with the `Case`→
`Seq` fix's target bytes (`Op::Case`/`Op::Seq` handling vs the `Result`
return type). One sequencing note: if W1's `Case`→`Seq` downgrade lands
first, any new/changed code paths in `handle_op`'s `Op::Seq` arm will
already return the same `Result<_, ExecutionError>` type W2 boxes — no
extra work either order, but landing W2 first (as the plan already has it,
parallel in Phase 1) means W1's design/measurement from day one reflects the
smaller `Result`, which is marginally cleaner for attributing W1's own gain
in its final benchmark pass.

**W3 (eu-4zhi, block index, sequenced after W1):** the `Case`→`Seq`
downgrade in §3 is scoped to **arithmetic/comparison-style uniform-branch
`Case`s** (`Native`-tagged branches: Num/Str/Sym/Zdt/other) — it is a
*different* `Case` family from the block-lookup path (`LookupOr`/
`SafeLookup`, which cases on `Block`/data-constructor tags via
`bc_lookup_in_block`, an O(n) linear scan gated off on bytecode per the
07-03 report §5). **No direct code overlap** — W3 does not touch
`encode_node`'s `StgSyn::Case` arm, and W1 (as scoped in §4) should not touch
`bc_lookup_in_block`. The one thing worth flagging for whoever writes W1's
design note: if W1's *second* step (lever (a), superinstruction fusion) is
later broadened to *any* `Case` fusion beyond the uniform-branch case,
its author must re-check it does not change the `Case` encoding shape that
W3's (not-yet-designed) index-rebuild-on-evacuation logic will be written
against — W3 is sequenced after W1 specifically so this risk is inspectable
rather than concurrent, and this spike found no reason to depart from that
sequencing.

---

## 6. What was and wasn't done

- No VM, compiler, or source changes. `git status`/`git diff` on this branch
  against `origin/master` show only this report file added.
- No new dump phases, `eprintln!`, or instrumentation added — all profiling
  used `sample` (already on `PATH`) attached to the unmodified release
  binary, and `eu dump stg` (an existing, documented diagnostic command).
- `samply` was available and considered, but `sample` was used instead to
  match the 2026-07-03 report's methodology for direct comparability, and
  because it needs no separate viewer step for a text-only report — future
  spikes may prefer `samply` for interactive flamegraph review.
- Every number in §1–§3 is transcribed from real command output produced
  during this session; none are estimated or extrapolated without being
  labelled as such (§3.3 is explicit about not claiming an exact
  instruction-level percentage where the evidence does not support one).

## 7. Reproduction

```sh
git worktree add /tmp/eu-repro -b repro/w0-spike origin/master
cd /tmp/eu-repro
cargo clean && cargo build --release
cargo run --release -p xtask -- prelude-compile && cargo build --release

BIN=./target/release/eu
# baseline (§1): 7-run medians, bc vs HeapSyn, --heap-limit-mib 12288 under timeout 600
timeout 600 $BIN --heap-limit-mib 12288 tests/harness/bench/001_naive_fib.eu -t bench-naive-fib
EU_HEAPSYN=1 timeout 600 $BIN --heap-limit-mib 12288 tests/harness/bench/001_naive_fib.eu -t bench-naive-fib
# (repeat for 005_drop_cons -t bench-drop-cons, 007_short_lived -t bench-short-lived;
#  cd examples/aoc25 && ../../target/release/eu day03.eu -t part-2 / day09.eu -t part-1)

# profiling (§2): attach `sample` to the *eu* child, not the timeout wrapper
timeout 600 $BIN --heap-limit-mib 12288 tests/harness/bench/001_naive_fib.eu -t bench-naive-fib &
TPID=$!; sleep 0.3; EUPID=$(pgrep -P $TPID | head -1)
sample $EUPID 8 -file fib.sample.txt
wait $TPID

# STG evidence (§3): the 25-leaf uniform Case pattern
$BIN dump stg tests/harness/bench/001_naive_fib.eu | grep -c "LTE("   # → 25
```
