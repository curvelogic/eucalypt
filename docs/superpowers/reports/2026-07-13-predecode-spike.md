# Pre-decode IR falsification spike — the 0.13 Tier-1 GATE experiment

- **Bead:** eu-2sa6.9 (0.13 Tier-1 GATE, the pre-decode spike)
- **Author:** Furnace
- **Date:** 2026-07-13
- **Branch:** `spike/predecode-ir` (base `master` @ `7a82ad2f`, PR #986 merged;
  spike commit `37045ba7`)
- **Toolchain:** rustc 1.97.0 (2d8144b78), clean release build; embedded prelude
  blob regenerated BEFORE building (`cargo run -p xtask --release --
  prelude-compile` → 265438 bytecode bytes / 2634 constants / 548 global forms,
  blob 457443 bytes) so global-form fusion is present (the fair-blob discipline
  the plan mandates; a blob-less build would contaminate the ratios per
  eu-2sa6.5).
- **Host:** Apple M-series, macOS 26.5.1 (Darwin 25.5.0), aarch64. Shared dev
  machine; load noted with every measurement (interleaving is the noise defence).
- **Status:** THROWAWAY SPIKE — the DATA is the deliverable, not mergeable code.
  Draft PR, do not merge.

> The falsification target (review B §2a / exec-summary §6): is the residual
> ~1.6–1.8×/tick bytecode↔HeapSyn gap on op-dense / alloc-bound cases caused by
> the **byte-stream re-decode + dispatch envelope** (the 14.5% decode bucket,
> `Op::from_u8` match, bounds-checked reads, per-op operand reconstruction), or
> is it inherent to code being off the GC heap? If the former, decode-once into a
> flat typed instruction representation should move drop_cons decisively toward
> HeapSyn.

---

## 1. Verdict — **CONFIRMED**

Pre-decoding the operand stream into a typed, off-GC-heap instruction record
**removes the entire measured decode bucket (14.3% → 0.0% of main-thread
self-time) and cuts per-tick mutator cost by a consistent ~18–22% on every one
of the four cases**, with *identical tick and allocation counts* (it is a
representation change, not a semantic one). This:

- flips **naive_fib below HeapSyn** (per-tick 1.47× → 1.20×; wall-mutator
  1.13× → 0.92×),
- brings **short_lived** (per-tick 1.35× → 1.09×) and **aoc25 day03-p2**
  (per-tick 1.47× → 1.14×) to ≈ parity, and
- moves **drop_cons**, the extreme LET-per-cons + CASE probe, **decisively** —
  per-tick 1.60× → 1.25× (wall-mutator 1.55× → 1.22×; ~60% of the excess-over-
  HeapSyn closed) — though it does **not** reach ≤ 1.1× on operand-pre-decode
  alone.

"The gap barely moves" (the refutation criterion) is clearly false: the decode
bucket vanishes and per-tick cost drops ~21% on the primary probe. The
irreducibility hypothesis is **falsified**. Lever (a) — decode-once-at-load into
a flat typed IR — is validated as the decisive *per-tick* lever and should be the
0.13 centrepiece.

**The one honest nuance:** this spike pre-decoded only the *operands* (refs,
arg-offsets, form headers, branch-table entries) into the record; it still
**rebuilds the per-Case branch-table GC `Array` on every Case evaluation**
(continuation type unchanged). drop_cons's residual 1.25×/tick localises in the
profile to exactly that rebuild (`Array<Option<u64>>::push`, still 2.8% under
predecode) plus the shared dispatch + managed-alloc envelope. A **production**
lever (a) must also materialise branch tables and form-header arrays *into the
record* (as review B §2a specifies: "`FormHeader`s … branch tables (pre-built
`Array`s) … materialised into the record") to close drop_cons the rest of the
way. Operand-pre-decode alone gets 3/4 cases to ≤ 1.1× and drop_cons ~60% there.

---

## 2. Tick-identity proof (the representation-change invariant)

Flag OFF vs ON, same binary, `-S` (deterministic ticks) and rendered output:

| Case | ticks (bc, flag OFF) | ticks (bc, flag ON) | allocs OFF/ON | output OFF == ON |
|------|---------------------:|--------------------:|--------------:|:----------------:|
| 005_drop_cons | 2,980,164 | 2,980,164 | 400,076 / 400,076 | ✅ (`a`) |
| 001_naive_fib | 88,853,753 | 88,853,753 | — / — | ✅ (`1346269`) |
| 007_short_lived | 5,346,038 | 5,346,038 | — / — | ✅ (`990000`) |
| aoc25 day03-p2 | 50,389,079 | 50,389,079 | — / — | ✅ (`172162399742349`) |

Ticks and allocations are **bit-identical** flag-on vs flag-off on every measured
case, and rendered results match — exactly as a pure representation change
requires. (Correctness gates §5 extend this to the full harness.)

---

## 3. Three-mode measurement (same binary, interleaved triples)

Protocol: for each case, each round runs `[bc-predecode, bc-normal, hs]` as an
interleaved triple; 11 rounds; `timeout 600 --heap-limit-mib 12288`; foreground.
The headline metric is the **`-S` VM Mutator** figure (eval-only; excludes
parse/compile/render) — the precise, drift-robust metric the 07-12 spike used for
the sub-200ms cases (wall-clock at `/usr/bin/time -p` 0.01s granularity is
startup-dominated and useless below ~200ms; it is reported as a cross-check for
the long cases in §3.2). Medians of 11; load 2.6→3.8 over the run.

### 3.1 VM-Mutator medians (eval-only, seconds)

| Case | bc-predecode | bc-normal | hs | **pd/hs** | bc/hs | **pd/bc** |
|------|-------------:|----------:|---:|----------:|------:|----------:|
| drop_cons   | 0.063551 | 0.080924 | 0.052097 | **1.220** | 1.553 | **0.785** |
| naive_fib   | 1.693074 | 2.078210 | 1.846638 | **0.917** | 1.125 | **0.815** |
| short_lived | 0.105238 | 0.129748 | 0.100100 | **1.051** | 1.296 | **0.811** |
| day03-p2    | 1.002020 | 1.292105 | 0.908183 | **1.103** | 1.423 | **0.775** |

min/max spread (s): drop_cons pd 0.0574/0.0704, bc 0.0730/0.0884, hs
0.0463/0.0566; fib pd 1.6515/1.7553, bc 2.0267/2.1516, hs 1.8103/1.9265;
short_lived pd 0.0934/0.1158, bc 0.1157/0.1425, hs 0.0909/0.1111; day03 pd
0.9706/1.0911, bc 1.2413/1.3612, hs 0.8589/0.9798.

The **`pd/bc` column is the drift-immune finding** (same binary, same session,
same load): pre-decoding removes **18.5–22.5%** of mutator time on every case.

### 3.2 Per-tick cost (the invariant; ns/tick = mutator ÷ ticks)

| Case | ticks bc / hs | ns/tick bc-normal | ns/tick bc-predecode | ns/tick hs | per-tick **bc/hs** | per-tick **pd/hs** | per-tick reduction |
|------|--------------:|------------------:|---------------------:|-----------:|-------------------:|-------------------:|-------------------:|
| drop_cons   | 2,980,164 / 3,060,166 | 27.2 | 21.3 | 17.0 | 1.60 | **1.25** | −21.5% |
| naive_fib   | 88,853,753 / 115,779,125 | 23.4 | 19.1 | 15.9 | 1.47 | **1.20** | −18.6% |
| short_lived | 5,346,038 / 5,547,645 | 24.3 | 19.7 | 18.0 | 1.35 | **1.09** | −18.9% |
| day03-p2    | 50,389,079 / 52,119,226 | 25.6 | 19.9 | 17.4 | 1.47 | **1.14** | −22.5% |

Per-tick is the honest cross-engine comparison (fib wins *wall* despite a higher
per-tick cost because fusion already cut its tick count 23% below HeapSyn). The
~18–22% per-tick reduction is remarkably uniform — the signature of removing a
*fixed per-op* envelope, exactly what a byte-stream re-decode is.

### 3.3 Wall-clock cross-check (long cases, `/usr/bin/time -p` real, median of 11)

For the cases where the mutator dominates and 0.01s resolution is adequate,
wall-clock corroborates: naive_fib pd/hs **0.906**, bc/hs 1.104; short_lived
pd/hs **0.917**, bc/hs 1.167. (drop_cons/day03 wall is startup-dominated /
was mis-pathed in the first pass; the §3.1 mutator figures supersede.)

### 3.4 Anchoring to the 07-12 baseline

The 07-12 allocation-gap spike measured drop_cons bc/hs = **1.765×** and per-tick
bc 30.6 / hs 17.0 ns (1.80×/tick). This session's bc/hs is **1.55×** (per-tick
1.60×) — lower, consistent with the documented 10–35% machine/load drift (07-09
§1.2–1.3; this run was low-load, 2.6–3.8). The drift-immune `pd/bc` ratio and the
profile bucket shares (§4) are what the verdict rests on, not the absolute bc/hs.

---

## 4. Profile — the decode bucket vanishes

`/usr/bin/sample` @ 1 kHz, main-thread self-time, on `drop_cons` scaled to
`take(1000000)` (~4s steady-state window), matching the 07-12 methodology.

### 4.1 Leaf self-time of the decode symbols (samples ≈ ms)

| symbol (self-time) | bc-normal | bc-predecode |
|--------------------|----------:|-------------:|
| `read_ref` | 175 | **0** |
| `read_arg_offsets` | 134 | **0** |
| `read_form_header` | 29 | **0** |
| `arg_ref` collect (`try_process`/`from_iter`) | 45 | **0** |
| `resolve_ref` | 8 | 23 |
| **decode bucket total** | **383 (14.3%)** | **0 (0.0%)** |
| `handle_op` / `handle_op_predecoded` (self) | 857 (32.0%) | 636 (31.1%) |
| **main-thread eu self-time total** | **2675** | **2042 (−23.7%)** |

The measured decode bucket (**14.3%**) matches review B's projected **14.5%** to
within noise, and pre-decoding removes it **entirely** — `read_ref`,
`read_arg_offsets`, `read_form_header` and the per-op `arg_ref` collect drop to
**zero** samples. `handle_op`'s own self-time also falls 857 → 636 (−25.8%
absolute) as `Op::from_u8`, the per-read bounds checks and operand
reconstruction disappear (it stays ~31% of the pie only because the whole pie
shrank). Total main-thread CPU: **−23.7%**, consistent with the −18–22% mutator
wall reduction in §3.1.

### 4.2 Where drop_cons's residual 1.25×/tick goes (bc-predecode profile)

After the decode bucket is gone, the remaining bc-predecode self-time is:
dispatch (`handle_op_predecoded` 636 + `dispatch` 265), managed alloc
(`try_allocate` 143 + `find_space` 51 + bump), env-frame + branch-table `Array`
builds (`Array<BcValue>::push` 68 + `Array<Option<u64>>::push` **57** — the
per-Case branch-table **rebuild** this spike kept), `enter_local` 88, and the
return/saturate envelope (`return_data` 54, `return_fun` 50,
`env_from_data_args` 45, `make_arg_array` 40, `saturate_with_array` 51). The
`Array<Option<u64>>::push` 57 (2.8%) is the clearest **bytecode-only** residual a
production lever removes by materialising the branch table into the record; the
rest is the dispatch + alloc envelope shared with (and, for env-build, *cheaper*
than) HeapSyn.

---

## 5. Correctness gates (all green)

- **Flag OFF, `cargo test --release`:** green — 1272 lib + 480 harness + all
  integration targets, 0 failed. (Only the `rustdoc` doctest driver errors, a
  pre-existing rustup-shim quirk on this host, unrelated to the change.)
- **Flag ON, `EU_PREDECODE=1 cargo test --release --test harness_test`:** green
  — 480 passed, 0 failed.
- **Tick-identity flag-on vs flag-off:** bit-identical on all four measured cases
  (§2), and outputs match.
- **`EU_GC_VERIFY=2` + `EU_GC_STRESS=1`, flag ON:** clean on drop_cons and
  short_lived (exit 0, correct results) — the pre-decode cache lives on the
  system heap, holds no GC pointers, and is never scanned; nothing new enters the
  collector's scan set.
- **clippy `--all-targets -D warnings`:** clean. **`cargo fmt --all`:** applied.

---

## 6. What the spike did (mechanism)

Behind `EU_PREDECODE=1`:

- A `Vec<Option<Instr>>` sized to `code.len()`, indexed by **byte offset**, is
  added to `BytecodeMachine`. `Instr` is a typed enum, one variant per opcode,
  holding the decoded operands (scalars, `DecodedRef`s, inline `SmallVec`s of
  refs / offsets / `FormHeader`s / branch entries) — **no GC pointer lives in
  it**, so it stays off the collector's scan set.
- On first execution of an offset, `handle_op_predecoded` decodes the opcode once
  (`decode_instr`, mirroring `handle_op`'s byte-reads exactly) and memoises it;
  every subsequent execution dispatches over the typed record — HeapSyn's
  `match code` shape (`vm.rs:427`) but with the code off the GC heap. Because the
  code buffer is immutable, lazy caching is sound; after warmup the steady-state
  loop is pure typed dispatch.
- **`CodeRef` offsets are unchanged** (still byte offsets). Every child offset a
  node holds (branch target, body, scrutinee, atom) maps 1:1 into the same cache
  when the closure carrying it is later entered — so the value model, env frames,
  and continuations are byte-for-byte identical; only the decode is replaced.
- Wired into the main dispatch loop (`run` / `drive_whnf` via `dispatch`) **and**
  the re-entrant intrinsic-force loop (`drive_to_whnf` via `BcBifContext`), so
  both the top-level computation and intrinsic-internal forcing run pre-decoded.
- Flag OFF ⇒ the byte path is unchanged and taken verbatim (byte-identical).

Files: `src/eval/bytecode/machine.rs` (the `Instr` enum + `decode_instr` +
`handle_op_predecoded` + `step_predecoded`, and the `dispatch` / `drive_to_whnf`
branch + machine/`BcBifContext` fields), `src/eval/bytecode/mod.rs`
(`predecode_enabled()`). Measurement scripts: `measure_mutator.sh` (the precise
`-S` VM-Mutator harness), `measure_predecode.sh` (wall cross-check).

---

## 7. What a production lever (a) should look like

The spike confirms the mechanism; a production implementation should go two steps
further than this throwaway did, because the profile shows exactly where the
residual is:

1. **Decode once at load, eagerly, by reachability walk** (not lazily): from the
   root/global/template entry offsets, decode-and-follow into the flat IR so the
   steady-state loop never branches on a cold slot, and the byte buffer can be
   dropped from the hot path entirely (kept only as the BV5 blob / serialisation
   wire form, decoded once per process — exactly review B §3's end-state).
2. **Materialise the branch tables and form-header arrays *into the record*,**
   not just the operand refs. This spike kept the per-Case GC `Array` rebuild
   (the `Array<Option<u64>>::push` 2.8% residual) and the per-Let form-header
   loop; a production lever should hold the branch table as a pre-built,
   shareable structure referenced by the `Branch` continuation (a refcount-style
   clone, as HeapSyn does at `vm.rs:470`) so no per-Case rebuild occurs. This is
   what closes drop_cons's remaining 1.25×/tick — operand-pre-decode alone got it
   60% of the way; the branch-table materialisation is the rest.
3. **Fold BV2 in for free** (review B §3): in a typed record the `Smid` is just a
   field (or a side table keyed by instruction index), so `Op::Ann` /
   `Op::DirectApp` annotation handling collapses into the record — same refactor.
4. Keep `handle_op_predecoded`'s `match Instr` dense and `#[repr]`-friendly so
   LLVM lowers it to a jump table; the spike's version already removes
   `Op::from_u8`, which is the bulk of lever (e).

Order relative to the 0.13 roadmap: this is the real content of "Phase-4 collapse
at parity" (eu-oufc). BV3 register frames are orthogonal and should land *after*
pre-decode (on the cheap dispatch loop, not the expensive one); fused primops
(shipped) are complementary tick-count reducers. The evidence here supports
review B's reframing of the flattened-node interpreter from "rejected fallback"
to **destination**: the byte stream is the right serialisation format and the
wrong execution format.

---

## Appendix — reproduction

```sh
git worktree add /tmp/eu-predecode -b spike/predecode-ir origin/master
cd /tmp/eu-predecode
TC=~/.rustup/toolchains/stable-aarch64-apple-darwin/bin
RUSTC=$TC/rustc $TC/cargo run -p xtask --release -- prelude-compile   # fair blob FIRST
RUSTC=$TC/rustc $TC/cargo build --release

# tick-identity + correctness
EU=./target/release/eu
$EU -S tests/harness/bench/005_drop_cons.eu -t bench-drop-cons | grep Ticks
EU_PREDECODE=1 $EU -S tests/harness/bench/005_drop_cons.eu -t bench-drop-cons | grep Ticks   # identical
EU_PREDECODE=1 EU_GC_VERIFY=2 EU_GC_STRESS=1 $EU tests/harness/bench/005_drop_cons.eu -t bench-drop-cons

# interleaved three-mode VM-Mutator medians (the headline table)
ROUNDS=11 bash measure_mutator.sh

# profile the decode bucket (read_* symbols) shrinking to zero
EU_PREDECODE=1 $EU --heap-limit-mib 12288 /tmp/big_drop1m.eu -t bench-drop-cons >/dev/null 2>&1 &
TPID=$!; sleep 0.4; sample $(pgrep -P $TPID | head -1) 4 1 -f /tmp/sample_bc_predecode.txt -mayDie
```
