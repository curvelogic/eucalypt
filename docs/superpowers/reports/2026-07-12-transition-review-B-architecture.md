# Review B — Engine Architecture & the Options Catalogue

**Dimension:** whether the remaining bytecode↔HeapSyn per-op gap on op-dense /
alloc-bound workloads is *architecturally irreducible while HeapSyn exists*, or
an artifact of the current byte-stream encoding/dispatch design.
**Method:** code reading (`src/eval/bytecode/`, `src/eval/machine/vm.rs`,
`env.rs`, `infotable.rs`) + the two Stopwatch profiling spikes (2026-07-09,
2026-07-12) + ROADMAP §6.4/§7. Read-only; no builds.

---

## Executive summary (verdict)

1. **The irreducibility hypothesis is FALSE.** The ~1.6–1.8× per-tick gap on
   op-dense/alloc-bound cases is not inherent to "code off the GC heap" (BV1's
   actual goal). It is a consequence of a *second, independent* design choice —
   representing code as a **re-decoded byte stream** — which the current design
   conflates with the first.
2. **Proof from the profiles themselves:** on the hot cases bytecode has *no GC
   frames at all* (day08 explicit), yet still loses 1.6–1.8×/tick to HeapSyn,
   which *does* pay GC on its code nodes. HeapSyn's materialised-walk (no
   re-decode) currently out-earns its GC-residency cost. A representation that
   is **both** pre-decoded **and** off the GC heap gets both wins and should beat
   HeapSyn everywhere.
3. The per-op tax has two visible components in code: **re-decode** (read_ref /
   read_arg_offsets / read_form_header + bounds checks + operand reconstruction,
   measured 14.5% of bc mutator, *zero* HeapSyn analogue) and a **heavier
   dispatch envelope** around `handle_op` (extra closure-deref, `Op::from_u8`
   match, per-read slice bounds checks; dispatch bucket +557 ms vs hs).
4. The coherent end-state that beats HeapSyn on all workloads: **decode-once-at-
   load into a flat, fixed-width, typed instruction array in a non-GC `Vec`**
   (the ROADMAP's own "flattened-node interpreter" fallback, §7 decision 1) +
   fused primops (shipped) + BV3 register frames. The compact byte stream is
   retained *as the serialisation format* (BV5 blob), decoded once per process.
5. BV3 (register frames) is a **both-engines** env-walk win, not a
   bytecode-vs-HeapSyn differentiator — the two engines share `EnvironmentFrame`
   verbatim (`closure.rs:30`, `env.rs:371 cell()`). BV2 (side tables) is minor.
   Neither closes the decode gap; only pre-decoding does.
6. Caveat (needs measurement, not code-visible): the *magnitude* of the win from
   pre-decoding is a reasoned projection. It should be validated by a spike
   before committing to Phase-4 collapse.

---

## 1. Per-op cost anatomy (bytecode vs HeapSyn)

Both engines share: the continuation stack model (`BcContinuation` mirrors
`Continuation` 1:1, `cont.rs:25`), the `EnvironmentFrame<C>` cactus stack
(`closure.rs:30` — `BcEnvFrame = EnvironmentFrame<BcValue>`), de Bruijn
resolution via `cell()` (`env.rs:371`, walks parent chain `idx - len` per hop),
and the deferred-BIF boundary. The static closure part is structurally identical
(`Closing<S> = InfoTagged<S> + env`, `env.rs:53`; `BcClosure = InfoTagged<CodeRef>
+ env`, `closure.rs:35`) **except** the code field: `RefPtr<HeapSyn>` (8-byte GC
pointer) vs `CodeRef` (`u32` offset, `mod.rs:7`). So the *only* per-op structural
difference is: HeapSyn dereferences a materialised typed node; bytecode
re-decodes a byte stream.

**The shared dispatch preamble.** HeapSyn `handle_instruction`
(`vm.rs:390`): `let code: &HeapSyn = unsafe { &*self.closure.code().as_ptr() }`
(`vm.rs:409`) — **one pointer deref**, then `match code` (`vm.rs:427`) where every
operand is an already-typed struct field. Bytecode `handle_op` (`machine.rs:1716`):
`as_closure()` match + `ok_or_else` (1722) → `pc = closure.code() as usize` (1730)
→ `read_u8` + `Op::from_u8` + `ok_or_else` (1731, a **byte→enum match that
HeapSyn does not perform**) → `match op` (1735). Every `read_*` is a
bounds-checked slice index (`program.rs:130,138,146`).

### LET (the alloc-bound probe — `drop_cons` is LET-per-cons)
- **Bytecode** (`machine.rs:1843`): `read_u16` count → loop `read_form_header`
  (`machine.rs:182`: 4 slice reads = kind u8 + arity u8 + smid u32 + body u32)
  straight into a managed `Array` (1851–1855) → alloc env frame (1857). Per
  binding: 10 bytes decoded + `bc_info` reconstruction (`machine.rs:1629`) +
  `Array::push`. **Note: PR #984 already removed the transient `Vec<FormHeader>`
  the 2026-07-12 report flagged** — `Op::Let` now reads directly into the array
  (mirrors `Op::Case`), and `Op::LetRec` (1862) uses an inline
  `FormHeaders = SmallVec<[FormHeader;8]>` (1869). The report's recommended
  lever (c) is landed.
- **HeapSyn** (`vm.rs:625`): `EnvBuilder::from_let` over an already-materialised
  `bindings: Array<LambdaForm>` — **no header decode; the arity/smid/body are
  typed fields read by pointer.**
- **Delta:** the header decode (10 bytes × count, per execution) + `bc_info`
  packing is pure bytecode overhead. Measured: `read_form_header` is inside the
  +553 ms decode bucket (14.5%), zero HeapSyn analogue (2026-07-12 §3.2).

### CASE
- **Bytecode** (`machine.rs:1768`): `read_u32` scrutinee + `read_u8` min_tag +
  `read_u8` len + **loop `read_u32` into a fresh managed `Array` of len entries**
  (1777–1781) + `read_u8` has_fb (+`read_u32`). Pushes `Branch` continuation.
- **HeapSyn** (`vm.rs:460`): `branch_table.clone()` of an **already-built
  `Array<Option<...>>`** (a refcount-style clone, `vm.rs:470`) — no per-Case
  table rebuild, no decode.
- **Delta:** bytecode rebuilds and re-populates the branch-table `Array` on
  **every** Case evaluation (`Array<Option<u64>>::push`, 83 ms in the 07-12
  profile; 4.43% on fib in 07-09). This is the single clearest "materialise once
  vs rebuild every time" divergence. (The 07-09 report's uniform-branch
  Case→Seq / FusedPrimop work, shipped in #982, removes the *arithmetic* Case
  traffic but not genuine data-constructor matches.)

### APP
- **Bytecode** (`machine.rs:1831`): `read_u8` flags → `read_ref` callable
  (tag u8 + u32, reconstruct `DecodedRef` via tag match, `machine.rs:85`) →
  `read_arg_offsets` (u8 count + n×u32 into a `SmallVec`, `machine.rs:167`) →
  `make_arg_array` (`machine.rs:1593`), which for each offset wraps a
  `BcClosure::new(off, env)` — **cheap: no atom-node allocation** because args
  were pre-encoded as `OP_ATOM` nodes at compile time (`encode.rs:147`,
  operand-model doc `encode.rs:8`). Push `ApplyTo` + `enter_callable`.
- **HeapSyn** (`vm.rs:481`): `create_arg_array` **allocates a `HeapSyn::Atom`
  node per lazy arg into the GC heap** (`vm.rs:489`).
- **Delta — bytecode WINS here:** its arg-array build is *cheaper* (352 ms vs
  HeapSyn's 690 ms env-build bucket, 07-12 §3.2), because the pre-encoded atom
  offsets replace HeapSyn's per-arg heap allocation. This is a genuine BV1 win
  and evidence the encoding is not all cost.

### BIF
- **Bytecode** (`machine.rs:1973`): `read_u8` intrinsic + `read_arg_offsets` +
  per-offset `arg_ref` re-decode (`machine.rs:1585`: skip OP_ATOM byte,
  `read_ref` again) → stash in `pending_bif`/`pending_bif_args`. The dispatch
  tail (`machine.rs:2296`) then `materialize_bif_args` → a `Vec<Ref>` **drawn
  from a recycle pool** (`bif_arg_pool`, `machine.rs:300,463`; post-#984 no
  per-dispatch malloc after warmup) and pushes a rooted `BifFrame`
  (`machine.rs:224`, needed because there is no live code node to re-read across
  a GC, unlike HeapSyn).
- **HeapSyn** (`vm.rs:618`, `vm.rs:1663`): re-derives args directly from the
  **rooted `HeapSyn::Bif` node's `args: Array<Ref>`** — the node is already a
  heap root the collector updates, so no separate frame/materialisation.
- **Delta:** bytecode needs an extra rooting/materialisation dance (`BifFrame`)
  precisely *because* code is off-heap — a structural cost of the off-heap-code
  choice, but small and not on the op-dense hot path.

### LOOKUP (LookupLit)
- Nearly identical shape (`machine.rs:2005` vs `vm.rs:669`): decode smid + key
  ref + obj ref + default offset; fast-path if obj is already a WHNF block
  (`decode_cons`, `machine.rs:820` — which itself re-decodes the cons node from
  bytes), else push `LookupLitForce`. `bc_lookup_in_block` (`machine.rs:894`) is
  an O(n) scan; the block-index (eu-4zhi/W3) is disjoint from the alloc gap
  (2026-07-12 §5: zero block-lookup samples in every alloc-bound case).

**Where the extra per-tick cost lives (grounded):** 2026-07-12 §3.2 bucket
diff — dispatch/resolve loop **+557 ms** (bc 47.4% vs hs 41.8%, `handle_op` alone
1070/3803 ≈ 28%), bc-re-decode **+553 ms** (14.5%, zero hs), system malloc **+261
ms** (now largely retired by #984). Offsetting bytecode wins: env-build **−338
ms**, managed alloc **−254 ms**, return dispatch **−125 ms**. Net **~1.8×/tick**
with *identical allocation counts* (07-12 §2: bc allocs ≤ hs on every case).

---

## 2. The irreducibility test — options catalogue

For each lever: mechanism · expected impact (grounded) · effort · risk · cheap
validating experiment.

### (a) Pre-decoded / threaded representation — **the decisive lever**
- **Mechanism:** decode the byte stream **once at load** into a flat
  `Vec<Instr>` of fixed-width, typed instruction records living in a non-GC
  `Vec` (like `BytecodeProgram.code` today, `program.rs:35`). `CodeRef` offsets
  become array indices; `FormHeader`s (already a 12-byte `Copy` struct,
  `machine.rs:174`), branch tables (pre-built `Array`s), and `DecodedRef`s
  (`machine.rs:74`) are materialised into the record. `handle_op` becomes
  `match instrs[ip]` over typed fields — structurally identical to HeapSyn's
  `match code` (`vm.rs:427`) **but with the code off the GC heap and never
  scanned/evacuated/forwarded.**
- **Impact:** removes the entire **14.5% decode bucket** (07-12 §3.2, the
  largest single fusion-immune contributor) and most of the dispatch-envelope
  excess (`Op::from_u8` match at `machine.rs:1731`, per-read bounds checks,
  operand reconstruction). Retains BV1's off-heap-code win (no `GcScannable for
  HeapSyn`) *and* its leaner arg build (§1 APP). Because HeapSyn beats bytecode
  today *despite* paying GC on code that bytecode doesn't (proof in exec-summary
  §2), a representation with HeapSyn's walk cost **and** bytecode's zero-GC-code
  should land below HeapSyn on op-dense cases and stay above it on GC-heavy ones
  (day08). This is the only lever that plausibly delivers "beats HeapSyn
  everywhere."
- **Effort: L.** New in-memory IR + one-time decoder; `handle_op` rewritten to
  index typed records; GC scanning of code disappears (simpler). Blob/BV5
  serialisation format unchanged (byte stream stays the wire form; decode-once
  at load).
- **Risk: M–H.** Larger resident code footprint (fixed-width records > packed
  bytes); must preserve byte-identical behaviour under `EU_GC_VERIFY=2`; the
  decode-once step adds startup cost (amortised, and BV5's whole point was
  startup — measure). It is the ROADMAP's *acknowledged fallback* (§7 decision
  1: "a flattened-node interpreter — lower risk, smaller win"), so not novel.
- **Cheap experiment:** hand-write a pre-decoded inner loop for the `drop_cons`
  hot opcodes (LET/CONS/CASE/APP/ATOM) behind an env flag and re-run the 07-12
  harness. If bc/hs on `drop_cons` moves from 1.77× toward ≤1.0×, the hypothesis
  is confirmed. This is a 1–2 day spike, not a commitment.

### (b) Denser / fixed-width operand encoding
- **Mechanism:** varint small offsets, pack min_tag/len, shrink the 5-byte
  `Ref` (`encode.rs:93`). **Impact:** small — the decode sub-bucket is real but
  "roughly a third of the core-loop total and does not touch `handle_op`'s
  dispatch cost or the branch-table `Array` allocation" (07-09 §4b). **Effort
  S–M, risk L.** **Verdict: dominated by (a)** — if you pre-decode, wire width
  is irrelevant to the hot loop; denser bytes only help the blob size. Do not
  pursue as a perf lever.

### (c) More / deeper superinstructions (FusedPrimop family)
- **Mechanism:** the shipped `Op::FusedPrimop` (`opcode.rs:32`, `encode.rs:134`,
  8 whitelisted primops `encode.rs:451`) already collapsed the 25-leaf
  Case-of-Case force trees (07-09 §3). Extend to force-then-case on small known
  tag sets (list cons/nil in fold/map) and alloc-thunk-then-update
  (`enter_local` black-hole dance, `machine.rs:1636`).
- **Impact:** already delivered fib's tick-count drop (07-12 §2: fib bc ticks
  21% below hs). Remaining candidates are non-uniform Case traffic; **additive,
  bounded** — 07-09 §4a recommends re-profiling *after* fusion before choosing
  targets. **Effort M (per superinstruction), risk M** (new opcodes,
  byte-identical + blob-parity invariants). Complementary to (a) but not a
  substitute — fusion reduces *tick count*, (a) reduces *per-tick cost*.

### (d) BV3 register frames
- **Mechanism:** flat register frames for compiler-flagged non-escaping hot
  captures, replacing cactus-env walk (`env.rs:371`). **Impact: large but
  SHARED** — both engines use the *same* `EnvironmentFrame` (`closure.rs:30`),
  so the O(n²) higher-order-fold pathology (ROADMAP §7.4: foldl 13.5M→52M→204M
  ticks from local `op` re-resolution) afflicts HeapSyn identically. BV3 helps
  both engines equally; it is **not a bytecode-vs-HeapSyn differentiator** and
  does not close the decode gap. **Effort L, risk H** (shares CG escape
  analysis; the flat-closure revert, ROADMAP §10, is the cautionary tale).
  Pursue for absolute throughput, not for parity.

### (e) Dispatch-loop mechanics in Rust
- **Mechanism:** `match op` → computed-goto/jump-table; `#[inline]` on `read_*`
  (already `#[inline(always)]`, `program.rs:129`); shrink `Result<_,
  ExecutionError>` (eu-adnu, done — boxing). **Impact:** the giant `match op`
  (`machine.rs:1735`) with all arms inlined is `handle_op` = 28% self-time; LLVM
  already lowers a dense `#[repr(u8)]` match to a jump table, so gains are
  modest without pre-decode. `get_unchecked` on `code[pc]` would elide bounds
  checks (offsets are encoder-verified) — real but unsafe, S effort. **Verdict:**
  marginal on its own; folds naturally into (a), where the typed-record match
  removes the `Op::from_u8` step entirely.

### (f) gc_poll / tick accounting overhead
- **Mechanism:** `run` decrements a countdown and polls every 500 ticks
  (`machine.rs:2221–2239`); `dispatch` calls `metrics.tick()` +
  `metrics.stack()` every step (`machine.rs:2285`). **Impact:** small and
  **symmetric** with HeapSyn (`vm.rs:2140` mirrors it). `metrics.stack(len)` per
  tick is a cheap max-update. **Verdict:** not a differentiator; low priority.

### (g) Instruction-pointer / operand caching
- **Mechanism:** cache decoded `FormHeader`s / refs keyed by code offset
  (2026-07-12 §5d suggestion). **Impact:** a half-measure toward (a) with worse
  cache behaviour (indirection through a side map). **Verdict: dominated by (a)**
  — if you're going to decode-once, materialise into the instruction stream, not
  a side cache.

**Catalogue verdict:** exactly one lever (a) attacks the *per-tick* gap at its
root; (c) is a complementary *tick-count* reducer already in flight; (d) is a
shared absolute-throughput win miscategorised as a parity lever; (b)(e)(f)(g)
are marginal or subsumed by (a).

---

## 3. Convergence analysis — what beats HeapSyn everywhere

**The end-state (coherent, and largely pre-figured in the ROADMAP):**

> A **decode-once-at-load, flat, fixed-width, typed instruction array** in a
> non-GC `Vec` (lever a) — the execution IR — fed by the **compact serialised
> byte stream** as the wire/cache/blob format (BV5, unchanged), with
> **fused primops** (shipped) collapsing hot operand-force sequences and
> **BV3 register frames** removing the shared env-walk for hot captures.

This design dominates HeapSyn on every axis simultaneously:
- **Op-dense / alloc-bound (where HeapSyn wins today):** pre-decoding removes the
  14.5% decode bucket + dispatch envelope → per-tick cost converges on HeapSyn's
  `match code`, while code stays off the GC heap (HeapSyn's residual GC-on-code
  cost, small on these cases but nonzero, is gone) and arg-build stays leaner
  (§1 APP). Net: at or below HeapSyn.
- **GC-heavy (where bytecode already wins):** unchanged — day08 stays 7 s vs
  HeapSyn's >500 s timeout (07-09 §1.3). Off-heap code + small managed heap.
- **Startup (where BV5 already wins):** the byte stream remains the embedded
  prelude format; decode-once is a bounded, amortisable load step.

**Stepwise path (each step independently shippable, byte-identical-gated):**
1. **Now (0.12.1, mostly landed):** fused primops (#982), `ExecutionError`
   boxing (eu-adnu), transient-`Vec` removal (#984). These bank the tick-count
   and system-malloc wins and shrink the gap to the *pure* decode+dispatch
   envelope — isolating exactly what (a) must attack.
2. **Spike (gate):** hand-rolled pre-decoded inner loop for `drop_cons`'s hot
   opcodes; measure bc/hs. **This is the missing experiment that would falsify or
   confirm "irreducible."** Decision point for 0.13.
3. **0.13 — pre-decode (lever a):** build the flat typed instruction IR + decoder;
   retire byte-stream re-decode from the drive loop; keep the blob format. This
   is the real content of "Phase-4 collapse at parity" (eu-oufc) — HeapSyn can
   only be retired once bytecode reaches parity on op-dense cases, which
   pre-decode is what actually delivers.
4. **0.13+ — BV3 register frames** for absolute throughput on higher-order
   recursion (both-engines win, but only bytecode remains).

**How the 0.13 roadmap items fit / conflict:**
- **BV2 (side tables for annotations, ROADMAP §6.4-4):** additive, low-risk;
  removes the `Ann` dispatch step and moves `Smid`s out of the stream. *Fits
  naturally into (a)* — in a pre-decoded IR, the smid is just a record field or a
  side table keyed by instruction index; BV2 and (a) are the same refactor done
  together. No conflict.
- **BV3 (register frames, §6.4-5):** orthogonal to (a); do *after* pre-decode so
  the frame-model change lands on the cheap dispatch loop, not the expensive one.
  Shares CG escape analysis. No conflict, clear ordering.
- **BV4 (superinstructions, §6.4-6):** already in flight (FusedPrimop);
  complementary. One watch-out (07-09 §5): broadening fusion beyond uniform Case
  must not disturb the encoding shape the block-index (eu-4zhi) will assume.
- **Tension to flag:** the ROADMAP frames the linearised byte stream as the
  *decided* execution IR (§7 decision 1), treating the flattened-node
  interpreter as a rejected "fallback." The evidence (HeapSyn beating bytecode
  despite paying GC-on-code) argues the fallback is in fact the **destination**,
  not a consolation — the byte stream is the right *serialisation* format but the
  wrong *execution* format. Owner should revisit that decision explicitly: keep
  the stream for the blob/cache/PP wire (§7 decision 5, all three consumers want
  compact bytes), decode once into the flat IR for execution.

---

## 4. What is code-visible vs needs measurement

- **Code-visible (high confidence):** the re-decode + dispatch-envelope structure
  and its absence in HeapSyn (§1); the shared env model (so BV3 is not a
  differentiator); that #984 already retired the transient-`Vec` lever the 07-12
  report recommended; the leaner bytecode arg-build.
- **Measured (Stopwatch, this machine/moment):** 14.5% decode, +557 ms dispatch,
  ~1.6–1.8×/tick, identical alloc counts, no-GC-frames on hot cases.
- **Needs measurement (the projection):** the *magnitude* of pre-decoding's win.
  The argument that it flips bytecode below HeapSyn is a reasoned inference from
  "HeapSyn wins despite GC-on-code," not a measured result. **The gating spike in
  §3 step 2 is the honest way to settle it before committing to Phase-4
  collapse.** Ratios are machine/load-sensitive (07-09 §1.2–1.3 documents
  10–35% drift) — take before/after on one machine under one load discipline.

**Bottom line:** "irreducible while HeapSyn exists" is wrong. The gap is the
byte-stream re-decode cost, which is decoupled from BV1's off-heap-code goal and
removable by decode-once pre-decoding — the ROADMAP's own flattened-node design,
reframed from fallback to destination. Ship the in-flight tick/alloc levers,
run the pre-decode spike, and a bytecode engine that exceeds HeapSyn on all
workload types is a coherent, reachable end-state.
