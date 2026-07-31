# PP — Process parallelism (par-map + safe reductions) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `par-map` plus a safe associative-reduction vocabulary
(`par-sum`/`par-max`/`par-min`/`par-concat`) that evaluate independent pure
computations in parallel via Unix COW-fork, each combinator semantically
identical (byte-for-byte) to its sequential form.

**Architecture:** Five STG intrinsics (`PARMAP`, `PARSUM`, `PARMAX`, `PARMIN`,
`PARCONCAT`). Each `execute()` runs entirely against the *neutral* intrinsic
ABI (`IntrinsicMachine`) so it works identically on the HeapSyn and bytecode
engines. On unix, above a size threshold and with W>1, it forks W workers who
each evaluate their contiguous chunk in the inherited COW heap and write
fully-forced results (or a worker-local partial) into an anonymous
`MAP_SHARED` mmap arena at index-addressed slots; the parent `waitpid`s and
reassembles deterministically in index order. Below threshold, on W≤1, or on
non-unix, it falls back to a transparent sequential map — identical result.

**Tech Stack:** Rust, `libc` (fork/waitpid/mmap/_exit — already a dependency),
the eucalypt STG machine, its neutral intrinsic ABI, `serde_json::Number`.

## Global Constraints

- **UK English** in all code, comments, docs (optimise, serialise, colour…).
- **No clippy warnings**: `cargo clippy --workspace --all-targets -- -D warnings`.
- **`cargo fmt --all`** before every commit.
- **Full test suite** (`cargo test`), not just `--lib`, must be green.
- **Both engines**: every eucalypt-level test runs under the default bytecode
  engine AND `EU_HEAPSYN=1` (spike risk R5 — engine equivalence).
- **GC soundness**: exercise under `EU_GC_VERIFY=2` (parent and workers).
- Wrap every `eu` invocation in `timeout 60 ./target/release/eu … --heap-limit-mib 12288`.
- **Semantic identity is the core guarantee**: every `par-*` must produce the
  byte-identical result of its sequential form, across W=1,2,4 and all N.
- **Purity + serialisable-data contract** (spec §3, §6): non-serialisable
  result or `xs` element → runtime error naming the value kind + combinator.
- **Impure-lazy-producer boundary check** (spike R2, spec §6 addendum): if any
  non-exhausted impure `LazyProducer` is live at `par-*` entry, raise the
  boundary error rather than forking.
- **Worker hygiene** (spike R3/R4, spec §4 addendum): child leaves only via
  `libc::_exit`, never unwinds past the fork frame, never writes stdout, resets
  SIGINT to default; parent SIGKILLs + reaps workers on interrupt.
- Fork/mmap/_exit code is `#[cfg(unix)]`; a `#[cfg(not(unix))]` path compiles
  and always runs sequentially.

## File structure

- Create `src/eval/stg/parallel/mod.rs` — module root; re-exports; the
  engine-neutral driver `par_evaluate()` shared by all five intrinsics;
  worker-count/threshold policy; the sequential fallback; the producer-boundary
  check. Non-unix: driver is sequential-only.
- Create `src/eval/stg/parallel/arena.rs` — `#[cfg(unix)]` anonymous
  `MAP_SHARED` mmap arena with per-worker segments, length-prefixed records,
  `munmap`-on-drop (adapted from the proven spike `Arena`).
- Create `src/eval/stg/parallel/fork.rs` — `#[cfg(unix)]` fork/waitpid worker
  spawn + join with the hygiene rules (`catch_unwind`+`_exit`, SIGINT reset,
  parent kill-on-interrupt).
- Create `src/eval/stg/parallel/serialise.rs` — the value serialiser:
  `serialise_value()` (deep-force walk of a WHNF `AbiClosure` → bytes, via the
  neutral ABI) and `deserialise_value()` (bytes → rebuilt `AbiClosure` via the
  neutral ABI). Reduction ops for the fused combinators.
- Create `src/eval/stg/parallel/intrinsic.rs` — the five `StgIntrinsic` impls.
- Modify `src/eval/stg/mod.rs` — `pub mod parallel;` and register the five
  intrinsics with `rt.add(...)`.
- Modify `src/eval/intrinsics.rs` — five catalogue entries (`PARMAP` etc.).
- Modify `src/eval/error.rs` — one new `ExecutionError` variant
  `NotSerialisable(Smid, Box<(String /*combinator*/, String /*kind*/)>)`.
- Modify `src/eval/machine/intrinsic.rs` — add neutral `apply1_thunk` to the
  `IntrinsicMachine` trait (HeapSyn default), so f-application is engine-neutral.
- Modify `src/eval/bytecode/machine.rs` — bytecode override of `apply1_thunk`
  using the existing `apply1_template`.
- Modify `src/eval/stg/stream.rs` — `ProducerTable` live-impure tracking +
  `any_live_impure_producer()` accessor.
- Modify `lib/prelude.eu` — surface `par-map`/`par-sum`/`par-max`/`par-min`/
  `par-concat` combinators over the `__PAR*` intrinsics.
- Create `tests/harness/2xx_pp_par_map.eu` and siblings — equivalence,
  fallback, error, and reduction harness tests.
- Modify `.github/workflows/*.yml` — a Linux CI job exercising PP tests on both
  engines (spike R7 — validate the mechanism off macOS).

---

## Task 1: Error variant for non-serialisable boundary values

**Files:**
- Modify: `src/eval/error.rs`

**Interfaces:**
- Produces: `ExecutionError::NotSerialisable(Smid, Box<(String, String)>)`
  where the tuple is `(combinator_name, value_kind)`. Its `Display` reads e.g.
  `"par-map: cannot serialise a function across the parallel boundary"`.

- [ ] **Step 1:** Add the variant to the `ExecutionError` enum near the other
  boundary errors (after `NotScalar`), with a doc comment. Wire its `Display`
  arm: `par-{combinator}: cannot serialise a {kind} across the parallel
  boundary`. Add its `Smid` to the `smid()`/diagnostic accessor if the enum has
  a match that must be exhaustive (follow the pattern of a neighbouring
  single-Smid variant like `NotScalar`).
- [ ] **Step 2:** `cargo build` — expect it to fail only if a match is
  non-exhaustive; fix by adding the arm. Expected: clean build.
- [ ] **Step 3:** Commit `feat(pp): NotSerialisable boundary error variant (eu-u9xj.6)`.

---

## Task 2: Neutral `apply1_thunk` on the intrinsic ABI

**Files:**
- Modify: `src/eval/machine/intrinsic.rs` (trait default, near `apply2_thunk`)
- Modify: `src/eval/bytecode/machine.rs` (override, near `apply2_thunk`)

**Interfaces:**
- Produces: `IntrinsicMachine::apply1_thunk(&self, view, f: AbiClosure, a:
  AbiClosure) -> Result<AbiClosure, ExecutionError>` — a lazy updatable `f(a)`
  thunk value handle. HeapSyn default builds `App(L0,[L1])` over a `[f,a]` env
  frame; bytecode override uses `self.program.apply1_template`.

- [ ] **Step 1:** Add the trait default mirroring `apply2_thunk` but 2-slot
  frame `[f, a]` and `view.app(Ref::L(0), [Ref::L(1)])`.
- [ ] **Step 2:** Add the bytecode override mirroring its `apply2_thunk` but a
  2-value frame over `self.program.apply1_template`.
- [ ] **Step 3:** `cargo build`. Expected: clean.
- [ ] **Step 4:** Commit `feat(pp): neutral apply1_thunk on intrinsic ABI (eu-u9xj.6)`.

---

## Task 3: Live-impure producer tracking

**Files:**
- Modify: `src/eval/stg/stream.rs`

**Interfaces:**
- Produces: `stream::any_live_impure_producer() -> bool` — true iff at least
  one registered `!is_pure()` producer has not yet been observed exhausted.

- [ ] **Step 1:** Add `live_impure: std::collections::HashSet<u32>` to
  `ProducerTable`. In `register`, if `!producer.is_pure()`, insert the id. In
  `producer_next` and `producer_drain`, when a producer yields `None`
  (exhausted), remove its id from `live_impure`. Add
  `fn has_live_impure(&self) -> bool { !self.live_impure.is_empty() }`.
- [ ] **Step 2:** Add module fn `pub fn any_live_impure_producer() -> bool`
  reading the thread-local table.
- [ ] **Step 3:** Add a `#[test]` in `stream.rs`: register a dummy impure
  producer that yields one value then `None`; assert `any_live_impure_producer()`
  is true after register, false after the value is drained. Register a pure
  producer; assert it never flips the flag.
- [ ] **Step 4:** `cargo test -p eucalypt stream` (or the crate name) — expect
  PASS.
- [ ] **Step 5:** Commit `feat(pp): track live impure producers for par boundary (eu-u9xj.6)`.

---

## Task 4: The mmap arena (`#[cfg(unix)]`)

**Files:**
- Create: `src/eval/stg/parallel/arena.rs`
- Modify: `src/eval/stg/mod.rs` (add `pub mod parallel;`)
- Create: `src/eval/stg/parallel/mod.rs` (stub `pub mod arena;` for now)

**Interfaces:**
- Produces: `Arena` with:
  - `Arena::new(size: usize) -> Arena` — `mmap(MAP_SHARED|MAP_ANON)` (adapted
    from the spike; `munmap` on `Drop`).
  - `fn segment_base(&self, w: usize) -> usize` — byte offset of worker w's
    segment (arena split into W equal segments).
  - A `SegmentWriter { base, cursor, end }` returned by
    `arena.writer(w, n_workers)` with `fn push(&mut self, bytes: &[u8]) ->
    Result<(), ArenaOverflow>` (writes a `u64` LE length prefix then the bytes;
    errors on segment overflow) and `fn count(&self) -> u64`.
  - `fn reader(&self, w, n_workers) -> SegmentReader` with `fn next(&mut self)
    -> Option<&[u8]>` reading length-prefixed records until the segment's
    written count is exhausted. Record count per segment is stored by the writer
    in the first 8 bytes of the segment (header), so the reader knows how many
    records to expect.
- Reduction combinators use a single record per worker (the partial);
  `par-map` uses one record per element.

- [ ] **Step 1:** Port the spike `Arena` (mmap/munmap, `write_u64`,
  `read_u64`, `write_bytes`, `read_bytes`) with UK spelling and doc comments.
- [ ] **Step 2:** Add per-worker segment layout: `segment_size = size /
  n_workers`, `segment_base(w) = w * segment_size`. First 8 bytes of each
  segment = record count (written last by the writer). Records follow from
  offset +8, each `[u64 len][len bytes]`.
- [ ] **Step 3:** Implement `SegmentWriter::push` (bounds-check against
  `segment_base(w+1)`; on overflow return `Err(ArenaOverflow)`), `finish()`
  (writes the record count into the segment header), and `SegmentReader::next`.
- [ ] **Step 4:** Unit test (`#[cfg(all(test, unix))]`): a single-process
  round trip — create an arena, write 3 variable-length records into segment 0,
  `finish`, read them back identically; assert overflow errors when a record
  exceeds the segment.
- [ ] **Step 5:** `cargo test -p eucalypt arena` — PASS.
- [ ] **Step 6:** Commit `feat(pp): anonymous MAP_SHARED per-worker arena (eu-u9xj.6)`.

---

## Task 5: Value serialiser / deserialiser

**Files:**
- Create: `src/eval/stg/parallel/serialise.rs`
- Modify: `src/eval/stg/parallel/mod.rs` (`pub mod serialise;`)

**Interfaces:**
- Produces:
  - `pub fn serialise_value(machine: &mut dyn IntrinsicMachine, view:
    MutatorHeapView, whnf: &AbiClosure, combinator: &str, out: &mut Vec<u8>) ->
    Result<(), ExecutionError>` — deep-forces and byte-encodes the
    serialisable-data subset; `Err(NotSerialisable(...))` on a
    function/IO/closure. `whnf` is assumed already forced to WHNF.
  - `pub fn deserialise_value(machine: &mut dyn IntrinsicMachine, view:
    MutatorHeapView, cur: &mut &[u8]) -> Result<AbiClosure, ExecutionError>` —
    rebuilds a WHNF value handle via the neutral ABI.
- **Wire format** (self-describing, little-endian):
  - `0x00` null (Unit) · `0x01` true · `0x02` false
  - `0x03` number: `1 byte kind (0=u64,1=i64,2=f64)` + `8 bytes LE`
  - `0x04` symbol: `u32 len` + utf8 · `0x05` string: `u32 len` + utf8
  - `0x06` zdt: encode via its millisecond/`Zdt` representation (`8+ bytes`)
  - `0x07` list: `u32 count` + `count` encoded values
  - `0x08` block: `u32 count` + `count` × (`u32 keysym len`+utf8, encoded value)

- [ ] **Step 1: Write the failing round-trip test.** In `serialise.rs`
  `#[cfg(test)]`, build a runtime with the list/block/box intrinsics, construct
  values on a real machine (number, string, symbol, bool, null, nested list,
  block of scalars), `serialise_value` then `deserialise_value`, render both to
  a canonical string, assert equal. (Model the machine setup on the existing
  `force.rs` unit tests + `testing::machine`.)
- [ ] **Step 2:** Run it — expect FAIL (functions undefined).
- [ ] **Step 3: Implement `serialise_value`.** Match `machine.data_tag(view,
  whnf)`:
  - `None` → `machine.value_native` → encode scalar (Num/Str/Sym/Zdt); if
    `value_native` is `None` it is a function/opaque → `Err(NotSerialisable)`.
  - `Unit`→0, `BoolTrue`→1, `BoolFalse`→2.
  - `BoxedNumber|BoxedString|BoxedSymbol|BoxedZdt` → `value_native` → scalar.
  - `ListNil` → `0x07` + count 0. `ListCons` → walk: repeatedly `data_field 0`
    (head), `machine.force` it, recurse; `data_field 1` (tail),
    `machine.force`, continue until `ListNil`. Buffer element encodings, then
    emit `0x07` + count + elements.
  - `Block` → `data_field 0` = kv list; force; walk it as ListCons of
    `BlockPair`; each pair `data_field 0` = key (force, `value_native` → Sym or
    Str), `data_field 1` = value (force, recurse). Emit `0x08` + count + pairs.
  - `BlockPair`/`BlockKvList` encountered directly → treat as a block of one /
    the kv list (defensive). `IoReturn|IoBind|IoAction|IoFail|Clause` → `Err`.
- [ ] **Step 4: Implement `deserialise_value`.** Read the tag byte:
  - null → `data_value(Unit, [])`; true/false → `data_value(BoolTrue/False,[])`.
  - number → `data_value(BoxedNumber, [native_value(Num)])`; string →
    `BoxedString`; symbol → `BoxedSymbol` (intern via `symbol_pool_mut`); zdt →
    `BoxedZdt`.
  - list → read count, recurse count values into a `Vec<AbiClosure>`, fold into
    a cons list value handle (ListNil then ListCons via `data_value`).
  - block → read count pairs; build each `BlockPair(sym, value)` via
    `data_value`; fold the pairs into a kv cons list; build `data_value(Block,
    [kvlist, native_value(Num(0))])` (the `no_index` sentinel is boxed-zero,
    per `dsl::no_index`).
- [ ] **Step 5:** Run the round-trip test — expect PASS. Add a
  function-value case asserting `Err(NotSerialisable)`.
- [ ] **Step 6:** `cargo test -p eucalypt serialise` — PASS.
- [ ] **Step 7:** Commit `feat(pp): value serialiser for the parallel boundary (eu-u9xj.6)`.

---

## Task 6: Fork/worker coordination (`#[cfg(unix)]`)

**Files:**
- Create: `src/eval/stg/parallel/fork.rs`
- Modify: `src/eval/stg/parallel/mod.rs` (`#[cfg(unix)] pub mod fork;`)

**Interfaces:**
- Produces: `pub fn run_workers(n_workers: usize, arena: &Arena, worker:
  impl Fn(usize) -> Result<(), ExecutionError> + std::panic::RefUnwindSafe)`
  → `Result<(), ForkError>`. It forks `n_workers` children; each runs
  `worker(w)` inside `catch_unwind`, then `libc::_exit(0|42)`; immediately after
  fork the child resets SIGINT to `SIG_DFL`. The parent `waitpid`s all; if any
  child exits non-zero or is signalled, it SIGKILLs the survivors, reaps them,
  and returns `Err(ForkError::Worker { w, .. })`.
- Worker never touches stdout; diagnostics (if any) go to stderr only.

- [ ] **Step 1:** Implement `run_workers`: a fork loop collecting pids; child
  branch resets SIGINT (`libc::signal(SIGINT, SIG_DFL)`), runs
  `catch_unwind(AssertUnwindSafe(|| worker(w)))`, `_exit(0)` on `Ok`, `_exit(42)`
  on error/panic. Parent waitpid loop as in the spike; on the first failure,
  `libc::kill(pid, SIGKILL)` the rest and reap.
- [ ] **Step 2:** Unit test (`#[cfg(all(test, unix))]`): `run_workers(4, ...)`
  where each worker writes its index into arena segment w; parent reads back all
  four. A second test where worker 2 returns `Err` asserts `Err(ForkError)` and
  that the parent still returns (no zombie — best-effort: waitpid returns).
- [ ] **Step 3:** `cargo test -p eucalypt fork` — PASS.
- [ ] **Step 4:** Commit `feat(pp): fork/join worker coordination with hygiene (eu-u9xj.6)`.

---

## Task 7: The neutral parallel driver + sequential fallback

**Files:**
- Modify: `src/eval/stg/parallel/mod.rs`

**Interfaces:**
- Consumes: `arena`, `fork::run_workers`, `serialise`, `apply1_thunk`,
  `any_live_impure_producer`.
- Produces:
  - `pub enum Reduction { Map, Sum, Max, Min, Concat }` with `fn combine(&self,
    machine, view, a, b) -> Result<AbiClosure, ExecutionError>` (associative
    combine of two already-deserialised WHNF values) and `fn identity(&self,
    machine, view)` where meaningful.
  - `pub fn par_evaluate(machine: &mut dyn IntrinsicMachine, view, emitter,
    f: &Ref, xs: &Ref, reduction: Reduction, combinator: &str) ->
    Result<(), ExecutionError>` — the shared driver invoked by all five
    intrinsic `execute()`s. It:
    1. Boundary check: if `any_live_impure_producer()` → `Err(NotSerialisable
       with kind "live streaming import")` (spec §6 addendum).
    2. Resolve `f` (callable) and collect the spine of `xs` into a
       `Vec<AbiClosure>` of element closures (walk ListCons; error on non-list).
    3. Decide `W = min(cores-1, N)` (via `std::thread::available_parallelism`);
       if `W<=1` or `N < THRESHOLD` or `cfg!(not(unix))` → **sequential
       fallback**.
    4. Otherwise fork (Task 8).
- **Sequential fallback** (also the correctness oracle): for each element build
  `apply1_thunk(f, elem)`, `machine.force`, and either collect the forced
  result (Map) or fold with `reduction.combine`. For Map, `return_closure_list`;
  for reductions, `set_result` the folded value. **No serialisation** on this
  path — the forced heap values are returned directly.

- [ ] **Step 1:** Implement `Reduction::combine`:
  - `Sum` → build `ADD(a,b)` application and force (reuse the `ADD` global via
    `apply2_thunk` with the `ADD` gref, or numeric add on `value_native`).
  - `Max`/`Min` → compare `value_native` numbers, return the larger/smaller
    closure.
  - `Concat` → list append: walk `a` into elements, append `b` as the tail
    (build via `data_value` ListCons fold). `Map` → unused (panics if called).
- [ ] **Step 2:** Implement the spine collector (`ListCons` walk to
  `Vec<AbiClosure>`; each element left as-is/unforced — forcing happens per
  worker/sequentially).
- [ ] **Step 3:** Implement `par_evaluate` sequential path only (fork path is a
  `todo!`-free `unreachable` guarded by the W/threshold decision for now — but
  compile-safe: return sequential when unix fork not yet wired).
- [ ] **Step 4:** Commit `feat(pp): neutral par driver + sequential fallback (eu-u9xj.6)`.

---

## Task 8: Wire the fork path into the driver

**Files:**
- Modify: `src/eval/stg/parallel/mod.rs`

**Interfaces:**
- Consumes: Tasks 4–7.

- [ ] **Step 1:** In `par_evaluate`, when forking: allocate an `Arena` sized
  `min(N * PER_ELEM_CAP, ARENA_MAX)` (default `PER_ELEM_CAP = 4 KiB`,
  `ARENA_MAX = 256 MiB` — over-provision virtual, demand-zero physical).
  Partition `0..N` into W contiguous chunks. `run_workers(W, &arena, |w| { for
  i in chunk(w) { let t = apply1_thunk(f, xs[i]); let v = machine.force(t);
  serialise_value(machine, view, &v, combinator, &mut buf); writer.push(&buf) }
  // for reductions: fold locally, push one record })`.
- [ ] **Step 2:** Parent: after join, for `Map` read each segment's records in
  worker-index (= global index) order, `deserialise_value` each, collect into a
  `Vec<AbiClosure>`, `return_closure_list`. For reductions: `deserialise_value`
  the one partial per worker, fold left with `reduction.combine` in worker-index
  order, `set_result`.
- [ ] **Step 3:** **Determinism guard**: a debug-assert (or an
  `EU_PP_VERIFY`-gated check) that the forked result equals the sequential
  result on the same inputs is impractical in-process post-fork; instead the
  harness tests (Task 11) assert equivalence. Add a code comment pointing to
  them.
- [ ] **Step 4:** Commit `feat(pp): COW-fork parallel execution path (eu-u9xj.6)`.

---

## Task 9: The five intrinsics

**Files:**
- Create: `src/eval/stg/parallel/intrinsic.rs`
- Modify: `src/eval/stg/parallel/mod.rs` (`pub mod intrinsic;`)

**Interfaces:**
- Produces: `PARMAP`, `PARSUM`, `PARMAX`, `PARMIN`, `PARCONCAT` — each
  `StgIntrinsic` with `name()`, `wrapper()` (default `wrap(...)`, arity 2,
  strict in arg 1 = `xs` only, so the spine is forced; `f` stays lazy), and
  `execute()` delegating to `par_evaluate(machine, view, emitter, &args[0]
  /*f*/, &args[1] /*xs*/, Reduction::X, "par-map"/…)`.

- [ ] **Step 1:** Implement the five structs + `StgIntrinsic`/`CallGlobal2`
  impls. `execute()` bodies are one-liners into `par_evaluate`.
- [ ] **Step 2:** `cargo build` — clean.
- [ ] **Step 3:** Commit `feat(pp): PARMAP/PARSUM/PARMAX/PARMIN/PARCONCAT intrinsics (eu-u9xj.6)`.

---

## Task 10: Registry, runtime registration, prelude surface

**Files:**
- Modify: `src/eval/intrinsics.rs`
- Modify: `src/eval/stg/mod.rs`
- Modify: `lib/prelude.eu`

**Interfaces:**
- Consumes: Task 9.
- Produces: eucalypt combinators `par-map(f, xs)`, `par-sum(f, xs)`,
  `par-max(f, xs)`, `par-min(f, xs)`, `par-concat(f, xs)`.

- [ ] **Step 1:** Add five catalogue entries to `intrinsics.rs`
  (`PARMAP`…`PARCONCAT`), each `ty: function(vec![any(), list(), <ret>])`,
  `strict: vec![1]`. Match the numbering comment style.
- [ ] **Step 2:** `rt.add(Box::new(parallel::intrinsic::ParMap));` … in
  `mod.rs`.
- [ ] **Step 3:** In `lib/prelude.eu`, add (near `map`), with doc metadata:
  ```
  par-map(f, xs): __PARMAP(f, xs)
  par-sum(f, xs): __PARSUM(f, xs)
  par-max(f, xs): __PARMAX(f, xs)
  par-min(f, xs): __PARMIN(f, xs)
  par-concat(f, xs): __PARCONCAT(f, xs)
  ```
  (Receiver `xs` is the last argument, so `xs par-map(f)` = `__PARMAP(f, xs)`.
  Keep doc strings free of backticks and `{...}` — see CLAUDE.md gotchas.)
- [ ] **Step 4:** `cargo xtask prelude-compile` to refresh `lib/prelude.blob`.
- [ ] **Step 5:** Smoke test both engines:
  `timeout 60 ./target/release/eu -e 'x: [1,2,3,4] par-map((_ * _))' --heap-limit-mib 12288`
  and the same under `EU_HEAPSYN=1`; assert identical output.
- [ ] **Step 6:** Commit `feat(pp): register par-* intrinsics + prelude combinators (eu-u9xj.6)`.

---

## Task 11: Harness tests — equivalence, fallback, reductions, errors

**Files:**
- Create: `tests/harness/2xx_pp_par_equivalence.eu`
- Create: `tests/harness/2xx_pp_par_reductions.eu`
- Create: `tests/harness/errors/2xx_pp_non_serialisable.eu` (+ `.expect`)
- Modify: `tests/harness_test.rs` (register the new tests)

**Interfaces:**
- Consumes: Task 10 combinators.

- [ ] **Step 1:** `2xx_pp_par_equivalence.eu` — compute `RESULT` from checks:
  for several `f` and input sizes (spanning below and above THRESHOLD), assert
  `xs par-map(f) = xs map(f)`; assert order preservation; assert small-N (W≤1)
  and large-N agree. Follow the `RESULT`-from-checks pattern of
  `tests/harness/189_r9oy_union_as_spec.eu`.
- [ ] **Step 2:** `2xx_pp_par_reductions.eu` — assert `xs par-sum(f) = xs
  map(f) sum`, likewise `par-max`/`par-min`/`par-concat` against their
  sequential equivalents; include an empty list and a singleton.
- [ ] **Step 3:** `errors/2xx_pp_non_serialisable.eu` — `par-map` returning a
  function (e.g. `[1,2,3] par-map((x) -> (y) -> x)` — use a real higher-order
  form) → boundary error; `.expect` with `exit:` and a `stderr:` regex on the
  message.
- [ ] **Step 4:** Register in `tests/harness_test.rs`.
- [ ] **Step 5:** `cargo test --test harness_test pp` under the default engine
  and `EU_HEAPSYN=1 cargo test --test harness_test pp` — both PASS.
- [ ] **Step 6:** **Fault-injection**: temporarily break `par_evaluate` (e.g.
  reverse the reassembly order, or drop the last element); confirm the
  equivalence test FAILs under both engines; restore; confirm PASS. Record this
  in the PR body.
- [ ] **Step 7:** Commit `test(pp): par-* equivalence/reduction/error harness tests (eu-u9xj.6)`.

---

## Task 12: GC-soundness + engine-equivalence validation

**Files:** none (validation only) — capture evidence for the PR body.

- [ ] **Step 1:** Run the full suite under GC verification, both engines:
  `EU_GC_VERIFY=2 cargo test --test harness_test pp` and with `EU_HEAPSYN=1`.
  Expected: green, no verification panic in parent or workers.
- [ ] **Step 2:** Run a larger data-parallel program (a scaled day09/day10
  shape) under `EU_GC_VERIFY=2` on both engines and confirm identical output.
- [ ] **Step 3:** Record all commands + observed output for the PR body (spike
  R5 engine-equivalence, GC integrity §8).

---

## Task 13: Linux CI job for PP (spike R7)

**Files:**
- Modify: `.github/workflows/<ci>.yml`

- [ ] **Step 1:** Add a job (or extend the Linux test job) that runs the PP
  harness tests on both engines on `ubuntu-latest` — the mechanism is
  Darwin-validated only; this proves it off macOS.
- [ ] **Step 2:** Commit `ci(pp): run PP tests on Linux, both engines (eu-u9xj.6)`.

---

## Task 14: Final gates + PR

- [ ] **Step 1:** `rustup update stable`, `cargo fmt --all`,
  `cargo clippy --workspace --all-targets -- -D warnings`, full `cargo test`.
- [ ] **Step 2:** Push `feat/furnace-pp-parallelism`; open a PR to `master` for
  **wicket** review. Body states: what was implemented; fmt/clippy/test all
  green; both-engine + `EU_GC_VERIFY=2` evidence; the fault-injection procedure
  and its result; that this touches GC/fork/unsafe and needs recorded review by
  someone other than the author before merge. Do NOT self-merge.
- [ ] **Step 3:** Message the coordinator with the PR URL.

---

## Self-review notes

- **Spec coverage:** §3 combinators → Tasks 9–10; §4 COW-fork/hygiene → Tasks
  6, 8; §6 serialiser + non-serialisable error → Tasks 1, 5; §6 producer
  boundary (spike addendum) → Tasks 3, 7; §6a arena → Task 4; §7 cost/threshold
  → Task 7; §8 testing (equivalence, fallback, GC) → Tasks 11–12; engine
  equivalence R5 → Tasks 10–12; Linux R7 → Task 13.
- **Out of scope (correctly omitted):** general `par-fold`, shared-memory
  model, Windows/spawn (eu-9udwg).
- **Type consistency:** `par_evaluate`, `Reduction`, `serialise_value`,
  `deserialise_value`, `apply1_thunk`, `any_live_impure_producer`,
  `run_workers`, `Arena` — names are used consistently across tasks.
