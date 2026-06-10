# Feasibility Study: Compiling Eucalypt Instead of Interpreting STG

Status: draft for discussion
Date: June 2026

This study analyses the feasibility of replacing the current STG
interpreter with a compilation strategy — in particular compiling core
or STG to WebAssembly and executing it in a WASM virtual machine — and
compares that against alternative targets, including a custom bytecode
VM designed specifically for eucalypt. It concludes with a phased,
evidence-gated plan.

## 1. Executive Summary

**Compiling STG to WASM is technically feasible but is the wrong first
move.** The dominant costs of a WASM backend are not code generation
but building a second runtime system: a lazy-evaluation object model
(thunks, updates, blackholes, partial applications), a GC story, and a
bridge to the ~190 Rust intrinsics that implement most of eucalypt's
observable semantics. That is GHC-wasm-backend-scale work (multiple
person-years to parity) and it attacks the wrong bottleneck for
eucalypt's primary workload — short-lived configuration generation
runs, where the ~85 ms front-end pipeline floor and not interpretation
speed dominates.

**A custom bytecode VM is the highest-value evolution of the current
machine.** Measurements show the interpreter executes only ~2.4 M
machine ticks/second (~420 ns/tick) on compute-bound code, largely
because compiled code itself lives as a pointer graph in the GC heap,
every `let` allocates an environment frame, and every source
annotation costs a dispatch step. A flat, register-oriented bytecode
held outside the managed heap, with side tables for source locations
and a serialisable format enabling a precompiled prelude, addresses
interpretation overhead *and* startup latency *and* GC load — while
keeping the GC, intrinsics, emitter, error machinery, and debugging
tools unchanged. It is also the natural stepping stone to any later
acceleration tier (Cranelift JIT or a WASM backend), because those
tiers can consume bytecode rather than re-deriving everything from
STG trees.

**Recommendation:** profile-driven Phase 0, then build the bytecode VM
behind a flag with differential testing against the harness, then
bytecode caching for startup. Revisit WASM later as an *embedding and
distribution* opportunity (sandboxed plugins, WASM components,
playground acceleration), not as the primary execution engine.

## 2. Baseline: How Eucalypt Executes Today

### 2.1 Pipeline

```
source ──rowan parse──► AST ──desugar/cook/inline/prune──► Core
      ──stg-compile──► Rc<StgSyn> tree ──loader──► HeapSyn graph (in GC heap)
      ──Machine::run──► step loop over (code ptr, env ptr) closures
```

Key properties of the machine (`src/eval/machine/vm.rs`):

- A closure is `(RefPtr<HeapSyn>, RefPtr<EnvFrame>)`; both live in the
  managed heap. The compiled program is **loaded into the GC heap**
  by `src/eval/memory/loader.rs` on every run and is scanned (and
  potentially evacuated) by the collector like any other data.
- The step loop dispatches on the `HeapSyn` variant: `Atom`, `Case`,
  `Cons`, `App`, `Bif`, `Let`, `LetRec`, `Ann`, `Meta`, `DeMeta`,
  `BlackHole`. Continuations (`Branch`, `Update`, `ApplyTo`, `DeMeta`,
  `CaptureEnd`) live on a machine-owned stack that is precisely
  scanned by the GC.
- Laziness is implemented with update-in-place: entering a thunk
  blackholes its environment slot and pushes an `Update`
  continuation; an `IF` special case (`suppress_update`) avoids O(N)
  update chains in tail-recursive conditionals.
- Every `Let`/`LetRec` allocates an `EnvFrame` in the managed heap
  (with a shared-backing/remap optimisation for small constructor
  environments).
- Source locations (`Smid`) are carried by `Ann` nodes — each one
  costs a full dispatch step at runtime — plus annotations stamped on
  environment frames for error traces.
- The ~190 intrinsics (`make_standard_runtime`, `src/eval/stg/mod.rs`)
  implement arithmetic, blocks (including the entire merge/lookup
  machinery), strings/regex, datetimes, sets, vecs, ndarrays, graph
  algorithms, encodings, the render/emit pipeline, and the IO monad.
  These are Rust code invoked by index from `Bif` instructions; many
  re-enter the machine via `evaluate_to_whnf`.
- Memory is managed by a custom Immix-style collector (32 KB blocks,
  128-byte lines, 16-byte headers, evacuation with forwarding
  pointers, **no per-object `Drop`** — see ADR-001 for how that
  constraint killed the persistent-blocks experiment).
- The whole stack already cross-compiles to `wasm32` — the browser
  playground runs the *interpreter* compiled to WASM
  (`src/wasm.rs`, `src/wasm_pipeline.rs`), including the custom GC
  inside linear memory.

### 2.2 Where the time goes (measurements)

Measured on this study's container with a clean `cargo build
--release` (virtualised hardware — treat as indicative magnitudes,
not precise numbers).

**Startup floor** — `eu -e 'true'`:

| Phase | Time |
|-------|------|
| parse | 20.7 ms |
| translate (desugar) | 15.4 ms |
| merge | 8.5 ms |
| retarget + cook + eliminate | ~22.8 ms |
| stg-compile | 0.2 ms |
| stg-execute | <1 ms |
| **total wall clock** | **~85 ms** |

Almost all of this is the front-end pipeline re-processing the
embedded 2,262-line prelude **on every single run**. For the
config-generation workloads eucalypt is built for, this floor is the
user-visible performance of the tool.

**Compute-bound** — `bench/001_naive_fib.eu` (`fib(30)`):

| Metric | Value |
|--------|-------|
| Machine ticks | 134.6 M |
| wall clock (execute) | 56.5 s |
| throughput | **~2.4 M ticks/s (~420 ns/tick)** |
| machine allocs | 18.8 M |
| heap blocks allocated | 203,292 (≈ 6.6 GB through the bump allocator) |
| GC collections | 1 (mark 2.5 ms, sweep 78 ms) |
| sys time | ~8 s (block acquisition / page faults) |

At roughly 50 ticks and ~7 heap allocations per `fib` call, and 420 ns
per tick, the machine is one to two orders of magnitude slower than
mature bytecode interpreters (which typically sustain 50–500 M simple
ops/s) and three to four orders off native code. The headroom is
enormous — but note where it is: dispatch through a heap pointer
graph, allocation churn, and the costs structural to the current
design, *not* missing native code generation per se.

**Intrinsic/pipeline-bound** — the block-lookup and string benchmarks
collapse almost entirely into the front-end (static name resolution
and inlining fold them at compile time; 9 ticks at runtime). Real
config workloads sit between the extremes but generally lean heavily
on intrinsics (blocks, merge, render, regex) whose cost a compiler
backend would not touch at all.

### 2.3 The Amdahl framing

Any compilation strategy accelerates exactly one slice of the budget:
the dispatch/environment overhead of executing STG terms. It does not
accelerate:

- the front-end pipeline (the entire cost of typical small runs);
- the ~190 Rust intrinsics, including the whole render/emit path;
- garbage collection;
- parsing of imported YAML/JSON/TOML/CSV/XML inputs.

This is the central feasibility fact. Before committing to *any*
backend, Phase 0 below quantifies the dispatch share on representative
real workloads. For `fib`-like code the share approaches 100%; for
`eu`-as-config-tool it may be a minority of an 85 ms run.

## 3. Option W: Compile Core/STG to WASM, Run in a WASM VM

### 3.1 What this actually entails

Generating WASM instructions for STG terms is the easy ~20%. The hard
80% is the runtime system the generated code needs:

1. **A lazy object model.** Closures, thunks (mutable: entry must
   blackhole, completion must overwrite with an indirection), partial
   applications (PAPs) and a generic-apply protocol for unknown-arity
   calls (the eval/apply machinery GHC implements with a family of
   `stg_ap_*` functions), data constructors with tags, and eucalypt's
   unusual extras: `Meta`/`DeMeta` transparency and the emitter
   `CaptureEnd` protocol.
2. **Control flow.** STG execution is continuation-driven and deeply
   tail-recursive. WASM's `return_call` (tail-call proposal) is now
   standard across engines and in Wasmtime, which makes the
   closure-entry-function style viable; without it everything
   trampolines through a dispatch loop and most of the gain
   evaporates.
3. **A GC story** — the fork in the road:
   - **W1: linear memory + our own RTS** (the GHC wasm backend
     approach). Port the object model and a collector into the
     module's linear memory, maintain an explicit shadow/STG stack so
     roots are findable (generated code cannot scan the WASM value
     stack). We could compile the existing Rust GC and intrinsics to
     `wasm32` as a runtime module — the playground proves they
     compile — but generated code would have to interoperate with
     Rust-defined object layouts (`repr(Rust)`, unstable) or we
     redesign the object model with explicit layouts. Either way this
     is a second RTS to build and maintain.
   - **W2: WASM-GC proposal.** Structs/arrays managed by the engine's
     collector; thunk update is plain field mutation; tail calls +
     typed funcrefs complete the picture. This discards our Immix
     investment, our heap-limit controls, GC metrics, and `EU_GC_*`
     debugging in the compiled path. Engine GC performance for
     allocation-furious lazy functional code is good in browsers
     (production WASM-GC since 2023/24, built for Kotlin/Dart-scale
     workloads) but still maturing in server-side engines like
     Wasmtime. We also cannot tune it.
4. **The intrinsic boundary.** Two bad choices:
   - *Host functions*: every block merge, lookup, regex match, render
     event crosses the guest/host boundary. Natives that are Rust
     objects (interned symbols, `HeapString`, `serde_json::Number`,
     `chrono` datetimes, sets, vecs, ndarrays, producer handles) must
     be marshalled or held as externref handles with lifetime
     management on both sides. The render path is emitter-streaming —
     one host call per scalar emitted.
   - *Compile intrinsics into the module* (only viable under W1):
     drags regex, chrono, serde_json, sha2 into the generated-code
     world and binds us to the layout-compatibility problem above.
5. **Everything else that makes eu, eu.** Source annotations and
   stack traces (need side tables mapping code offsets to Smids), the
   `dump` tooling, step limits for the browser, heap limits, the
   statistics machinery, `expect`/test mode, the IO monad driver
   loop. All must be rebuilt or bridged for the compiled path.

### 3.2 Startup latency: the silent killer

Today a trivial run completes in ~85 ms total. A WASM path adds, per
run: STG→WASM codegen for prelude + user code, then Cranelift
compilation inside Wasmtime (typically tens to hundreds of ms for a
non-trivial module), then instantiation. Mitigations exist —
precompiled `.cwasm` for the prelude baked in at build time,
content-hash caching for user units, Winch/baseline compilation — but
the mitigation machinery is itself a project, and the best case
roughly breaks even with today on small runs. For eucalypt's primary
use-case, the WASM backend's most likely user-visible effect is
*slower* invocations.

### 3.3 Engine and packaging

- **Wasmtime** is the obvious embedding: mature, supports tail calls
  and WASM-GC, fuel/epoch interruption (a better step-limit than the
  current tick counter), per-store memory limits. Cost: a very large
  dependency (tens of MB on the binary, significant compile time),
  and a hard fork in behaviour between the native CLI and the
  `wasm32` playground build — Wasmtime does not run *on* wasm32, so
  the playground would need the browser engine as its backend
  (compile-in-browser) or keep the interpreter, meaning two execution
  paths forever.
- **Interpreted engines (wasmi etc.)** would replace our specialised
  interpreter with a generic one — strictly worse.

### 3.4 What we would gain

Honest accounting of the upside, because some of it is real:

- **Throughput on compute-heavy code**: plausibly 10–50× on
  `fib`-like programs once mature (native-class code, engine-grade
  GC). Eucalypt's benchmarks would look transformed; typical config
  runs would not.
- **Sandboxing and resource limits as engine features**: fuel,
  memory caps, deterministic execution. (Note `eu` already has step
  and heap limits, and `io.shell` deliberately punches through any
  sandbox on the CLI.)
- **Distribution and embedding**: compile a eucalypt program once
  into a WASM component (WASI P2) callable from other languages and
  runtimes — config-generation logic as a portable library. This is
  a genuinely *new capability*, not an optimisation, and is the
  strongest argument for ever doing W-something.
- **Playground**: user code at near-native speed in the browser
  (today the interpreter itself runs as WASM, so user code pays
  interpretation cost on top). Rarely the bottleneck for
  playground-sized snippets.

### 3.5 Risks

| Risk | Severity | Notes |
|------|----------|-------|
| Effort to parity: second RTS + 190-intrinsic bridge + tooling | **Very high** | GHC's wasm backend took a team years on top of an existing code generator |
| Startup regression for the dominant workload | **High** | Mitigation machinery (caching, AOT prelude) is its own project |
| Two execution engines diverge during the (long) migration | High | Every language change lands twice; harness must run both |
| Intrinsic boundary marshalling erases throughput wins | High | Render/emit and block ops are the hot path of real workloads |
| WASM-GC performance outside browsers | Medium | Improving, but out of our control |
| Loss of GC/debug tooling investment in compiled path | Medium | ADR-001 shows how load-bearing this tooling is |
| Binary size / dependency weight (Wasmtime + Cranelift) | Medium | eu is currently a lean single binary |

**Verdict: not as the execution engine, not now.** Re-open as a
*distribution/embedding* project (W2, components, playground) once the
bytecode work has stabilised an explicit, serialisable code format to
compile from.

## 4. Other Standard Targets (brief)

- **Cranelift direct JIT (native, in-process).** Strictly better than
  WASM *for performance*: no guest/host boundary (intrinsics called
  directly via extern shims), no engine sandbox tax, one moving part.
  The hard problem is the same — GC-cooperative compiled frames
  (stack maps/safepoints, or keeping the explicit machine stack and
  only JIT-ing closure bodies, which converges on "bytecode VM with a
  JIT tier"). Doesn't run on wasm32, so the playground keeps the
  interpreter. Best understood as a *tier on top of bytecode*, not an
  alternative to it.
- **LLVM AOT / transpile to Rust or C.** Compiler-toolchain
  dependency at user runtime, seconds of compile latency; a
  non-starter for a config tool.
- **JavaScript.** Semantics mismatch (laziness, tail calls,
  numerics), and we'd inherit a JS engine. No.
- **JVM / GraalVM-Truffle, BEAM.** Excellent lazy/functional hosting
  in principle; catastrophic distribution mismatch for a small static
  CLI binary. No.

## 5. Option B: A Custom Bytecode VM for Eucalypt

The question posed: what would we gain by designing our own VM and
bytecode rather than adopting a standard target? Answer: it is the
only option that attacks all three measured cost centres (dispatch,
GC load, startup) without rebuilding the runtime semantics that
already work.

### 5.1 Why the current interpreter is slow — specifically

The 420 ns/tick is not "interpretation is slow" in the abstract; it
decomposes into removable structural costs:

1. **Code is data in the GC heap.** Every `HeapSyn` node is a managed
   allocation with a 16-byte header, reached by pointer-chasing.
   Executing means cache-missing through a graph; collecting means
   scanning (and evacuating!) code. The loader re-allocates the whole
   program into the heap on every run.
2. **Environment churn.** Every `let` allocates an `EnvFrame`;
   `fib(30)` does 18.8 M allocations and pushes 6.6 GB through the
   bump allocator. Most bindings never escape and could live in VM
   registers/stack slots.
3. **Annotations are instructions.** Each `Ann` costs a dispatch step
   purely to update the current `Smid`.
4. **Boxing traffic.** Arithmetic round-trips through
   `BoxedNumber` constructors, `case` unboxing, and `Bif` indirection
   — several ticks and often allocations per add.
5. **Generic dispatch.** A `match` over 11 syntax variants plus
   continuation pops, where a compiler could have emitted a fused
   sequence (the STG optimiser already recognises many such patterns
   but can only express them as more tree).

### 5.2 Design sketch

- **Code representation:** flat `u32` instruction arrays in an
  immutable, non-GC code arena, one code object per compiled lambda
  form. Constant pool (numbers, strings, symbols — interned once at
  load, not per run). Function table for intrinsics exactly as
  today's `Bif` indices.
- **Side tables, not instructions:** code-offset → `Smid` line table
  replaces `Ann` nodes; debug names likewise. Zero runtime cost until
  an error needs a trace.
- **Execution model:** keep the proven semantics — eval/apply,
  update-in-place thunks, blackholing, the continuation forms
  (`Branch`/`Update`/`ApplyTo`/`DeMeta`/`CaptureEnd`), the
  `suppress_update` refinement — but express them over a register
  frame + explicit VM stack. Heap-allocate environments only for
  bindings that escape into thunks/closures (escape analysis at
  STG→bytecode time; laziness forces captured envs to the heap, but
  the measured churn is dominated by non-escaping frames).
- **Instruction design:** specialise the hot patterns into
  superinstructions: `UNBOX_NUM`, fused `ARITH_ADD_NN` on registers,
  `CASE_BOOL`, `LOOKUP_SYM k` with inline cache slot,
  `ENTER_THUNK`/`UPDATE`, `TAILCALL_KNOWN f`. The existing optimiser's
  pattern knowledge transfers directly.
- **GC interface unchanged in kind:** the VM's registers and stack
  are precise roots with `GcScannable` implementations, exactly as
  the machine's stack is today. Code arena is never scanned. The
  Immix collector, heap objects, natives, and all 190 intrinsics are
  untouched.
- **Serialisable by construction:** flat code + constant pool gives a
  cacheable unit format. Embed the **precompiled prelude** in the
  binary at build time (the build already self-hosts `build.eu`);
  cache user units by content hash. This converts the 85 ms floor
  into "parse + compile the user's file only" — likely the single
  most user-visible win in the whole programme.

### 5.3 What we gain (and what we honestly don't)

Expected gains, with reasoning:

| Dimension | Expectation | Basis |
|-----------|------------|-------|
| Dispatch-bound throughput | 3–10× | Flat fetch + superinstructions vs heap-graph walk at 420 ns/tick; AST→bytecode transitions in other languages (YARV, Lua pre-JIT era) sit in this band, and our baseline is unusually heavy |
| Allocation volume | 5–20× reduction on let-heavy code | Register frames eliminate the dominant `EnvFrame` class; `fib` does ~7 allocs/call today |
| GC time & heap size | Material reduction | Code leaves the heap; fewer objects to scan; smaller working set |
| Startup | 85 ms → plausibly 20–30 ms for small runs | Prelude precompiled; parse/desugar only the user unit |
| Compiled artefact caching | New capability | Unit-level bytecode cache |
| Browser playground | Same code path | Bytecode VM compiles to wasm32 like the rest of the crate |

What it does *not* give: native-code throughput on pure numeric
kernels (that needs a JIT tier), or any speedup to intrinsic-internal
work. Both are fine — the bytecode format is precisely the input a
later Cranelift tier or WASM emitter would want.

### 5.4 Risks

| Risk | Severity | Mitigation |
|------|----------|------------|
| Env/laziness redesign subtly changes semantics (update timing, blackholes, `suppress_update`) | **High** | Keep continuation semantics identical; differential-test entire harness interpreter-vs-VM; port the GC verification modes |
| Escape analysis wrong → thunks see stale registers | High | Start conservative (heap envs everywhere = parity with today), tighten incrementally with the harness as oracle |
| Two engines during migration | Medium | Time-boxed: interpreter retired once harness + benchmark parity reached; both share heap/intrinsics so divergence surface is the step loop only |
| Bytecode format churn breaks cache | Low | Version stamp; cache is an optimisation, rebuild on miss |
| Effort underestimate | Medium | Scope is one subsystem (`machine/` + a loader replacement), not a second RTS; existing metrics/statistics make progress measurable |

Rough effort: **3–5 engineer-months to parity**, versus an estimated
**18–36 months** for a production WASM backend with runtime parity.

## 6. Comparison

| | WASM (W1/W2) | Cranelift JIT | **Bytecode VM** | Status quo |
|---|---|---|---|---|
| Compute throughput | ★★★★ | ★★★★★ | ★★★ | ★ |
| Typical config-run latency | ✗ (likely worse) | ✗ (JIT warmup) | ★★★★ (better via caching) | baseline |
| Effort to parity | Very high | High | Moderate | — |
| Keeps GC/intrinsics/tooling | Partially (W1) / No (W2) | Mostly | **Yes** | Yes |
| Playground (wasm32) story | Split or browser-compile | Interpreter fallback | Same engine | Same engine |
| New capabilities | Components, sandboxed embedding | — | Unit caching, future tiers | — |
| Migration risk | Big-bang or long dual-engine | Medium | Incremental, differential-testable | — |

## 7. Recommended Plan

Each phase has an explicit gate; later phases are commitments only
after earlier evidence supports them.

**Phase 0 — Attribution profiling (1–2 weeks).**
Instrument or sample representative workloads (bench suite + several
realistic config-generation programs): time split across front-end /
dispatch / intrinsics / GC; tick-type histograms; allocation-by-class
(EnvFrame vs data vs code). Deliverable: a short report fixing the
expected ceiling for each option.
*Gate:* if dispatch+env+GC < ~30% on representative workloads, stop —
invest in front-end speed and intrinsics instead.

**Phase 1 — Bytecode VM core (4–6 weeks).**
Instruction set spec; STG→bytecode compiler (reusing the existing
compiler's lowering and optimiser patterns); VM loop with
conservative (heap-env) execution model; precise root scanning;
`--engine vm` flag. Deliverable: harness green under differential
testing on a meaningful subset.
*Gate:* ≥2× on dispatch-bound benchmarks at full harness parity.

**Phase 2 — Parity and performance (4–6 weeks).**
Full intrinsic coverage (mechanical: `Bif` indices unchanged), meta /
emitter-capture / IO-monad paths, error traces from side tables,
statistics integration, `EU_*` debug modes. Register frames +
escape-analysed environments; superinstructions guided by Phase 0
histograms. Deliverable: full harness + benchmark suite parity;
interpreter deprecation decision.
*Gate:* no benchmark regresses; compute benchmarks ≥3×.

**Phase 3 — Compiled-unit caching (2–4 weeks).**
Stable serialisation; prelude precompiled at build time and embedded;
content-hash cache for user units; `eu dump bytecode`. Deliverable:
small-run wall clock materially below the current 85 ms floor.

**Phase 4 — Acceleration/embedding tier (optional, evidence-gated).**
Two independent candidates, both consuming bytecode:
(a) Cranelift template-JIT for hot closures (performance);
(b) WASM emission for the playground and a WASI-component export
story (distribution/embedding — the durable part of the WASM
opportunity). Decide on Phase 0/2 evidence and user demand; neither
blocks Phases 1–3.

## 8. Appendix: Measurement Notes

- Clean `cargo build --release`, Linux container, virtualised CPU; no
  pinning — magnitudes only.
- `eu -e 'true'`: 85 ms wall; phase times from `-S` (parse 20.7 ms,
  translate 15.4 ms, merge 8.5 ms, cook 16.0 ms, stg-compile 0.24 ms,
  execute <1 ms).
- `001_naive_fib.eu -t bench-naive-fib`: 134,626,849 ticks, 18,847,772
  machine allocs, 203,292 blocks (32 KB each) allocated, 1 GC
  (mark 2.5 ms / sweep 77.5 ms), execute 56.46 s, sys 7.9 s.
- `006_block_lookup` / `011_string_split_join`: 9 ticks at runtime —
  statically folded by the core pipeline; they currently benchmark
  the front end, which itself supports the startup-latency findings.
