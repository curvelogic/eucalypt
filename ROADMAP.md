# Eucalypt — Roadmap to 1.0 and beyond

- **Status:** Plan of record
- **Date:** 2026-06-23
- **Baseline:** eucalypt 0.10.1

---

## 1. Purpose

This is the single, self-contained roadmap for eucalypt's evolution to **1.0** and
the shape of the work beyond it. It is a **direction reset**: the runtime half of
the previous plan (a generational-GC line as the engine of 1.0 performance) was
attempted in 0.10.0, did not pay off, and has been retired. What replaced it is the
subject of this document.

The reset rests on one empirical turn. The previous plan was written against a
0.7.1 baseline in which *mark time was >95% of VM time*; it built two large
engineering bets — a generational GC and persistent O(log n) blocks — on that
premise. By 0.10.0 the premise no longer described real workloads: strict prelude
globals eliminated the recurring collections, and on the AoC-2025 corpus **GC
dropped to zero collections** while the dominant costs became **VM dispatch** and
**environment-frame walking**. The generational GC and flat-closure experiments
that targeted the old picture were both built, measured, and reverted (§10). The
lesson the one *successful* 0.10.0 change taught — strict eager evaluation, a
*compile-time* change — is the thesis of this plan: **the bytes we generate are
the bottleneck; win in compilation and dispatch, not in heap representation.**

The document has two halves. **Sections 2–6** are the plan: what eucalypt is and
where it now stands, the principles, the settled decisions, the deliberate
non-goals, and the shape and sequencing of the work. **Section 7** is the work
itself, grouped into eight pillars, each item carrying its problem (grounded in the
current code with `path:line` references), design, phasing and success criteria.
**Sections 8–9** give the critical path and the index. **Section 10** is the
supplement: the superseded and abandoned work, with its post-mortems, kept so the
reasoning is not lost and not repeated.

---

## 2. What eucalypt is, and where it stands

Eucalypt's **centre of gravity is data**: generating, templating and transforming
YAML/JSON/TOML is the prime case, and the great majority of uses spit out config.
But eucalypt is a real lazy, pure, functional language, and it carries **no false
ceiling** — it must extend cleanly into general computation, and we remove
artificial limits rather than advertise them (Principle 5). Its peers for the data
case are the configuration languages (Jsonnet, Dhall, CUE, Nickel, Pkl, KCL,
Starlark); for the general case its lineage is the lazy functional languages
(Haskell/STG, Unison). Every idea is weighed against the data centre of gravity,
but generality is a legitimate, intended use.

### The implementation, in brief

The compile pipeline is: **parse** (a Rowan lossless tree — the sole parser) →
**desugar** → **cook** (shunting-yard fixity resolution, seedable from prelude
operators via `distribute_with_prelude`, `src/core/cook/fixity.rs:21`) →
**eliminate** → **inline** (×2) → **fuse** → **eliminate/compress** → **verify** →
**type-check** (bidirectional, advisory, then *erased*) → **STG compile**
(`src/eval/stg/compiler.rs`) → **STG optimise** → **load to heap**
(`src/eval/memory/loader.rs:120`) → run on the **STG machine**.

There are two STG representations, and the boundary between them is central to this
plan:

- **`StgSyn`** (`src/eval/stg/syntax.rs:118`) — the off-heap, `Rc`-based,
  pointer-free IR. It is the optimisation target, and it already has an
  arena-flattened, index-referenced, **postcard-serialisable** form
  (`ArenaStgSyn`/`PreludeBlob`, `src/eval/stg/blob.rs`). This is, in effect, a
  proto-bytecode.
- **`HeapSyn`** (`src/eval/memory/syntax.rs:227`) — the on-heap form the legacy
  tree-walk VM executes. On this path the loader translates `StgSyn → HeapSyn`
  into the GC heap on **every run**, and **compiled code is GC-scanned and
  evacuated like data** (`impl GcScannable for HeapSyn`,
  `src/eval/memory/syntax.rs:400-547`). *(Since 0.12/BV1 this describes only the
  `EU_HEAPSYN=1` opt-out engine — the default bytecode engine executes a flat
  opcode stream from an off-heap arena and none of this applies to it.)*

- **The VM** is a Spineless Tagless G-machine (`src/eval/machine/vm.rs`): the loop
  `run`→`step`→`handle_instruction` (`vm.rs:1842,1663,389`) dispatches a `match` on
  `HeapSyn` (`vm.rs:426`), with **six** continuation kinds (`Branch`, `Update`,
  `ApplyTo`, `DeMeta`, `SeqBind`, `CaptureEnd`; `src/eval/machine/cont.rs:34`) on
  an off-heap `Vec` stack (`vm.rs:282`), and **192** intrinsics
  (`src/eval/intrinsics.rs:49`) invoked via a deferred-BIF path (`vm.rs:518,1722`).
- **Environment frames** (`src/eval/machine/env.rs:191`) are a cactus stack: each
  frame holds an `Array<C>` of bindings plus a `next` pointer to its parent; a
  local reference walks the chain (`cell`/`get`, `env.rs:334,352`), and **every
  `let` allocates a frame** (`from_let`/`from_letrec`, `vm.rs:525`).
- **Annotation nodes** (`StgSyn::Ann`, `src/eval/stg/syntax.rs:152`) carry source
  `Smid`s inline in the code and **cost a dispatch step each** (`vm.rs:536`).
- **The GC** is Immix-inspired (`src/eval/memory/`): 32 KiB blocks, 128 B lines,
  mark-sweep with opportunistic evacuation and lazy sweep, single-threaded
  stop-the-world. Excellent debug instrumentation (`EU_GC_VERIFY`, `EU_GC_STRESS`,
  `EU_GC_POISON`).
- **The type checker** (`src/core/typecheck/`) is bidirectional and **advisory**:
  it emits warnings and is erased before code generation. The gradual type system
  the language was built toward is **complete** (HKT shipped in 0.7.0).
- **Demand analysis** (`src/core/demand.rs`, `src/core/analyse_demand.rs`) carries
  a four-field `Demand` (cardinality, strictness, whnf, recursive) on every
  `CoreBinding` (`src/core/binding.rs:9`); `take_lambda_form`
  (`src/eval/stg/compiler.rs:540`) consults it to choose `Value` vs `Thunk`, and
  strict non-recursive bindings are wrapped in `Seq` forms (`compiler.rs:756`).
- **Blocks** are cons-lists of key/value pairs with **O(n) lookup**,
  insertion-ordered.

### What 0.7–0.10 shipped (ledger)

| Release | Landed |
|---|---|
| **0.7.0** | Gradual type system completed — HKT, row inference, structural operator constraints, `Partial(T)`/`T?` |
| **0.7.1** | One-compile pure-document path; the Unit Interface + cross-import bracket fix; `export: :internal` |
| **0.8** | Real semver + stability tiers; `requires` guard; prelude selection; deprecation lifecycle; resilient parser (phase 1); conformance corpus + golden sidecars; GC-verified CI |
| **0.9** | Pre-compiled prelude **blob** (postcard-serialised arena STG); incremental query-based LSP; `eu doc`; demand annotations on core bindings; parser error recovery (phase 2); proptest + fuzz targets |
| **0.10** | **Strict eager evaluation** (the one runtime win — −38% allocs/−34% ticks on prelude-heavy, −76% on folds); prelude demand signatures; git imports restored; *generational GC attempted and reverted; demand cardinality/update-elision attempted and abandoned (§10)* |

### Where it actually stands now (the empirical picture)

Two measurements reset the priorities. Both reproduced on the current build.

1. **Startup is a front-end tax, and it is not yet banked.** `eu -e 'true'` spends
   ~53 ms here (the feasibility study measured ~85 ms on its reference machine — the
   same tax, cited as "today's startup" throughout this document), of which **~100% is
   the front-end re-processing the 2,262-line prelude** (parse 23%, cook 24%,
   translate 22%, merge 13%); actual evaluation is *microseconds* (`stg-eval`
   15 µs). The prelude **blob** that would eliminate
   parse/cook exists only when separately generated (`cargo xtask prelude-compile`,
   `xtask/src/main.rs:64`) and embedded behind `cfg(prelude_blob_ok)`
   (`src/driver/resources.rs:6`, `build.rs:35`); a plain `cargo build` has **no
   blob** and silently runs source-prelude — verified: the default path and
   `--source-prelude` are timing-identical. And even *with* the blob, the program
   is still loaded into the GC heap as scanned `HeapSyn` on every run. **For the
   config centre of gravity, this startup floor — not interpretation speed — is the
   user-visible performance.**

2. **For compute, dispatch and env-walk dominate; GC does not.** Across the
   AoC-2025 corpus, after strict prelude globals, **every program does 0 GC
   collections**. The costs that remain (CPU profiles in
   `examples/aoc25/PERFORMANCE.md`): **VM dispatch** (`handle_instruction` +
   `Machine::run`) at **60–81%**, and **`EnvironmentFrame::get`** at **10–39%**
   (day10-p2 39%, day06-p2 33%, day11-p2 27%). `day11-p1` is the clean microbench —
   66.5 M ticks, 357 K allocs, **0 collections**, ~all time in the dispatch loop.
   Allocation machinery is material only on a couple of programs (day10-p1 28%).

These two facts, plus the three reverts (§10), define the work: **the engine
materialises code as scanned heap data, dispatches it through a tree-walk over
`HeapSyn`, and walks a cactus env on every variable** — and the front-end re-derives
the prelude on every invocation. Bytecode and smarter codegen attack all of this;
heap-representation changes (generational GC, persistent blocks, flat closures) did
not.

---

## 3. Principles (non-negotiable)

1. **Syntactic conservatism.** The surface syntax stays as it is. New machinery
   lives in metadata, symbols, strings, blocks, operators, or idiot-bracket pairs —
   never in new keywords, statement forms, lambda arrows, or `let … in`. **Generality
   comes from the runtime and the libraries, never from new surface syntax.**
2. **Structural over nominal.** No named classes, no `implements`, no nominal types.
   Constraints reference shapes and functions, not names.
3. **Gradual and inference-first — and checking is cheap enough to default on.**
   Types are advisory; a feature must deliver value when only the prelude is
   annotated. Type-checking adds no measurable cost over the front-end already paid
   (verified: `eu check` ≈ `eu eval`), so it runs by **default**, emitting warnings
   that never affect output or exit code.
4. **Single-threaded lazy-pure runtime.** Evaluation is lazy, pure and
   single-threaded over an `UnsafeCell` heap whose soundness depends on
   stop-the-world access (`src/eval/memory/heap.rs:8`). IO is an explicit monad
   interpreted by the driver. The heap stays single-threaded; **parallelism is
   process-level and isolated** (Pillar PP), never shared-memory mutation.
5. **Tool-first centre of gravity, no false ceiling.** Eucalypt generates,
   templates and transforms structured data before it is anything else — but it is a
   real language and must extend cleanly into general computation. We remove
   artificial limits rather than market them.

---

## 4. Decisions (settled)

These are resolved, with their reasoning, so they are not re-litigated.

### 4.1 Versioning & stability (shipped in 0.8, unchanged)

Real semver against eucalypt's real API (syntax + prelude + rendered output), with
the CI build number in `+build.N` metadata. 1.0 commits to an enumerated stable
surface in tiers — **Stable** (syntax, prelude v1, block semantics, CLI,
import/export formats, embedding API), **Experimental** (the type-annotation DSL and
checker), **Not covered** (internal IR/STG/GC/`dump`, error prose). The `requires`
guard enforces version ranges read-side; the prelude evolves via an opt-in **v2**
coexisting with a frozen **v1**; a deprecation lifecycle retires prelude functions
in order. The deep-merge-as-default change ships as a MAJOR or behind v2, never a
silent flip.

### 4.2 Boundary soundness — decided, and it gates type-directed codegen

The gradual boundary stays **advisory and optimistic**: an `any` value flowing into
a typed position is trusted at runtime — no cast, no proxy, no blame — and types are
erased before execution. Sound/guarded gradual typing is declined (Takikawa et al.
POPL 2016: up to ~100× from per-boundary proxies; eucalypt is AOT and has no JIT to
recover it). The blessed static contract is **`eu check --strict`** as the CI gate.

**The consequence for codegen (Pillar CG):** a type-directed optimisation may fire
**only on a `Synthesised` type — one proven from code — never on a type merely
`Trusted` across an `any`.** Under a naïve unboxed path a lying `any` would
reinterpret a string pointer as an integer: memory corruption, not a type error. So
type-gated optimisation carries a provenance bit and restricts to synthesis, with an
optional, off-by-default `--strict-boundary` test mode inserting shallow checks at
annotated ingress. This is decided now so CG's type-gated tier is unblocked.

### 4.3 The string type-DSL and the `s"…"` value surface

Type annotations remain strings in `type:` metadata, parsed by a dedicated DSL
(`src/core/typecheck/parse.rs`); no reserved type bracket is added. Where a type
must be referenced in *value* context, the **`s"…"` string-prefix** is the surface:
it produces first-class **type-data**, kept deliberately distinct from a bare
symbol. This is promoted to a near-term item (Pillar SV) because it is the keystone
of the validation/schema differentiation.

### 4.4 Ad-hoc polymorphism — "typeclasses without classes" (shipped)

Operator/overload constraints reference shapes and functions, not names (`<(a, a)`
= "there exists a `<` at this shape"). Shipped in 0.7.0; this is the whole
mechanism — there is no nominal typeclass system coming.

### 4.5 Runtime performance is won in compilation, not heap representation

This supersedes the previous "generational GC + persistent blocks" performance
plan. The evidence (§2, §10) is that heap-representation rewrites did not pay and
that the cost lives in code materialisation, dispatch and env-walk. Therefore:

- **A bytecode VM is the spine of runtime performance** (Pillar BV): flat code in a
  non-GC arena (code leaves the scanned heap), register frames (env-walk), side
  tables (annotation dispatch), superinstructions (hot patterns), and
  serialisability (the startup win). Estimated 3–10× on dispatch-bound code and a
  large drop in GC load, keeping the Immix GC, all intrinsics, the emitter, the
  error machinery and the `EU_*` tooling. **Status (BV1, eu-enyv):** the bytecode
  engine is now the **default**; the legacy HeapSyn tree-walk machine is retained
  behind `EU_HEAPSYN=1` as the perf baseline and differential-testing engine. The
  Phase 4 collapse (deleting HeapSyn + retiring `GcScannable`) is deferred pending
  an A/B perf study.
- **Demand- and type-directed compilation rides on it** (Pillar CG): the bytecode is
  the substrate the smarter codegen emits into.
- **The speculative generational-GC rebuild is shelved** (§10), to be revisited only
  when a real memory-pressure workload exhibits the regime it targets. **Persistent
  O(log n) blocks are retained** as a forward item (Pillar DS) — eucalypt is an
  intensely merge-heavy language and the case is structural, not workload-of-the-day —
  but sequenced *after* bytecode (which removes the code-as-scanned-data churn and
  makes the GC-native node approach cheap) and built GC-native to avoid the ADR-001
  finalisation leak.

### 4.6 1.0 is a milestone, not a feature bucket

**No feature is scheduled *for* 1.0.** Every capability in this plan ships in an
ordinary **point release** (0.11, 0.12, …) on the existing high-cadence
continuous-delivery model. 1.0 is the *milestone we reach* once the point releases
have delivered the surface and it has been proven — the moment we **decide we are
going for 1.0**: ratify and freeze the stable-surface tiers (§4.1), turn on the
version contract, declare the commitment. It carries gates (a complete surface, W5
conformance green, the deprecation lifecycle exercised), not features.

A corollary: the **freeze is gated on the surface, not on the engine.** Bytecode is a
multi-release programme (§6.4) that changes *no observable semantics* — output is
byte-identical across engines, which is exactly what the conformance corpus (W5)
proves — so the bytecode programme may still be mid-flight when 1.0 is declared. We
neither block the freeze on the rewrite nor declare 1.0 *because* the rewrite is done.

### 4.7 WASM is a distribution target, not an execution engine

The STG→WASM feasibility study (`docs/development/stg-compilation-targets-feasibility.md`)
is settled: compiling to WASM as an *execution engine* attacks the wrong bottleneck
at the wrong price. Code generation is the easy 20%; the hard 80% is a second
runtime system (lazy object model, a GC story that either rebuilds a linear-memory
RTS or discards the Immix investment, a bridge across 192 intrinsics, the per-scalar
streaming render path), with Wasmtime codegen + instantiation likely making small
runs *slower* than today's 85 ms — an estimated 18–36 months to parity behind a long
dual-engine migration. The durable WASM opportunity is **distribution**: sandboxed
WASI components and playground acceleration, revisited *after* the bytecode exists to
compile from. Post-1.0 candidate (§10 records the analysis).

---

## 5. Non-goals (won't-do)

- **A package registry.** Distribution is met with **git + GitHub/GitLab** alone —
  content-addressed imports, an in-language manifest, version selection (W18).
- **Rust-style editions.** The `requires` guard and opt-in prelude v2 cover the real
  need without forcing the desugarer to carry every historical semantic path.
- **Capability *types*.** The hermetic/determinism *mode* (W17) ships; a capability
  *type system* does not.
- **Algebraic subtyping / MLsub core swap.** Collides with the shipped HKT keystone;
  reassess only if the hand-rolled bidirectional core visibly creaks.
- **Nominal types / classes.** Stay structural (Principle 2).
- **Sound/guarded gradual typing** and an always-on runtime type check (§4.2).
- **WASM as an execution engine** (§4.7) — distribution only, and only post-bytecode.
- **A speculative generational-GC rebuild** — shelved (§4.5, §10); revisit only on a
  demonstrated memory-pressure workload. (Persistent O(log n) blocks are *not* a
  non-goal — they are retained as Pillar DS, sequenced after bytecode.)
- **Shared-memory parallelism / a `Send`+`Sync` heap.** Parallelism is process-level
  and isolated (Principle 4, Pillar PP).

---

## 6. The shape of the plan

### 6.1 The eight pillars

| Pillar | Theme | The shared thing |
|---|---|---|
| **BV — Bytecode VM** | A flat, serialisable, arena-resident execution form | Code leaves the GC heap; dispatch, frames and annotations are restructured; the program becomes serialisable bytes |
| **CG — Code generation** | Demand- and type-directed core→STG/bytecode | One demand/type analysis decides thunk-vs-value, direct dispatch, key resolution, unboxing and selective lifting |
| **TY — Typing default-on** | The checker earns its keep | Cheap, quiet checking made the default; the forcing function that hardens the checker toward CG's type-gated tier |
| **SV — Type-value surface** | `s"…"` type-data → validation, optional fields, prefix-lists, schema | Types become ordinary values; one type-data source is validated, generated, coerced, defaulted, documented and exported |
| **DS — Block & value model** | Persistent O(log n) blocks; arbitrary-value `vec` | The merge-heavy core data structure made sub-linear with structural sharing, GC-native, on the bytecode-reduced churn |
| **PP — Process parallelism** | Isolated forked workers, data-only boundary | Purity + IO-isolation make a scatter/gather `par-map`/`par-fold` safe; an mmap arena is the transport and coordination layer |
| **EF — Effects & IO** | Composing monads; native filesystem/IO capabilities | One effect story so state+random+IO compose, and native `walk`/`read`/`stat` so filesystem trees are practical (Principle 5) |
| **EC — Ecosystem & surface** | Modules, interactive surface, reproducibility, conformance | The cross-unit interface, the cache, and the proof corpus that let the surface be completed and frozen |

### 6.2 The dependency spine (`→` = "must precede")

- **BV0** (encoding + ceiling spike) **→ BV1** (threaded interp) **→ BV5** (serialised
  prelude), **BV2** (side tables), **BV3** (register frames), **BV4** (superinstructions).
- **CG (type-free tier)** is independent of BV and helps the current VM today; **CG
  (type-gated tier)** needs §4.2 (decided) and lands cleanest on BV.
- **BV3** (register frames) and **CG selective-lifting** share one escape/demand
  analysis — sequence them together.
- **TY** is independent; it is the forcing function that matures the checker CG's
  type-gated tier depends on.
- **BV5** (serialisable bytecode) makes **PP** workers cheapest to spawn, and is the
  natural successor to the existing `ArenaStgSyn`/postcard blob.
- **Per-unit incremental caching** (**CU2**, the successor to the whole-program
  **BV5-cache**) is gated on **separate unit compilation** (**CU1**): today the
  pipeline merges units immediately after per-unit desugar, so everything downstream
  is whole-program. Restructuring to defer the merge (and replay the cross-unit
  `UnitInterface` state) is a pillar-scale prerequisite (**CU1**, eu-nzps) — sequence
  it with **W18** (modules), which wants the same per-unit boundary.
- **SV** is independent of the runtime work; **optional fields** and the
  **prefix-list type** are type-vocabulary enrichments it surfaces; **W8/W22**
  (doc/schema) consume the same type-data.
- **DS** (persistent blocks) is sequenced **after BV** — bytecode removes the
  code-as-scanned-data churn and makes the GC-native CHAMP/RRB nodes cheap; it shares
  BV's GC-scannable-array machinery. Independent of SV/CG.
- **EF** is largely independent: **EF1** (effect composition) is prelude-level for its
  near-term steps; **EF2** (native filesystem IO) is intrinsics + prelude, and its
  *streaming* form composes with DS and the streaming candidate; the algebraic
  effect-rows end-state (EF1.3) is post-1.0.
- **EC**: **W18** (modules) builds on the shipped Unit Interface + git imports;
  **W19** (watch/REPL) builds on the cache + BV5 startup; **W5** (conformance) is the
  proof that BV changes nothing observable.

### 6.3 Release mapping

All features ship in **point releases**; 1.0 is a milestone, not a bucket (§4.6).
The near releases are concrete; the later ones are a likely grouping, freely
reordered as the cadence dictates.

| Release | Theme | Items |
|---|---|---|
| **0.11** | Codegen wins, typing on, type-value foundation, bytecode spike | CG type-free tier; TY default-on; SV `s"…"` + `as-spec`; **optional record fields**; **BV0** gate |
| **0.12** *(shipped scope)* | The bytecode core + the startup win | **BV1** (default engine, code out of the heap; Phase-4 collapse deferred) + **BV5 embedded prelude** (dual-form blob, deterministic; startup win on release binaries). *Unit cache spec'd + held (eu-lb0r)* |
| **0.12.1** | Close the engine gap | **BV4** superinstructions / decode-cost fusion (eu-9mvh) + `ExecutionError` boxing (eu-adnu); bytecode block index (eu-4zhi). *Gap-close only — the Phase-4 collapse it unlocks lands in 0.13.* |
| **0.13** | Frames, annotations, type-gated codegen, prefix-lists | **Phase-4 collapse** — retire HeapSyn once at parity (eu-oufc); **BV5-cache** whole-program unit cache (eu-lb0r); **BV2** side tables; **BV3** register frames + CG selective lifting; **CG** type-gated (unboxing); **prefix-list type** |
| **0.14** | Polish, parallelism, effects, contracts | **PP** `par-map`/`par-fold`; **EF2** native filesystem IO + **EF1** effect-composition combinators; **W16** contracts; presence inference |
| **0.15+** | Value model, ecosystem, surface | **DS** persistent blocks + `vec`; **EF1** unified effect context; **W18** modules + **CU1** separate-unit compilation; **W19** watch/REPL + **CU2** per-unit incremental cache; **W17** hermetic; **W22** schema interop |
| **1.0** *(milestone)* | Decide, prove, freeze | *No features.* Surface complete + **W5** conformance green → ratify and freeze the stable-surface tiers, turn on the version contract (§4.6) |
| **post-1.0** | Curated bets | EF1.3 algebraic effect-rows; WASM-as-distribution; parallel Model B (maybe-never); a true separate-nursery GC *iff* a workload demands it |

### 6.4 The bytecode programme is multi-version — phased so each step ships value

Bytecode is the largest single effort in this plan and the highest-risk (it
rewrites the hottest, most delicate code). It is sequenced so each phase is
independently shippable and de-risks the next, and so the most user-visible win (the
startup floor) is banked early:

1. **BV0 — encoding + ceiling spike (a gate, not a commitment).** Define a flat
   bytecode over `StgSyn` building on the existing `ArenaStgSyn` arena form; encode
   just enough to run `day11-p1` (the 81%-dispatch microbench) end-to-end on a
   minimal threaded interpreter; **measure the dispatch ceiling**. Proceed only if
   ≥~2×; otherwise reassess. Weeks, low commitment.
2. **BV1 — threaded-code interpreter, code out of the GC heap.** Replace the
   `HeapSyn` tree-walk (`vm.rs:389`) with opcode dispatch over arena-resident
   bytecode; keep continuations, env frames and the intrinsic boundary intact to
   isolate variables. The big correctness lift; full harness green under
   `EU_GC_VERIFY=2`. **Shipped in 0.12 as the default engine** — on the default
   path code never enters the GC heap. The *physical* retirement of
   `impl GcScannable for HeapSyn` (deleting the HeapSyn machine + loader) is the
   **Phase-4 collapse, deferred** (eu-oufc; see §4.5): HeapSyn is retained behind
   `EU_HEAPSYN=1` as the perf baseline and differential reference until the
   decode-cost gap closes (eu-9mvh).
3. **BV5 — serialisation & embedded prelude.** Execute directly from
   serialised bytecode; embed the prelude bytecode at build time. Reuses the
   postcard/`cfg(prelude_blob_ok)` plumbing. **Shipped in 0.12 in dual-form** — the
   blob carries the pre-encoded `BytecodeProgram` *alongside* the `ArenaStgSyn`
   heap-graph form (not superseding it) while HeapSyn is retained; the bytecode
   engine executes the embedded program without per-run re-encoding, and blob
   generation is deterministic and CI-guarded. The startup floor (~10 ms measured)
   lands on **release/CI binaries** (which run `xtask prelude-compile`); a plain dev
   `cargo build` still falls back to source-prelude. The **content-hash
   whole-program unit cache did not ship** — spec + plan are merged
   (`docs/superpowers/specs/2026-07-03-bv5-unit-cache-design.md`, `#953`); it is a
   separate deliverable (**BV5-cache**, §9, 0.13, eu-lb0r), and the *per-unit
   incremental* cache is a further step behind the defer-merge restructure
   (**CU1/CU2**, §7, eu-nzps/eu-y7le).
4. **BV2 — side tables for annotations.** Move `Smid`s out of the instruction stream
   (retire the `Ann` dispatch step, `vm.rs:536`) into offset-keyed side tables the
   error machinery reads. Additive, low risk.
5. **BV3 — register frames.** Replace cactus-env churn (`env.rs:191`, `from_let`
   `vm.rs:525`) with flat register frames for the common case, spilling to heap
   frames only where a closure genuinely escapes. This is the structural env-walk fix
   — and the place the reverted flat-closure idea (§10) **correctly relocates**: a
   *selective, compiler-decided* transform on hot deep captures, not a universal
   runtime representation. Shares CG's escape analysis. High value, high risk.
6. **BV4 — superinstructions.** Profile-guided fusion of the boxing-heavy hot
   sequences (force-then-case, alloc-thunk-update, literal arithmetic). Additive and
   iterative.

---

## 7. The work

### Pillar BV — Bytecode VM

The six phases are specified in §6.4; this is the engineering detail and the forks a
multi-release programme must settle up front.

**The transition, concretely.** Today: `StgSyn` (`src/eval/stg/syntax.rs:118`, the
`Rc`, off-heap optimisation IR) → `load` (`src/eval/memory/loader.rs:120`) → `HeapSyn`
(`src/eval/memory/syntax.rs:227`, a `NonNull` pointer graph allocated *into the GC
heap every run* and scanned by `impl GcScannable for HeapSyn`, `syntax.rs:400-547`) →
`handle_instruction` tree-walk over it (`src/eval/machine/vm.rs:389`, the match at
`:426`). BV replaces the `HeapSyn` execution IR **and its loader** with **flat
bytecode in a non-GC arena** and an **opcode dispatcher**: a closure's code field
moves from `RefPtr<HeapSyn>` to a bytecode offset (`SynClosure` is the touch point),
`GcScannable for HeapSyn` is retired (code leaves the scanned set), and the loader
stops re-allocating the program each run. The six-kind continuation machine
(`src/eval/machine/cont.rs:34`) and the deferred-BIF intrinsic boundary
(`vm.rs:518,1722`, `intrinsics.rs:49`) stay intact through BV1 to isolate the change.

**Key design decisions (settle at/around BV0):**

1. **Source IR & encoding — decided: a linearised opcode stream** (best for threaded
   dispatch + superinstructions, since dispatch is the target), with **BV0 the
   empirical confirmation**. The existing `ArenaStgSyn`/`PreludeBlob` form
   (`src/eval/stg/blob.rs`, postcard, index-referenced) is proto-bytecode but a *node
   pool*, not a linear stream; the encoder builds *from* `StgSyn`/`ArenaStgSyn`, reusing
   that arena machinery. (Fallback if BV0 disappoints: a flattened-node interpreter —
   lower risk, smaller win.)
2. **Constant/heap split.** Code lives in the non-GC arena; the **values it references**
   (interned symbols, `HeapString` literals) must stay GC-/pool-managed. Decide the
   constants-pool boundary so no GC scan ever touches code and no arena offset is ever
   moved by evacuation.
3. **Continuations & intrinsics — unchanged in BV1.** Keep the off-heap
   `Vec<Continuation>` and the `pending_bif` mechanism; bytecode is about *code
   representation*, not the 192 intrinsics or the control model. Revisit folding control
   flow into the stream only post-BV1.
4. **Frame model — deferred to BV3, decided: selective.** BV1 keeps the cactus
   `EnvFrame` (`src/eval/machine/env.rs:191`); BV3 introduces **register frames** for
   *only the hot/deep captures CG's escape analysis flags*, not all frames — the lesson
   of the flat-closure revert (§10), where a universal capture tax sank the win. The
   selectivity threshold is profile-tuned. **The sharpest motivating case is
   higher-order recursion:** `foldl(op, …)` is **O(n²)** purely because the *local* `op`
   is re-resolved through the env-walk every step — measured, a higher-order fold runs
   13.5M → 52M → 204M ticks at N = 5k/10k/20k (O(n²)), while the *identical* recursion
   with a hard-coded **global** `+` is O(n) (945K → 1.9M → 3.8M), since globals resolve
   in O(1) with no env-walk. Register frames make local resolution O(1), turning
   higher-order folds linear. (Strictness is *orthogonal* — a strict and a lazy
   accumulator scale identically; only first-order-vs-higher-order matters.)
5. **One serialisation format, three consumers.** BV5's bytecode-bytes format
   (extending the postcard arena form) is reused by the **embedded prelude**, the
   **content-hash unit cache**, and **PP's IPC wire payload** (Pillar PP) — design it
   once to serve all three.

**BV0 — the gate, made actionable.** Encode the minimal opcode subset `day11-p1` needs
(`Atom`/`Case`/`Cons`/`App`/`Bif`/`Let` + the six continuations) from its `StgSyn`; run
it on a threaded-dispatch loop; measure dispatch throughput against the current
`handle_instruction` walk on the *same* program (local baseline: 66.5 M ticks, ~all
dispatch, 0 GC). **Go if ≥~2×; reassess otherwise.** Weeks of work, and the encoder
seeds BV1 — nothing is thrown away.

**Success.** BV1: code no longer appears in GC scans; `day11-p1` dispatch improves by
the BV0-measured factor; full harness byte-identical under `EU_GC_VERIFY=2`.
*(Realised, 0.12: harness byte-identical ✓ and code off the scan set on the default
path ✓, but the dispatch criterion landed at **parity** (day11-p1 bc/hs ≈ 0.93–0.97),
not a multiple — the leaner dispatch is offset by instruction-stream decode
(`read_*`, ~40% of CPU on call-dense code) and per-instruction `ExecutionError` drops.
The measured verdict — see `docs/superpowers/reports/2026-07-03-bytecode-vs-heapsyn-ab-rerun.md`
— is a competitive co-engine: parity-or-faster on pure-dispatch and env-walk
workloads (higher-order `count` 0.66×, `foldl` 0.89×), 1.2–2.05× behind on
allocation+call-dense compute. Closing that gap is eu-9mvh/eu-adnu and the BV4
fusion work, and gates the Phase-4 collapse.)* BV5:
`eu -e true` startup floor drops to ~20–30 ms on a plain build with no separate blob
step. *(Realised, 0.12: ~10 ms measured — but on release/CI binaries only, where
`xtask prelude-compile` embeds the blob; a plain dev `cargo build` still runs
source-prelude. The "no separate blob step for every build" promise is open — it
needs either a committed blob (rejected: reproducibility) or a two-stage build.)*
BV3: `EnvironmentFrame::get` falls out of the top of the AoC profiles, **and a
higher-order fold `range(0,N) foldl((_+_),0)` scales linearly** — the cleanest
regression benchmark for the env-walk line (it is O(n²) today purely from local-`op`
resolution; the first-order equivalent is already O(n)). Every phase: rendered output
byte-identical across the conformance corpus (W5).

### Pillar CG — Demand- and type-directed compilation

**Problem.** The engine pays for dispatch and boxing it can often avoid. Every call
goes through generic apply even when the callee is statically known; literal-key
block lookups intern a symbol and hash it in the hot loop (`day11-p2`); lazy
accumulators build O(n) `Update` chains (`day01` reaches stack depth 28,626); every
`+` pays a tag-`case` wrapper even where `number → number → number` was proven. The
demand infrastructure exists (`Demand` on every `CoreBinding`, `core/binding.rs:9`;
`take_lambda_form` consulting it, `compiler.rs:540`; `Seq` strict eager eval,
`compiler.rs:756`) and the strictness half already paid (−76% on folds, 0.10.0).

**Design — two tiers.** A core→STG/bytecode optimisation family driven by the demand
annotation and (for the second tier) erased type facts threaded as a node-keyed
side-table with a `Synthesised`/`Trusted` provenance bit (§4.2).

- **Type-free tier (independent of BV, helps the current VM now):**
  - **CG1 Known-call direct dispatch** — a statically-known callee compiles to a
    direct enter, bypassing generic apply. Attacks the 60–81% dispatch cost.
  - **CG2 Literal-key resolution** — `.key` / `lookup(:k)` with a literal key resolves
    to a pre-interned symbol/offset at compile time, removing intern + sip-hash from
    hot loops.
  - **CG3 Strict accumulators (worker/wrapper)** — extend demand from prelude
    signatures to discovered user recursion; emit strict-spine folds, collapsing the
    `Update`-frame chains a lazy left fold accumulates.
  - **CG4 Selective lambda-lifting / pre-projection** — demand/escape analysis lifts
    hot deep captures into explicit arguments. Feeds BV3 register frames; this is the
    sound, targeted form of the reverted flat-closure idea. **This — not CG3 — is what
    addresses the higher-order-fold O(n²)** (§6.4 item 4): the cost is re-resolving the
    local `op` through the env-walk, so lifting/flattening that capture (or BV3's
    register frames) makes it O(1); a strict accumulator (CG3) does *not* help.
- **Type-gated tier (needs §4.2 — decided; lands cleanest on BV):**
  - **CG5 Unboxing + direct intrinsic dispatch** — on a `Synthesised` numeric type,
    take the unboxed `Native::Num` path and skip the tag-`case`. Plus dead-branch
    elimination on literal types and interpolation specialisation (the most pervasive
    generation win). Fires only on proven-concrete types; a single optimised node
    meeting a mis-typed operand would be a memory-safety bug, so synthesis-only is
    mandatory.

**The type-gated mechanism (how facts survive erasure).** The checker is advisory and
its in-tree annotations are erased before STG compile (§4.2) — so banking a type fact
cannot mean *keeping the annotation*. Instead the checker emits a **node-keyed
side-table**: for each actionable core node it records the synthesised type plus a
**provenance bit** — `Synthesised` (proven from the code) vs `Trusted` (rests on an
annotation that crossed an `any`). The *table* survives into the inliner/specialiser
even though the tree's types are gone. CG5 consults it and fires **only where both the
callee's signature and its arguments are `Synthesised`-concrete**; a `Trusted` fact
never licenses unboxing, because under the unboxed path a lying `any` would reinterpret
e.g. a string pointer as an integer — memory corruption, not a type error (§4.2). The
target is concrete: `+` compiles to a tag-`case` wrapper and `Add::execute`
(`src/eval/stg/arith.rs`) re-checks operand kinds at runtime even where the checker
proved `number → number → number`; the STG already has the unboxed `Native::Num`
representation, so what CG5 adds is *permission, from a `Synthesised` type, to take the
direct path*. It is purely additive — the boxed wrappers remain for every unspecialised
call — and it is the high-risk surface that makes the whole tier a careful, profile-gated
effort (kill switch: if real render workloads show <2×, generation being block/string-
heavy rather than arithmetic-heavy, it is shelved).

**Success.** Measurable dispatch reduction on `day11`; literal-key hot loops shed the
intern/hash cost; `day01`-class folds show flat stack depth under `EU_STACK_DIAG=1`
with no `suppress_update`; unboxed arithmetic shows the expected speedup on
`Synthesised` numeric code with zero effect on output; no harness regression.

### Pillar TY — Typing default-on

**Problem.** The checker is complete but advisory and *erased*: it banks none of its
value, and most users never run it. Yet it is cheap (rides the prelude type cache;
`eu check` ≈ `eu eval`, verified) and quiet on real code (0 warnings across the AoC
corpus; 56/60 sampled harness files clean), and it is genuinely useful (it catches
`[1,2,3] str.split-on(",")` precisely, though it still misses `1 + "hello"`).

**Design.** Make checking the **default** for every run: warnings to stderr that
never affect output or exit code, with a `--no-check` escape and a measured
per-invocation latency budget (the config centre of gravity is latency-sensitive).
The type system stays Experimental (§4.1) — but its *warning surface* becomes
default-visible, so warning stability and prose matter more. Two steps: **(1)** a
warning inventory across harness + AoC + conform, triaging the handful of existing
warnings to confirm they are intentional, not false positives; **(2)** flip the
default. Default-on is itself the forcing function that surfaces and closes checker
gaps (e.g. `1 + "hello"`), maturing the checker toward CG's type-gated tier.

**Success.** Checking on by default with no measurable regression on the config
path; zero spurious warnings on the harness and AoC corpus; the gaps default-on
surfaces become tracked checker fixes.

### Pillar SV — The type-value surface: `s"…"`, validation & schema

This is the data-language differentiation no peer combines (gradual structural types
+ metadata extensibility + pay-as-you-go validation), and it is more than a
validation onramp. It is independent of the runtime work, design-heavy and low-risk.

**The aspiration — types as ordinary values, one source many uses.** Today a type
lives only as a string in `type:` metadata: it is checked, then erased. The
**`s"…"` string-prefix** (§4.3) brings a type into *value* context as first-class
**type-data** — a value you can bind, store in a block, compute with, and pass to a
function. That one move turns the type system from a check-and-erase tool into a
**data vocabulary** with many consumers fed from a single source, with a runtime
**spec value** as the hub:

- **validate** — `as-spec` lowers type-data into a spec over the `match?` predicate
  vocabulary; apply it at ingress for structured, located blame (the validation core).
- **generate** — the same spec drives example/fixture/mock generation (property-test
  seeds, golden inputs) — the Clojure-spec conform-and-gen duality.
- **coerce / parse** — type-directed parsing at ingress: `bytes parse-as(s"…")`.
- **default-fill** — type-data carrying field defaults completes a partial block
  (ties directly to optional fields, below).
- **describe** — `eu doc` renders type-data to human docs and to JSON Schema; an
  ingested external schema (incl. a Kubernetes CRD) becomes type-data → a contract.
- **reflect** — `typeof(value)` returns type-data, so types become comparable,
  decomposable and queryable at runtime.

`s"…"` is therefore the wedge toward the long-standing "types as first-class values"
aspiration. The type-DSL stays the authoring surface (no reserved bracket, §4.3);
`s"…"` is its value-context dual; the spec is the runtime projection. The near-term
core is `s"…"` + `as-spec` + ingress validation + optional fields; **generation and
full reflection are the aspirational end-state**, pursued once the vocabulary proves
out — but the design of `s"…"`/type-data should be chosen so they are reachable, not
foreclosed. (Its mirror image is **code-as-data**: the AST embedding that renders
eucalypt *source* from eucalypt data — Pillar EC, EC-embed. Types-as-data and
code-as-data are the two halves of eucalypt's reflective surface.)

**The work:**

- **SV1 `s"…"` value semantics (the surface is already lexed).** `s"…"` already
  tokenises as `S_STRING` (`src/syntax/rowan/lex.rs`, *"type-data literal — no
  interpolation, no escape processing"*) and is used today only by `monad:` element-type
  hints, which read its *raw text*; in value position it currently evaluates to a plain
  string. The work is to give it **type-data value semantics**, decided as follows:
  - **Opaque value, rendering as its canonical type-DSL string.** `s"…"` parses (via the
    type-DSL, `src/core/typecheck/parse.rs`) at *compile time* and reifies an **opaque
    type-data value** — `t"…"` is the exact precedent (a zdt native that renders as ISO).
    It renders as its source string, so existing output is unchanged. No checker type
    reaches runtime (§4.2): it is a data value that *encodes* a type.
  - **Clean single-brace records — the point of the surface.** Inside `s"…"`, records use
    plain `s"{ name: String, age: Number }"` — **no interpolation, no `{{…}}` escaping**.
    This is deliberately cleaner than the `type:` metadata form (which needs `{{…}}` to
    coexist with interpolation); the type-DSL parser gains a single-brace mode for
    `s"…"` content.
  - **A simple `to-data` projection — the Type embedding.** Type-data is opaque but
    trivially openable: `to-data` projects it to an ordinary **tagged-list** structure
    and `from-data` (a prelude builder function, uniform with `to-data` and kept at the
    data level) rebuilds it — e.g. `s"[String]" to-data` → `[:t-list [:t-prim :string]]`,
    `s"A | B"` → `[:t-union …]`. Tagged-list (not block) for terseness and **uniformity
    with the existing embeddings**: this is the **Type embedding** with a `t-*` vocabulary
    that mirrors the checker's type constructors **1:1** (`src/core/typecheck/types.rs` —
    `t-prim`/`t-list`/`t-record`/`t-union`/`t-fn`/`t-forall`/`t-app`/`t-con`/`t-mu`…),
    completing the family — AST `a-*`, Core `c-*`, STG `s-*`, **Type `t-*`** — all
    round-trippable data projections of an internal IR (ties to EC-embed).
- **SV2 `as-spec` / `to-spec`.** Lower type-data into a runtime spec speaking the
  `match?` predicate vocabulary; consumers auto-lower a type via `to-spec`. Prelude
  work on `match?` and the predicate intrinsics, consuming the `to-data` projection.
- **SV3 Structural contracts & runtime validation (W16).** Apply specs explicitly at
  data ingress (`parse-as`/import sites and user checkpoints), with cost paid only
  where written — the runtime dual of the optimistic erased boundary (§4.2)
  (Findler–Felleisen; Clojure spec; Nickel contracts).
- **SV4 `eu doc` schema extraction / schema interop (W8/W22).** `eu doc` reads the
  `s"…"` surface to emit schemas; JSON Schema export; ingest of external schemas
  (incl. CRDs, which need optional fields) as contracts.

**Backward compatibility.** The `s"…"` *surface* is already shipped, so there is no
lexer collision to manage. The only behavioural change is value-position semantics
(today a plain string → type-data), and its blast radius is tiny: the live uses are
`monad:` hints that read raw text and are unaffected, and `s"…"` sits in the
**Experimental** tier (§4.1), where evaluation may change in a MINOR with a changelog
entry. Rendering BC is preserved by construction (type-data renders as its source
string). The **`t-*` projection schema is the versioned surface** — documented and
tolerant of additive growth — while the type-DSL grammar and the opaque value's
internals stay Experimental and free to evolve; the projection schema freezes when the
type representation stabilises (the embed-format discipline: document changes, keep
old projections readable).

#### SV — Type-vocabulary enrichments (the type representation itself)

Two missing pieces of the type vocabulary, surfaced through `s"…"` and consumed by
every spec use above. Both are type-checker work (`src/core/typecheck/`), distinct
from the runtime pillars.

- **Optional (presence-annotated) record fields — promoted.** Record types are
  required-keys-only today (`src/core/typecheck/types.rs`); a missing required field
  is a warning. Real config — and the downstream eucalypt-grove work where this keeps
  recurring — needs a field that *may be absent*: a key-side **`name?: T`** slot,
  kept distinct from the value-side `T?`/`Partial` (a present-but-partial *value*).
  A record type partitions into required and optional fields; presence subtyping makes
  a record with the optional field present a subtype of one where it is optional;
  `match?`/`me?` gain an optional arm. The annotated form ships near-term (0.11);
  presence *inference* follows. It composes with default-fill (a spec can supply the
  value for an absent optional field) and is a hard prerequisite for honest schema
  import. **Priority raised** from the previous plan on the strength of repeated
  real-world need.
- **Prefix-list type — new.** A list with a **fixed-shape prefix followed by a
  variable-length homogeneous tail** — `[A, B, C…]` meaning "an `A`, then a `B`, then
  zero or more `C`". The type DSL has fixed tuples (`(A, B)`) and homogeneous lists
  (`List(T)`) but nothing between them, so the canonical hiccup/markup element —
  `[tag, attrs, …content]` = `[Symbol, Block, (String | Element)…]`, the shape
  `lib/markup.eu` is built on (`tag = head`, `attrs = second`, `content = _ tail
  tail`) — cannot be typed. Add a rest-element form to the type DSL (the `…` tail,
  cf. TypeScript `[string, number, ...boolean[]]`), with subtyping and indexed-access
  rules (`head`/`second`/`tail` project the prefix precisely, the tail homogeneously).
  Flows through validation/generation/schema unchanged.

**Success.** `s"{ name: String }"` parses with clean single braces and renders as
its source string; `to-data`/builder round-trips type-data through the `t-*`
tagged-list form; a user validates an imported manifest against a spec derived from a
`type:` annotation and gets a precise, located failure; the same spec serves `match?`,
fills defaults, and exports to JSON Schema; optional fields and the markup prefix-list
shape are both expressible and check correctly.

### Pillar DS — Block & value model

**Problem.** Blocks — the thing eucalypt exists to produce, and merge — are cons-lists
with **O(n) lookup** (`src/eval/stg/block.rs`), and **merge is catenation**:
`merge`/`deep-merge`/`<<` walk both operands and re-emit a fresh spine with no
structural sharing. Eucalypt is an *intensely merge-heavy* language — layered config
is merge upon merge — so this is a structural cost of the core use case, independent
of any one workload. The empirical AoC corpus does not exercise it (those programs are
compute-heavy, not merge-heavy), which is why it was wrongly de-prioritised in the
GC-era plan; the merge-heavy case is the *config* case, the centre of gravity.

**Why it stalled before, and why bytecode unblocks it.** A prior persistent O(log n)
attempt (`im_rc::OrdMap`) was reverted (ADR-001): its `Rc` nodes lived on the Rust
heap outside the bump allocator, which recycles by overwrite and never runs `Drop`,
so the nodes leaked — 220–580% GC-churn regressions. The wall was **GC finalisation**,
not block design. Bytecode (BV) changes the calculus twice: it takes *code* out of the
scanned heap (cutting the churn baseline these structures were charged against), and
its arena/`GcScannable`-array machinery is exactly what a GC-native persistent node
needs.

**Design.** Store the persistent structures **inline in the GC heap** as ordinary
scannable objects — no `Rc`, no Rust-heap nodes, no finaliser. A **threshold hybrid**
preserves the common small-block case and insertion order: below ~16 keys a block stays
a plain cons-list (ordered, allocation-light); at/above, two GC-native structures — a
**CHAMP** key-index (`SymbolId → order-slot`) and a persistent **RRB order-sequence**
holding the `(key, value)` pairs in insertion order. Lookup is two O(log n) descents;
render walks the order sequence; override updates one slot; merge shares unchanged
sub-trees, so `<<`/`deep-merge` allocates proportional to the *difference*. Both node
types reuse one `GcScannable` array impl, so the collector surface is small.

- **DS1 Persistent O(log n) blocks** — the CHAMP+RRB hybrid above. Sequenced after
  BV; validated under `EU_GC_VERIFY=2`/`POISON=1`/`STRESS=1`; gated on
  **independently-verified merge-heavy benchmarks** (not microbenchmarks), with the
  ADR-001 non-regression as a hard bar.
- **DS2 Generalise `vec` to arbitrary values** — let the O(1)-indexed sequence hold
  blocks/lists (the array-of-objects shape of CSV/JSON inputs, or a multi-thousand-entry
  filesystem listing from EF2), not just scalars, via
  a GC-traced `Array<Closure>` modelled on `EnvFrame`; also fixes the current
  `Native::Vec` backing leak. Shares DS1's GC-array machinery; otherwise independent.

**Success.** Lookup on a 1,000-key block grows logarithmically; merge of two large
blocks differing in a few keys allocates proportional to the difference; rendered key
order is byte-identical to the cons-list across the conformance corpus; **no GC-churn
regression** (ADR-001 does not recur); `vec` round-trips a list of blocks preserving
order and identity.

### Pillar PP — Process-level parallelism

**The aspiration.** Multi-core for the cases that are *safe by construction*, without
surrendering the single-threaded lazy-pure heap (Principle 4: the heap is a
deliberately non-`Sync` `UnsafeCell` whose soundness rests on stop-the-world access).
Purity makes a parallel merge order-independent and IO-isolation makes effects
independent, so a whole class of work parallelises with **zero single-thread cost and
fully reproducible results** — provided we never try to *share the heap*. The model is
OS processes (forks), not threads. Three rungs of increasing coupling:

1. **Batch.** Run `eu` over many files / many targets at once. A runner concern,
   trivially safe, available first — the embarrassingly-parallel base case.
2. **Data-parallel combinators.** `par-map` / `par-fold`: scatter a list across forked
   workers, each applying a **named, addressable** transform (a binding name resolved
   in both processes — *not* an arbitrary closure, which cannot cross heaps), then
   gather and **merge deterministically**. This is the core surface a user reaches for.
3. **A shared-memory results / work arena.** The specific aspiration: a memory-mapped
   region that collaborating `eu` forks write into and coordinate over, instead of
   funnelling everything through a parent process.

**What the shared arena can and cannot hold — the load-bearing constraint.** It
**cannot hold live heap**: pointers are process-local and the GC moves objects. So the
mmap region is not a shared heap; it is a fast **transport + coordination** layer
carrying:

- **serialised values** — and here PP and BV compound: **BV5's serialisable
  bytecode-value form is the natural wire payload**, so forks exchange eucalypt
  *values* as compact bytes with no re-parse. The serialisation built for the embedded
  prelude doubles as the IPC format.
- **a results board** — pre-sized slots workers fill, gathered when all complete
  (zero-copy, no parent bottleneck).
- **coordination metadata** — a work-claim index (claim-next-chunk counters/flags) for
  dynamic load balancing / work-stealing among peers. Just integers, not heap.

So the end-state is a small set of **deterministic parallel combinators** — `par-map`,
`par-fold`, batch — that "just work" over an mmap arena for the parallelisable slice,
with named transforms as the unit of work and serialised bytes as the only thing that
crosses a process boundary.

**Scope, honestly.** This serves the embarrassingly-parallel slice: independent
`map`/`fold` elements, file batches, and the AoC cases that decompose (day08
independent pair distances, day09-p1 O(n²) pairs, day10-p1 DFS branches). It does
**not** parallelise inherently sequential algorithms (day10-p2 branch-and-bound,
day11-p2 DP — those are CG/BV's job), and it is **not** fine-grained intra-evaluation
parallelism. That last — shared-thunk, shared-heap threads (**Model B**) — needs a
`Send`+`Sync` heap and a parallel collector (a superset of the shelved GC work) and
stays a **maybe-never fork** (§10). PP is always *advisory parallelism, never
observable concurrency*: deterministic merge, composes with hermetic mode (W17).

**Phasing.** Workers reuse the existing `io.exec` subprocess machinery; the data-only
boundary (named transforms, serialised bytes) is implementable before BV, but is
*cheapest and cleanest on BV5's serialisation* — so the combinators can prototype
early and the mmap-arena transport lands once the value-serialisation form exists.

**Success.** >1.5× on a file batch and on one large data-parallel `map` across 4+
cores; **zero** single-threaded cost; identical deterministic results to the
sequential run; the transform unit is something a user reaches for without contortion.

### Pillar EF — Effects & IO: composition and capabilities

Two long-standing rough edges keep real data-gathering programs awkward, and
Principle 5 (no false ceiling) says we fix them rather than wave them off as "outside
the sweet spot."

**EF1 — Composing effects.** Today `io`, `state`, `random`, `let` and `for` are each an
independent `monad{bind, return}` (`lib/prelude.eu:34,162,2155,2163`, `lib/state.eu:55`),
with no transformer or lift *between effects* — `state.lift` only lifts a *pure
function* into state. So a stateful, seeded, IO-driven pipeline (accumulate state while
shelling out, with a controlled PRNG) has no supported path; you hand-roll the
threading. The runtime rules out a heavy transformer zoo (Principle 4: single-threaded
lazy-pure; IO interpreted by the driver). Staged direction:

1. **Composition combinators (near-term, prelude-level).** `state` and `random` are
   *pure value-threading* monads, so they already run inside any context — what is
   missing is glue: `lift`/interop to run a `state` or `random` computation inside an
   IO pipeline and thread its result, plus `for`-comprehension across them. No core
   change.
2. **A unified effect context.** Because `state` (thread a state block) and `random`
   (thread a seed) are both pure value-threading and only IO is driver-interpreted, one
   effect monad can carry `(state, seed)` alongside IO — an RWS+IO-shaped context the
   existing namespaces become operations *within*. More eucalypt-idiomatic (one
   structural monad) than a transformer stack.
3. **Algebraic effect-rows (the long arc).** The general, typed answer — typed effects
   beyond the IO monad (promoted from a deferred candidate). Heavier type machinery; the
   end-state, not the near-term step.

**EF2 — Native filesystem / IO capabilities.** The entire IO surface is `io.shell` /
`io.exec` plus env/args/epoch (`lib/prelude.eu:34-78`) — there is **no** `io.read-file`,
`io.ls`, `io.walk` or `io.stat`. So gathering a filesystem tree means shelling out
(`ls`/`find`), receiving one large stdout *string*, splitting it, and rebuilding a
cons-list. **This is a capability and ergonomics gap, not a performance blocker** —
profiling shows the shell call and the `vec.of` build are cheap (the slowness one hits
aggregating the result is a separate `foldl` defect, handled as a bug fix, not here).
Native `io.read-file`, `io.walk`/`io.ls` (a directory tree → structured data directly),
`io.stat` and `io.glob` are still worth it: they remove the subprocess + string
round-trip, give a typed structured result instead of line-parsing, and can be
**lazy/streamable** so a large tree need not fully materialise (ties to DS and the
streaming candidate).

**Why this belongs (not a sweet-spot exception).** The data sweet spot is about the
*shape* of the work, not a ceiling on input source or scale (Principle 5). Gathering and
transforming filesystem trees is squarely a data-tool job; that it is currently
impractical is a gap, not a boundary. The scaling half — thousands of strings/blocks
held at once — is attacked by DS (arbitrary-value `vec`, persistent blocks), BV (startup,
dispatch, GC load) and streaming; EF2 removes the *capability* gap and the
string-round-trip overhead on top.

**Success.** A stateful, seeded, IO-driven pipeline composes without hand-rolled
threading; `io.walk` ingests a multi-thousand-file tree into a structured value in well
under a second, lazily where the consumer is lazy, with no shell-out.

### Pillar EC — Ecosystem & surface (the surface floor)

The surviving cross-unit and surface work that lets 1.0 be declared and frozen.

- **W18 Module & package system (git-only, content-addressed).** Build on the shipped
  Unit Interface and git fetch backend: content-addressed git imports (commit pin +
  `sha256:` content hash), an eucalypt-syntax manifest discovered by upward search,
  namespace isolation, and a lockfile pinning the closure (Go `go.sum` model). No
  registry (§5). MVS/lockfile depth are post-1.0.
- **W19 Interactive surface — `eu watch` & REPL (Phase 1).** Re-render on change, and
  a thin REPL over the cache, leaning on BV5 startup and the incremental query core
  (W7). Full notebook is post-1.0.
- **W17 Hermetic mode for reproducible rendering.** Pin ambient inputs (clock, env,
  seeded PRNG, deterministic ordering) so a render is a pure function of declared
  inputs; composes with content-addressed imports and PP's deterministic merge.
- **W7 Incremental, query-based core (continue).** The memoised query graph over the
  Unit Interface; front-end scope for 1.0, serving both LSP and CLI re-compiles.
- **CU Separate unit compilation & per-unit incremental cache.** Today the pipeline
  merges units *early* (`prepare.rs:169-188`) — before cook / hoist / DCE / inline /
  verify / STG-compile — and desugar threads cross-unit state via `UnitInterface`, so
  a per-unit "edit one file → fast recompile" is impossible without **deferring merge**
  and replaying interface state (**CU1**). Once units compile independently, cache each
  unit's compiled form keyed on its transitive-source hash, reusing **BV5-cache**'s
  serialisation format and version/build-id invalidation envelope (**CU2**). CU2 is the
  payoff `eu watch`/REPL (W19) most wants; CU1 is the enabler shared with the module
  system (W18). Both 0.15+. (BV5-cache's *whole-program* cache — a hit only when the
  entire input set is byte-identical — is the 0.13 step that comes first and builds the
  reusable envelope.)
- **W5 Conformance, property tests & fuzzing (continue to the 1.0 bar).** The golden
  corpus + properties + fuzzers — and now the **proof that every BV phase changes
  nothing observable** (the dual-engine conformance the corpus was built for). Bar:
  golden coverage of every export format and the frozen prelude set; zero panics after
  a 24-hour fuzz run; full harness green under `EU_GC_VERIFY=2`.
- **EC-embed The embedding bridges — eucalypt as an export target, and the macro
  substrate.** There are **two distinct embedding formats**, and the distinction is
  the whole point. The **AST embedding** (`src/syntax/export/embed.rs`, *"allow parsed
  AST to be quote-embedded as eucalypt"*) represents the **front-end syntax tree**
  (Rowan nodes — blocks, lists, declarations, units) as eucalypt data, and renders
  back to source via `pretty.rs`/`format.rs` (and `eu fmt`). The **Core embedding**
  (`src/core/export/embed.rs`, the `c-*` tagged-list vocabulary with `parse-embed:
  :CORE`, documented in `docs/development/embed-format.md`) represents the **desugared
  Core IR**, for constructing and testing compiler intermediate representations
  (there is a parallel `s-*` STG embedding in `src/eval/stg/embed.rs`, and the `t-*`
  **Type embedding** is the `to-data` projection of `s"…"` from Pillar SV — so the
  full family is AST `a-*`, Core `c-*`, STG `s-*`, Type `t-*`). They serve
  different ends: the **AST** embedding is the path for *generating eucalypt source
  from data* (the syntax level — `data → AST embedding → rendered .eu`); the **Core**
  embedding is the path for *metaprogramming over the compiled IR* (the substrate any
  future macros build on). Separately, `-x eu` (`src/export/eu.rs`) renders plain
  *values* as eucalypt-source data.

  **This must not die — and the AST half is already half-dead:** 10 of the 11 `Embed`
  impls in the AST module currently return `None` (only `Soup` is wired). The
  aspiration — transform a data structure into an AST embedding and render it as
  eucalypt — is *exactly* the gap: the AST embedding needs completing so eucalypt can
  emit `.eu` *code* (lambdas, operators, bindings), not just `-x eu` *data*. Three
  commitments: **(1) keep both alive** — the embed formats are "stable by convention…
  not enforced by snapshot tests" today; promote the Core round-trip (embed→disembed)
  and the AST embed→`pretty`→reparse round-trip into the **conformance corpus (W5)** so
  neither can silently rot, and version the two tag vocabularies as documented
  surfaces; **(2) complete the AST embedding** — fill the stubbed node impls and unify
  AST-embed + `pretty`/`fmt` + `-x eu` into one coherent "eucalypt as an export target"
  story that round-trips code as well as data; **(3) worked examples** — living harness
  examples that emit `.eu` source from data (generate a block of bindings; round-trip a
  parsed unit through AST-data and back). **Macros stay deferred** (below), but the
  **Core** embedding is their substrate, so keeping it warm keeps that door open at no
  extra cost.

#### Deferred candidates (kept warm, not scheduled)

Recorded so they are not forgotten, able to swap in if priorities shift:

- **Macros / homoiconicity** — compile-time metaprogramming over the core, built on
  the **Core** embedding (EC-embed). Deferred, not declined; the substrate is maintained.
- **First-class type values** — types as ordinary values beyond the `s"…"` surface;
  the SV aspiration taken to its limit (reflection, type-level computation).
- **Alternative backends** — core→WASM and other targets, post-bytecode and as
  **distribution** not speed (§4.7); the consumer of W5's cross-backend conformance.
- **Streaming & bounded-memory** — process inputs that don't fit the heap (including
  large **filesystem trees**, the EF2 motivating case); the principled answer to the
  large-data regime that doesn't require winning the GC fight. A partial `LazyProducer`
  spec already exists.
- **Lazy-evaluation debugging** — a DAP server / value-provenance for stepping lazy
  evaluation.
- **Algebraic effect-rows** — typed effects beyond the IO monad; now the long-arc
  end-state of **Pillar EF** (EF1.3), not a free-standing candidate.

---

## 8. The critical path

**CG type-free tier + TY default-on + SV `s"…"` + optional fields (0.11) → BV0 gate →
BV1 + BV5-prelude (0.12, the bytecode core; shipped) → BV4 decode-fusion +
ExecutionError boxing (0.12.1, close the engine gap) → Phase-4 collapse (retire
HeapSyn, 0.13) → BV2 + BV3 + CG type-gated (0.13) → SV
contracts + DS + modules (0.14–0.15) → W5 conformance green → the 1.0 milestone:
ratify and freeze the surface (§4.6).** DS, PP and the post-1.0 candidates slot
alongside or follow. The 1.0 freeze is gated on the surface and conformance, not on
the bytecode programme being finished — and no feature is scheduled *for* 1.0; they all
land in point releases first.

## 9. Index

| ID | Item | Release |
|---|---|:---:|
| **BV0** | Bytecode encoding + dispatch-ceiling spike (gate) | 0.11 |
| **BV1** | Threaded interpreter; code out of the GC heap *(shipped — default engine; Phase-4 collapse deferred to 0.13)* | 0.12 |
| **BV5** | Embedded bytecode prelude *(shipped, dual-form)* | 0.12 |
| **BV5-cache** | Content-hash whole-program unit cache (spec `#953`; dispatch-ready, unbuilt — eu-lb0r) | 0.13 |
| **BV2** | Side tables for annotations | 0.13 |
| **BV3** | Register frames | 0.13 |
| **BV4** | Superinstructions / decode-cost fusion *(promoted — gates Phase-4)* | 0.12.1 |
| **CG1–4** | Type-free codegen (direct dispatch, key resolution, strict folds, lifting) | 0.11 |
| **CG5** | Type-gated codegen (unboxing, dead-branch, interpolation) | 0.13 |
| **TY** | Typing default-on | 0.11 |
| **SV1–2** | `s"…"` type-data + `as-spec`/`to-spec` | 0.11 |
| **SV — optional fields** | Optional (presence-annotated) record fields *(promoted)* | 0.11 |
| **SV — prefix-list type** | Fixed-prefix + variable-tail list type (markup/hiccup) | 0.13 |
| **SV3 (W16)** | Structural contracts & runtime validation | 0.14 |
| **SV4 (W8/W22)** | `eu doc` schema / schema interop | 0.15+ |
| **DS1** | Persistent O(log n) blocks (GC-native CHAMP+RRB) | 0.15+ |
| **DS2 (W14)** | Generalise `vec` to arbitrary values | 0.15+ |
| **PP** | Process parallelism (`par-map`/`par-fold`, mmap arena) | 0.14+ |
| **EF1** | Effect composition (combinators → unified context → effect-rows) | 0.14 → post-1.0 |
| **EF2** | Native filesystem/IO capabilities (`walk`/`read`/`stat`, streamable) | 0.14 |
| **W18** | Module & package system (git, content-addressed) | 0.15+ |
| **CU1** | Separate unit compilation — defer merge (per-unit incremental prerequisite) | 0.15+ |
| **CU2** | Per-unit incremental cache (`edit one file → fast recompile`) | 0.15+ |
| **W19** | `eu watch` & REPL (Phase 1) | 0.15+ |
| **W17** | Hermetic mode | 0.15+ |
| **EC-embed** | Embedding bridge: `.eu` code export, harden + exemplify (macro substrate) | 0.12+ |
| **W7** | Incremental query core (continue) | point releases |
| **W5** | Conformance / property / fuzz (continue) | →1.0 gate |
| *1.0* | *Milestone: freeze the surface, turn on the version contract* | *no features* |
| *WASM-distribution* | WASI components / playground accel | post-1.0 candidate |

Settled decisions (§4) and non-goals (§5) are not scheduled work items.

---

## 10. Supplement — superseded & abandoned work

Kept so the reasoning is not lost and the experiments are not repeated. None of the
following is scheduled; each records *why*.

### 10.1 The generational-GC line (W10) — attempted and reverted (0.10.0)

The previous plan's runtime keystone. A full **sticky-mark-bit** generational
collector was implemented, passed all tests under `EU_GC_VERIFY=2`/`EU_GC_STRESS=1`,
and was **reverted**: no wall-clock benefit, and 10–1500% regressions at every
nursery-budget tested. *Why it failed:* (1) the first minor after every major is a
full live-set trace (the mark-state flip makes everything look young); (2) AoC live
sets (100–600 MiB grids/memo tables) dwarf any nursery budget, forcing repeated
majors; (3) the write barrier + dirty-frame scan cost was paid with almost nothing to
skip. *What would work:* a **separate copying nursery** (e.g. 8 MiB) disjoint from the
Immix heap, so a minor never sees old objects — the classic generational layout, which
a single mark bit over a shared address space cannot emulate. **Verdict:** shelved
(§4.5). Revisit only when a real memory-pressure workload (large data under a tight
`--heap-limit`) exhibits the regime; profile after BV/CG first, since both reduce the
thunk churn a nursery would target.

### 10.2 Flat closures — built and reverted (0.10.0)

Targeting the env-walk cost (`EnvironmentFrame::get`, up to 39% on AoC), capture
frames were replaced with `(frame_ptr, physical_index)` pairs. Env `get` self-time
fell 27% but **net wall time regressed 2–10% everywhere**: eucalypt's env chains are
shallow (avg depth 2), so the flat path costs about the same as the cactus path while
every frame creation pays a capture-recipe tax (28–50% of closures capture nothing),
and the wider `Ref` dispatch hurt branch prediction. **Verdict:** abandoned as a
*uniform runtime representation*; the idea relocates — correctly — to **BV3 register
frames + CG4 selective lifting**, a targeted, compiler-decided transform on hot deep
captures rather than a universal one. Archived at `archive/flat-closures`.

### 10.3 Demand cardinality / update-elision (W11 second half) — abandoned

The strictness half of demand analysis succeeded and shipped (the `Seq` eager-eval
path, §2). The **AtMostOnce / update-elision** half (compiling a single-use binding as
`Value` to skip the `Update` frame) is **unsound in eucalypt**: render traversal
enters each block value twice; dynamic `.key` lookups enter closures the static
analysis cannot predict; higher-order use (a binding passed to `foldl`/`map`) is
referenced once syntactically but entered per element. The forced-`Multi` fixup that
prevents the regression also eliminates virtually all opportunities. **Verdict:**
abandoned; the strictness half is retained and extended by CG. Possible future
enablers (a single-entry `RenderKv`; escape analysis) are noted but not scheduled.

### 10.4 STG→WASM as an execution engine — declined (feasibility study)

Full analysis in `docs/development/stg-compilation-targets-feasibility.md`. Compiling
STG to WASM is *feasible* but attacks the wrong bottleneck at the wrong price:
codegen is the easy 20%, the hard 80% is a second RTS (lazy object model; a GC story
that rebuilds a linear-memory RTS or discards Immix; a 192-intrinsic bridge; the
per-scalar streaming render bridge), with per-run Wasmtime codegen + instantiation
likely *slower* than today's 85 ms on the small runs that dominate; 18–36 months to
parity behind a dual-engine migration. **Verdict:** declined as an engine (§4.7),
retained as a **post-1.0 distribution** target (WASI components, playground), to be
compiled *from the bytecode* once it exists. The study's measurements (startup floor;
~2.4 M ticks/s; code-as-scanned-heap-data) are the evidence base for Pillars BV and CG.

### 10.5 The old GC-as-spine performance thesis — superseded

The previous roadmap (baseline 0.7.1) made a generational GC (W10) and persistent
blocks (W13) the engine of 1.0 performance, on the premise *mark > 95% of VM time*.
Strict prelude globals (0.10.0) removed the recurring collections that premise rested
on; on real workloads GC now does **0 collections** while dispatch and env-walk
dominate. The performance plan is therefore rebuilt around compilation and dispatch
(§4.5): **Pillars BV and CG supersede W10 and W13.**
