# 0008 — Parallelism: isolated workers first, the shared-heap fork later

- **Status:** Draft proposal for review
- **Track:** B — performance, runtime & concurrency
- **Classification:** Stage-C-Fork (model B); model A is near-ish, post-0004
- **Suggested horizon:** A post-1.0 (near); B post-1.0 / maybe-never
- **Related:** [0004](0004-compiled-unit-caching.md) (cheap worker startup — *enables A*), [0005](0005-generational-gc.md) (the GC rebuild *B* needs), [0010](0010-capability-determinism-types.md) (parallelism must stay unobservable), ADR-001

## Summary

Eucalypt has no concurrency, and acquiring it the *obvious* way is expensive: the
heap is an `UnsafeCell` structure deliberately not `Sync`
(`src/eval/memory/heap.rs:8-11`), so a shared-memory thread model means a major
heap/GC rebuild **and** an uncertain tax on the single-threaded common case. But
there are **two** models, and the proposal should lead with the cheap one:

- **A — Isolated workers (the JavaScript / Web Worker model).** Separate
  evaluator instances, each keeping its own single-threaded heap exactly as it
  is, communicating by passing *serialised data*. No shared heap → **no
  single-threaded tax**. Eucalypt's purity and isolated IO make it safe by
  construction, and 0004's embedded prelude makes a worker *cheap to spawn*. It
  serves the real use cases — batches of files, and data-parallel `map`/`fold`
  via scatter-gather. **Its cost is paid at the *interface*** (parallel work must
  be expressed as serialisable-data → addressable-transform → serialisable-data),
  which is the design crux and where friction lives.
- **B — Shared-heap threads (the GHC sparks model).** Transparent, fine-grained
  parallelism on one `Send`/`Sync` heap + concurrent GC + blackhole-locking. Its
  unique value is what A *can't* express — fine-grained shared-thunk speculation —
  but it is the big rebuild with the unpredictable single-threaded tax. High-risk;
  reach for it only if A proves insufficient.

The recommendation: pursue **A** as the pragmatic path; keep **B** documented as
the hard fork. Both stay advisory and deterministic — *parallelism*, never
observable *concurrency*; no new syntax.

## Motivation

For a tool that renders a 200-line YAML file in milliseconds, parallelism is pure
overhead — that stays true. The motivating cases are specific and real:

- **Batches of files.** Rendering or transforming many independent inputs is
  *embarrassingly parallel*, yet today it is sequential. This is the clearest win
  and needs nothing shared.
- **Data-parallel work inside one program.** A `map`/`fold` over a large list (a
  big imported dataset; an Advent-of-Code search space) can run wide if the
  per-element work is independent — which, in a pure language, it is.

Eucalypt sits in the language class where parallelism is *safe by construction*:
referential transparency makes evaluation order unobservable, and **all effects
are isolated in the driver's IO loop** (`src/driver/io_run.rs`), so the pure
evaluator never races on state. The open question for the *shared-heap* model (B)
is its **single-threaded tax** — atomic refcounts, synchronised allocation,
memory barriers, a non-atomic mark bit (`heap.rs:1198`) made atomic — paid by the
99% single-threaded case, and genuinely hard to predict without building it. The
isolated model (A) makes that question *moot* by not sharing a heap at all, which
is the strongest reason to lead with it.

## Model A — Isolated workers (recommended)

Each worker is a full evaluator with its **own heap**; they coordinate by passing
*serialised values*, not pointers. The pattern is **scatter-gather**: partition
the immutable input into chunks, hand each chunk to a worker that applies a pure
transform, merge the results.

**Why it fits eucalypt.** Purity means there is no mutable state to share and the
merge is order-independent. IO-isolation means each worker's effects are
independent. And — crucially — **0004's embedded prelude removes the startup
cost** that makes process parallelism look heavyweight today: you don't need to
*share* across workers, you just *spawn* them cheaply. For "process lots of files"
each worker reads *different* files, so there is nothing to share at all.

**What it serves, and what it doesn't.** It covers the file batch (one chunk per
worker) and data-parallel `map`/`fold` over a big list — a large fraction of
realistic large-workload and AoC cases. It does **not** cover *fine-grained*
parallelism that shares thunks across workers (speculatively forcing both `if`
branches of one computation; parallel graph reduction over a deeply-shared
structure) — that is B's niche.

**Prior art.** JavaScript **Web Workers** (isolated heaps, `postMessage`, no
shared mutable memory) are the direct model. **Starlark** is the config-space
precedent: it parallelises module loading precisely by making shared data
immutable — eucalypt's purity gives it the same opening. The compute pattern is
classic **scatter-gather / map-reduce**.

### The interface to eucalypt code — the crux

A's whole cost lands here: a shared-heap model parallelises *transparently*
(closures and lazy graphs just work because the heap is shared); an isolated
model requires the parallel work to be **expressible across a serialisation
boundary**. Four frictions, with their mitigants:

1. **The boundary is data-only and strict.** Only serialisable values cross —
   numbers, strings, symbols, blocks, lists: *exactly what eucalypt already
   renders and parses*. Closures, lenses (metadata-carrying functions), IO
   actions, and lazy/infinite structures do **not** cross. So data crossing the
   boundary must be **forced** to serialisable normal form — parallelism induces
   strictness at the boundary. *Mitigant:* eucalypt is a data tool; the values
   you actually fan out over are already plain data, and `render-as`/`parse-as`
   already exist. *Friction that remains:* you cannot scatter an infinite/streaming
   structure, and a chunk built lazily from a large context must be forced first.
2. **The transform must be addressable, not an arbitrary closure.** A worker has
   its own heap and re-instantiates the program; it applies a transform
   identified by **name** (a top-level binding) or as an **evaluand** (an
   expression over the chunk), not a closure capturing non-serialisable runtime
   state. *Mitigant — the key one:* eucalypt's **existing model already is this
   contract.** `eu -e <transform> <chunk>` *is* a worker: compile the program,
   apply a named transform to data input, render data output. The IO driver
   already spawns and manages subprocess workers for `io.exec`
   (`src/driver/io_run.rs`). So the worker interface is not new machinery — it is
   the CLI evaluand-on-data model, parallelised. *Friction that remains:* a
   transform that closes over large *runtime-computed* context (not reconstructable
   from source + chunk) doesn't fit cleanly.
3. **Effects across workers.** Each worker's IO is independent (IO-isolation
   helps), but if the transform performs effects, ordering and merging need care.
   *Mitigant:* restrict parallel work to **pure** transforms (the common,
   recommended case); deterministic merge of pure results preserves
   reproducibility ([0010](0010-capability-determinism-types.md)).
4. **Serialisation cost.** Round-tripping chunks through a wire format has
   overhead; granularity must be coarse enough to amortise it (chunk, don't
   per-element). *Mitigant:* an internal binary form (the same `StgSyn`/value
   serialisation 0004 considers) rather than YAML for the in-process case.

**The honest trade:** model A moves the cost from the *runtime* (B's heap-tax,
paid always) to the *interface* (paid only when you parallelise, and only in
expressiveness). For a **data** tool this is a far better trade than for a
general-purpose language — values are already serialisable data and the
evaluand-on-data contract already exists — but it is **not zero-friction**: you
give up transparent parallelisation of arbitrary closures and lazy graphs, and
the user must structure parallel work as data → named-transform → data. Whether
that friction is acceptable is the thing to prototype on a real fan-out.

**Surface (no new syntax).** A `par-map` / `par-fold`-style prelude combinator
that scatters a list (in chunks) to workers applying a named transform and merges
results — restricted to serialisable elements and an addressable transform, with
a clear diagnostic when given a non-serialisable value or a non-addressable
closure. Orchestration lives in the IO driver. Advisory: on a one-element list or
a single-core/`--no-parallel` build it is identity; semantics never change, only
timing.

## Model B — Shared-heap threads (the hard fork, demoted)

The GHC **Strategies / sparks** model: `x \`par\` y` sparks parallel evaluation
to WHNF on a *shared* heap; the runtime promotes a spark to a thread when a core
is idle; advisory and deterministic. Its unique value over A is **fine-grained,
shared-thunk** parallelism A cannot express. The price is a rebuild of the most
delicate code in the tree:

- **Blockers:** the `UnsafeCell`, non-`Sync` heap (`heap.rs:8-11`) → a `Send`/`Sync`
  heap with a parallel-or-concurrent collector (a *superset* of 0005, the OCaml-5
  experience confirms the GC is the gating cost); **thunk-update + blackholing as
  THE parallel-laziness hazard** (`vm.rs:434-449,533`) → an atomic thunk-claim
  with a lock-free fast path and block-and-wake slow path (Harris/Marlow 2005);
  `Rc` pervasive in core/STG (`expr.rs:357,479`, `Rc<StgSyn>`) not `Send`;
  non-atomic mark bit (`heap.rs:1198`).
- **Enablers** (why the door is worth keeping open): purity (order unobservable);
  the off-heap `(closure, stack)` machine state (`vm.rs:250-258`) makes a spark a
  self-contained relocatable unit, reusing `suspended_stacks` (`vm.rs:295`); IO
  isolation. And **speculation needs GC-level spark pruning** (the space leak
  "Seq no more", Marlow 2010, fixes) — without it, sparking the wrong `if` branch
  and retaining it is a memory bug.
- **The central open question:** the **single-threaded tax**. A `Send`/`Sync`
  heap with synchronised allocation and atomic marks taxes every single-threaded
  run; the magnitude is unpredictable and could erase the gains. *That uncertainty
  is precisely why A — which avoids it — comes first.*

## Interaction with the existing roadmap

- **A depends on [0004](0004-compiled-unit-caching.md)** (embedded prelude → cheap
  workers) and reuses the IO driver's subprocess machinery; it is near-ish and
  carries *no* runtime risk. **B depends on [0005](0005-generational-gc.md)** (its
  `Send`/`Sync` parallel GC is a superset of the generational work) and is the
  long, correctness-critical chain — competing with 0005/0012/0020 for scarce
  deep-systems effort.
- Both must stay **advisory/deterministic** — parallelism, not concurrency — to
  preserve reproducible rendering ([0010](0010-capability-determinism-types.md)).

## Implementation sketch

**Model A (post-0004, Medium):**
1. A value (de)serialisation form for chunks (internal binary; reuse 0004's).
2. A `par-map`/`par-fold` prelude combinator + worker orchestration in the IO
   driver (reuse the `io.exec` subprocess machinery); the worker applies a named
   transform / evaluand to a chunk and returns serialised results; the driver
   merges. Forcing-to-serialisable and the addressable-transform check at the
   boundary, with clear diagnostics.
3. Advisory degradation (identity when not worth it / disabled).

**Model B (post-1.0 / maybe-never, High):** the strict chain — `Send`/`Sync` heap
+ parallel GC (folded into 0005) → atomic thunk-claim / blackhole-locking → sparks
+ work-stealing scheduler → `Eval`-style Strategies with GC spark-pruning → the
advisory surface. Each arrow is months of race-prone unsafe work; the existing
`EU_GC_VERIFY`/`STRESS`/`POISON` infrastructure is necessary but not sufficient
(concurrency needs race detection too).

A **Phase 0 (now, free)** applies regardless: an ADR recording the enabling
invariants that, if quietly broken, slam the door on *both* models — the
`(closure, off-heap stack)` shape, total IO isolation, the thunk-update/blackhole
protocol as the designated sync point, and `if`-as-an-ordinary-function.

## Alternatives considered

- **Lead with the shared-heap rebuild (the prior framing).** Rejected: it pays an
  always-on single-threaded tax for a fine-grained case eucalypt rarely needs,
  when A serves the actual use cases at far lower risk.
- **Async / effects-style concurrency** (`forkIO`, OCaml domains+effects).
  Rejected: observable nondeterminism, contradicting deterministic rendering.
- **GPU/SIMD data-parallelism.** Wrong shape for irregular, pointer-heavy graph
  reduction over heterogeneous structured data.

## Risks & what would kill this

- **A's interface friction is too high.** If real parallel work routinely closes
  over non-serialisable runtime context, or the forcing-at-the-boundary defeats
  the laziness users rely on, A's ergonomics fail. *Falsifier:* prototype `par-map`
  on a representative fan-out (a file batch and one large data-parallel `map`); if
  expressing it is awkward or the serialisation cost dominates, rethink the surface.
- **B's single-threaded tax erases its gains.** A `Send`/`Sync`-heap prototype
  showing the synchronisation tax wiping out parallel speedup on representative
  workloads settles B against proceeding.
- **It starves higher-value work.** Every month on B is a month not on 0004/0005
  or the type system — which help *every* user. (A is cheap enough to dodge this.)
- **Speculation leaks space** (B only): without GC spark-pruning, the Marlow-2010
  bug.

## Success criteria

- **A:** **>1.5×** on a file batch and on one large data-parallel `map` across 4+
  cores; **zero** single-threaded cost; deterministic, identical results; and —
  the real test — an interface a user reaches for without contortion.
- **B:** pursued only if A demonstrably cannot serve a compute-heavy
  *intra-program* case **and** a heap prototype shows the single-threaded tax is
  acceptable. Absent both, keep the door open and spend the effort elsewhere.

## References

**Eucalypt source (verified):** `src/eval/memory/heap.rs:8-11,1196-1198,1716-1717`
(non-`Sync`, single-threaded heap; non-atomic mark bit) · `src/eval/machine/vm.rs:250-258`
(off-heap `(closure, stack)` state), `:295` (`suspended_stacks`), `:434-449,533`
(blackhole/`Update`), `:1879` (`io_yielded`) · `src/core/expr.rs:357,479` (`RcExpr`)
· `src/eval/stg/syntax.rs` (`Rc<StgSyn>`) · `src/driver/io_run.rs` (IO isolated;
subprocess worker machinery for `io.exec`) · `src/bin/eu.rs:26-34` (single worker
thread) · ADR-001.

**Papers / systems:** Trinder et al., *Algorithm + Strategy = Parallelism* (1998)
· Marlow et al., *Seq no more* (2010) · Harris, Marlow, Peyton Jones, *Haskell on
a Shared-Memory Multiprocessor* (2005) · Sivaramakrishnan et al., *Retrofitting
Parallelism onto OCaml* (ICFP 2020) · Fluet et al., *Implicitly-threaded
Parallelism in Manticore* (ICFP 2008) · MDN, *Web Workers* · Bazel, *Starlark*
(parallel loading via immutability).
