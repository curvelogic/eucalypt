# 0008 — Parallel & speculative evaluation: the concurrency roadmap

- **Status:** Draft proposal for review
- **Track:** B — performance, runtime & concurrency
- **Classification:** Stage-C-Fork
- **Suggested horizon:** post-1.0
- **Related:** H4/H6c (type-system-evolution.md), ADR-001, sibling proposals [0005](0005-generational-gc.md), [0012](0012-algebraic-subtyping-fork.md), [0020](0020-persistent-blocks-gc-finalisation.md)

## Summary

Eucalypt has no concurrency today and cannot acquire it cheaply: the heap is an
`UnsafeCell`-based structure that is *deliberately* not `Sync`, sound only under
stop-the-world, single-threaded access (`src/eval/memory/heap.rs:8`). Yet the
language's three defining properties — purity, laziness and a fully isolated IO
layer — are *precisely* the substrate on which data-parallel and speculative
evaluation pay off. This is the GHC setting. This document is the honest
strategic roadmap: what parallelism would buy a configuration/data tool, the
major heap/GC rebuild it requires, why it is firmly **post-1.0**, and — the only
actionable part today — which enabling invariants to record and preserve now so
the door stays open. It is a "could", deliberately not a "should-soon".

## Motivation

Eucalypt evaluation is single-threaded by construction, and the constraint is
load-bearing, not incidental. The heap's safety model states it outright:
`Heap` is not `Sync`, "all heap access occurs on a single thread", and "the
mutator and collector never run concurrently"
(`src/eval/memory/heap.rs:9-11`). The mark bit that drives collection is a
plain `bool` (`heap.rs:1198`) with the comment "Plain bool suffices as each
Heap is used from a single thread" (`heap.rs:1197`); it is flipped by a
non-atomic `self.mark_state = !self.mark_state` between collections
(`heap.rs:1716-1717`). There is essentially no threading anywhere in
`src/eval/`: the only atomics are in the crash-diagnostics ring buffer
(`src/eval/machine/crash.rs:9`, single-writer mutator thread) and the debug
flag cache (`src/eval/memory/gc_debug.rs:27`). One worker thread runs
everything, on a 64 MiB stack (`src/bin/eu.rs:26-34`).

This is not a problem to fix for its own sake. For a tool that renders a 200-line
YAML file in milliseconds, parallelism is pure overhead. The motivation is
strategic and forward-looking: as eucalypt is pushed at *larger* workloads —
hundred-document fan-outs, large generated manifests, heavy `map` over big
imported datasets — single-core throughput becomes the ceiling, and eucalypt
happens to sit in the exact language class where parallelism is *safe by
construction*. The purpose of this proposal is to state honestly what that would
cost and to protect the option, not to schedule it.

## Prior art & landscape

The reference design is **Glasgow Parallel Haskell**, because the problem is
identical: parallelising a lazy, pure, graph-reducing runtime.

- **Evaluation Strategies** (Trinder, Loidl, Hammond et al., 1998) separate
  *what* is computed from *how much* and *in what order* it is evaluated, built
  on two primitives: `seq` (sequence) and `par` (spark a parallel evaluation to
  WHNF). `x \`par\` y` sparks `x` and returns `y`; sparks sit in a queue and the
  runtime promotes one to a real thread only when a core is idle. Crucially,
  parallelism is *advisory*: strategies never change a program's result, only
  its timing. This maps cleanly onto eucalypt's stance that types and metadata
  are advisory, and that evaluation order is unobservable.
- **"Seq no more: Better Strategies for Parallel Haskell"** (Marlow, Maier,
  Loidl, Aswad, Trinder, Haskell Symposium 2010) is the document to internalise
  before writing a line of code. It replaces `seq`-based strategies with an
  **`Eval` evaluation-order monad** and fixes a subtle but ruinous *space leak*:
  the original strategies retained the entire structure being evaluated,
  defeating speculation. The new formulation lets the **garbage collector prune
  sparks** whose results are no longer needed — essential for *speculative*
  parallelism, where most sparked work may turn out to be unwanted. The lesson
  for eucalypt is sharp: speculation without GC-level spark pruning is a memory
  bug, not a feature.
- **"Haskell on a Shared-Memory Multiprocessor"** (Harris, Marlow, Peyton Jones,
  2005) is the canonical treatment of *the* parallel-laziness hazard and is
  directly relevant below: when two threads force the same thunk, the runtime
  must coordinate so the work happens once. GHC uses a largely lock-free scheme
  with proper locking only on the rare path where a thread genuinely blocks on
  another's in-progress thunk — "blocking is rare", so the fast path stays cheap.

As contrasts, two strict-functional systems took different roads:

- **Multicore OCaml (OCaml 5)** retrofitted parallelism onto a mature runtime
  (Sivaramakrishnan et al., "Retrofitting Parallelism onto OCaml", ICFP 2020).
  It separates *concurrency* (effect handlers) from *parallelism* (**domains**,
  mapped to OS threads) and rebuilt the GC: a stop-the-world *parallel* minor
  collection plus a *mostly-concurrent* mark-and-sweep major collection. It is
  the proof that a single-threaded managed runtime *can* be made parallel — and
  the proof of how much GC surgery that costs.
- **Manticore / Parallel ML** (Fluet, Rainey, Reppy, Shaw, ICFP 2008) offers
  *implicitly-threaded* parallel forms (parallel tuples, arrays, comprehensions)
  whose semantics are *sequential* — they are hints the compiler may ignore.
  This is the most eucalypt-shaped surface: no new control flow, just a marker
  that some independent work *may* go wide.

What eucalypt should borrow: the **advisory, deterministic, strategy/spark**
model (Strategies + `Eval`), and the **GC-prunes-speculation** discipline. What
it should *not* borrow: GHC's `MVar`/`forkIO` *concurrency* (a different feature
with observable nondeterminism — out of scope here), and any surface that adds
control-flow keywords.

## Proposed design

The honest design has two parts: a long enabling sequence (mostly invisible),
and a deliberately tiny surface. Per the non-negotiables (`_house-style.md` §1),
**no new syntax** is introduced.

### The blockers (what must change first)

1. **The `UnsafeCell`, non-`Sync` heap.** Sound parallel mutation/collection
   requires either a `Send`+`Sync` heap with synchronised allocation and a
   parallel-or-concurrent collector, or per-domain heaps with a careful
   cross-heap pointer policy. Either is a *major* rewrite of
   `src/eval/memory/`. This is the long pole.
2. **Thunk update + blackholing — THE parallel-laziness hazard.** On forcing a
   thunk the VM overwrites its environment slot with a `BlackHole` closure to
   catch cyclic re-entry, then an `Update` continuation overwrites it with the
   result (`src/eval/machine/vm.rs:434-449`; the `BlackHole` error path is
   `vm.rs:533`). Single-threaded, this is a sentinel. With two workers it is a
   *race*: both may find an un-blackholed thunk and duplicate (or, worse,
   corrupt) the update. This needs the Harris/Marlow treatment — an atomic
   claim on the thunk header, a lock-free fast path, and a block-and-wake slow
   path for the rare genuine collision. Touches the hottest code in the VM.
3. **`Rc` is not `Send`.** Core and the STG backend use `Rc` pervasively:
   `RcExpr` wraps `Rc<Expr<Self>>` (`src/core/expr.rs:479`,
   `CoreExpr = Expr<RcExpr>` at `expr.rs:357`), and `StgSyn` uses `Rc<StgSyn>`
   throughout (70 occurrences in `src/eval/stg/syntax.rs`). None of this can
   cross a thread boundary. Note this affects *compiled programs*, not the
   runtime graph: the VM's live data is raw-pointer (`RefPtr`) heap closures
   (`vm.rs:249-255`), so a runtime spark need not carry `Rc`. But sharing
   compiled code across workers would require `Arc` (or load-once-per-worker).
4. **Non-atomic mark bit.** `mark_state: bool` (`heap.rs:1198`) and per-object
   header marks must become atomic, or marking must be partitioned per domain.

### The enablers (why the door is worth keeping open)

- **Referential transparency makes evaluation order unobservable.** The pure
  core performs no effects; result equals result regardless of *when* a thunk is
  forced. This is the precondition Strategies depends on, and eucalypt has it.
- **`if` is a function with independent branches.** It is an ordinary intrinsic,
  not control-flow syntax. Both arms are self-contained expressions, so a
  speculative worker could begin forcing the unlikely branch while the predicate
  evaluates — the textbook speculation case, available *for free* semantically.
- **The continuation stack is off-heap.** Machine state is a `(closure, stack)`
  pair: `closure: SynClosure` plus `stack: Vec<Continuation>` "stored inline,
  not on the eucalypt heap" (`vm.rs:250-258`). A spark is therefore a
  **self-contained, relocatable unit** — a closure to force plus its own fresh
  stack — exactly the shape a work-stealing scheduler wants. The machinery for
  suspending and resuming such stacks already exists for the IO loop
  (`suspended_stacks`, `vm.rs:295`).
- **IO is already isolated.** Effects live entirely in the driver's IO-monad
  interpret loop (`src/driver/io_run.rs:1-11`): the STG machine evaluates to
  WHNF and *yields* on an IO constructor (`io_yielded()`, `vm.rs:1879`); the
  driver runs the effect and re-enters. The pure evaluator could parallelise
  *without touching effect ordering at all*, because it never performs effects.
  This cleanly sidesteps the hardest part of parallelising an impure language.

### The surface (deliberately minimal, no new syntax)

When (if ever) the runtime supports it, the user-facing surface is one prelude
combinator and/or one metadata hint — never a keyword:

```eu
# Advisory: this map's element computations are independent and may go wide.
big-list map(expensive) //parallel

# Or via metadata on a binding whose value is an independent fan-out:
` { strategy: :parallel }
manifests: documents map(render)
```

Here `//parallel` is a hypothetical prelude *combinator* (a `par`-flavoured
strategy applied by catenation), and `strategy:` is ordinary metadata read by
the compiler. Both are *advisory*: on a single-threaded build, or for a small
list, they are identity. Semantics never change; only timing does. This honours
syntactic conservatism (machinery in metadata/operators/prelude) and the
gradual/advisory philosophy. No `par` keyword, no parallel-block form.

## Interaction with the existing roadmap

This proposal is, candidly, a **"big rebuild" fork competing for the same scarce
implementation effort** as the other deep-runtime bets. Three items contend for
one budget:

| Fork | Core cost | Independent? |
|------|-----------|:------------:|
| [0005](0005-generational-gc.md) advanced GC | nursery + trigger + defrag completion | partly |
| [0012](0012-algebraic-subtyping-fork.md) MLsub | type-engine rewrite | yes |
| **0008 (this)** parallel eval | `Send`/`Sync` heap + parallel GC + blackhole locking | **builds on 0005** |

Crucially, **0008 depends on 0005, not the reverse.** A `Send`/`Sync`,
parallel-or-concurrent collector is a *superset* of the generational work; doing
0005 first and well is the natural substrate, and the OCaml 5 experience
confirms the GC is the gating component. By contrast 0012 is orthogonal (it
touches `src/core/typecheck/`, not the runtime) and should be sequenced on its
own merits. There is also a soft tie to
[0020](0020-persistent-blocks-gc-finalisation.md): both 0008 and 0020 stress the
GC-finalisation story (ADR-001), so any heap redesign should weigh both at once.

The dependency chain is therefore strict and long:

> stable 1.0 runtime → advanced/parallel GC (0005, redone as `Send`+`Sync`) →
> atomic thunk-claim / blackhole-locking in the VM → sparks + a work-stealing
> scheduler → an `Eval`-style Strategies layer with **GC spark-pruning** →
> the advisory prelude/metadata surface above.

Each arrow is months of work and a correctness-critical change to the most
delicate code in the tree.

## Implementation sketch

Indicative only; not for scheduling before 1.0.

- **Phase 0 (now, cheap): document & preserve invariants.** Land an
  `ADR-00x: enabling invariants for future parallelism` recording the four
  facts that, if quietly broken, would slam the door: (a) the
  `(closure, off-heap stack)` machine-state shape (`vm.rs:250-258`); (b) total
  IO isolation in the driver (`io_run.rs`); (c) the thunk-update/blackhole
  protocol as the designated synchronisation point (`vm.rs:434-449`); (d) that
  `if` and friends stay ordinary functions with independent operands. Add a
  CI-visible note so future heap/VM changes consider parallel-friendliness.
  *Cost: documentation only.*
- **Phase 1: `Send`/`Sync` heap + parallel GC.** Folded into 0005. The largest,
  riskiest phase; OCaml 5 is the template.
- **Phase 2: atomic thunk-claim / blackhole-locking.** Header CAS on force,
  lock-free fast path, block-and-wake slow path (Harris/Marlow 2005).
- **Phase 3: sparks + work-stealing scheduler.** Each spark is a
  `(closure, fresh stack)`; reuse `suspended_stacks` machinery (`vm.rs:295`).
- **Phase 4: Strategies + GC spark-pruning + the advisory surface.** The
  Marlow 2010 `Eval`-monad discipline, then `//parallel` / `strategy:`.

Risk is **high** throughout: this is unsafe-heavy, race-prone code where bugs
are nondeterministic. The excellent existing debug infrastructure
(`EU_GC_VERIFY`, `EU_GC_STRESS`, `EU_GC_POISON`) is necessary but not sufficient
— concurrency needs additional race-detection scaffolding.

## Alternatives considered

- **Process-level parallelism (do nothing in-runtime).** Run N independent `eu`
  invocations across N input files from a shell or build system. For the
  multi-document case this captures most of the realisable win at *zero* runtime
  risk, and is the right answer for the foreseeable future. The in-runtime story
  only beats it for *intra-document* parallelism (one big `map`, one expensive
  field) — a genuinely narrower case.
- **Async/effects-style concurrency (à la OCaml domains + effects, or `forkIO`).**
  Rejected: introduces *observable* nondeterminism, contradicting deterministic
  rendering and the capability/determinism direction
  ([0010](0010-capability-determinism-types.md)). Eucalypt wants *parallelism*
  (unobservable), not *concurrency* (observable).
- **GPU/SIMD data-parallelism.** Wrong shape for irregular, pointer-heavy graph
  reduction over heterogeneous structured data.

## Risks & what would kill this

- **The cost/benefit never closes for a config tool.** If real workloads stay
  small, parallelism is permanent overhead. *This is the most likely outcome*,
  and it is why the recommendation is "preserve the option", not "build it".
- **Speculation leaks space.** Sparking the wrong `if` branch and retaining it is
  the exact bug Marlow 2010 fixed; without GC spark-pruning, speculation is a
  liability.
- **Amdahl + spark overhead dominate.** If sparks are too fine-grained the
  scheduler costs more than it saves; granularity control is itself hard.
- **It starves higher-value work.** Every month here is a month not spent on
  0004 (caching), 0005 (GC), or the type-system roadmap — all of which help
  *every* user, not the large-workload minority.

Falsification: a prototype `Send`/`Sync` heap that shows the synchronisation tax
erases parallel gains on representative workloads would settle the question
against proceeding.

## Success criteria

Should this ever be pursued, success is narrow and measurable: **>1.5× speedup**
on a genuinely parallel benchmark (large-list `map`, multi-document render) on
4+ cores, with **zero** change to single-threaded results, **no** regression in
small-config latency (advisory hints must be free when unused), and **no** new
nondeterminism. Absent those, the correct outcome is to keep the door open and
spend the effort elsewhere.

## References

**Eucalypt source (verified):**
`src/eval/memory/heap.rs:8-11,1196-1198,1716-1717` (non-`Sync`,
single-threaded heap; non-atomic mark bit) · `src/eval/machine/vm.rs:250-258`
(off-heap `(closure, stack)` machine state) · `vm.rs:295` (`suspended_stacks`) ·
`vm.rs:434-449,533` (blackhole / `Update`) · `vm.rs:1879` (`io_yielded`) ·
`src/core/expr.rs:357,479` (`RcExpr`) · `src/eval/stg/syntax.rs` (`Rc<StgSyn>`)
· `src/driver/io_run.rs:1-11` (IO isolated in the driver) ·
`src/eval/machine/crash.rs:9`, `src/eval/memory/gc_debug.rs:27` (only crash /
debug atomics) · `src/bin/eu.rs:26-34` (single 64 MiB worker thread) · ADR-001
(`docs/development/architectural-decisions.md`).

**Papers / systems:** Trinder, Loidl, Hammond et al., *Algorithm + Strategy =
Parallelism* (1998) · Marlow, Maier, Loidl, Aswad, Trinder, *Seq no more: Better
Strategies for Parallel Haskell* (Haskell Symposium 2010) · Harris, Marlow,
Peyton Jones, *Haskell on a Shared-Memory Multiprocessor* (Haskell Workshop
2005) · Sivaramakrishnan et al., *Retrofitting Parallelism onto OCaml* (ICFP
2020) · Fluet, Rainey, Reppy, Shaw, *Implicitly-threaded Parallelism in
Manticore* (ICFP 2008).

**Sibling proposals:** [0005 — generational GC](0005-generational-gc.md),
[0010 — capability & determinism types](0010-capability-determinism-types.md),
[0012 — algebraic-subtyping fork](0012-algebraic-subtyping-fork.md),
[0020 — persistent blocks & GC-finalisation](0020-persistent-blocks-gc-finalisation.md).
