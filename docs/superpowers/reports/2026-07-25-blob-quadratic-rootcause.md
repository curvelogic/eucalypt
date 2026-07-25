# Blob-vs-source quadratic: root cause found — an unmemoised argument-indirection chain on the lazily-threaded operator, created because `eager_args` never fires in blob-compiled prelude bindings

- **Date:** 2026-07-25
- **Bead:** eu-e3c3i (root-cause diagnosis; analysis only, **no shipped change**).
  Companions: eu-n8c5e (blob pipeline spikes, PRs #1062/#1065), eu-2sa6.2 (BV3),
  eu-gmdl5 (CG4), eu-7x0r (blob diagnostics population).
- **Worktree/commit:** `.claude/worktrees/agent-a75eb69e7b49371ae`, `master` at
  `8efc367a`.
- **Toolchain:** rustc stable (`stable-aarch64-apple-darwin`), Darwin 25.5.0
  arm64.
- **Build provenance:** fresh `cargo build --release` in this worktree, `cargo
  run -p xtask --release -- prelude-compile` (`lib/prelude.blob` 598,731 bytes,
  sha256 `de68f848…`), second `cargo build --release` to re-embed. One binary;
  `EU_SOURCE_PRELUDE=1` selects the source-prelude path on the same binary.
- **Shipped-output invariant verified:** after all diagnostic code in this
  spike was in place, `cargo xtask prelude-compile` was re-run and its default
  output is **byte-identical** (sha256 `de68f848184815ee…`, same as the
  pre-spike blob and the same 598,731-byte figure as both prior n8c5e spikes).
  All diagnostic code is uncommitted; nothing ships.

## 0. Protocol-compliance disclosure

Load average ranged **4.2–9.5** throughout (multiple other agents active in
this session), violating PROTOCOL §2's quiet-machine precondition. Per
protocol §5, **every load-bearing figure in this report is a deterministic VM
tick or instruction count read under `EU_HEAPSYN=1 -S` or from the
deterministic step trace — measured-verified**, immune to load. The single
wall observation (the 49 KB `str.len` hang) is a binary
completes-vs-times-out check at a 20 s timeout with 0.09 s *user* time on the
fixed side, not a comparative wall figure.

## 1. The root cause, in one paragraph

**Blob-compiled prelude self-recursive HOFs re-resolve their lazily-threaded
operator parameter through a per-iteration-growing chain of non-updateable
`Atom{Ref::L}` indirection closures, because the argument arrays at their
self-recursive call sites are built by the lazy `create_arg_array` instead of
`create_arg_array_eager`.** At every recursive call, `create_arg_array`
(`src/eval/machine/env_builder.rs:266-280`) wraps each `Ref::L` argument in a
freshly allocated `Atom{Ref::L}` closure over the caller's frame. For the
`op` parameter of `foldl` — which is never forced-through, only re-passed —
each iteration's wrapper points at the previous iteration's wrapper, so after
k iterations resolving `op` chases k alias closures. These wrappers are plain
non-updateable closures (`SynClosure::new`), so the VM's `Atom{Ref::L}`
handler (`src/eval/machine/vm.rs:443-479`) pushes **no Update frame** for
them: nothing collapses the chain, and step k re-walks all k links — Σk =
N(N−1)/2 extra machine steps, each a single shallow env `get()`. The VM
already has the exact countermeasure — `create_arg_array_eager`
(`env_builder.rs:282-301`, doc comment at `:99-102`: *"Used at self-recursive
call sites to prevent O(n) indirection chain build-up"*) — selected by the
`eager_args` flag on `App`/`DirectApp`, which the STG compiler sets **iff the
callee name equals the enclosing recursive binding's name**
(`src/eval/stg/compiler.rs:1140-1143`, applied at `:1232-1247`). That
self-recursion context (`self_recurse_name`) is seeded **only** from
`b.demand.recursive` (`compiler.rs:908-915`) — a demand-analysis product —
and additionally is dropped at `compile_body`'s `Expr::Meta` arm
(`compiler.rs:1729`). xtask's per-binding blob compile
(`xtask/src/main.rs`, step 6) runs no demand analysis and passes no
self-recursion name, so `eager_args` never fires anywhere in the shipped
blob, and every self-recursive prelude combinator (`foldl`, `foldr`, `map`,
hence `count`/`sum`/`filter`/`str.len`) is O(N²) in its lazily-threaded
parameters. Source mode runs demand analysis over the whole merged unit,
marks these bindings `recursive`, sets `eager_args`, and is exactly linear.

## 2. How it was found (deterministic, trace-level)

### 2.1 The tick curve is exactly ½N² (measured-verified)

`range(0,n) count`, blob, HeapSyn ticks: 1,288 / 2,378 / 4,633 at N=5/10/20
and 710,723 / 2,421,223 / 8,842,223 / 33,684,223 at N=1k/2k/4k/8k — all seven
points fit **ticks = 0.5·N² + 210.5·N + 223 exactly** (e.g. predicts 164,423
at N=400; measured 164,423). Allocations are exactly linear (19,133 / 38,133
/ 76,133 / 152,133): the machine re-executes **non-allocating** work
quadratically. The quadratic term's coefficient of exactly ½ means **one
machine instruction per ordered pair (i, j), i<j** — one instruction executes
k times at step k.

### 2.2 That one instruction, identified

Using the HeapSyn step trace (`-d` → `trace_steps`, `vm.rs:2073`) at
N=10/20/30 and a per-(instruction, env-shape) second-difference histogram,
exactly **one** signature carries the entire quadratic term:

```
✳2 | [×2]→[×3]→[×8]     counts 45 / 190 / 435  =  N(N−1)/2 exactly
```

i.e. `Atom{Ref::L(2)}` evaluated in foldl's body scope (`[×2]` let frame over
`[×3]` λ-args frame over foldl's `[×8]` meta letrec). Logical slot 2 there is
**`op`, foldl's operator parameter**. In the trace the hop repeats k times
consecutively at step k with the continuation stack **unchanged** during the
hops (no Update frames pushed — these closures are not update-thunks), each
hop resolving one shallow `get()` (one parent-frame hop; depth 1). The chain
terminates at the concrete operator lambda (`λ{2} ⊗345(✳0)` — count's
`(n el) → n inc`), which is then applied.

### 2.3 Why the chain exists and grows

Blob `foldl` (`eu dump runtime`, global `⊗437`) compiles to:

```
λ{3} let [0] thunk ⊗274(✳2);                        # nil?(l)
         [1] thunk let [0] thunk (… op(i, head l) …);
                       [1] thunk ⊗280(✳2)            # tail(l)
             in seq ✳0 in seq ✳1 in →⊗437(✳2, ✳0, ✳1)
     in ⊗266(✳0, ✳3, ✳1)                             # if(...)
```

Note the accumulator and tail **are already seq'd** — the hand-curated
`build_prelude_signatures` table (`src/core/analyse_demand.rs:148-161`) marks
`foldl` strict in all three args, so `DirectApp` (`→⊗437`) fires and per-arg
strictness works. The one thing the signature table **cannot** supply is the
*self-recursion* fact, and `eager_args` is keyed on exactly that
(`is_self_recursive`, `compiler.rs:1140-1143`). So the recursive call's
argument array is built by lazy `create_arg_array`: `op` (position 0, ref
`✳2`) gets a fresh `Atom{✳2}` wrapper over the current frame each iteration.
The seq'd `acc`/`tail` slots hold WHNF values, so their wrappers are 1-hop —
constant. Only `op`'s wrapper points at the previous iteration's `op` slot,
whose content is the previous wrapper: the chain grows by exactly one link
per element, is walked once per element, and nothing memoises it — the
wrappers are non-updateable by construction, so this is not a *broken*
update path; there is simply no update path for transparent alias closures.

## 3. The three decisive experiments

All ticks measured-verified (deterministic); same binary throughout.

### 3.1 X1 — source mode with demand analysis suppressed → quadratic returns

`EU_SOURCE_PRELUDE=1`, full pipeline including inline/`tag_combinators`
untouched, only `--suppress-demand-analysis` added:

| N | source (normal) | source + suppress-demand |
|--:|--:|--:|
| 1,000 | 124,373 | 245,857 |
| 2,000 | 248,623 | 741,607 |
| 4,000 | 497,123 | 2,483,107 |

Normal source is exactly ×2.00/doubling. Suppressed-demand source fits
**0.125·N² + 120.75·N + 107 exactly** — quadratic with coefficient N²/8:
the source prelude's 4-way-unrolled specialised copy wraps the operator once
per 4 elements and walks the chain 4× per recursive call ((1/4)²·4 = 1/8).
**Demand analysis — not specialisation — is the lever that makes source
linear.** (Consistent with the Wicket-adjudicated Exp 3 of
`docs/superpowers/specs/2026-07-13-recursive-combinator-copy-specialisation-design.md` §0.2.)

### 3.2 X3 — supply the missing product to the blob → exactly linear

POC (uncommitted): `cargo xtask prelude-compile-poc selfrec <out>` passes
each peeled binding's **own name** as `self_recurse` into the per-binding STG
compile (new diagnostic entry `stg::compile_named`), plus threading
`self_recurse` through `compile_body`'s `Expr::Meta` arm (mirroring the fix
already present in `compile_binding`'s Meta arm). Run via an
`EU_PRELUDE_BLOB_PATH` override (also uncommitted):

| N | blob baseline | blob selfrec-POC | fit |
|--:|--:|--:|---|
| 1,000 | 710,723 | 203,228 | |
| 2,000 | 2,421,223 | 406,228 | |
| 4,000 | 8,842,223 | 812,228 | |
| 8,000 | 33,684,223 | 1,624,228 | **203·N + 228 exactly** |

`sum`: 212·N + 237 exactly. `filter(...) count`: 299·N − 35,285 (zero
quadratic term). Correctness: count=1000, sum=499500 at N=1000; **full
harness suite 505/506** with the POC blob — the single failure
(`test_193_1tkk_7_12_curated_trace`) fails **identically** on today's
unmodified baseline (eu-7x0r's known pre-existing blob diagnostics gap);
zero regressions. POC blob is the **same size** as baseline (598,731 bytes;
only boolean flags differ) — no blob-format change, no size cost, unlike the
+52%/+172% of the per-binding-specialisation POC. The 49 KB `str.len` case
(finding F): baseline blob times out at 20 s; POC blob completes (`49000`)
in 0.09 s user time.

Residual constant vs source (203,228 vs 124,373 at N=1k, ~63%) is the known
un-inlined-alias constant tax (eu-n8c5e Q1/Q3 population) — complexity class
fixed, constant factor out of scope here.

### 3.3 X4 — the dispatched H5 experiment: per-binding demand + reflatten → no effect at all

POC mode `demand`: run `analyse_demands` + `reflatten` over each peeled
binding body (no inline), then compile as today. Result: **tick-for-tick
identical to baseline at every N** (710,723 / 2,421,223 / 8,842,223 /
33,684,223). A peeled binding is a standalone `Meta(doc, Lam(...))` whose
self-reference is `Var::Free` — there is no binding structure for the
analysis to mark `recursive`, so the one demand product that matters here is
underivable per-binding. **H5 as stated ("add demand/reflatten to the blob
pipeline") is refuted as a fix — but the mechanism is still demand's
*product*: `eager_args` at self-recursive call sites, which xtask can supply
structurally since it knows each binding's own name.**

## 4. Hypothesis adjudication

| # | Hypothesis | Verdict | Evidence |
|---|---|---|---|
| H1 | Deep cactus-env walk | **REFUTED (re-confirmed)** | Each quadratic hop is a single `get()` resolving logical slot 2 against a `[×2]` top frame — one parent hop, depth 1, at every chain position (trace §2.2). Depth stays flat *during* the quadratic workload because the quadratic is chain **length** (closure count), not walk depth. Consistent with eu-qm7f and adjudicated Exp 5. |
| H2 | `Ref::G` opacity / un-inlined prelude | **REFUTED as the cause** | The chain hops are `Ref::L` resolutions; the n8c5e POC showed a genuine local self-reference still quadratic-trending. Un-inlining costs a constant (the ~31.6%/63% tax), not the class. |
| H3 | General env-lookup cost (BV3/CG4 lever) | **REFUTED as the cause** | Per-lookup cost is O(1) and shallow; the *count* is quadratic. Cheaper lookups (BV3 register frames) would scale the quadratic's constant, not linearise it. The lookups are on the critical path only because the alias chain multiplies them. |
| H4 | Thunk-chain re-traversal / broken memoisation | **PARTIALLY CONFIRMED — refined** | The quadratic *is* an unmemoised indirection-chain re-traversal (re-walked per step). But no update machinery is *broken*: update-thunks work (updates fire throughout the trace); the chain links are non-updateable `Atom` alias closures for which no memoisation exists by construction. It is also not the *accumulator* (that is seq'd via the curated strict signature) — it is the **operator parameter**. |
| H5 | Demand/strictness not applied in blob | **CONFIRMED — with a decisive refinement** | X1: suppressing demand makes even fully-specialised source quadratic (N²/8, exact). X4: but per-binding demand+reflatten in xtask is a measured no-op — the load-bearing demand product is `demand.recursive` → `self_recurse_name` → `eager_args`, which per-binding analysis cannot derive over a peeled body. Supplying it structurally (X3) flips the blob to exactly linear. |
| H6 | Copy-spec narrow-shape | **CONFIRMED (still holds)** | User-call-site shapes are linear in both configs because user units run demand analysis: the specialised local copy is marked `recursive` and gets `eager_args`. Blob-internal shapes never do. |

## 5. How the mechanism explains every finding

- **A. count/sum/filter O(N²) blob, ×2.00 source.** Chain on the
  lazily-threaded parameter in blob `foldl`/`foldr`/`map`; source gets
  `eager_args` from demand. Measured: blob ticks 0.5N²+210.5N+223 exactly;
  POC with the flag: 203N+228 exactly.
- **B. Env depth flat 2–3.** Each chain hop is one shallow `get()`; depth
  never depends on chain position (§2.2, §4-H1).
- **C. N(N−1)/2 shallow lookups, lookups:ticks ~1:1.** Each hop is exactly
  one `Atom{Ref::L}` tick performing exactly one `get()`. The histogram
  isolates the entire quadratic term to that single instruction signature
  (45/190/435 = N(N−1)/2 exactly).
- **D. Per-binding specialisation = 3.7× constant win, still quadratic.**
  The n8c5e POC's unrolled copy adds one wrapper per ~4 elements instead of
  per element and walks the chain per unrolled block: coefficient drops ~4×
  (their measured 3.7–3.9×) but remains quadratic — exactly the N²/8
  signature X1 reproduces in suppressed-demand source mode.
- **E. bench-022 linear in BOTH configs; constant blob-excess.** The
  literal-lambda shape is specialised in the *user* unit, which always runs
  demand analysis → local copy marked recursive → `eager_args`. The 31.6%
  excess is the separate un-inlined-alias constant tax.
- **F. str.len hangs on 49 KB.** `count ∘ letters`: 49,000²/2 ≈ 1.2×10⁹
  chain hops. Reproduced (baseline times out at 20 s); POC blob completes in
  0.09 s user time.
- **G. Blob compiles each peeled binding from COOKED.** The skipped product
  that matters is precisely demand's `recursive` flag feeding `eager_args`
  (and nothing reachable by re-running demand per-binding — X4).
- **H. Ingestion linear and cheap.** No self-recursive lazily-threaded
  parameter anywhere in the shell/split/vec path — no chain.
- **I. Demand analysis historically frustrated by the blob prelude.** The
  frustrated product is exactly this one: self-recursion detection cannot
  arise from a peeled binding; the curated signature table covers per-arg
  strictness but not self-recursion.
- **J. eu-7x0r (bonus).** Unrelated population, confirmed directly:
  `test_193` fails identically with and without the POC blob. Not force-fit.

## 6. Implication — where the fix routes

**The fix routes to the blob pipeline (xtask + one compiler threading fix) —
NOT to BV3/CG4, and not to eu-2sa6.18's architecture.** This corrects the
eu-n8c5e Phase-2 POC's conclusion ("source linearity is a whole-program
special case; route via BV3/CG4 or eu-2sa6.18"): that POC rebuilt
localisation but not the demand product, so it missed the actual lever.
Source-mode linearity is portable to the peeled blob with:

1. Pass each peeled binding's own name as the self-recursion context in
   xtask's per-binding STG compile (the binding *is* its own name — no
   analysis needed).
2. Thread `self_recurse` through `compile_body`'s `Expr::Meta` arm
   (`compiler.rs:1729`), mirroring the fix already present in
   `compile_binding`'s Meta arm.

Measured effort/risk of the indicated fix (from the POC, which is this fix):
~15 lines across `xtask/src/main.rs`, `src/eval/stg/mod.rs`,
`src/eval/stg/compiler.rs`; **no blob wire-format change; zero blob size
change; laziness preserved** (`create_arg_array_eager` passes the existing
closure — which may still be an unforced, shared, memoisable thunk — instead
of allocating an alias wrapper; it forces nothing); harness 505/506 with the
only failure pre-existing. Residual risks for the real PR: (a) mutually
recursive prelude pairs are not covered by a self-name match (none of the
eu-e3c3i population is mutually recursive); (b) Engine Performance protocol
applies — shipped-blob shape changes need a measured-verified before/after on
the canonical suite plus the scaling curves; (c) owner decision, since it
changes the shipped blob. BV3/CG4 remain justified on their own, different
population (general cross-function-call lookup cost, per the corrected
eu-98zg/day07 account) — but the eu-e3c3i quadratic should not be routed
there. eu-7x0r remains a separate fix.

## 7. Artefacts

- Diagnostic code (all **uncommitted**, this worktree only): `xtask/src/main.rs`
  (`prelude-compile-poc selfrec|demand <out>`), `src/eval/stg/mod.rs`
  (`compile_named`), `src/eval/stg/compiler.rs` (`compile_with_self_recurse`,
  Meta-arm threading), `src/driver/eval.rs` (`EU_PRELUDE_BLOB_PATH`).
  Default `prelude-compile` output verified byte-identical (sha256
  `de68f848…`).
- POC blobs (scratchpad, not checked in): `poc-selfrec.blob` (598,731 B),
  `poc-demand.blob` (600,594 B).
- Traces/histograms (scratchpad): `trace_{5,20}.txt`, `h{10,20,30}.txt`;
  bench programs `scale_*.eu`, `tiny_*.eu`, `sf_*.eu`, `strlen.eu`.
- Key code: `src/eval/machine/env_builder.rs:99-102,266-301`,
  `src/eval/machine/vm.rs:443-479,502-558`,
  `src/eval/stg/compiler.rs:908-915,1140-1143,1232-1247,1729`,
  `src/core/analyse_demand.rs:148-161`, `xtask/src/main.rs` step 6.
