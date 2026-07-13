# Blob prelude string/list penalty — Phase-1 diagnosis (eu-2sa6.12)

- **Date:** 2026-07-13
- **Bead:** eu-2sa6.12 (P1). Report-only spike; no fix landed.
- **Worktree/branch:** `/tmp/eu-blobdiag`, `spike/blob-string-penalty` off master
  `dded5b90`.
- **Toolchain:** rustc 1.97.0 (`stable-aarch64-apple-darwin`); macOS 26.5.1
  (Darwin 25.5.0), Apple aarch64.
- **Two binaries, one source tree:** blob config = `xtask prelude-compile` +
  `cargo build --release` (`/tmp/eu-blobdiag-blob-eu`); source config =
  `rm -f lib/prelude.blob` + rebuild (`/tmp/eu-blobdiag-source-eu`). Both wrapped
  in `timeout`, `--heap-limit-mib 12288`.
- **Machine load:** elevated during the session (concurrent agent builds; 5-min
  load 10–15, 1-min dipped to ~2.4 for the timed rounds). Wall figures are
  therefore **measured-single**. The load-bearing evidence is the **deterministic**
  layer (ticks / allocs / blocks / max-stack — drift-free) and **profile shares**
  (robust to load per PROTOCOL §4); both are unambiguous and mutually corroborating.

---

## 1. Executive summary

The embedded prelude blob makes string- and list-heavy workloads **~2× slower on
HeapSyn and ~1.3–1.6× slower on bytecode**, at **identical tick and allocation
counts**, on both engines. The cause is **not** decode cost, allocation
explosion, or cache locality of scattered nodes.

**Mechanism:** blob generation (`xtask/src/main.rs`) **deliberately skips the
`inline()` pass** so it can keep one `LambdaForm` per prelude name. `inline()`
does two independent jobs — cross-binding inlining *and* intra-body Let/thunk
flattening. Skipping it wholesale preserves every prelude body's deep internal
nesting (**1874 thunks / 538 letrec, max pretty-print nesting 171** in the loaded
globals) that full-source compilation flattens away (**4 thunks, nesting 34**).
Those deep bodies become **deep runtime lexical-environment chains**;
`EnvironmentFrame::get` (HeapSyn) / `enter_local` (bytecode) are O(chain depth)
and run on every variable reference, so the same tick count costs more wall.
String work is hit hardest because structural `=` (`s = ""`) and interpolation
`JOIN` resolve many variables per list element through these un-flattened prelude
helpers.

**Recommended fix:** at blob generation, flatten each *peeled* binding body
(apply `reflatten` / the non-eliminating, non-cross-inlining part of the
simplifier) **after** the peel step, so each stored form keeps its own global
slot but has shallow internal env nesting. Generation-only, wire-format-preserving
(still one `LambdaForm` per name; just flatter). Projected to return blob to
~parity with source on 018/019.

**Confidence:** high on the mechanism (deterministic + dual-engine profile
agreement + exact source-line root cause). Medium on the fix's *magnitude* until a
Phase-2 implementation spike confirms per-binding reflatten reproduces source's
shallow chains without collapsing the binding set.

---

## 2. Reproduction (018_string_scale, `-t bench-string-scale`)

### 2.1 Deterministic layer (HeapSyn `-S`, drift-free)

| Metric | blob | source | note |
|---|--:|--:|---|
| Ticks | 76,814,993 | 76,751,915 | **identical** (Δ 0.08%) |
| Allocs | 364,176 | 385,181 | blob **fewer** (−5.5%) |
| Blocks allocated | 5,654 | 5,016 | blob **more** (+12.7%) |
| Max Stack | **7,019** | **21,017** | blob **3× shallower** |
| GC collections | 0 | 0 | at 12 GiB, as production |

Same logical reductions, **fewer** allocations, yet more resident blocks and a
3× shallower continuation stack. This rules out an allocation-count story and
points at a structural difference in how the two configs lay out execution.

### 2.2 Wall (interleaved bc/hs/bc/hs, N=5, 1-min load ~2.4; measured-single)

| | blob | source | blob penalty | bc/hs |
|---|--:|--:|--:|--:|
| bytecode | 2.13 (2.09–2.22) | 1.68 (1.55–1.76) | **1.27×** | blob 0.562 / source 0.903 |
| HeapSyn | 3.79 (3.63–3.96) | 1.86 (1.85–1.88) | **2.04×** | — |

Matches the clean-room re-baseline (§3 of `2026-07-13-clean-room-rebaseline.md`:
bc/hs blob 0.566 / source 0.923). The bc/hs *ratio* distortion on 018 is entirely
the HeapSyn denominator degrading ~2× under blob.

### 2.3 019_list_scale (`-t bench-list-scale`) — same signature

| Metric | blob | source |
|---|--:|--:|
| Ticks | 55,803,488 | 55,707,408 (identical) |
| Allocs | 156,182 | 174,189 (blob fewer) |
| Blocks | 3,220 | 2,510 (blob +28%) |
| Max Stack | **6,016** | **18,008** (3× shallower) |
| bc wall (N=3) | 1.90 (1.74–1.90) | 1.21 (1.19–1.25) → **1.57× (+57%)** |

Identical ticks, blob fewer allocs / more blocks / 3× shallower stack — the exact
018 signature.

---

## 3. Mechanism: deep prelude env chains, not decode or locality

### 3.1 Profiles (`sample` of the real eval-thread PID; shares are the metric)

**HeapSyn, 018** — heaviest self-time leaf in *both* configs is
`EnvironmentFrame::<Closing<NonNull<HeapSyn>>>::get` (the lexical frame walk):

| leaf | blob | source |
|---|--:|--:|
| `EnvironmentFrame::get` | **1359 (55.1%)** | **509 (38.4%)** |
| `MachineState::handle_instruction` | 767 | 460 |
| `Machine::run` | 321 | 332 |

Env-frame resolution jumps from 38% to 55% of eval-thread work under blob. Same
number of `get` calls (ticks equal) → each `get` walks **more frames**.

**Bytecode, 018** — the analogous local-env primitives dominate the delta:

| leaf | blob | source |
|---|--:|--:|
| `enter_local` | **433** | 310 |
| `dispatch` | 408 | 404 |
| `handle_op` | 293 | 298 |
| `drop_glue<ExecutionError>` | **257** | 53 |
| `read_ref` | 104 | 99 |

`enter_local` +40% and a **5× jump in `drop_glue<ExecutionError>`** (the `Result`
returned by ref/local resolution being dropped) — again pure local-environment
resolution.

**Bytecode, 019** — even stronger (explains the larger bc penalty on lists):
`enter_local` blob **476** vs source **173** (2.75×); `drop_glue<ExecutionError>`
125 vs 41 (3×).

### 3.2 The structural cause (`dump runtime`, both configs)

| | blob | source |
|---|--:|--:|
| runtime-dump lines | 8,522 | 1,879 |
| `thunk` occurrences | **1,874** | **4** |
| `letrec` | 538 | 11 |
| `let` | 842 | 155 |
| max pretty-print nesting (env-depth proxy) | **171** | **34** |

- **Source config** compiles `lib/prelude.eu` *together with* the user program and
  runs the full pipeline including `inline()`. Prelude helpers are inlined and
  flattened into the user code: runtime globals shrink to intrinsic wrappers +
  a handful, 4 thunks, shallow nesting. The deep *continuation* stack (21k) is
  inlined recursion; the *lexical* chains are shallow → cheap `get`.
- **Blob config** loads the prelude as **352 standalone global closures**, each
  retaining its full internal `let`/`letrec`/`thunk` nesting (1874 thunks, nesting
  171). Calls into them build **deep lexical env chains**; the continuation stack
  stays shallow (7k) but every variable reference walks a long frame list.

### 3.3 Root cause — exact source line

`xtask/src/main.rs`, blob generation, lines 168–175:

```
// NOTE: We skip `inline()` deliberately: the inline pass aggressively
// folds internal Let bindings into their single call site, reducing the
// ~295 prelude top-level bindings to only a handful in the compiled STG.
// Preserving all Let bindings here ensures the blob has one LambdaForm
// per prelude name ...
loader.fuse_destructure().context("fuse_destructure")?;
```

Generation runs parse → desugar → cook → `hoist_namespaces` → `fuse_destructure`,
then peels each binding to its own global slot — but **never flattens the peeled
bodies**. `inline()` conflates two effects; the skip correctly avoids
cross-binding folding (which would destroy the one-form-per-name property) but
also throws away the **intra-body flattening** that source config gets for free.
The `inline_cores` list (133 entries) only captures combinator-shaped bodies
(`App(Intrinsic, vars)` / `Var`); the nested hot helpers — structural `=`,
`str`/`JOIN` concat, `foldl`, `+` overload dispatch — are **not** covered and
remain deep global closures.

### 3.4 Why the workload asymmetry (item 4)

- **String (018) hits HeapSyn hardest (2.04×):** HeapSyn's `EnvironmentFrame::get`
  is a genuine recursive pointer-chase up the frame list (true O(depth)). `s = ""`
  expands to recursive structural equality plus `JOIN` interpolation — many
  per-element variable resolutions through the deepened chains. Bytecode's
  `enter_local` is a flatter offset access, so it absorbs the same depth for only
  1.27×.
- **List (019) hits bytecode relatively harder (1.57×):** the `map(_ + 1)` / `sum`
  fold's inner closures are the ones deepened under blob, and bytecode
  re-resolves them via `enter_local`/`read_ref` every element (2.75× the sample
  share). HeapSyn on 019 is already dominated by the shared O(n²) fold/env-walk
  ceiling, so the blob delta is proportionally diluted there.
- **Arithmetic (fib blob 1.13, day03 1.39) barely moves:** those touch few, mostly
  `inline_cores`-covered combinators (`SUB`, comparison) per tick, so their env
  chains are not meaningfully deepened.

The single cause (deeper prelude env chains under blob) manifests at a
magnitude set by *which* hot closures a workload deepens × *which* engine's
env-access primitive re-walks them.

---

## 4. Recommended fix + owner decision points

### 4.1 Fix (Phase-2 candidate)

**Flatten each peeled binding body at blob generation, after the peel step, before
serialising.** Apply `reflatten` (and the non-eliminating, non-cross-inlining
subset of the simplifier) to each `binding_body`. This preserves one `LambdaForm`
per prelude name (the property the current skip protects) while collapsing the
internal `let`/`letrec`/`thunk` nesting so the stored forms present **shallow env
chains** — matching what source compilation produces per body.

- **Wire-format-preserving:** still one form per binding; only the bodies get
  flatter. No decoder, VM, or blob-schema change. A `xtask prelude-compile`
  regeneration ships it.
- **Projected effect:** brings blob HeapSyn 018 from 2.04× toward source's 1.86 s
  (removing ~1.9 s / ~50% of the string penalty), blob bc 018 1.27×→~1.0, blob bc
  019 1.57×→~1.0. Magnitude is **projected** pending the Phase-2 spike — the
  residual gap is whatever source additionally gains from *cross-boundary*
  inlining of prelude calls into user code, which per-body flattening cannot
  recover (that would need the `inline_cores` route, below).
- **Alternative / complement — expand `inline_cores`** to cover the nested hot
  helpers so user-side inlining flattens them into user code (full source parity).
  More invasive, risks the binding-count growth the current comment warns about,
  and needs care to keep bodies self-contained. Recommend only if per-body
  reflatten leaves a material residual.

### 4.2 Does it survive lever (a)? — **YES, unless generation is changed.**

Lever (a) (pre-decode, `2026-07-13-predecode-spike.md`) reworks blob **decode**
but **keeps the wire format**. The deep nesting is baked into the **stored** forms
(both the `nodes`/`forms_pool` arena the HeapSyn engine loads *and* the pre-encoded
`bytecode` image). Decoding them faster does not flatten them, so **this defect
survives lever (a)**. Because the fix is a **generation-time** transform, it should
either (i) land now as an independent 0.13 change (low risk: regeneration only), or
(ii) be folded into lever (a)'s generation step. Given released binaries embed the
blob and pay up to ~2× on string-heavy workloads *today*, landing it promptly in
0.13 is warranted; it does not need to wait for lever (a).

### 4.3 Owner decision points

1. **Land in 0.13 now, or bundle with lever (a)?** Recommend now — generation-only,
   independent, and users pay today.
2. **Per-body reflatten (4.1) vs expand `inline_cores` (4.1 alt) vs both?**
   Recommend per-body reflatten first; measure residual before touching
   `inline_cores`.
3. **Scope of Phase-2 spike:** confirm per-binding reflatten (a) reproduces
   source's shallow chains, (b) preserves one-form-per-name and all type-summary
   bindings, (c) yields byte-identical rendered output on both engines across the
   canonical suite, and (d) does not regress blob load time or size.

---

## 5. Evidence index (this worktree)

- Binaries: `/tmp/eu-blobdiag-blob-eu`, `/tmp/eu-blobdiag-source-eu`.
- Profiles: `/tmp/prof-{blob,source}.txt` (HeapSyn 018),
  `/tmp/prof-bc-{blob,source}.txt` (bytecode 018),
  `/tmp/p019-bc-{blob,source}.txt` (bytecode 019).
- STG/runtime dumps: `/tmp/{blob,source}-stg.txt`, `/tmp/{blob,source}-rt.txt`.
- Root cause: `xtask/src/main.rs` §168–175; `src/eval/stg/runtime.rs` `globals()`;
  `src/eval/stg/blob.rs` `PreludeBlob`.

---

## 6. Addendum (2026-07-13, Phase-2): the §4.1 fix is REFUTED — record correction

**Status of §4.1 ("flatten each peeled binding body / apply `reflatten`"): REFUTED
by implementation and measurement.** Do not carry it forward as the fix
recommendation. This addendum documents why, and states the open candidate that
replaces it. Implementation evidence: branch `perf/blob-reflatten` (worktree
`/tmp/eu-blobfix`, pushed, no PR — a spike/evidence artefact, not a production
change).

### 6.1 What was implemented

Exactly what §4.1 recommended: `core::reflatten::reflatten()` applied to each
peeled binding body in `xtask/src/main.rs`, immediately after
`peel_all_let_bindings` and before STG compile, with deterministic
before/after instrumentation (Let-node count, max nesting depth) printed by
`xtask prelude-compile`.

### 6.2 Result: a byte-for-byte no-op

- **Core-level stats, all 352 peeled bindings: Let nodes 124 → 124, max nesting
  depth 4 → 4 — unchanged.** There was essentially nothing for a Let-chain-merge
  pass to find.
- `dump runtime` output is **byte-identical** (0-line diff) between the
  unmodified blob and the reflattened blob, for both `018_string_scale.eu` and
  `001_naive_fib.eu`.
- Ticks/allocs/blocks/max-stack identical for 018 (76,814,993 ticks, unchanged)
  and fib (HeapSyn ticks 115,779,125 → 115,779,125, exact same-commit A/B — safe,
  zero regression, but also zero effect).
- Wall times unchanged: 018 blob-config still ~4.5–4.6 s HS / ~2.1–2.2 s bc vs
  source's ~1.86 s / ~1.68 s. **No movement at all.**

### 6.3 Why §4.1 was wrong: the whole-file metric flaw

The "1874 thunks / 538 letrec / max nesting 171" figures in §3.2 came from
`dump runtime`'s pretty-printer over the **entire loaded prelude** — all 352
bindings' `:doc`/`:type` metadata blocks and bodies concatenated into one
output, with indentation measured as the single deepest point *anywhere in that
file*. That is a coarse whole-file proxy, not the hot path's actual per-call
lexical env-chain depth, and it does not correspond to Core-level Let nesting
within any individual binding. §3.2 should have been checked at the Core level
(pre-STG-compile) before recommending a Core-level flattening pass — it wasn't,
and the recommendation followed the wrong number.

The §3.1 profile evidence (`EnvironmentFrame::get` / `enter_local` dominating
the eval-thread samples, 55% vs 38% on HeapSyn 018) remains valid and was
re-confirmed; only the §3.2/§4.1 causal attribution and fix were wrong. Deep
runtime env chains are real and measured — they just are not caused by
un-flattened per-binding Let nesting, since there isn't any material nesting to
flatten (124 Lets total, max depth 4, entirely within normal function-body
range).

### 6.4 Open candidate hypothesis: the user→prelude demand-signature boundary

A follow-on spike (reverted, not in the pushed branch) wired
`analyse_demands`-derived signatures into the **blob's own internal** STG
compile (prelude-binding-to-prelude-binding calls only). It also produced no
measurable tick or wall change, and tracing why exposed the likely real
mechanism:

`src/driver/eval.rs:406-414` runs demand analysis like this on every `eu`
invocation, blob or source:

```rust
if !stg_settings.suppress_demand_analysis {
    let (annotated, _signatures, named_signatures) =
        crate::core::analyse_demand::analyse_demands(&self.evaluand);
    self.evaluand = annotated;
    stg_settings.user_demand_sigs = named_signatures;
    ...
}
```

This analyses **only `self.evaluand`** — the user's own program. In blob mode
the prelude source is never loaded into the user compile at all
(`driver/prepare.rs`: "skip loading the prelude" when the blob is active), so
`named_signatures` — and therefore `stg_settings.user_demand_sigs` — is
necessarily **empty at every call site where user code calls into a prelude
global** (`map`, `foldl`, `+`, structural `=`, string `JOIN`, …). Under
source config, the prelude is compiled *together with* the user program, so
these same call sites get real, analysed strictness signatures for free.

**Candidate mechanism:** the STG compiler falls back to a conservative
(lazier, more thunk-building) calling convention at every user→prelude call
site in blob mode, because it has no demand signature to consult there — not
because of any structural difference in the prelude bodies themselves. This
is consistent with the §2.1 finding that blob has *fewer* allocations but
*more* resident blocks (bigger, more general-purpose closures rather than
demand-specialised ones) and a *shallower* continuation stack (deferred/lazy
work rather than direct strict recursion).

**Status: untested at the user→prelude boundary, stated as the open candidate
only.** Validating it — whether merging a blob-carried
`named_demand_sigs: HashMap<String, DemandSignature>` into
`stg_settings.user_demand_sigs` under blob mode moves 018/019 materially
toward source levels, without breaking fib ticks or byte-identical harness
output — is the subject of a separate, explicitly-scoped validation spike (not
this report). If validated, an audited production bead follows; if refuted,
this defect's fix is deferred to the in-memory blob pipeline redesign (filed
separately), which sidesteps the whole boundary by not needing an isolated
prelude-only compile at all.

### 6.5 Evidence index (addendum)

- Reflatten implementation + instrumentation: branch `perf/blob-reflatten`
  (worktree `/tmp/eu-blobfix`, pushed, no PR), `xtask/src/main.rs`.
- Byte-identical `dump runtime` diffs, fib exact-A/B tick match: this session's
  transcript (not separately archived; reproducible via
  `cargo xtask prelude-compile` with/without the `reflatten` call added at the
  point documented in the branch diff).

---

## 7. Second addendum (2026-07-13, validation spike): §6.4 demand-sig hypothesis is ALSO REFUTED

**Status of §6.4's open candidate: REFUTED by end-to-end implementation and
measurement.** This closes eu-2sa6.12's search for a wire-format-preserving
generation-time fix; both candidate mechanisms have now been tried, measured,
and ruled out. Implementation evidence: branch `spike/named-demand-sigs`
(worktree `/tmp/eu-blobsig`; throwaway hack-quality spike, not pushed — see
§7.6). No production PR resulted, per instruction.

### 7.1 What was implemented

End-to-end wiring, minimal/hack quality:

1. `PreludeBlob` gained a `named_demand_sigs: HashMap<String, DemandSignature>`
   field (`src/eval/stg/blob.rs`), populated at blob-generation time in
   `xtask/src/main.rs` by running `core::analyse_demand::analyse_demands` over
   the merged (pre-peel) prelude core expression and keeping the returned
   `NamedSignatureTable` — **the peeled binding bodies themselves were left
   untouched** (this spike, unlike §6, does not modify the compiled prelude
   forms at all; it only adds a side-table to the blob).
2. `src/driver/eval.rs` (~line 406-414, the per-invocation demand-analysis
   step that already runs on `self.evaluand` for every `eu` invocation, blob
   or source) now merges `blob.named_demand_sigs` into
   `stg_settings.user_demand_sigs` via `.extend()` whenever a prelude blob is
   active, so user-code call sites into prelude globals gain access to the
   blob-computed signatures — exactly the missing link described in §6.4.

### 7.2 Result: also a no-op — `dump stg` byte-identical for the user program

- `xtask prelude-compile` collected **103 named signatures**. Blob file grew
  (461,636 vs 460,201 bytes) — the table really is being generated and
  serialised.
- But **`dump stg` for `018_string_scale.eu` is byte-identical** (0-line diff)
  between the unmodified blob and the spike blob carrying + merging
  `named_demand_sigs`. The merge reaches the compiler (`stg_settings` is
  populated) but changes **nothing** about how the user's benchmark compiles.
- Deterministic ticks/allocs/blocks/max-stack for 018, 019, and fib are all
  **unchanged** from the unmodified blob (018: 76,814,993 ticks; 019:
  55,803,488 ticks; fib HeapSyn: 115,779,125 ticks — exact matches).
- Wall times unchanged: 018 blob-config still ~2.3 s bc / ~3.8–4.2 s HS —
  **no movement toward source's ~1.68 s / ~1.86 s.**

### 7.3 Why: the two hot-path function classes both bypass the mechanism

Probing `named_demand_sigs` for the benchmark's own hot symbols
(`range(0, 7000) map(mk-line) foldl(force-count, 0)`, with
`force-count(acc, s): acc + if(s = "", 0, 1)`) explains it completely:

| Symbol | In `named_demand_sigs`? | Why |
|---|---|---|
| `map`, `foldl`, `range`, `sum`, `cons`, `nil?` | **absent** | `analyse_lam`'s signature-recording (`analyse_demand.rs` ~L609-617) only fires when a `Let`-binding's RHS is **literally `Expr::Lam`**. These prelude functions are written **point-free** (combinator composition, per the project's own style guide), so their RHS is never a bare `Lam` and no signature is ever recorded — in blob **or** source config. This is not a blob-vs-source gap; the mechanism structurally cannot see point-free definitions. |
| `+`, `=` | **present**, and genuinely strict (`[Strict/AtMostOnce, Strict/AtMostOnce]`) | But irrelevant: `(l + r): __ADD(l, r)` (`lib/prelude.eu:657`) is exactly the "combinator lambda whose body is `App(Intrinsic, vars)`" shape that the **pre-existing** `inline_cores` fixed-point (§3.3 of the original report; already shipped, not part of this spike) already collects and pre-expands. By the time STG-compile runs, `l + r` and `s = ""` have already been substituted to the raw intrinsic application by the core-level inline pass (`inline_cores` are "injected as Let bindings before the inline pass" per `blob.rs`'s own doc comment) — the compiler never sees a `Var::Free("+")` call site to look a signature up against; it resolves via `Expr::Intrinsic` / the compiler's separate `intrinsic_demand_sigs` table instead (`compiler.rs` ~L1122-1134), which already existed and is unaffected by this spike. |

Both hot-path classes bypass the injected table for structurally different,
independent reasons — one because no signature is ever recorded for
point-free code, the other because the call site is inlined away before the
lookup would occur. There is no adjustment to the injection point that fixes
both without a fundamentally different mechanism (e.g. actually inlining
prelude bodies into user code, which is the `inline()`/`eliminate` route
already ruled out of blob generation for the one-form-per-name property, or
computing signatures for combinator-shaped point-free bindings too — a
non-trivial extension to `analyse_demand`'s extraction rule, not a
generation-time wiring change).

### 7.4 Safety check (requested): do the injected sigs match what source-mode would compute?

For `+`/`=`, yes, necessarily: `analyse_lam`'s signature extraction is a
pure, bottom-up structural function of a `Lam`'s own body and has no
dependency on the enclosing scope or call site — `(l + r): __ADD(l, r)` has
no free variables to pick up outer context from, so applying `analyse_demands`
to it in isolation (blob generation, this spike) or embedded in the full
merged program (source config) is the same deterministic computation over the
same sub-tree and must produce the same signature. This isn't independently
re-verifiable via a source-mode "named-signature dump" for the same reason the
mechanism is moot in practice: source config inlines `+`/`=` away before any
per-name signature would be surfaced for comparison. The algorithmic argument
stands in place of a live A/B; it does not change the §7.2/§7.3 finding, since
the signatures — correct or not — never reach a call site that matters for
`map`/`foldl`/`range`, which is where the actual wall-clock cost lives (§3.1's
profile evidence attributes the dominant cost to exactly these functions'
call/env-resolution paths, not to `+`/`=`).

### 7.5 Correctness smoke (harness, both engines)

`cargo test --release --lib --tests` (bytecode) and
`EU_HEAPSYN=1 cargo test --release --lib --tests` (HeapSyn), full suite
(doctests excluded — a pre-existing, unrelated `rustdoc`/`RUSTC`-override
toolchain-invocation issue on this box, not a code regression), worktree
`/tmp/eu-blobsig`:

| Engine | Tests | Failures |
|---|--:|--:|
| bytecode | **1,908** | **0** |
| HeapSyn | **1,908** | **0** |

Both green. The change is additive and gated (`#[serde(default)]` on the new
blob field, `#[cfg(not(target_arch = "wasm32"))]` on the merge site,
`.extend()` never removes existing signatures), so no behavioural regression
was expected; the full harness pass confirms it. §7.2's byte-identical
`dump stg` for the one workload that exercises the new path is itself
stronger evidence of no observable-behaviour change for that program.

### 7.6 Disposition

**eu-2sa6.12 characterised-but-unfixed.** Both wire-format-preserving,
generation-time candidate fixes (§4.1 reflatten, §6.4/§7 demand-sig boundary)
are implemented, measured, and refuted. The root mechanism (§3.1: deep
runtime lexical-environment chains under blob, `EnvironmentFrame::get` /
`enter_local` dominating the profile) is confirmed and stands; no fix that
preserves the current blob wire format and one-form-per-name generation
approach has been found. Closing this bead as characterised; the redesign bead
(in-memory blob pipeline, filed separately) inherits the problem and should
be evaluated for whether a from-scratch pipeline can co-compile
prelude+user (recovering real inlining and demand propagation) without
reintroducing the "recompile prelude source every run" cost blob generation
exists to avoid.

Both spike branches (`perf/blob-reflatten`, `spike/named-demand-sigs`) are
kept as evidence artefacts; neither is intended to merge.
