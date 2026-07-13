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
