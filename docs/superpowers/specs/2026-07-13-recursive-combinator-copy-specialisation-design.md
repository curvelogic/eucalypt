# Production design — criterion-based copy-specialisation of recursive higher-order combinators

- **Bead:** eu-yuto (0.13, parent epic eu-2sa6)
- **Author:** Furnace
- **Date:** 2026-07-13 (revised 2026-07-14 after mechanism adjudication)
- **Status:** DESIGN NOTE ONLY — no implementation. Owner sign-off gates the
  build. The mechanism has been independently adjudicated (§0); the design is
  written from the verified mechanism, not from the original spike framing.
- **Supersedes:** the eu-v8n8 spike (PR #1007, `spike/inline-cores-cluster`) —
  a blob-only `inline_cores` evidence vehicle whose quantitative claims are
  **reinstated** (adjudicated, §5.1). This note specifies the **production**
  form: a semantic criterion in `tag_combinators` (`src/core/inline/tag.rs`) in
  the **shared** core pipeline, so blob, source, user-defined combinators and
  alternative preludes all fuse identically (owner architecture principle — no
  prelude special-casing).

### Framing correction (why this note is not called "peeling")

The original commission and the spike used the word *peeling* / *unrolling*,
implying a fixed number of loop iterations are copied out and the residual
recursion continues in the shared global definition. **The adjudicated mechanism
is not that** (§0, Exp 1): the inline pass creates **one full, local,
self-recursive specialised copy** of the combinator at the call site, and the
*entire* N-iteration recursion runs in that local copy (it tail-calls its own
`letrec` binding, not the global). There is no per-level unrolling and no
"unroll depth" constant. The correct name is **copy-specialisation**, and the
bloat model is *one local copy per qualifying call site*, not per-level growth.
This also disposes of the owner's arithmetic objection to the peeling account
(a bounded peel cannot linearise iterations `3..N` if they run the shared
definition — but they do not; they run the local copy).

### Provenance discipline (mandatory for every figure)

After three confounded measurement rounds during the mechanism phase (root
cause: build-time inputs — compiler source and the `include_bytes!`-embedded
`lib/prelude.blob` — changed without rebuilding the `eu` binary; see bd memory
`stale-binary-rebuild-trap`), **every quantitative figure in this note carries
per-cell provenance**: either an adjudicated cell (binary + blob SHA + worktree
commit) or an explicit `pending re-verification (Wicket)` tag. Do not treat any
untagged number as evidence. Re-verification runs under the gold-standard
protocol (one disposable `cargo clean` worktree per configuration; SHA256 of
blob + binary + bench per cell; never reuse a worktree across arms).

---

## 0. Mechanism — verified

### 0.1 Executive summary

The blob string/list penalty (eu-2sa6.12 → eu-7xvv → eu-qm7f → eu-ttpl → eu-v8n8)
resolves to a single mechanism on the recursive **higher-order** list combinators
(`map`, `foldl`, …). Run as un-fused standalone global closures, their
function/operator argument is a lazy parameter the compiler cannot see through,
so resolving it per element forces a **quadratic *count* of individually-shallow
environment lookups** — the reduction cost is env resolution by count, not by
walk depth (§0.4). When the inline pass distributes the combinator into a call
site, it produces a **local, self-recursive, operator-specialised copy**; demand
analysis then specialises that copy's now-concrete operator strictly, collapsing
the per-position repeated lookups, and the workload becomes **O(N)**. Both the
localisation and the demand specialisation are required.

### 0.2 The experimental matrix

Five experiments, adjudicated by Wicket on hash-verified `cargo clean`
disposable worktrees at commit `f8686d0a`: flag-off (`/tmp/eu-adjudicate-flagoff`,
binary `b2bf8a67…`, blob `e59640e6…`) and flag-on (`/tmp/eu-adjudicate-flagon`,
binary `af644ce8…`, blob `d6cd1555…`). Bench is the verbatim committed
`022_hof_fold.eu` (`grand: range(0, 10000) foldl((_ + _), 0)`); scaling variants
change only `N`. All figures deterministic (ticks / allocs / stack); no wall.
Ticks are **bytecode engine** with explicit `-t <target>` unless a row says
otherwise (Wicket found the no-`-t` render can double-count or short-circuit
targets). The 022 mechanism cell is reported on both engines: HeapSyn
`52,225,448 → 2,230,410`, bytecode `52,125,436 → 2,130,398`.

| # | Experiment | Result | What it establishes | Provenance |
|---|---|---|---|---|
| 1 | **Residual call target** — inspect the fused STG | flag-on dump is a fused structure that **opens with `letrec [0] λ{3}`**; its general (recursive) case **tail-calls `✳5(…)` — the letrec's own binding**, not the global slot. flag-off is the un-fused global call. STG line counts: fused ≈ 382–386, un-fused ≈ 164–168. | The whole recursion runs in **one local specialised copy**; it does **not** escape to the shared global after any number of steps. Disposes of the peeling/arithmetic objection. | Wicket adjudicated (flag-on/flag-off worktrees @ `f8686d0a`) |
| 2 | **Inline-depth sensitivity** — vary `prepare.rs:329` iteration count (`-t bench-scale`) | depth-0 (no inline pass) = **baseline quadratic** (`13,597,816 / 52,195,316 / 204,390,316` at 5k/10k/20k, ratios `3.838×`/`3.916×`); depth-1 = `1,025,304 / 2,050,304 / 4,100,304` = **exact `2.0000×`/`1.9999×` (linear)**; depth ≥1 gives no further change. | One distribution pass suffices to create the local copy. **There is no unroll-depth constant** — `PEEL_UNROLL_DEPTH` is removed from the design. | Wicket adjudicated (binaries `depth0` `b818e1e6…`, `depth1` `417b858a…`) |
| 3 | **Demand dependence** — `--suppress-demand-analysis` **on the fused (flag-on) binary** | normal `1,065,279 / 2,130,279 / 4,260,279` (ratios exactly `2.0000×`); suppress-demand `4,200,265 / 14,650,265 / 54,300,265` (ratios `3.49×`/`3.71×`, **quadratic-trending**) *with the local copy present*. **Methodology:** suppression is only meaningful flag-**on** — on the un-fused binary it is a null test (no concrete operator to specialise, ticks unchanged to 1 part in 1e6). | Demand analysis is **necessary, not merely a consequence** of localisation. Localisation alone does not linearise; the copy's operator must also be specialised strict. | Wicket adjudicated |
| 4 | **Quantity scaling of the un-fused quadratic** (5k/10k/20k, `-t`) | **ticks quadratic** (`13,562,816 / 52,125,316 / 204,250,316`, ratios `3.84×`/`3.92×`); **allocs linear** (`90,137 / 180,137 / 360,137`, `~2.0×`); **blocks linear** (`1,046 / 2,087 / 4,168`); **max-stack linear** (`5,011 / 10,011 / 20,011`); GC 0. | The quadratic is in reduction ticks; allocation, blocks and stack are all O(N). Isolates the quadratic to reduction work; *which quantity within reduction* is settled by Exp 5. | Wicket adjudicated (Furnace cross-check within ~0.1%) |
| 5 | **Env-lookup histogram** — `EU_ENV_DEPTH_HISTOGRAM` (`env.rs:36-134`, eu-qm7f), N-scaling | **per-lookup depth flat at all N** (mean `0.979 / 0.989 / 0.995`, **max exactly 2**); **total `EnvironmentFrame::get` calls `13,157,734 / 51,315,234 / 202,630,234`** (5k/10k/20k), tracking `N(N−1)/2` (`12,497,500 / 49,995,000 / 199,990,000`) to within `0.7–3%`; **lookups/ticks `0.967 / 0.983 / 0.991`**, converging on ~1:1. | The quadratic is a quadratic **count** of shallow env lookups — **env resolution by count, not by walk depth, and not thunk-chain reduction**. Settles §0.4. | Wicket adjudicated (flag-off worktree @ `f8686d0a`) |

### 0.3 The causal account (survives the arithmetic)

Un-fused, `foldl`'s operator is an opaque lazy parameter; resolving it per
element drives a **quadratic count of shallow environment lookups** (Exp 4+5,
§0.4). The inline pass (`Loader::inline` = `tag_combinators` then
`reduce::inline_pass` = `distribute ∘ beta_reduce`) **distributes the combinator
body into the call site as one local `letrec` copy that recurses on its own
binding** (Exp 1) — so every one of the N iterations runs the local copy, with
the concrete operator `(_ + _)` beta-substituted in. Demand analysis, running
per-invocation on the user core, then sees a concrete operator and specialises it
**strict** (Exp 3), collapsing the repeated per-position lookups to O(1) each —
**O(N)** overall. Remove *either* the localisation (Exp 2, depth-0) *or* the
demand specialisation (Exp 3, suppress-demand) and the quadratic returns. Both
legs are load-bearing.

### 0.4 The quadratic's shape — SETTLED (env resolution by count, not depth)

Direct measurement (Exp 5, `EU_ENV_DEPTH_HISTOGRAM` on the hash-verified
flag-off worktree, N-scaling) settles what the O(N²) is:

- **Per-lookup depth is flat at every N** — mean `0.979 / 0.989 / 0.995`, max
  exactly 2. The environment walk does **not** get longer with N or with chain
  position (unchanged from eu-qm7f's original shallow-walk finding).
- **The number of lookups is quadratic.** Total `EnvironmentFrame::get` calls are
  `13,157,734 / 51,315,234 / 202,630,234` at N = 5k/10k/20k, tracking the
  triangular number `N(N−1)/2` (`12,497,500 / 49,995,000 / 199,990,000`) to
  within ~0.7–3% and converging — the classic "position k does ~k lookups"
  signature. The **lookups-per-tick ratio is `0.967 / 0.983 / 0.991`**,
  converging on ~1:1 — i.e. the tick cost *is* the lookup count.

So the quadratic is a **quadratic count of individually-shallow environment
lookups**: resolving the lazily-threaded operator/bindings is repeated for every
position below the one being forced, giving the triangular signature. It is
**env resolution by count, not by walk depth** — and it is **not** thunk-chain
reduction per se (Exp 4's linear stack is the deep structure being consumed, not
the source of the quadratic).

This reconciles the prior accounts cleanly:
- **ROADMAP §6.4's instinct was right, its literal mechanism wrong.** The cost is
  environment resolution — but not because walk *depth* grows (it never does);
  because the *count* of shallow lookups grows quadratically.
- **My earlier "NOT env re-resolution" reading was wrong.** It *is* env
  resolution — measured by count rather than depth.
- **eu-qm7f is unaffected.** It measured walk *depth* (shallow, 2–3) and stands;
  Exp 5 adds the orthogonal *count* half.

The design does not hinge on this, but it fixes the **BV3/CG4 sizing target**
(§5.3): the lever must eliminate the *repeated per-position lookups* (CG4
operator lifting / BV3 register frames replacing `get()`-indirection), **not
shorten walk distance** — walk distance was never the bottleneck.

---

## 1. The criterion

`tag_combinators` (`tag.rs:18`) today tags a lambda `Lam(_, false, _)` as
inlinable (`Lam(_, true, _)`) iff `closed_body` (every node a parameter ref, a
literal, an intrinsic, or an `App` of closed sub-expressions — **no free
variables, no `Let`, no nested lambdas**) or `destructuring`. A self-recursive
combinator such as `map(f, l): if(l nil?, l, cons(l head f, l tail map(f)))`
fails `closed_body` on three counts: it references `map`/`cons`/`if`/`nil?` as
free variables, its body is not a bare application tree, and it recurses. The
production criterion adds a third admission leg, `recursive_combinator`.

### 1.1 The two load-bearing legs

A `Lam(_, false, scope)` is tagged inlinable under the new leg when **both**
hold:

1. **Higher-order self-recursion — it applies one of its own parameters, and
   recurses on its own name.** `map` applies `f` (`l head f`), `foldl` applies
   `op` (`op(i, l head)`); both call themselves. **Justification:** the O(N²)
   pathology is *created* by a lazily-passed operator being applied per element
   (Exp 4), and the fix is localisation + demand specialisation of that concrete
   operator (Exp 1, 3). `fib` — first-order self-recursion, applies no parameter
   — is the **null control**: it neither qualifies nor benefits (ticks
   bit-identical flag-on/off, per Wicket's adjudicated null-control cell). A
   first-order recursive function (`take`, `drop`, `iota`) has no
   parameter-application indirection to collapse, so it is correctly excluded.

2. **Body within a size cap** (`MAX_COPY_BODY_NODES`, §1.2). **Justification:**
   the transform reproduces the body as one local copy **per qualifying call
   site** (§1.2), so an unusually large recursive combinator would bloat user
   code in proportion to its call-site count. The cap is a guard against a
   single pathologically large combinator (a hand-written recursive
   parser-combinator, say), keeping per-admitted-binding cost bounded.

**Leg 3 of the original draft is deleted.** It claimed the self-reference
resolves to the shared global slot (`Ref::G`) as "the residual recursion after
the bounded unroll". Exp 1 refutes this: the self-reference resolves to the
**local `letrec` binding** of the specialised copy (`✳5(…)`), and the whole
recursion is local. Free-variable resolution is still required for safety, but
it is a property of the inline set (§3.2), not a statement about where recursion
goes.

### 1.2 Named constants

| Constant | Proposed value | Rationale |
|---|---|---|
| `MAX_COPY_BODY_NODES` | **48** core-expr nodes | The 8 qualifying combinators' bodies are 6–30 nodes; 48 admits `scanr`/`window`-class bodies with headroom while rejecting anything an order larger. A cap by *node count* (not bytes) is representation-stable. Measure-and-tune during implementation on the qualifying-set survey (§2). |

`PEEL_UNROLL_DEPTH` (a proposed constant in the original draft) is **removed**.
Exp 2 shows one distribution pass produces the single local self-recursive copy
and depth beyond 1 changes nothing; there is no unroll count to tune. The bloat
model is therefore **one local copy per qualifying call site**.

### 1.3 Margins (where the criterion must be explicit)

- **Mutually-recursive pairs** (`f` calls `g`, `g` calls `f`, neither calls
  itself). Leg 1 as stated ("recurses on its own name") **excludes** these. This
  is deliberate for the first version: mutual recursion needs both members in
  the inline set and a cycle-aware fixed point, and no qualifying prelude
  combinator is mutually recursive (§2). Stated and deferred; revisit only if a
  measured mutual-recursive hot combinator appears.
- **Combinators that apply a parameter only on some branches** (exactly
  `map`/`foldl`'s shape — the parameter is applied only in the recursive
  branch). Leg 1 requires the parameter be applied *somewhere* in the body, not
  on every path. This is correct: demand analysis handles per-branch strictness
  itself, and the laziness-preservation test (188, §4.2) covers the unused
  branch.
- **Recursion via a nested local helper** (`take-while(p?, l): { aux(xs,
  prefix): … aux(xs tail, …) … }.aux(l, [])`). The *top-level* binding is **not**
  self-recursive — the recursion lives in the nested `aux`. Leg 1 operates on
  the top-level binding, so `take-while`/`window`/`window-all` do **not** qualify
  directly (their `aux` is not a top-level combinator). They still benefit
  transitively wherever they call a qualifying combinator. Stated; a future
  refinement could tag nested self-recursive higher-order lambdas, out of scope
  here (no measured need).

---

## 2. Qualifying-set survey and full-set measurements

### 2.1 The set the criterion admits

Surveyed against `lib/prelude.eu` (checked, not guessed). **Directly qualifying**
(self-recursive, applies a parameter, within cap):

| Combinator | line | applies | note |
|---|---|---|---|
| `map(f, l)` | 1463 | `f` | |
| `foldl(op, i, l)` | 1403 | `op` | |
| `foldr(op, i, l)` | 1411 | `op` | |
| `scanl(op, i, l)` | 1415 | `op` | |
| `scanr(op, i, l)` | 1419 | `op` | nested block-lookup body (near cap) |
| `map2(f, l1, l2)` | 1474 | `f` | `zip-with` is an alias of this |
| `drop-while(p?, l)` | 1359 | `p?` | |
| `iterate(f, i)` | 1423 | `f` | infinite generator; local copy is safe |

**Benefit transitively** (not self-recursive themselves, but defined in terms of
a qualifying combinator, so they fuse once their callee is copy-specialised):
`filter` (→`foldr`, 1498), `zip`/`zip-with` (→`map2`, 1478/1482),
`all`/`all-true?` (→`map`/`foldl`), `any`/`any-true?` (→`map`/`foldr`), `mapcat`
(→`map`, 1525), `remove` (→`filter`), `zip-apply`/`zip-kv` (→`zip-with`),
`map-values`/`map-keys` (→`map`).

**Correctly excluded** — first-order recursion (no parameter applied):
`take`/`drop`/`repeat`/`iota`/`cycle`/`interleave`/`stream-advance`/`window`/
`window-all`; and nested-`aux` recursion: `take-while`/`take-until`.

### 2.2 Full-set bloat and blob-gen cost

The `map`+`foldl` cell is **Wicket-adjudicated** (re-derived by
`xtask prelude-compile` in both worktrees, blob SHA256 byte-identical
before/after each derivation — a safe re-derivation, not a rebuild-drift risk):

| Metric | flag-off (map+foldl absent) | flag-on (map+foldl only) | Δ |
|---|---|---|---|
| blob file size | 461,638 B | 461,979 B | **+341 B** |
| inline_cores count | 133 | 135 | +2 |
| arena nodes | 7,104 | 7,104 | **0** |
| STG lambda forms | 352 | 352 | 0 |
| bytecode size | 269,176 B | 269,176 B | **0** |

Wicket's mechanistic note: **the bytecode image is unchanged, and this
generalises to *any* `inline_cores` membership change** — bytecode compiles from
the prelude's own STG forms unconditionally, whereas `inline_cores` membership
only affects which raw Core is additionally serialised for injection
(structurally independent of *which* combinators are in the set). So the
qualitative "bytecode-image unchanged" claim holds for the full-8 set too.

The **full-8-combinator bloat figure is not measurable from existing code** and
is tagged **requires a prototype** (not "pending re-verification"): the spike's
`CLUSTER` list is hardcoded to `["map", "foldl"]` (`xtask/src/main.rs:334`); the
other six combinators are not implemented in any buildable branch, so no rerun
can produce the number. The original draft's **+1,505 B for 8** is *consistent in
shape* with the adjudicated **+341 B for 2** (~188 vs ~170 B/combinator) — a
plausibility check, not proof. Gate: the implementation PR carries an
adjudicated full-set blob-size + bytecode-size + arena-node cell before/after.

### 2.3 Full-set tick effect on the canonical suite

**All eight cells are Wicket-adjudicated** (bytecode engine, `-t <target>`,
`-I` for 021; worktrees @ `f8686d0a`, binaries `b2bf8a67`/`af644ce8`). The
`map`+`foldl` flag reproduces the carried-over spike figures to within 0.1–0.3%
for seven rows; **020 was corrected** (see below).

| Bench | class | flag-off | flag-on | Δ | allocs off/on | Provenance |
|---|---|---|---|---|---|---|
| 022_hof_fold | C | 52,125,436 | 2,130,398 | **−95.91%** | 180,163 / 217,643 | Wicket adjudicated |
| 018_string_scale | G | 76,730,983 | 46,020,189 | −40.02% | 364,176 / 395,650 | Wicket adjudicated |
| 019_list_scale | H | 55,719,473 | 51,160,960 | −8.18% | 156,182 / 160,676 | Wicket adjudicated |
| 016_import_export_yaml | I | 59,711,048 | 56,042,568 | −6.14% | 1,794,817 / 1,814,846 | Wicket adjudicated |
| 017_import_export_toml | I | 59,537,724 | 55,869,244 | −6.16% | 1,158,667 / 1,178,696 | Wicket adjudicated |
| **020_lookup_curve** | E | **164,630,267** | **164,465,805** | **−0.10%** | 5,553,215 / 5,595,923 | Wicket adjudicated (**corrected**, see note) |
| 015_block_merge | D | 98,669,543 | 96,056,695 | −2.65% | 448,718 / 456,248 | Wicket adjudicated |
| 021_io_loop | L | 3,958,278 | 3,958,278 | 0% (io-bound) | 499,537 / 499,537 | Wicket adjudicated |

**020 correction.** The original draft cited baseline `1,721,105` / `1,556,643`
(−9.6%). That baseline was measured on **HeapSyn by mistake** — `EU_HEAPSYN=1`
on the identical flag-off binary/file reproduces `1,721,105` exactly, while the
bytecode engine (every other row) gives `164,630,267 → 164,465,805 = −0.10%`. A
one-row engine mix-up, not a re-verification gap. Corrected, **020 does not
meaningfully benefit** from `map`/`foldl` fusion (its `foldl` call is a minor
fraction of a lookup-dominated workload).

The full set is monotonically non-regressive and **extends the win beyond
`map`/`foldl` to the import (016/017) workloads** via `filter`/`foldr`/`all`/
`any` transitively — the concrete argument for **admitting the whole
criterion-qualifying set, not a curated list**. (The earlier claim that the win
also extends to the lookup workload 020 is **withdrawn** — 020 is
lookup-dominated and does not benefit, as the corrected row shows.) These are
`map`+`foldl`-only figures; the full-8-set deltas require a prototype (§2.2).

---

## 3. Both-paths mechanics

The owner principle is that the fix must apply to both prelude approaches
identically, with no prelude special-casing. `tag_combinators` is the single
shared choke point that delivers this.

### 3.1 Source (merge-cook) path

`Loader::inline` (`driver/source.rs:743`) runs `tag_combinators` then
`reduce::inline_pass` on the merged core expression — which, on the
source-prelude path, **contains the prelude bindings merged into the user
program**. Extending `tag_combinators` therefore tags the merged `map`/`foldl`
lambdas inlinable, and `inline_pass` distributes each into the user's call sites
as a local specialised copy. No new source-path wiring is needed — the criterion
change alone activates it.

### 3.2 Blob (generation) path

On the blob path the prelude is pre-compiled; the combinator body reaches the
user compile only if it is in the blob's `inline_cores`, injected as a local
`Let` scope by `inject_prelude_inline_cores` (`source.rs:720`, called at
`prepare.rs:323`). **Two things are jointly required for the win on this path,
and the spike shipped only the first** (which is why the flag needs the
criterion to be more than an evidence vehicle):

1. **Local availability:** `map`/`foldl` must be in `inline_cores` so injection
   makes them locally visible. `xtask`'s fixed-point (`xtask/src/main.rs` §4b)
   admits a binding when `Lam(_, true, _)` **and** `all_free_vars_in_set(body,
   set)` (`tag.rs:80`). The recursive combinators must (a) be tagged `true` by
   the §1 criterion and (b) have `all_free_vars_in_set` treat the binding's
   **own name** as resolvable (the self-reference is satisfied by the binding's
   own injected `Let` slot). This is a two-line predicate extension.
2. **Inlinability tag:** the injected local binding is only distributed if
   `inlinable()` (`reduce.rs:92`) returns true for it — i.e. it is tagged
   `Lam(_, true, _)` by the §1 criterion. Injection **without** the criterion is
   inert: `distribute` skips an untagged local binding. (This is the mechanism
   the shared criterion supplies on both paths.)

The injected `inline_cores` `Let` scope makes all members mutually visible, so a
copy-specialised combinator's operand references bind to their (already-expanded)
siblings.

### 3.3 User-defined combinators and alternative preludes

Because the criterion is purely structural and lives in the shared inline pass,
a *user's* own recursive higher-order combinator (`my-fold(op, acc, xs): …`) is
tagged and copy-specialised by the same code, in the same compile, with no
prelude knowledge. Alternative preludes (`{ prelude: "…" }`) route through the
identical pipeline. This is the property the blob-only `inline_cores` hook could
not provide.

### 3.4 Tripwire implications — **projected** (needs the impl + adjudicated cells)

- The eu-v8n8 blob-only divergence closes. The spike reported source 018 = 76.8M
  vs blob 018 = 46.1M (a ~1.665× divergence). With the criterion on both paths,
  the source merge-cook pass copy-specialises the same `map`/`foldl` →
  **projected source 018 ≈ 46M**, returning the map/foldl-workload divergence to
  ≈1.0. Projected; the implementation PR must land an adjudicated source-vs-blob
  018 cell.
- The **fib-based tick-parity tripwire is unaffected**. `fib` is first-order and
  does not qualify (Exp-adjacent null control), so its ticks do not change on
  either config. The tripwire measures the demand-analysis handicap on strict
  first-order recursion, which this change does not touch; that remains eu-npp9's
  domain (§5.2). The design states plainly that this change does **not** move the
  tripwire toward parity.

---

## 4. Demand-analysis interaction and the metadata override

### 4.1 Why the demand specialisation is necessary (not merely consequent)

`analyse_demands` runs per-invocation on the user's core expression
(`driver/prepare.rs:374`). The original draft called the strict specialisation a
mere *consequence* of exposing the concrete operator. **Exp 3 corrects this:**
on the **fused (flag-on)** binary with demand analysis suppressed, the workload
is still quadratic-trending (`3.49×`/`3.71×`). (Methodology: suppression is only
meaningful flag-on — on the un-fused binary it is a null test, since there is no
concrete operator to specialise before fusion; ticks are unchanged there.) So
localisation and
demand specialisation are **two independently necessary legs**: the copy exposes
the concrete operator, and demand analysis must then specialise it strict so the
repeated per-position env lookups (§0.4) collapse to O(1) per step. Neither alone
linearises 022.

### 4.2 Where it cannot specialise (and why that is safe)

Where the operator genuinely *is* lazy in a position (an unused argument, a
conditionally-forced branch), demand analysis is sound by construction and leaves
it lazy — copy-specialisation changes efficiency, not evaluation order. Evidence:
the 188 laziness regression (an unused argument threading a
`map`/`foldl`-with-a-bomb through recursion — forcing it panics) passes flag-on
and off on all three engines, as do 170/171/179/181. The production design
**ports test 188** (it becomes engine- and path-independent, running on the
default build once the criterion lands). Note eucalypt evaluates list *elements*
eagerly (lazy streams deferred), so there is no element-laziness to break; 188
targets the real property — that copy-specialising a body into an unused position
is not promoted strict.

### 4.3 Metadata override — **recommend against (for now)**

An optional `` ` { inline: true|false } `` annotation could force/forbid
copy-specialisation per binding. **Recommendation: do not add it in the first
version.**
- An opt-*in* (`inline: true`) is unnecessary — the criterion is the gate.
- An opt-*out* (`inline: false`) is a plausible size escape hatch, but adds a
  user-visible surface and a footgun (silently disabling a large win) for a need
  that has not appeared. The size cap (§1.2) already handles the "too big" case
  structurally.
- The clean place to plug one in later is a check in `tag_combinators` reading
  the `Meta` node before the criterion legs. Noted and deferred.

---

## 5. Relationship to adjacent work

### 5.1 eu-v8n8 spike / PR #1007 — **quantitative claims reinstated; still superseded in form**

#1007 is a blob-only `inline_cores` hook behind `EU_BLOB_INLINE_CLUSTER`. Its
headline numbers are **adjudicated and reinstated** (022 −95.91%, §2.3). The
production form supersedes it in *form* (shared pipeline, both configs, no flag,
user combinators) but relies on its evidence. Recommend the owner decide #1007's
disposition (merge as evidence vehicle vs close-once-superseded) at design
sign-off; either way the production PR **ports the laziness regression test
(188)** reframed as a default-build test (§4.2). The design should **not** repeat
the earlier recommendation to close #1007 as "invalid" — that recommendation was
made under a confounded reading and is withdrawn.

### 5.2 eu-npp9 — **cluster-fusion scope subsumed; global-form fusion remains**

eu-npp9 ("make global-form fusion fire on the source-prelude merge-cook path")
had two implicit components: (a) fusing the intrinsic *global forms* (the
`SUB`/comparison wrapper CAFs), and (b) the recursive-combinator cluster. **This
design subsumes (b) entirely** — the criterion copy-specialises the combinators
on the source path directly. **eu-npp9 retains only (a):** the
global-form / demand-signature handicap on strict first-order recursion (the fib
tripwire). Independent mechanisms; recommend eu-npp9's scope be narrowed in its
bead to (a) only, with a pointer here.

### 5.3 BV3 / CG4 — **the O(N²) fold motivation is removed; sizing target corrected**

The canonical O(n²) fold case (022-class) was a headline motivator for the
register-frame (BV3) and codegen (CG4) levers. **If this lands, that specific
motivation is gone** — 022 drops to O(N) (§2.3, adjudicated). What remains of
BV3/CG4's win case is dispatch/env-lookup cost on non-combinator hot paths.

The mechanism diagnosis is now **settled** (§0.4): the un-fused quadratic is a
quadratic *count* of shallow env lookups, not growing walk depth. This corrects
the BV3/CG4 sizing target: the lever must **eliminate the repeated per-position
lookups** — CG4 lifting the operator / BV3 register frames replacing
`get()`-indirection — **not shorten walk distance**, which was never the
bottleneck (walk depth is flat at max 2). Flag this to the BV3/CG4 owners as a
re-baselining trigger with the corrected target, not a cancellation.

### 5.4 Wall / ledger — **PROTOCOL run required post-landing**

All figures here are deterministic (ticks/allocs/bloat). The wall confirmation
is a `cargo xtask engine-ab` + prelude/user-compile timing run on a **quiet
machine** per PROTOCOL, done when the production change lands. Deterministic
ticks are the primary evidence; wall is corroboration.

---

## 6. Risks

1. **Code-size growth at scale.** Copy-specialisation reproduces each admitted
   body as one local copy **per qualifying call site**. Adjudicated for
   `map`+`foldl`: blob +341 B, **bytecode image unchanged**, arena nodes
   unchanged (§2.2); the bytecode-invariance generalises to any `inline_cores`
   membership (§2.2 note). The full-8-set blob figure **requires a prototype**
   (§2.2). User-code growth is bounded by the size cap and the call-site count.
   **Low-to-medium risk**; the size cap (§1.2) is the explicit guard. Gate: an
   adjudicated full-set blob-size + representative-user-compile core-node count
   before/after.
2. **Compile-time cost.** The inline pass already runs on every compile; the
   criterion admits ~8 more inlinable lambdas. Spike prelude-compile: 0.07–0.08 s
   (**pending re-verification** — a wall figure, not yet on the gold-standard
   protocol). Gate: an adjudicated `eu`-compile wall (via `-S` phase timing) on a
   `map`/`foldl`-heavy user program before/after.
3. **Typechecker / LSP interaction.** `tag_combinators` runs in the inline pass,
   **after** type-checking and cook; copy-specialisation is a post-typecheck core
   transform, so inference and warnings are computed on the pre-transform form
   and unaffected. LSP navigation operates on source, not the transformed core.
   **Low risk**; gate is `eu check` output byte-identical before/after on the
   canonical suite.
4. **Termination / bloat blow-up** from the recursion. Bounded: the inline pass
   runs a fixed iteration count and `distribute` substitutes once per scope
   without re-entering nested lambda bodies, and Exp 2 shows one pass produces
   the single local self-recursive copy (no per-level growth). Gate: the
   implementation must assert the pass terminates (a non-terminating compile is a
   criterion bug), and the full 3-engine byte-identical suite must pass in both
   configs.

---

## Appendix — file:line index (master @ 5f550b49)

- `src/core/inline/tag.rs:18` — `tag_combinators` (the extension point).
- `src/core/inline/tag.rs:45` — `closed_body` (current admission predicate).
- `src/core/inline/tag.rs:80` — `all_free_vars_in_set` (blob-gen fixed-point gate;
  §3.2's self-reference extension).
- `src/core/inline/reduce.rs:86` — `inline_pass` = `distribute` ∘ `beta_reduce`.
- `src/core/inline/reduce.rs:92` — `inlinable()` (`Lam(_, true, _) | Intrinsic`).
- `src/core/inline/reduce.rs:96` — `distribute` (substitutes once per `Let` scope).
- `src/driver/source.rs:743` — `Loader::inline` (source-path `tag_combinators` +
  `inline_pass`).
- `src/driver/source.rs:720` — `inject_prelude_inline_cores` (blob-path injection).
- `src/driver/prepare.rs:323` — `inject_prelude_inline_cores()` call site.
- `src/driver/prepare.rs:329` — `for _ in 0..2 { loader.inline()? }` (inline pass).
- `src/driver/prepare.rs:374` — per-invocation `analyse_demands`.
- `src/driver/resources.rs:11` — `PRELUDE_BLOB_BYTES = include_bytes!(…prelude.blob)`
  (the build-time embed; provenance discipline, header note).
- `xtask/src/main.rs` §4b — the `inline_cores` fixed-point (blob-gen collection).
- `lib/prelude.eu:1403,1411,1415,1419,1423,1463,1474,1359` — the qualifying set.
- PR #1007 / `spike/inline-cores-cluster` — the eu-v8n8 evidence vehicle, incl.
  test `188_v8n8_cluster_laziness.eu`; adjudicated cells at `f8686d0a`.
