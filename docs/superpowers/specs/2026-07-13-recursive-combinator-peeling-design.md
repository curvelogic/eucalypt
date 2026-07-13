# Production design — criterion-based peeling of recursive higher-order combinators

- **Bead:** eu-yuto (0.13, parent epic eu-2sa6)
- **Author:** Furnace
- **Date:** 2026-07-13
- **Status:** DESIGN NOTE ONLY — no implementation. Owner sign-off gates the
  build. Contingent (satisfied) on Wicket's #1007 mechanism-of-magnitude
  verdict: the N=5k/10k/20k/40k scaling on the 022 fold shape confirmed a
  genuine **O(N²)→O(N)** transition (flag-off per-doubling ratio 3.84→3.96×,
  converging on 4×; flag-on exactly 2.0000× — see §0), so the higher-order
  account is confirmed, not refuted.
- **Supersedes:** the eu-v8n8 spike (PR #1007, `spike/inline-cores-cluster`) —
  a blob-only `inline_cores` evidence vehicle. This note specifies the
  **production** form: a semantic criterion in `tag_combinators`
  (`src/core/inline/tag.rs`) in the **shared** core pipeline, so blob, source,
  user-defined combinators and alternative preludes all fuse identically
  (owner architecture principle — no prelude special-casing).

## 0. Executive summary

The blob string/list penalty (eu-2sa6.12 → eu-7xvv → eu-qm7f → eu-ttpl → eu-v8n8)
resolves to a single mechanism: the recursive **higher-order** list combinators
(`map`, `foldl`, …) run as un-fused standalone global closures. Because their
function/operator argument is a lazy parameter the compiler cannot see through,
each element builds a thunk for `op(acc, x)` / `f(x)`; the thunks chain, and
forcing an O(N)-deep chain per element is **O(N²)**. Inlining the combinator body
into the call site exposes the *concrete* operator to demand analysis, which
specialises it strictly, eliminating the thunk chain — **O(N)**.

The eu-v8n8 spike proved this with a blob-only `inline_cores` hook fusing
`map`/`foldl`: verified same-session A/B, 018 −40%, 019 −8%, 022 −96%, fib
(first-order control) unchanged; Wicket independently reproduced every figure and
established the complexity-class transition. But that hook is blob-only, so it
**overshoots source** (018 blob 46.1M vs source still 76.8M — a new ~1.67×
divergence) and special-cases the prelude.

This note specifies the production form: **peel a self-recursive lambda into its
call sites when (1) it applies one of its own parameters, (2) its body is within
a size cap, and (3) its free variables resolve within the inline set or globals**
— a criterion added to `tag_combinators`, in the shared pipeline. Because
`tag_combinators` runs on both the source merge-cook inline pass
(`driver/source.rs:744`) and the blob-generation fixed-point (`xtask`), both
configs and user combinators fuse identically. Measured on the full 8-combinator
qualifying set (§2): **+1505 bytes** of blob, bytecode unchanged, blob-gen time
unchanged, no bench regressed, and the win extends beyond `map`/`foldl` to
import/reshape/lookup workloads (016/017 −6.1%, 020 −9.6%). All numbers below are
**deterministic layers only** (the machine was busy; no wall claims — those are a
gated PROTOCOL run in a quiet window, §5.4).

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

### 1.1 The three legs, and why each is load-bearing

A `Lam(_, false, scope)` is tagged inlinable under the new leg when **all** hold:

1. **Higher-order self-recursion — it applies one of its own parameters, and
   recurses on its own name.** `map` applies `f` (`l head f`), `foldl` applies
   `op` (`op(i, l head)`); both call themselves. **Justification:** the O(N²)
   pathology is *created* by a lazily-passed operator being applied per element.
   `fib` — first-order self-recursion, applies no parameter — is the **null
   control**: it neither qualifies nor benefits (ticks bit-identical flag-on/off,
   115,779,125 both, verified by Wicket). Wicket's scaling data
   (O(N²)→O(N)) is the direct positive evidence for exactly the higher-order
   class. A first-order recursive function (`take`, `drop`, `iota`) has no
   parameter-application indirection to collapse, so it is correctly excluded.

2. **Body within a size cap** (`MAX_PEELABLE_BODY_NODES`, §1.2). **Justification:**
   inlining a body reproduces it at each call site (bounded ×2 by unroll depth,
   §1.2), so an unusually large recursive combinator would bloat user code. The
   full 8-combinator qualifying set costs only **+1505 blob bytes** (§2), so the
   cap is *not* about aggregate count — it is a guard against a single
   pathologically large combinator (a hand-written recursive parser-combinator,
   say), keeping the transform's cost per admitted binding bounded.

3. **Free variables resolve within the inline set or globals.** `map`'s free
   vars are `map` (itself), `cons`/`head`/`tail`/`nil?`/`if` (already inline-set
   members or intrinsic aliases). **Justification:** this is the safety leg that
   lets the inlined copy resolve. The self-reference resolves to the shared
   global definition (`Ref::G`) at STG compile — the residual recursion after
   the bounded unroll — so peeling never duplicates the *whole* recursion, only a
   fixed prefix. It is also the leg the blob-generation fixed-point already
   embodies (`all_free_vars_in_set`, `tag.rs:80`), extended to treat the binding's
   **own name** as resolvable (self → global slot); see §3.2.

### 1.2 Named constants

| Constant | Proposed value | Rationale |
|---|---|---|
| `MAX_PEELABLE_BODY_NODES` | **48** core-expr nodes | The 8 qualifying combinators' bodies are 6–30 nodes; 48 admits `scanr`/`window`-class bodies with headroom while rejecting anything an order larger. A cap by *node count* (not bytes) is representation-stable. Measure-and-tune during implementation on the qualifying-set survey (§2). |
| `PEEL_UNROLL_DEPTH` | **2** (implicit) | Not a new constant — it is fixed by `prepare.rs:329` (`for _ in 0..2 { loader.inline()? }`) and `reduce::distribute` substituting each inline-set member once per `Let` scope without re-substituting into nested lambda bodies (verified by Wicket reading the source). The residual self-call after two unrolls stays `Var::Free` and resolves to the global slot. Two levels suffices because the win is the *demand-analysis specialisation* of the operator, which fires on the first inlined copy, not the unroll count. The design must **not** raise this to chase deeper unrolling — that trades bounded bloat for no measured gain. |

### 1.3 Margins (where the criterion must be explicit)

- **Mutually-recursive pairs** (`f` calls `g`, `g` calls `f`, neither calls
  itself). Leg 1 as stated ("recurses on its own name") **excludes** these — `f`'s
  body has no `Var::Free("f")`. This is deliberate for the first version: mutual
  recursion needs both members in the inline set and a cycle-aware fixed point,
  and no qualifying prelude combinator is mutually recursive (§2). The design
  states this exclusion explicitly and defers it; revisit only if a measured
  mutual-recursive hot combinator appears.
- **Combinators that apply a parameter only on some branches** (e.g. a combinator
  that applies `f` in the `cons` branch but not the `nil?` branch — which is
  exactly `map`/`foldl`'s shape: the parameter is applied only in the recursive
  branch). Leg 1 requires the parameter be applied *somewhere* in the body, not
  on every path. This is correct: demand analysis handles per-branch strictness
  itself, and the laziness-preservation test (188, ported below) covers the
  unused-branch case.
- **Recursion via a nested local helper** (`take-while(p?, l): { aux(xs, prefix):
  … aux(xs tail, …) … }.aux(l, [])`). The *top-level* binding `take-while` is
  **not** self-recursive — the recursion lives in the nested `aux`. Leg 1
  operates on the peeled top-level binding, so `take-while`/`window`/`window-all`
  do **not** qualify directly (their `aux` is not a top-level combinator). They
  still benefit transitively wherever they call a qualifying combinator. The
  design states this; a future refinement could tag nested self-recursive
  higher-order lambdas too, but it is out of scope here (no measured need).

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
| `iterate(f, i)` | 1423 | `f` | infinite generator; bounded unroll is safe |

**Benefit transitively** (not self-recursive themselves, but defined in terms of
a qualifying combinator, so they fuse once their callee is peeled): `filter`
(→`foldr`, 1498), `zip`/`zip-with` (→`map2`, 1478/1482), `all`/`all-true?`
(→`map`/`foldl`), `any`/`any-true?` (→`map`/`foldr`), `mapcat` (→`map`, 1525),
`remove` (→`filter`), `zip-apply`/`zip-kv` (→`zip-with`), `map-values`/`map-keys`
(→`map`).

**Correctly excluded** — first-order recursion (no parameter applied):
`take`/`drop`/`repeat`/`iota`/`cycle`/`interleave`/`stream-advance`/`window`/
`window-all`; and nested-`aux` recursion: `take-while`/`take-until`.

### 2.2 Full-set bloat and blob-gen cost — **measured-verified** (deterministic)

Admitting **all 8** directly-qualifying combinators (via the eu-v8n8 flag
mechanism, cluster extended to the full set — a measurement harness, not the
production form) vs the flag-off baseline:

| | inline_cores | blob bytes | bytecode bytes | arena nodes | prelude-compile |
|---|---|---|---|---|---|
| baseline | 133 | 461,638 | 269,176 | 7,061 | 0.08 s |
| full set (8) | 141 (+8) | **463,143 (+1,505)** | 269,176 (**identical**) | 7,104 (+43) | 0.07 s |

~188 bytes per admitted combinator; the compiled **bytecode image is
byte-for-byte unchanged** (the growth is entirely in the source-side
`inline_cores` payload the blob carries for user-compile injection). Blob-gen
wall is dominated by `cargo build` (~15 s both); the prelude-compile step itself
is 0.07–0.08 s either way — **no measurable compile-time cost**.

### 2.3 Full-set tick effect on the canonical suite — **measured-verified** (HeapSyn `-S -t`, both engines deterministic)

| Bench | class | baseline (flag-off) | full-set (flag-on) | Δ | map+foldl-only (eu-v8n8) |
|---|---|---|---|---|---|
| 015_block_merge | D | 98,788,746 | 96,175,898 | −2.6% | (unmeasured) |
| 016_import_export_yaml | I | 59,894,595 | 56,226,115 | **−6.1%** | (n/a — no map/foldl win) |
| 017_import_export_toml | I | 59,721,271 | 56,052,791 | **−6.1%** | (n/a) |
| 018_string_scale | G | 76,814,993 | 46,104,199 | −40.0% | 46,104,199 (same) |
| 019_list_scale | H | 55,803,488 | 51,244,975 | −8.2% | 51,244,975 (same) |
| 020_lookup_curve | E | 1,721,105 | 1,556,643 | **−9.6%** | (n/a) |
| 021_io_loop | L | 3,971,300 | 3,971,300 | 0% | 0% (io-bound) |
| 022_hof_fold | C | 52,225,448 | 2,230,410 | −96.0% | 2,230,410 (same) |

**No bench regressed.** The full set is monotonically beneficial and extends the
win to the import (016/017) and lookup (020) workloads that `map`/`foldl` alone
did not touch — these use `filter`/`foldr`/`all`/`any` transitively. This is the
concrete argument for **admitting the whole criterion-qualifying set, not a
curated `map`/`foldl` list**: the same criterion that captures the headline case
also captures the long tail, for negligible extra cost.

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
lambdas inlinable, and `inline_pass` distributes them into the user's call sites
exactly as the spike showed on the blob path. No new source-path wiring is
needed — the criterion change alone activates it.

### 3.2 Blob (generation) path

`xtask`'s `inline_cores` fixed-point (`xtask/src/main.rs` §4b) already calls
`tag_combinators` to compute the closed flag and admits a lambda when
`Lam(_, true, _)` **and** `all_free_vars_in_set(body, set)` (`tag.rs:80`). Two
changes land the criterion here:
1. `tag_combinators` tags the recursive higher-order combinators `true` (the
   §1 criterion) — the same change as §3.1.
2. `all_free_vars_in_set` (or the fixed-point's use of it) must treat the
   binding's **own name** as resolvable (a self-reference resolves to the
   binding's global slot, `Ref::G`, at STG compile). Without this the fixed-point
   still rejects `map` on its self-reference. This is leg 3 of the criterion made
   concrete on the blob side; it is a two-line predicate extension, not new
   machinery.

The injected `inline_cores` `Let` scope makes all members mutually visible
(`inject_prelude_inline_cores`, `source.rs:720`), so a peeled combinator's
operand references bind to their (already-expanded) siblings — verified by the
spike + Wicket.

### 3.3 User-defined combinators and alternative preludes

Because the criterion is purely structural and lives in the shared inline pass,
a *user's* own recursive higher-order combinator (`my-fold(op, acc, xs): …`) is
tagged and peeled by the same code, in the same compile, with no prelude
knowledge. Alternative preludes (`{ prelude: "…" }`) route through the identical
pipeline. This is the property the blob-only `inline_cores` hook could not
provide.

### 3.4 Tripwire implications — **projected** (not yet measured; needs the impl)

- The eu-v8n8 blob-only divergence closes. Today source 018 = 76.8M, blob 018 =
  46.1M (a 1.665× divergence, Wicket-confirmed). With the criterion on both
  paths, the source merge-cook pass fuses the same `map`/`foldl` → **projected
  source 018 ≈ 46M**, so the map/foldl-workload divergence returns to ≈1.0. This
  is the central sequencing win: the production form *removes* the divergence the
  spike introduces.
- The **fib-based tick-parity tripwire is unaffected either way** (1.106 byte /
  1.1228 predecode, within caps 1.12/1.13). `fib` is first-order and does not
  qualify — its ticks do not change on either config — so the tripwire neither
  worsens (as it would have under a blob-only landing on a fib-*using* probe) nor
  improves. The tripwire measures the *demand-analysis* handicap on strict
  recursion, which this change does not touch; that handicap remains eu-npp9's
  domain (§5.2). The design must state plainly that this change does **not**
  move the tripwire toward parity — only eu-npp9 (global-form fusion on the
  source path) does that.

---

## 4. Demand-analysis interaction and the metadata override

### 4.1 Why strict specialisation follows the peel

`analyse_demands` runs per-invocation on the user's core expression
(`driver/eval.rs`, `driver/prepare.rs:374`). Un-fused, `foldl`'s operator is an
opaque parameter, so the compiler assumes the lazy (thunk-building) calling
convention at every `op(acc, x)`. **After peeling, the operator at the call site
is concrete** (`+`, a user lambda, …) and its strictness is analysable, so the
thunk is not built — this is the O(N²)→O(N) collapse. The specialisation is a
*consequence* of the peel exposing the concrete argument, not a separate change.

### 4.2 Where it cannot specialise (and why that is safe)

Where the operator genuinely *is* lazy in a position (an unused argument, a
conditionally-forced branch), demand analysis is sound by construction and leaves
it lazy — the peel changes efficiency, not evaluation order. Evidence: the
188 laziness regression (an unused argument threading a `map`/`foldl`-with-a-bomb
through recursion — has teeth: forcing it panics) passes flag-on and off on all
three engines, as do 170/171/179/181. The production design **ports test 188** (it
becomes engine- and path-independent, running on the default build once the
criterion lands). Note eucalypt evaluates list *elements* eagerly (lazy streams
deferred), so there is no element-laziness to break; 188 targets the real
property — that inlining a fused body into an unused position is not promoted
strict.

### 4.3 Metadata override — **recommend against (for now)**

An optional `` ` { inline: true|false } `` annotation could force/forbid peeling
per binding. **Recommendation: do not add it in the first version.**
- An opt-*in* (`inline: true`) is unnecessary — the criterion is the gate; a
  binding that wants peeling should meet the criterion.
- An opt-*out* (`inline: false`) is a plausible correctness/size escape hatch,
  but adds a user-visible surface and a footgun (silently disabling a large win)
  for a need that has not appeared. The size cap (§1.2) already handles the "too
  big" case structurally.
- The clean place to plug one in later is a check in `tag_combinators` reading
  the `Meta` node before the criterion legs. The design notes this location and
  defers the feature until a concrete combinator needs it.

---

## 5. Relationship to adjacent work

### 5.1 eu-v8n8 spike / PR #1007 — **superseded; recommend CLOSE, not merge**

#1007 is a blob-only `inline_cores` hook behind `EU_BLOB_INLINE_CLUSTER`. The
production form supersedes it entirely (shared pipeline, both configs, no flag).
Recommend **closing #1007 unmerged** once this design is signed off and its PR
opens — merging-then-removing-the-flag is pure churn, and the spike's value
(the evidence and the reproduced scaling verdict) is already captured here and in
Wicket's review. **Port from #1007 into the production PR:** the laziness
regression test (188), reframed as a default-build test (§4.2).

### 5.2 eu-npp9 — **cluster-fusion scope subsumed; global-form fusion remains**

eu-npp9 ("make global-form fusion fire on the source-prelude merge-cook path")
had two implicit components: (a) fusing the intrinsic *global forms* (the
`SUB`/comparison wrapper CAFs), and (b) the recursive-combinator cluster. **This
design subsumes (b) entirely** — the criterion fuses the combinators on the
source path directly. **eu-npp9 retains only (a):** the global-form / demand-
signature handicap on strict first-order recursion (the fib +10.6% the tripwire
guards). Those are independent mechanisms; the design should recommend eu-npp9's
scope be narrowed in its bead to (a) only, with a pointer here.

### 5.3 BV3 / CG4 — **the O(N²) fold motivation is removed**

The canonical O(n²) fold/env-walk case (022-class) was a headline motivator for
the register-frame (BV3) and codegen (CG4) levers. **If this lands, that specific
motivation is gone** — 022 drops 52.2M→2.23M ticks and, per Wicket's scaling
data, becomes genuinely O(N). What remains of BV3/CG4's win case is the
*dispatch/env-walk cost on non-combinator hot paths* (direct first-order
recursion, deep frozen environments not created by lazy fold chains). BV3's
register frames still reduce per-tick env-walk cost generally; but its
motivating benchmark should be re-selected, because the fold case it was sized
against no longer exhibits the pathology. The design should flag this to the
BV3/CG4 owners as a re-baselining trigger, not a cancellation.

### 5.4 Wall / ledger — **PROTOCOL run required post-landing**

All figures here are deterministic (ticks/allocs/bloat). The wall confirmation
(that the tick collapse yields the projected wall win, and that user-compile wall
is unaffected) is a `cargo xtask engine-ab` + prelude-compile/user-compile timing
run on a **quiet machine** per PROTOCOL, to be done when the production change
lands (not now — the machine is busy). Deterministic ticks are load-independent
and stand alone as the primary evidence; wall is corroboration.

---

## 6. Risks

1. **Code-size growth at scale.** Inlining reproduces each peeled body (bounded
   ×2) at every call site. Measured: full-set blob +1505 B, **bytecode image
   unchanged**, arena +43 nodes. User-code growth is bounded by the ×2 unroll and
   the size cap. **Low risk**; the size cap (§1.2) is the explicit guard, to be
   tuned on the qualifying-set survey during implementation. Gate: measure blob
   size and a representative user-compile's core-node count before/after.
2. **Compile-time cost.** The inline pass already runs two iterations
   (`prepare.rs:329`) on every compile; the criterion admits ~8 more inlinable
   lambdas, adding marginal substitution work. Measured prelude-compile:
   0.07–0.08 s, **no change**. **Projected** user-compile: negligible (the pass
   structure is unchanged; a handful more closed lambdas). Gate: measure
   `eu`-compile wall (via `-S` phase timing) on a `map`/`foldl`-heavy user program
   before/after.
3. **Typechecker / LSP interaction.** `tag_combinators` runs in the inline pass,
   **after** type-checking and after cook; peeling is a post-typecheck core
   transform, so type inference and warnings are computed on the pre-peel form
   and unaffected. LSP navigation operates on source, not the inlined core, so it
   is untouched. **Low risk**; gate is that `eu check` output is byte-identical
   before/after on the canonical suite.
4. **Termination / bloat blow-up** from the recursion. Bounded and verified: two
   inline iterations, `distribute` substitutes once per scope without re-entering
   nested lambda bodies, residual self-call → global slot. The spike + Wicket
   confirmed no hang, no unbounded expansion (blob +1505 B for 8 combinators).
   Gate: the implementation must assert the unroll bound (a compile that never
   terminates would be a criterion bug), and the full 3-engine byte-identical
   suite must pass in both configs.

---

## Appendix — file:line index (master @ 5f550b49)

- `src/core/inline/tag.rs:18` — `tag_combinators` (the extension point).
- `src/core/inline/tag.rs:45` — `closed_body` (current admission predicate).
- `src/core/inline/tag.rs:80` — `all_free_vars_in_set` (blob-gen fixed-point gate;
  §3.2's self-reference extension).
- `src/core/inline/reduce.rs:86` — `inline_pass` = `distribute` ∘ `beta_reduce`.
- `src/core/inline/reduce.rs:92` — the inlinability check (`Lam(_, true, _)`).
- `src/core/inline/reduce.rs:96` — `distribute` (bounded substitution, verified).
- `src/driver/source.rs:743` — `Loader::inline` (source-path `tag_combinators` +
  `inline_pass`).
- `src/driver/source.rs:720` — `inject_prelude_inline_cores` (blob-path injection).
- `src/driver/prepare.rs:329` — `for _ in 0..2 { loader.inline()? }` (unroll depth).
- `src/driver/prepare.rs:374` — per-invocation `analyse_demands`.
- `xtask/src/main.rs` §4b — the `inline_cores` fixed-point (blob-gen collection).
- `lib/prelude.eu:1403,1411,1415,1419,1423,1463,1474,1359` — the qualifying set.
- PR #1007 / `spike/inline-cores-cluster` — the eu-v8n8 evidence vehicle, incl.
  test `188_v8n8_cluster_laziness.eu` and Wicket's scaling verdict.
