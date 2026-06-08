# Eucalypt — Roadmap to 1.0

- **Status:** Plan of record
- **Date:** 2026-06-08
- **Baseline:** eucalypt 0.7.0

---

## 1. Purpose

This is the single, self-contained roadmap for eucalypt's evolution to **1.0** and
the shape of the work just beyond it. It **supersedes and replaces** the earlier
set of design proposals: everything of lasting value from them — the reasoning,
the prior art, and the technical content of each change — is folded in here, and
the proposals are retired.

The document has two halves. **Sections 2–6** are the plan: what eucalypt is, the
principles it must not break, the decisions already settled, the things it will
deliberately *not* do, and the shape and sequencing of the work. **Section 7** is
the work itself — each item a self-contained section carrying its problem,
prior art, design, implementation, risks and success criteria, with `path:line`
groundings against the current code. **Sections 8–9** give the critical path and a
complete index.

Work items are numbered **W1–W22**, ordered by release. Decisions and non-goals
are not numbered — they govern the work rather than being scheduled.

---

## 2. What eucalypt is, and where it stands

Eucalypt is a **tool first** — for generating, templating and transforming
YAML/JSON/TOML — and a lazy, pure, functional language second. Its peers are the
configuration/data languages (Jsonnet, Dhall, CUE, Nickel, Pkl, KCL, Starlark)
and, for some ideas, Unison; they are not general-purpose languages, and every
idea below is weighed against the tool-first use.

### The implementation, in brief

The compile pipeline is: **parse** (a Rowan lossless tree — the sole parser; the
LALRPOP grammar is retired) → **desugar** → **cook** (shunting-yard operator
fixity resolution) → **eliminate** → **inline** (×2) → **fuse** →
**eliminate/compress** → **verify** → **type-check** (bidirectional, advisory,
then *erased*) → **STG compile** → **STG optimise** → **load to heap** → run on
the **STG machine**.

- **The VM** is a Spineless Tagless G-machine (`src/eval/machine/vm.rs`): five
  continuation kinds, an off-heap `Vec` continuation stack, ~181 intrinsics, a
  single worker thread with a 64 MiB stack.
- **The GC** is Immix-inspired (`src/eval/memory/`): 32 KiB blocks, 128 B lines,
  mark-and-sweep **with** opportunistic evacuation and **lazy** (deferred) sweep.
  It has excellent debug instrumentation — `EU_GC_VERIFY` (structural
  verification), `EU_GC_STRESS` (force evacuation every cycle), `EU_GC_POISON`
  (use-after-free detection).
- **The type checker** (`src/core/typecheck/`) is bidirectional, freshen-and-unify
  with subtyping, and **advisory** — it emits warnings and is then erased before
  code generation.
- **Blocks** — the core data structure — are **cons-lists** of key/value pairs
  with **O(n) lookup**, insertion-ordered (the order is observable output).

### What 0.7.0 already shipped

The gradual type system the language was built toward is **complete**. Stage A
(0.6.2) delivered `Dict`, equirecursive `Mu` types, literal-string types, flow
narrowing, `NonEmpty`, first-class alias references, and the `cond[…]`/`=>` clause
form. Stage B + **higher-kinded types** (0.7.0) delivered `Con`/`App`/`Kind`/
`forall`, higher-order pattern unification, HKT-typed `monad()`, dependent indexed
access, the prelude type-summary cache, `Partial(T)`/`T?`, full row inference, and
**structural operator constraints** (so `min`/`max` now carry `"<(a, a) => …"`
annotations). The road to 1.0 is therefore mostly *outside* the type system.

### The largest gaps

1. **No statement of what 1.0 commits to** — no stability discipline, no
   versioning that means anything (today: continuous delivery with a four-part
   build number; a genuine breaking change shipped in a patch-looking 0.6.2).
2. **Compile latency.** Every invocation re-parses and re-compiles the entire
   prelude — **~500–700 ms** of fixed overhead (parse ~120 ms, cook ~190 ms,
   eliminate ~110 ms, STG compile ~90 ms) that dwarfs the actual work (a lookup on
   a 1.15 MB input runs the VM in ~1 ms). It is the most *visible* performance
   problem.
3. **GC cost.** On traversal-heavy programs, mark time is **>95%** of VM time, and
   the collector re-marks the entire live set every cycle — there is no
   generational dimension.
4. **O(n) blocks.** Lookup and merge are linear, and the one attempt at persistent
   O(log n) blocks hit a GC-finalisation wall (it leaked, 220–580% regressions).
5. **No module/package system** beyond file imports; **no documentation
   generation**; **no interactive surface**; **no concurrency**.

---

## 3. Principles (non-negotiable)

1. **Syntactic conservatism.** The surface syntax stays as it is. New machinery
   lives in metadata (backtick blocks: `type:`, `monad:`, `export:`, …), symbols,
   strings, blocks, operators, or idiot-bracket pairs — never in new keywords,
   statement forms, lambda arrows, or `let … in` / `match … with`.
2. **Structural over nominal.** No named classes, no `implements`, no nominal
   types. Constraints reference shapes and functions, not names.
3. **Gradual and inference-first.** Types are advisory by default; a feature must
   deliver value when only the prelude is annotated.
4. **Single-threaded lazy-pure runtime.** Evaluation is lazy, pure and
   single-threaded over an `UnsafeCell` heap whose soundness depends on
   stop-the-world access (`src/eval/memory/heap.rs:8`). IO is an explicit monad
   interpreted by the driver loop; the pure core performs no effects.
5. **Tool-first.** Eucalypt generates/templates/transforms structured data before
   it is a language. Weigh every idea against that use.

---

## 4. Decisions (settled)

These are resolved, with their reasoning, so they are not re-litigated. They
govern the work in Section 7 but are not themselves scheduled.

### 4.1 Versioning & stability

Adopt **real semver** at 1.0 (semver.org): `major.minor.patch`, with the CI build
number moved to `+build.N` metadata (which semver ignores for precedence). The
fields are interpreted against eucalypt's real API — syntax + prelude + rendered
output — sharpened by Dhall's three-field rule and Rust RFC 1105 ("all major
changes are breaking, but not all breaking changes are major"): a **MAJOR** is a
change an *unmodified* file can observe in its output or a removal from the Stable
surface; **MINOR** is a backwards-compatible addition (including a new opt-in
prelude version); **PATCH** is no observable semantic change.

1.0 commits to an **enumerated stable surface in tiers** — **Stable** (won't break
except in a MAJOR, with a deprecation path for the prelude), **Experimental** (may
change in a MINOR; opt-in/advisory), **Not covered** (no promise). Core syntax,
the prelude v1 API, block-merge/lookup semantics, the CLI surface, the
import/export formats, and the WASM/embedding API are **Stable**; the
**type-annotation DSL and checker are Experimental** — which is what lets
lang + prelude reach 1.0 without the type system being "done" (it is advisory, so
it need not be). Internal IR/STG/GC/`dump` output and exact error *prose* are Not
covered (error *codes/locations* and exit codes are Stable).

The version contract is enforced read-side by **`requires`**, which already parses
full `semver::VersionReq` ranges (`lib/prelude.eu:15-17`, `src/eval/stg/version.rs`,
`VersionRequirementFailed` at `src/eval/error.rs:679`) — once versions *mean*
something, a unit pins the range it tolerates and **fails loud** on a binary that
moved past it, rather than silently re-rendering wrong (CUE's `language.version`
model). The prelude — the heaviest Stable surface (~228 declarations) — evolves
via an **opt-in v2** that coexists with a frozen v1 (`{ prelude: :v2 }`,
default v1), because the prelude is already a named, embedded, swappable resource
(`src/driver/resources.rs:14-18`); shipping two is two `include_bytes!` + a
selector, *not* a forked desugarer. A **deprecation lifecycle** (`deprecated:` /
`replaced-by:` metadata → use-site WARNING → LSP quick-fix → removal in a MAJOR or
simply omitted from v2) retires prelude functions in good order. The pending
deep-merge-as-default change ships **as a MAJOR or behind prelude v2 — never as a
silent default flip**.

### 4.2 Boundary soundness

Eucalypt's gradual boundary stays **advisory and optimistic by default**: an
`any` value flowing into a typed position is **trusted at runtime — no cast, no
proxy, no blame — and types are erased before execution**. This ratifies the
current behaviour as a permanent, *stated* guarantee: `is_consistent` short-
circuits on `any` (`src/core/typecheck/subtype.rs:181`), `unify` produces no
binding at the boundary (`src/core/typecheck/unify.rs:56`), warnings never block
evaluation (`check.rs:24`), and types never reach `stg::compile`.

This is the textbook Siek–Taha consistency relation (symmetric, non-transitive —
`any` launders any type into any other). The reason for declining **sound/guarded
gradual typing** is empirical and architectural: Takikawa et al., *Is Sound
Gradual Typing Dead?* (POPL 2016), measured partially-typed configurations at up
to ~100× slowdown from per-boundary proxies; Bauman et al. (OOPSLA 2017) showed a
tracing JIT recovers most of it — but **eucalypt is AOT-compiled and GC-bound
(mark >95%), with no JIT**, so it is the worst case. The blessed static contract
is therefore **`eu check --strict`** as the CI gate — its exit code is part of the
public interface (`src/driver/check.rs:114-117`). An **opt-in `--strict-boundary`
test mode** (off by default, on under the test harness) may later insert
*targeted, shallow* runtime checks at explicitly-annotated boundaries
(transient/shallow in the sense of Vitousek et al., POPL 2017; never whole-program
proxying) to catch latent mistyping where a 2–5× slowdown is irrelevant. The
plain-words policy is stated in the language reference so no one assumes "typed"
means "checked at runtime". Runtime *validation* where it genuinely earns its keep
is the job of contracts at data ingress (W16), not of always-on casts.

### 4.3 The string type-DSL

Type annotations remain **strings in `type:` metadata**, parsed by a dedicated
DSL (`src/core/typecheck/parse.rs`); inside those strings records use escaped
braces `{{..}}`. This is kept through 1.0 — **no reserved type bracket** is
added (it would burn scarce surface syntax and the metadata-string form composes
with everything). Where a type must be referenced in *value* context (W16), the
**`s"…"` string-prefix** form is the surface: it produces type-data, kept
deliberately distinct from a bare symbol (which would be "too magical"). The DSL
is the one Experimental surface (§4.1) and stays so until the open questions
around it settle.

### 4.4 Ad-hoc polymorphism — "typeclasses without classes"

Operator/overload constraints reference **shapes and functions, not names**: a
constraint like `<(a, a)` says "there exists a `<` at this shape", with no class
to declare or instance to register. Shipped in 0.7.0 as structural operator
constraints (live `TypeScheme.constraints`); the standing position is that this is
the whole mechanism — there is no nominal typeclass system coming.

### 4.5 Runtime performance is a 1.0 goal

The generational GC (W10) **and** strictness/demand analysis (W11) both ship
before 1.0. Latency (W6) and GC cost are the two most-felt runtime problems; W10
benefits every program and W11 compounds it. (The further, type-directed
performance work, W20, is a curated post-1.0 bet — see §4.6.)

### 4.6 Post-1.0 big rebuilds are curated, not pre-committed

The roadmap commits through 1.0. The large systems rebuilds beyond it —
type-directed compilation (W20) and parallel evaluation (W21) — are deliberate,
**one-at-a-time** candidates chosen *at* 1.0, never parallelised and never assumed.
They draw on the same scarce deep-systems expertise as the GC line and must not
starve it.

---

## 5. Non-goals (won't-do)

- **A package registry.** All distribution goals are met with **git + GitHub /
  GitLab** alone — content-addressed imports, an in-language manifest, version
  selection. There is no ecosystem of deployed third-party `.eu` files to justify
  an index or hosted registry, and we decline to build one (W18).
- **Rust-style editions.** Editions let one binary interpret old files under old
  *semantics* forever — valuable with many untouchable third-party crates, but it
  forces the desugarer to carry every historical semantic path indefinitely. At
  current scale that recurring cost buys a benefit the ecosystem does not warrant;
  the `requires` guard and opt-in prelude v2 (§4.1) cover the real need. Revisit
  only if a real third-party ecosystem appears.
- **Capability *types*.** The hermetic/determinism *mode* (W17) ships; a
  capability *type system* does not — too much machinery for the tool-first remit.
- **Algebraic subtyping / MLsub (an MLsub/MLstruct core swap).** The hand-rolled
  bidirectional checker plus shipped HKT is the committed core; an MLsub rebuild
  would buy principal types and "rows/unions for free" but **conflicts directly
  with the now-shipped HKT keystone** and is too cutting-edge to combine with it.
  Reassess only if the current core visibly creaks.
- **Nominal types / classes.** Stay structural (Principle 2). A nominal newtype,
  if ever, would itself be opt-in (a MAJOR or prelude v2), never a default.
- **Sound/guarded gradual typing** — see §4.2. **An always-on runtime type
  check** is declined by design.

---

## 6. The shape of the plan

### 6.1 The six families

Work groups along six commonalities — the real reason items sit together.

| Family | Members | The shared thing |
|---|---|---|
| **Cross-unit interface** | W3, W6, W7, W18 | One *Unit Interface* (W3) exposes each unit's cross-unit contributions; separate compilation, incremental queries and module imports all consume it. |
| **Compile latency & incrementality** | W2, W6, W7 | Eliminate redundant compiles, cache/embed the prelude, then a demand-driven query graph. One cache-key discipline. |
| **GC / memory** | W10, W13, W14 | Generational nursery first; then persistent blocks and vec-of-values on the GC-scannable-array machinery. One scarce skill set — *sequence, never parallelise*. |
| **Strictness / demand** | W9, W11, W20 | One demand annotation (W9) feeds the analysis (W11) feeds type-directed compilation (W20). |
| **Types-as-validation** | W16, W15, W8, W22 | The `s"…"` type vocabulary is reused by runtime validation, presence-typed fields, doc/schema extraction and schema interop. |
| **1.0 commitment** | W1, W5, W17 | Versioning & stable-surface tiers, a conformance corpus, reproducibility — what "1.0" *means* and how it is *proven*. |

### 6.2 The dependency spine

The edges that constrain ordering (`→` = "must precede"):

- **W3** (Unit Interface) **→ W6, W7, W18**; W3 also subsumes the cross-import
  bracket fix.
- **W9** (demand annotation) **→ W11** (the analysis that populates it fully).
- **W12** (git fetch backend) **→ W18** (modules).
- **W10** (generational GC) **→ W13** (persistent blocks) — same scarce GC
  expertise; W10's nursery also makes the persistent structures' short-lived nodes
  cheap.
- **W15** (presence) **→ W22**; and it enriches **W16**.
- **W16, W8 → W22** (an ingested schema *is* a contract; doc extraction feeds
  codegen).
- **W6 → W19, W21**; **W7 → W19** (per-prompt latency + reactivity).
- **W14** (vec) shares W13's GC-array machinery but is otherwise independent.
- **W20** is gated on the boundary-soundness decision (§4.2) and couples to **W11**.

### 6.3 The GC line, and the curated rebuilds

Five efforts draw on the same deep-systems skills (the GC, the core checker, the
STG machine) and **cannot all happen at once**:

| Effort | Buys | Verdict |
|---|---|---|
| **W10** generational GC | cuts the dominant mark cost; benefits *every* program | **Do it — 0.10.** |
| **W13** persistent blocks | O(log n) for large/merge-heavy configs | **0.11, after W10.** |
| **W20** type-directed compilation | types as a runtime asset | post-1.0 candidate; gated on §4.2. |
| **W21** parallel evaluation | multi-core for batches/large maps | post-1.0 candidate; Model B is a GC rebuild in itself. |
| *(MLsub core swap)* | principal types "for free" | **Won't-do** — collides with shipped HKT (§5). |

Spend the scarce systems effort on the GC (W10, then W13) before 1.0; take
W20/W21 one at a time afterwards.

### 6.4 Leverage vs cost

A second lens, orthogonal to release order (decisions and non-goals excluded):

| Bucket | Items |
|---|---|
| **Cheap & high-leverage** (design-heavy, do early) | W1, W2, W4, W6, W8 |
| **High-leverage, real engineering** | W3, W5, W7, W10, W13, W16, W18 |
| **Worthwhile, medium** | W9, W11, W12, W14, W15, W17, W19 |
| **Big bets / forks** (deliberate, post-1.0, one at a time) | W20, W21, W22 |

### 6.5 Release summary

| Release | Theme | Items |
|---|---|---|
| **0.8** | Versioning, foundations & front-end hygiene | W1–W5 |
| **0.9** | Compile latency, docs & incremental groundwork | W6–W9 |
| **0.10** | The runtime: GC & demand | W10–W12 |
| **0.11** | Blocks, value model & presence types | W13–W15 |
| **0.12** | Data-correctness & reproducibility | W16–W17 |
| **1.0** | Ecosystem floor, prove & freeze | W18–W19 (+ W5 bar, W1 freeze) |
| **post-1.0** | Breadth & curated bets | W20–W22, candidates |

## 7. The work

Each item carries its problem (grounded in the current code), the prior art that
shaped it, the chosen design, an implementation sketch with size/risk, the risks
that would falsify it, and how we would know it worked. Items phased across
releases keep one number with phase notes.

---

### 0.8 — Versioning, foundations & front-end hygiene

*Design-heavy, front-end, low-risk: make 1.0's shape real and stop the obvious pain.*

#### W1. Versioning & stability discipline

**Problem.** Eucalypt practises continuous delivery with a four-part build number
(`build.eu:7-13,59-62`); the `0.7.0` in `Cargo.toml` is a human-set prefix and the
fourth field carries no compatibility meaning. A genuine breaking change shipped
in a patch-looking **0.6.2** (the `cond` rewrite to the `=>` clause form,
`CHANGELOG.md:57`, `lib/prelude.eu:1168`): eucalypt's own callers were fixed by
hand, any user file broke silently. The risk worth managing is narrow — *a file
silently changes behaviour on a binary upgrade and nobody notices* — not "an
untouchable third-party file breaks" (there is no such ecosystem yet).

**Design.** This item *implements* the policy settled in §4.1; it is deliberately
proportionate, not a heavyweight charter. Four engineering pieces, all front-end /
loader / docs with no runtime risk:

1. **Semver plumbing** — adopt the field meanings of §4.1; move the build number to
   `+build.N` metadata at 1.0 (`build.eu`, release flow, docs).
2. **`requires` as a guard** — the runtime path already exists
   (`__REQUIRES`/`semver::VersionReq`); document the range-pin convention, model it
   in the shipped library units (`lens.eu`, `state.eu`), and optionally add an
   `eu`/LSP "minimum version / missing pin" hint.
3. **Opt-in prelude versioning** — ship a frozen **v1** and an in-development **v2**
   as two embedded resources; read `{ prelude: :v2 }` from unit metadata in the
   loader (beside `import:`), default to v1; key the prelude type cache on the
   selection (`src/driver/check.rs`; noted for W6). This is two `include_bytes!` +
   a selector — explicitly *not* editions (§5): a prelude is a *unit merged in*,
   not semantic lowering rules.
4. **Deprecation lifecycle** — `deprecated:` / `replaced-by:` metadata, a use-site
   WARNING (never an error under the advisory default), and an LSP quick-fix
   reusing the existing `WorkspaceEdit` machinery (`src/driver/lsp/actions.rs`).

The stable-surface **tier table** (§4.1) is ratified and published as a one-page
statement; the **type system is labelled Experimental** so it does not block a
lang/prelude 1.0. The **freeze** of the enumerated surface happens at 1.0.

**Implementation.** `build.eu` + release flow (S); `requires` docs + optional hint
(S); prelude versioning in `resources.rs`/`source.rs`/`check.rs` (M); deprecation
metadata + diagnostics + quick-fix (S). Front-end/loader only.

**Risks.** Two preludes drift (mitigate: v1 *frozen*, security-only; feature work
in v2). Premature 1.0 burns credibility (mitigate: the gate, and keeping the type
system Experimental). `requires` pins rot (mitigate: the missing-pin hint; model
it in shipped units).

**Success.** A published `1.0.0` with real semver and a one-page tier statement;
prelude v2 exists opt-in with v1 frozen; every Stable prelude element has a
deprecation→removal path; the deep-merge default, when it lands, ships as a MAJOR
or behind `prelude: :v2`, never a silent flip.

#### W2. Eliminate the double STG compile of plain documents

**Problem.** For a plain (non-IO) document — the common case — `try_execute`
compiles the prelude-plus-unit evaluand to STG **twice**: once headless to
classify IO-vs-document, then again under `RenderType::RenderDoc` on a *fresh*
machine to render (`src/driver/eval.rs:178-295`). The second compile (~90 ms, and
not even timed) exists only as a workaround: rendering the headless result in
place via `render_headless_result` (`src/driver/io_run.rs:1602`) crashes the GC on
stale string pointers left by the first run (`eval.rs:272-275`). The IO paths
already render in place safely; only the pure-document path recompiles.

**Design.** Fix the stale-string hazard so the pure-document result renders in
place like the IO cases → **one compile, one machine** on every plain-document
run, and a known heap-lifecycle landmine removed. Independent of, and complementary
to, the prelude embedding in W6 (W2 removes the *second* compile; W6 removes the
*prelude* compile).

**Implementation.** `src/driver/eval.rs:163-302`, `src/driver/io_run.rs:1602`.
Small but touches heap lifecycle — validate under `EU_GC_VERIFY=2` +
`EU_GC_STRESS=1`. P1 / high priority.

**Success.** A plain-document run performs exactly one STG compile (verifiable via
the `stg-compile` timing); the headless result renders in place; no GC crash under
verification across the harness; a regression test exercises it.

#### W3. The Unit Interface (and the cross-import bracket fix)

**Problem.** A dependency makes several *source-level contributions* to a dependent
unit's compilation, handled today by **four separate, inconsistent mechanisms**:
bracket content-modes (per-file `BracketRegistry`, no cross-file seeding —
`src/syntax/rowan/parse.rs:45`); the monad-namespace registry (seedable —
`src/core/desugar/desugarer.rs:171`); the operator table (rediscovered from the
merged tree at cook, no seeding — `src/core/cook/fixity.rs:143`); and type schemes
(`PreludeSummary`/`with_seed` — `src/core/typecheck/check.rs:197,382`). Operators
are even captured twice (typecheck *and* cook). One of these is an outright bug:
because a block-style idiot-bracket pair is decided per-file at parse, a pair
**defined in an imported file is invisible** to the importing file's pre-scan, so
its uses there default to soup/expr mode — a program that works in one file
**silently mis-parses** when the definition is moved to a library.

**Prior art.** A GHC `.hi` interface / ML signature: build a unit's interface once;
seed each phase of a dependent from its dependencies' interfaces. `PreludeSummary`
is already a partial one.

**Design.** Refactor the four contributions into a single per-unit **Unit
Interface** with one consistent pattern. This **fixes the bracket bug** (brackets
gain a slot and the seeding they lack — the block-vs-soup decision is deferred past
parse to the seedable desugar stage), removes the operator-table redundancy
(extract once, serve both cook and typecheck), and makes the cross-unit surface
explicit and testable. It is independently valuable *before* any separate
compilation, and is the foundation W6 (separate compilation), W7 (incremental) and
W18 (modules) all consume; demand signatures (W9) and exported-binding slots become
further fields of the same interface.

**Implementation.** `parse.rs:45` (brackets), `desugarer.rs:160-177` (the seedable
template), `fixity.rs:143` (operator rediscovery), `check.rs:197,382`
(`PreludeSummary` to generalise). Parse is the awkward phase (it runs *before*
import resolution), which is exactly why the bracket contribution must move to the
seedable desugar stage. Interfaces build bottom-up over the import DAG; mutual
recursion across units is the residual hard case. Medium / architecture.

**Success.** One mechanism instead of four; the imported-bracket repro parses
correctly; operators are extracted once; the interface is a documented, testable
artefact.

#### W4. Resilient parser front-end & error recovery

**Problem.** The syntactic LSP features already work by calling the Rowan parser
directly (`src/driver/lsp/mod.rs:805ff`). What is blocked is the *pipeline*: an
all-or-nothing shim discards the Rowan **lossless** tree on any error, so a single
typo turns the whole file opaque to diagnostics and to every downstream pass.

**Prior art.** Rowan (rust-analyzer's lossless syntax trees) is built for error
recovery — a tree is always produced, with error nodes in place.

**Design.** Two phases. **Phase 1 (0.8):** delete the all-or-nothing shim and
always surface the Rowan partial tree (~150 lines; immediate diagnostic
improvement). **Phase 2 (0.9):** real error-recovery in the parser — synchronising
on block/list boundaries and producing typed error nodes — so a malformed region
is isolated and the rest of the file still parses, cooks and checks. Bracket
recovery couples to W3 (the deferred block/soup decision).

**Implementation.** `src/syntax/rowan/`. Phase 1 small/low; Phase 2 medium. Honour
the panic policy — odd input yields a clean diagnostic, never a panic.

**Success.** A file with one error still yields diagnostics and hovers for the rest
of the file; the partial tree always reaches the pipeline.

#### W5. Conformance corpus, property tests & fuzzing

**Problem.** The 315+ harness tests are unusually good — *dogfooded*, written in
eucalypt's own `//=`/`//!`/`//=>` assertion operators (so they double as `eu test`
and as examples), with `.expect` sidecars for error/check tests. But the harness
is an internal regression suite, not yet (a) a **portable contract** an independent
backend could run, nor does it externally pin (b) **rendered output bytes** or the
(c) **prelude-v1 freeze**; there is **no property-based testing**, **no fuzzing**
of the parsers or the type-DSL, and GC verification runs in only one CI job.

**Prior art.** Dhall and the WebAssembly spec tests make the *files plus a
documented pass/fail convention* the contract, each implementation bringing its own
runner (never privileging one); Nickel's `.ncl`+`.expected` is the minimal form.
QuickCheck (Claessen & Hughes, ICFP 2000) / Rust `proptest` for properties;
CSmith (Yang et al., PLDI 2011) and metamorphic/differential testing (Rigger & Su,
OOPSLA 2020) for compilers; `cargo-fuzz`/libFuzzer for parsers.

**Design.** (1) **Document the corpus contract** and keep the `.eu` files as-is;
add a **golden-output `.expect` layer** to a curated subset — every export format,
and the prelude-v1 frozen set — extending the existing sidecar plumbing. This is
the executable form of the §4.1 freeze and "what would have caught the `cond`
break". (2) **Property tests** (`proptest`): round-trip idempotence
(render→parse), render determinism, `eu fmt` idempotence, subtyping
reflexivity/transitivity and the §4.2 consistency-symmetry property (needs an
`Arbitrary` for `Type` covering `Con`/`App`/`Forall`/`Mu`), and GC invariants over
generated structures. (3) **Three `cargo-fuzz` targets**: `fuzz_type_dsl`
(highest priority — the hand-written parser over untrusted annotation strings,
`src/core/typecheck/parse.rs`), `fuzz_parser` (Rowan), `fuzz_loader`
(YAML/JSON/TOML/CSV/XML import). (4) **Promote `EU_GC_VERIFY=2`+`STRESS`+`POISON`
to a full-harness CI job** on x86-64, and `asan` to a hard gate.

**Implementation.** Phase 1 (0.8): contract doc + golden sidecars + the GC-verified
CI job. Phase 2 (0.9): `proptest` + properties. Phase 3 (0.9–1.0): fuzz targets +
seed corpora, run to exhaustion before the freeze; every panic becomes a diagnostic
with a regression test. The reference runner and existing files are untouched.

**Success.** Golden sidecars cover every export format and the frozen prelude set;
the property suite passes; **zero panics after a 24-hour fuzz run** on each target;
the full harness is green under `EU_GC_VERIFY=2`; when the first alternative backend
lands it can be held to the *same* corpus with its own runner. (The cross-backend
payoff arrives with the forthcoming core→WASM backend — see Candidates.)

---

### 0.9 — Compile latency, docs & incremental groundwork

*Cheap-to-medium: attack the most visible perf problem and the authoring surface.*

#### W6. Compiled-unit & prelude caching (separate compilation)

**Problem.** Every `eu` invocation re-parses, re-cooks and re-compiles the entire
prelude from source — the ~500–700 ms fixed tax of §2, ~500× the VM work on a
typical run, paid on every run of a tool that lives in CI and scripts. It *looks*
cacheable (the prelude is fixed and embedded, `resources.rs:16`), and its **types**
are already cached (the `PreludeSummary`/`PRELUDE_CACHE`). But the **code** is not,
because the prelude has **no independent compiled form**: every input is spliced
into one core expression by `rebody` (`src/core/expr.rs:548`) *before* STG
compilation, and `stg::compile` runs once on the whole merged evaluand
(`eval.rs:187`). Caching the prelude's STG keyed on its bytes is caching something
that does not exist.

**Prior art.** GHC compiles each module to an object + an **interface (`.hi`)** that
carries exports *and unfoldings* (so cross-module inlining survives) *and strictness
signatures*. ML functors / 1ML / Backpack compile a module as a function of its
imports' signatures. Unison identifies definitions by content hash, making
linking/caching free. Python's `.pyc` is the security lesson: a cache of executable
artifacts is an RCE vector — keep it co-located and validated (PEP 552), or avoid
it.

**Design.** The real fix is **separate compilation** on top of the Unit Interface
(W3): give the prelude an independent compiled form whose cross-unit references are
unresolved externals, then **embed the compiled prelude in the binary** (mirroring
how `build-meta.yaml` is embedded via `include_bytes!`, with a build-time hash
check against `lib/prelude.eu`). This eliminates prelude compilation at runtime
with **no on-disk cache and no security surface**, and auto-versions (it *is* the
binary's prelude).

The **one genuine wall is operators**: cook discovers operators from the merged
`let`-nest and mis-parses an out-of-scope operator (`fixity.rs:143`; verified:
`-Q` + `2 + 3` → "unresolved variable '+'"), and there is no seeding mechanism
today. User code uses ~55 prelude operators pervasively, so cooking a unit
independently of its dependencies' operators is the load-bearing change — but for
the prelude floor it is bounded (the prelude's operator set is fixed and already
extracted on the type side, `check.rs:86`). Monad namespaces are already seedable
and bracket modes are handled by W3.

Four mechanisms, floor to radical: **(A)** stable-global-prefix — compile the
prelude once to a fixed `Ref::G` global-slot layout (its de Bruijn indices are
already body-independent), user bodies reference those slots; **(B)** symbolic
externals + link (GHC `.o`+`.hi`, carrying inline-able defs); **(C)**
units-as-functions (ML functors; a unit is `λimports. body`, the most
eucalypt-idiomatic since blocks are records); **(D)** content-addressed definitions
(Unison; unifies with W18). **The prelude floor (A, with operator seeding) is the
recommended, Medium-effort 1.0 target; arbitrary-unit separate compilation
(B/C/D) is Hard and post-1.0.**

**An on-disk cache is explicitly demoted.** After W2 + the embedded prelude, the
only remaining cacheable thing is the *whole merged evaluand* keyed on the full
input set — a hit only on exact re-runs, deduping nothing, and carrying a real
security surface (a cache of compiled units is executable content; content-hash
keying guards staleness, not tampering — a writable cache entry is RCE; a restored
CI cache is untrusted input). If ever pursued it serialises **`StgSyn`** (off-heap,
`Rc`-based, pointer-free — *not* `HeapSyn`), keyed `sha256(inputs) ∥ version ∥
prelude-version`, advisory (any miss/corruption → clean recompile), and refuses a
group/world-writable dir. A long-running daemon (Bloop-style) is deferred likewise
(a Unix-domain socket at `0600`, authenticated). Embedding avoids all of this for
the dominant cost.

**Implementation.** W3 (interface) → fixed global slots
(`compiler.rs`, `machine/mod.rs:55`) → **operator seeding** of cook (the real work)
→ deferred externals + link (`compiler.rs:1031`; the type checker's seed path is the
template) → embed with a build-time source-hash check; land W2 alongside. Prelude
floor (A): **Medium, feasible.** General units (B/C/D), the on-disk cache and the
daemon: **post-1.0**, gated on demonstrated need.

**Risks.** The operator wall may not yield cleanly to seeding (fallback: the
prelude floor, which still needs bounded operator seeding). Lost cross-unit
inlining/DCE — mitigate by carrying inline-able defs *and* strictness signatures
(W9) in the interface (GHC's lesson); the latency win should dominate but is the
benchmark to watch.

**Success.** The prelude is compiled **zero times at runtime**, output
byte-identical across the harness, cold latency down by the prelude's share, **with
no on-disk executable cache**; prelude operators resolve in separately-compiled
user code (the verified failure mode is gone); W2 lands (one STG compile per plain
document).

#### W7. Incremental, query-based core

**Problem.** The front-end pipeline (CLI and editor) recomputes everything on every
change. The LSP holds pipeline state but cannot reuse sub-results across edits or
across the CLI/editor boundary; re-evaluation re-runs the whole front-end.

**Prior art.** Salsa (rust-analyzer) and Adapton: a demand-driven query graph with
content-hashed inputs and memoised, invalidation-tracked derived queries — the same
discipline rustc's red-green incremental cache and Bazel's action cache use.

**Design.** Wrap the front-end passes as **queries** over the Unit Interface (W3):
parse, the per-unit interface, cook, check, compile become memoised nodes keyed on
content hashes, so an edit re-runs only the affected sub-graph. The embedded prelude
(W6) is the base case of the graph; the cache-key discipline is shared with W6.
**Scope for 1.0 is the front-end pipeline** (diagnostics, completion, re-compile);
incremental *evaluation* (re-running the VM incrementally) is post-1.0. A real
constraint: `RcExpr` and much of core/STG are `Rc`-based and **not `Send`**, so the
query store is single-threaded for now (it shares the runtime's threading posture).

**Implementation.** A query layer over `src/core/*` and `src/driver/*`, consuming
W3's interfaces. Begin in 0.9 (wrap the passes); the cache key must account for the
cross-unit inputs W3 exposes (brackets, operators, monad namespaces, schemes) — the
reason W3 is the prerequisite. Medium.

**Success.** An edit to one unit re-runs only its dependents' affected queries;
LSP latency on large workspaces drops; the same graph serves CLI re-compiles.

#### W8. `eu doc` — documentation & schema extraction

**Problem.** The prelude reference is hand-written and drifts from the code; there
is no way to extract documentation or a schema from a `.eu` unit's metadata and
types.

**Design.** A `eu doc` subcommand performing **source-level extraction** (not
eval-to-verify): read the `` ` `` metadata (`doc:`, `type:`, `deprecated:`) and the
binding structure straight from the parsed/desugared unit, reusing the symbol-table
machinery the LSP already has (`symbol_table.rs`). It reads the **`s"…"` type-DSL
surface** (§4.3) to emit schemas. A **doctest** mode runs the examples in `doc:`
metadata under the test harness (ties to W5). Regenerate the hand-written prelude
reference from the prelude itself.

**Implementation.** `src/driver/` new subcommand + the LSP extraction; render to
Markdown/JSON-schema. Small-to-medium, low risk. Reads W1's `deprecated:` metadata
and W3's interface where cross-unit.

**Success.** The prelude reference is generated and matches the code; a unit's
`type:`/`doc:` metadata produces a schema and a doc page; doctests run in CI.

#### W9. One demand annotation behind the strictness heuristics

**Problem.** Eucalypt has ~six separate hand-rolled mechanisms that all decide
*`Value`-vs-`Thunk` / skip-`Update`* from facets of one question — *is this binding
used-at-most-once / strict / already-WHNF?*: `LambdaForm::Value` vs `Thunk`
(`syntax.rs:213`); per-intrinsic `strict_args` (`intrinsics.rs`); per-intrinsic
`single_use_args` (`intrinsic.rs:119`); the bespoke `suppress_update` threading for
`IF` alone (`boolean.rs:143`, `vm.rs:270`); `is_whnf` (`syntax.rs:143`); and the
global `--suppress-updates` call-by-name escape hatch. Two are per-intrinsic hand
lists; one is bespoke threading for a single intrinsic.

**Design.** Unify them behind **one demand annotation per binding** (cardinality +
strictness + WHNF) as the single representation the `Value`-vs-`Thunk` and
update-suppression decisions consult. The existing heuristics populate it today;
the analysis (W11) populates it more completely later. This pays off *before* any
analysis — one decision point instead of six, testable and consistent — and the
annotation on an *exported* binding is a **strictness signature** the Unit Interface
(W3) carries for W6 (a missing signature only costs optimisation, never
correctness — unlike operators, it is not a wall).

**Implementation.** A demand annotation on core bindings; `take_lambda_form`
(`compiler.rs:506-517`) consults it instead of the scattered flags;
`single_use`/`suppress_update` become populators/overrides during transition.
Small-to-medium.

**Success.** One annotation drives the thunk/update decisions; at least one
`single_use_args` override is subsumed; no harness regression.

---

### 0.10 — The runtime: GC & demand

*The substantive engineering release: one major GC effort plus its multiplier.*

#### W10. Generational nursery & Immix completion

**Problem.** The GC is the largest runtime consumer on real workloads (**mark time
>95% of VM time** on traversal-heavy programs) and it **re-marks the entire live
set every cycle** — there is no notion of age, no remembered set, no way to skip a
long-lived object. The canonical case `008_long_lived_graph.eu` builds a 200-cell
list that survives the whole run and re-marks all 200 cells on every collection.
Three concrete deficiencies: the **trigger is crude** (a 500-step countdown,
`vm.rs:1760`, and `policy_requires_collection` returns false when no
`--heap-limit-mib` is set, so allocation rate is ignored despite being tracked,
`heap.rs:167`); the **top defrag tier is unwired** (`DefragmentationSweep`'s
`candidates()` is a stub `// TODO: all blocks`, `heap.rs:87`, so the
worst-fragmentation case silently degrades to non-compacting); and the GC
documentation is **stale and wrong** (it claims "no evacuation / eager sweep", but
the collector already does opportunistic evacuation with forwarding pointers,
`collect.rs:298,465`, and lazy deferred sweep, `heap.rs:674,680`). The collector is
"Immix without the *generational* dimension".

**Prior art.** Immix (Blackburn & McKinley, PLDI 2008) is the direct ancestor —
32 KiB blocks, 128 B lines, opportunistic evacuation — designed to sit *under* a
nursery ("as the mature space, Immix matches a tuned generational collector"). The
**sticky-mark-bit** algorithm (Demers et al., POPL 1990; Wingo 2022): flip the mark
bit only on major collections, trace minors from roots + a remembered set, so
"marking an object is tenuring, in place" — eucalypt already flips a single mark bit
per cycle (`flip_mark_state`, `heap.rs:1716`), making this a near-minimal delta. The
generational hypothesis holds strongly here (laziness manufactures vast short-lived
thunks). GHC's lesson for lazy languages: a generational collector needs a
**write barrier** on **thunk update** (the forced-thunk overwrite) to catch
old→young pointers, coupled with eager promotion or a remembered set; in eucalypt
thunk update is essentially the *only* in-place heap write. Data-language peers
offer no GC blueprint (they ride host GCs).

**Design.** A runtime-internal change, **no surface syntax**, four parts, sequenced
so each lands value independently:

1. **Allocation-rate trigger** — decouple "should we collect?" from the heap limit;
   fire on an adaptive nursery budget using the already-tracked allocation volume
   (`heap.rs:167,1771`). Prerequisite for a nursery.
2. **Sticky-mark-bit minor collection** — designate freshly allocated blocks as the
   **nursery** (the existing `head`/`overflow` active blocks); a **minor**
   collection traces from roots + remembered set **without flipping** the mark
   state, so any nursery object it marks is tenured in place; a **major** collection
   is today's full flip-and-trace. No copying, no second space, no new header bit.
3. **Write barrier on thunk update** — the single mutation site is
   `EnvFrame::update` (`env.rs:361`), reached when an `Update` continuation
   overwrites the black hole (`vm.rs:540`). **Eager promotion** (preferred): on
   update, if a mature frame is written with a nursery value, tenure it immediately
   (writes are write-once, so no repeated cost); a **remembered set** is the
   fallback for awkward cases.
4. **Finish `DefragmentationSweep`** — replace the stub with the full set of
   unpinned fragmented block indices (the enumeration already exists for the stress
   path); closes the worst-fragmentation regression.

Everything runs **inside the existing stop-the-world, single-threaded collector**
(`heap.rs:8`); concurrency is deferred to W21.

**Implementation (phased).** P0: rewrite the stale GC doc (trivial). P1:
allocation-rate trigger (small/low). P2: finish `DefragmentationSweep`
(small/low). P3: minor collection + nursery (medium / medium-high). P4: write
barrier with eager promotion (`env.rs:361`, `vm.rs:526`, `cont.rs`) (medium /
**high**). P5: remembered-set fallback + minor/major scheduling (medium). P0–P2 are
independently shippable wins; P3–P5 are the strategic core. A new `EU_GC_VERIFY`
checkpoint asserts the **no-untracked-old→young-edge** invariant after every minor
collection.

**Risks.** The write barrier may cost more than the nursery saves — a net
regression on `002_thunk_updates`/`001_naive_fib` not recovered on `008`/`007`
would falsify it (mitigate: eager promotion behind one predictable branch).
Generational rewrites are where GCs crash, and the evacuation path already has
documented aarch64-specific bugs (`collect.rs:1515`) — mitigate with the
`VERIFY`/`POISON`/`STRESS` harness, the new edge-invariant check, and W5 fuzzing.

**Success.** `008_long_lived_graph.eu` shows the `CollectorMark` fraction drop well
below the >95% regime; net wall-clock gains on `007_short_lived`/`004_generations`;
`009_fragmentation` no longer degrades; no regression on `001`/`002`; the full
harness green under `VERIFY=2`/`POISON=1` with the old→young invariant holding.

#### W11. Strictness & demand analysis

**Problem.** Every `let`-binding reaching the STG compiler without a WHNF witness is
compiled as a `Thunk` (`syntax.rs:213-225`): allocate the closure, black-hole it on
entry, push an `Update`, evaluate, write back, leave a dead object for the GC to
mark — on *every* binding unless the compiler can prove it WHNF or used-at-most-once.
In traversal-heavy workloads this thunk churn is exactly the mark pressure W10
attacks, from the other side. The hand-rolled heuristics (W9) only cover sites a
programmer or intrinsic author flagged in advance; **no pass discovers strictness
from the program text.**

**Prior art.** GHC's demand analysis (Sergey et al., POPL 2014) — a backward
abstract interpretation over a demand lattice carrying **strictness** (definitely
evaluated → no thunk) and **cardinality** (used at most once → no update). "One of
the highest-return optimisations for a lazy language." Projection-based strictness
(Wadler–Hughes, FPCA 1987) for finer demands (head/spine) is a post-1.0 refinement.
Among peers only Nickel is comparable (Rust, lazy, sharing) and publishes no
analysis — eucalypt's situation is closest to it; purity makes the analysis sound
(no side effect invalidates a strictness proof).

**Design.** A backward pass over **core expressions** (after inline/fuse, before STG
compile), populating the W9 demand annotation completely. A minimal two-point
lattice (`L`/`S`) plus usage (`U1`/`SU1`) suffices first: a binding compiles as
`Value` if `U1`/`SU1` (its memoisation is never exploitable). Application propagates
a callee's strictness signature to arguments (intrinsic signatures read from
`strict_args`; user functions analysed); `Case` joins across branches; `LetRec`
needs a fixed point bounded by the lattice depth (≤2 iterations). **Soundness is
one-directional**: for a pure language, a wrong `U1` changes only *cost* (O(1)→O(k)),
never results — so the first implementation is conservative (when in doubt, `Thunk`).

**Implementation.** A new `src/core/analyse/demand.rs` (~300–400 lines) wired
between fuse and STG compile; `take_lambda_form` consults the annotation (~10
lines); P3 is the LetRec fixed point (the fiddly part). The signature on an exported
binding rides in the Unit Interface (W3/W9) for W6.

**Risks.** A soundness slip duplicates expensive work (mitigate: conservative
default; property-test analysis results against a call-by-name run). Compile-time
cost is negligible (a linear pass adds milliseconds).

**Success.** ~20% reduction in allocation count on prelude-heavy transforms;
measurable mark-time reduction (`benches/gc.rs`); at least one `single_use_args`
override removed; tail-recursive conditionals show flat stack depth under
`EU_STACK_DIAG=1` with no `suppress_update` flag; no harness regression.

#### W12. Restore git imports (fetch-under-a-hash)

**Problem.** `{ import: { git: …, commit: …, import: … } }` is **documented as
working** (`docs/guide/imports-and-modules.md`, `docs/reference/import-formats.md`)
but does nothing in the Rust implementation. It was a real, shipped feature of the
**Haskell** eucalypt (added 2019, PR #115/`6e56dc5c` — `Driver/Git.hs` cloned a
repo at a commit into a `.eucalypt.d` cache) that was dropped wholesale in the
2021 Rust rewrite (`6217817f`) and never re-ported; the docs survived. Today
`scrape_rowan_imports` (`src/syntax/import.rs:259-293`) matches only string and
list elements — a `{ git: … }` block is silently dropped (the `_ => {}` arm), so the
user gets a downstream "unbound name", not a clear "git imports unsupported". There
is no `Git` variant in `Locator` (`src/syntax/input.rs:17`), no git dependency, and
the `Url` locator parses but has no fetch arm either. (An interim docs fix marking
the feature not-yet-implemented has shipped.)

**Design.** Restore it **consistent with the module system (W18), not as the bare
2019 form** — implement git as a **fetch backend beneath a content hash**: teach
the scraper the **block form** (the same change W3 and W18 need — read
`git:`/`commit:`/`import:` plus an optional `sha256:`); add a git-capable locator +
clone-and-cache at the pinned commit (keyed by commit SHA, the `.eucalypt.d` cache
the reference); and verify the optional `sha256:` **content** hash on the fetched
bytes (the commit SHA pins the ref; the content hash additionally defends against a
force-push / rewritten history). This makes the regression-fix the **foundation
W18 builds on** rather than a throwaway. The unwired `Url` locator is the same
loader gap and can be closed alongside. P1 (the docs are otherwise wrong).

**Implementation.** `src/syntax/import.rs:259-293` (block scraper, shared with
W3/W18), `src/syntax/input.rs:17` (a `Git` locator), a git dependency + cache, the
`Url` fetch arm in `src/driver/source.rs`. Medium.

**Success.** The documented git-import form fetches, pins by commit, verifies the
optional content hash, and resolves bindings; the silent-drop path is gone.

---

### 0.11 — Blocks, value model & presence types

*The second GC item — only after W10 — plus the value-model and type enrichments.*

#### W13. Persistent O(log n) blocks & the GC-finalisation fix

**Problem.** Blocks — the thing eucalypt exists to produce — are cons-lists with
**O(n) lookup** (`Block::wrapper`, `src/eval/stg/block.rs:79`; the `find` loop at
`:495`), and **merge is catenation**: `merge`/`deep-merge`/`<<` walk both operands
into an `IndexMap` and re-emit a fresh spine (`Merge::execute`, `block.rs:1453`),
O(n+m) with no structural sharing — and configuration work is merge-heavy by
nature. A partial mitigation exists (a cached `HashMap` index built lazily above
`BLOCK_INDEX_THRESHOLD = 16`, `block.rs:469`), but it is an **`Rc<BlockIndex>` on
the Rust heap** the GC never frees. A previous persistent O(log n) attempt
(`im_rc::OrdMap`) was **reverted (ADR-001)** not because it was slow but because it
**leaked**: the bump allocator recycles memory by overwrite and **never runs
`Drop`** (`src/eval/memory/bump.rs:294,371`), so the map's `Rc` nodes — on the Rust
heap, outside GC management — were stranded, producing **220–580% regressions** on
GC-churn benchmarks even for programs barely using blocks. This is the wall: a
**GC-finalisation** problem, not a block-design one.

**Prior art.** HAMT (Bagwell, *Ideal Hash Trees*, 2001) — the 32-way
bitmap-indexed hash trie behind Clojure/Scala immutable maps, O(log₃₂ n) with
structural sharing. **CHAMP** (Steindorfer & Vinju, OOPSLA 2015) — the modern
refinement: two compact arrays per node (inline data, child references), giving
1.3–6.7× faster iteration and a smaller footprint (adopted by Scala 2.13);
iteration speed matters because rendering and `deep-find`/`deep-merge` are
iteration-bound. **Persistent vectors** (Clojure's `PersistentVector`; RRB-trees,
Bagwell & Rompf, 2011) — the vector analogue, O(log₃₂ n) append/update/index with
sharing. The `im`/`im_rc` crates are `Rc`/`Arc`-node B-trees (2–3× slower than std
*before* any leak) — exactly the shape the GC cannot finalise. GC **finalisation**
itself is hazard-rich (resurrection, ordering, non-determinism; Java deprecated
`finalize()`; Hughes & Tratt, *The Finalizer Frontier*, 2024) — so where the
payload is just more heap data, make it **GC-managed data in the first place**.

**Design.** Store the persistent structures **inline in the GC heap** as ordinary
scannable objects (option (c) of four — (a) a finaliser table is the pragmatic
*fallback*; (b) arena allocation re-creates the leak; (d) switching to a
`Drop`-supporting collector discards the Immix investment). A CHAMP node is a
header-prefixed heap object with a `u32` datamap + `u32` nodemap and two inline
`Array`s, scanned by a new `impl GcScannable for ChampNode` modelled on the
existing `HeapSyn` arms — **no `Rc`, no Rust-heap nodes, no finaliser ever
required**; evacuation needs no new code (a header-prefixed object forwards like any
other).

**Order preservation is essential** and a hash trie cannot give it alone (CHAMP
orders by hash; blocks render in **insertion order**, verified against
`./target/release/eu`: override updates in place keeping position, a new key
appends). So a **threshold hybrid**: below 16 a block stays a plain **cons-list**
(ordered, allocation-light, faster than a trie at small *n* — purely additive for
the common case); at/above 16, **two GC-native structures** — a **CHAMP key-index**
(`SymbolId → order-slot`) and a persistent **order sequence** (a Bagwell/RRB
persistent vector) holding the `(key, value)` pairs in insertion order. Lookup is
two O(log n) descents; render/`ELEMENTS` walks the order sequence O(n) with values
in hand; override updates one slot leaving the CHAMP untouched; a new key appends +
inserts; both structures share unchanged sub-trees across a merge, so `<<`/
`deep-merge` allocates in proportion to the *change*. Both node types reuse the
**same** scannable `Array<Ref>` + `GcScannable` machinery, so the second structure
adds no new collector surface. The `Block` constructor becomes
`Block(repr, meta)` (the cons-list below threshold, the `(champ-index,
order-sequence)` pair above), with `meta` replacing the `Rc` index slot — deleting
the one leak tolerated today.

**Implementation (phased).** New `ChampNode` + order-sequence node heap types
(`src/eval/memory/`, `array.rs`); `GcScannable` for both (the genuine risk — GC
correctness); trie + order-sequence ops (lookup, append, update-in-place,
sharing merge, in-order iteration) in `block.rs`; dual `Block` repr + dispatch of
`LOOKUP`/`MERGE`/`DEEPMERGE`/`ELEMENTS`; validate under
`VERIFY=2`/`POISON=1`/`STRESS=1`. **Sequenced after W10** (same scarce GC
expertise; W10's nursery makes the many short-lived intermediate nodes cheap).

**Risks.** CHAMP/vector tracing bugs are the same class as the documented
aarch64 evacuation bugs — a use-after-free under `POISON=1` falsifies it. No net win
on real configs would collapse the value case (gate on independently-verified
benches, not microbenchmarks). Small blocks are protected by the cons-list
threshold.

**Success.** Lookup on a 1,000-key block grows ~logarithmically; merge of two large
blocks differing in a few keys allocates proportional to the *difference*;
`deep-find` scales toward near-linear; **no GC-churn regression** (the ADR-001
220–580% does not recur) and the `Rc<BlockIndex>` slot is gone; rendered key order
is byte-identical to the cons-list implementation across the harness; full harness
green under `VERIFY=2`/`POISON=1`.

#### W14. Generalise `vec` to hold arbitrary values

**Problem.** `vec` — the only O(1)-indexed sequence — holds **only primitive
scalars** (`Num`/`Str`/`Sym`), because its backing is `Vec<Primitive>`
(`src/eval/memory/vec.rs:14`) and `VEC.OF` rejects anything else
(`extract_primitive` panics, `src/eval/stg/vec.rs:86`). So you cannot get
random/indexed access to a sequence of **records (blocks) or nested lists** — the
canonical "array of objects" shape of CSV/JSON/YAML inputs — and must fall back to
O(n) cons-lists. "Non-primitive" is non-trivial: there is **no
`Native::Block`/`List`/`Closure`** (`src/eval/memory/syntax.rs:37-58`) — those are
`HeapSyn` closures with environments — so an element must be a **closure** (code +
captured env), and the right backing is a GC-managed **`Array<Closure>`**, not
`Array<Ref>`. Secondarily, `Native::Vec` is *marked but never scanned*
(`syntax.rs:343,380`) and its `Vec<Primitive>` buffer lives on the Rust heap, so a
reclaimed vec's backing **leaks** today (ADR-001 class) — tolerated only because
vecs are rare.

**Design.** Re-shape `HeapVec` to follow the **`EnvFrame`** pattern
(`src/eval/machine/env.rs:196,498`) — a GC-traced, evacuation-safe array of
heterogeneous closures already exists in-tree; `HeapString` is the template for a
traced native-with-backing. Store `Array<Closure>`; add `impl GcScannable for
HeapVec` and **push `Native::Vec` for scanning** (the correctness-critical wiring).
The seven intrinsics mostly *simplify* (the `Primitive` round-trip and `Boxed*`
re-wrapping disappear); `emit_vec` routes each element through the normal
per-element render. Moving to a GC `Array` also **fixes the backing leak**.
Strictness: store source element closures as-is (drop `VEC.OF`'s deep-force) so vec
inherits list laziness — O(1) is about *index*, not evaluation. Shares W13's
GC-scannable-array machinery; otherwise independent of it.

**Implementation.** `vec.rs` (storage), `syntax.rs` (scan wiring + `GcScannable`),
the seven intrinsics in `stg/vec.rs`, `emit_vec`. Sets/ndarrays and the collector
core are untouched. Medium GC risk, doubly precedented; validate under
`VERIFY=2`/`POISON=1`/`STRESS=1`.

**Success.** `vec.of`→`vec.to-list` round-trips a list of blocks/nested lists
preserving order and identity; `vec.nth` over a vec of blocks returns the block
(indexable further) in O(1); a non-primitive vec renders identically to the
equivalent list; no GC-churn regression and the prior backing leak is gone.

#### W15. Optional (presence-annotated) record fields

**Problem.** Record types cannot express that a field may be **absent**: a `Record`
is required-keys-only (`src/core/typecheck/types.rs:238`), and the checker reports a
missing required field (`subtype.rs:704`). Real config — and any ingested schema
with optional properties (W22) — needs *presence* in the type, distinct from
`Partial(T)`/`T?` which is about a *value* being partial, not a *field* being
optional.

**Prior art.** Presence polymorphism / row types with presence variables (Wand;
Rémy's "absent labels"); the row algebras already in eucalypt's lineage (Leijen's
scoped labels were recommended for *merge*, which is a different axis from
presence). The annotated case is what ships; full presence *inference* is deferred.

**Design.** A key-side `?` slot — **`name?: T`** in the type-DSL (the `?` attaches
to the key, distinct from the value-side `T?` which stays `Partial`). A record type
partitions into required and optional fields; presence subtyping makes a record
with a present optional field a subtype of one where it is optional, and the
merge×presence rule composes with block merge. `match?`/`me?` gain an **optional
arm** so a spec/contract (W16) can say "this key may be absent". The annotated form
is 1.0; **presence inference is post-1.0** (W20-era, or later).

**Implementation.** `src/core/typecheck/parse.rs` (the `?` key slot, kept separate
from the `:541-543` value-`?`→`Partial` path), `types.rs:238` (the
required/optional partition), `subtype.rs` (presence subtyping + the missing-field
rule), and the `me?` rule (`lib/prelude.eu:548-549`). Medium — type-checker work,
a different skill area from the GC items it shares the release with.

**Success.** `name?: T` checks and renders; a record missing an optional field is
well-typed; `match?` with an optional arm matches both presence and absence; W16
contracts and W22 schemas can express optional properties.

---

### 0.12 — Data-correctness & reproducibility

*The config story that distinguishes a 1.0 data tool.*

#### W16. Structural contracts & runtime validation

**Problem.** §4.2 keeps the type boundary optimistic, so **bad data still
renders** — a manifest assembled from imported (`any`-typed) data that violates an
annotation emits anyway. A config tool must offer runtime validation **where the
user asks for it** (at data ingress), without the always-on cost §4.2 declines.
Eucalypt already has the raw materials: `match?` and a predicate vocabulary, plus
the type-DSL.

**Prior art.** Clojure **spec** — specs are values: composable predicates that both
*validate* and *describe* data, kept separate from the type system, applied at
chosen points. Nickel pairs static types with **runtime contracts** at the
boundary. Findler–Felleisen contracts (ICFP 2002) — pay-as-you-go validation *where
written*, not at every crossing.

**Design.** A **two-stage spec model** unifying `match?`, types and runtime
validation. The **`s"…"` string-prefix** (§4.3) produces **type-data** (a
first-class value form of a type, distinct from a "magical" bare symbol);
**`as-spec`** transforms type-data into a runtime **spec value** speaking the
`match?` vocabulary (predicates, shapes, alternatives, optional arms); consumers
auto-lower a type to a spec via `to-spec`. Validation is applied explicitly at
ingress (`parse-as`/import sites and user-chosen checkpoints), returning structured
blame, never wrapping every boundary. Enriched by **W15**: a spec can mark a field
optional via the `me?`/`match?` optional arm. This is the runtime dual of §4.2 —
together: static checking via `eu check --strict`, optimistic erased boundaries in
production, contracts for the few places runtime validation earns its keep.

**Implementation.** Build on `match?` and the predicate intrinsics; `as-spec`/
`to-spec` in the prelude; the `s"…"` surface from §4.3 (its value-context machinery
lands here if not already present). Real engineering, no new core-runtime risk.

**Success.** A user can validate an imported manifest against a spec derived from a
`type:` annotation and get a precise, located failure; the same spec serves
`match?`; optional fields (W15) are expressible; the validation cost is paid only at
the chosen ingress points.

#### W17. Hermetic mode for reproducible rendering

**Problem.** Rendering can depend on ambient inputs — wall-clock time, environment
variables, randomness, filesystem/network — so the same source can render
differently on different runs/machines. A config tool's output should be
**byte-reproducible** when the inputs are fixed.

**Design.** A **hermetic mode** (the determinism *mode*; the capability *type
system* is a non-goal, §5) that pins the ambient inputs: a fixed
clock, a controlled environment, a seeded PRNG stream, and deterministic ordering,
so a render is a pure function of its declared inputs. It composes with hermetic
imports (W18, content-addressed + pinned) for end-to-end reproducibility, and with
W21 (any parallelism must stay **unobservable** — advisory, deterministic merge).

**Implementation.** Driver-level (`src/driver/`): a mode flag that routes `io.*`
ambient reads through pinned sources and fixes ordering/seeding; no core/runtime
change. Relates to W5 (golden output is reproducible by construction).

**Success.** Under hermetic mode, a fixed source + fixed declared inputs renders
byte-identically across runs and machines; non-deterministic ambient reads are
either pinned or rejected.

---

### 1.0 — Ecosystem floor, prove & freeze

#### W18. Module & package system (git-only, content-addressed)

**Problem.** There is no module/package system beyond raw file imports — no
versioned, shareable, integrity-checked dependencies, and no namespace isolation
between imported units. A 1.0 that invites third-party sharing needs an ecosystem
floor, but **without** building a registry (§5).

**Prior art.** Go's **registry-free, VCS-direct** model — depend on a repo by URL,
pin by version/commit, record integrity hashes in a lockfile (`go.sum`), select
versions by MVS — proves you can have a real package ecosystem with **no central
index**. Unison's content-addressing makes identity and caching intrinsic.

**Design.** Build on the git fetch backend (**W12**) and the Unit Interface
(**W3**). The 1.0 **core cut**: **content-addressed git imports** (fetch a repo at a
pinned commit, verify a `sha256:` content hash on the fetched bytes); an
**eucalypt-syntax manifest** discovered by upward search from the invoked file's
directory (degrading to today's behaviour, with a `--manifest` override); and
**namespace isolation** so imported units don't collide (consuming W3's interface).
Transitive integrity is recorded in a **lockfile that pins the closure** (the Go
`go.sum` model; a Merkle-tree hash over the import DAG is the noted alternative if
per-file hashes prove insufficient). **No registry** — distribution is git +
GitHub/GitLab alone. Deeper **version selection (MVS) and lockfile depth are
post-1.0** (W18-depth).

**Implementation.** The block-form scraper (shared with W3/W12), manifest discovery
+ parsing in `src/driver/source.rs`, namespace isolation over W3's interface, the
lockfile. Real engineering; the fetch/integrity halves are W12.

**Success.** A unit can depend on another repo by URL pinned to a commit + content
hash, with a discovered manifest and a lockfile pinning the closure; imported
namespaces don't collide; nothing requires a registry; renders are reproducible
with W17.

#### W19. An interactive surface — `eu watch` & REPL (Phase 1)

**Problem.** Data exploration and authoring have no interactive loop — every change
is a fresh cold invocation (the latency of §2), and there is no way to hold a
session and re-query.

**Prior art.** Unison's codebase-aware `watch` expressions; notebook surfaces for
data exploration.

**Design.** **Phase 1 (1.0):** `eu watch` (Unison-style) — re-render on change —
and a **thin REPL over the cache**, leaning on the embedded prelude/cache (W6) for
per-prompt latency and the incremental query core (W7) for reactive re-evaluation.
Phase 1 deliberately avoids holding fragile cross-prompt heap state: the
stale-string hazard is fixed separately (W2), and richer persistent-session state
is the post-1.0 full notebook. **Full notebook is post-1.0.**

**Implementation.** A driver surface over W6/W7; `src/driver/`. Medium.

**Success.** `eu watch` re-renders a file on save with sub-prompt latency; a REPL
evaluates expressions against a loaded unit over the cache.

*(Also at 1.0: **W5** reaches its bar — golden corpus green, property + fuzz
running; **W1** freezes the enumerated stable surface under real semver.)*

---

### post-1.0 — breadth and the curated bets

*The roadmap commits through 1.0. Beyond it, these are chosen deliberately, one at a time (§4.6).*

#### W20. Type-directed compilation

**Problem.** The checker is advisory and **erased** (§4.2): eucalypt pays the full
cost of a gradual type system and banks none of its runtime value. Every numeric
and structural op pays for dispatch it may not need — `+` compiles to a tag-`case`
wrapper (`src/eval/stg/arith.rs:452`) and `Add::execute` re-checks operand kinds at
runtime even where the checker already *proved* `number → number → number`; the STG
already has the unboxed `Native::Num` representation — what is missing is
*permission*, from a type, to take the direct path.

**Prior art.** GHC (demand analysis + worker/wrapper; unboxing is a *strictness*
result as much as a type one — binds this to W11), MLton (representation selection),
flambda (the pragmatic opt-in posture), Idris 2 (types decide what survives), Roc
(lambda-set specialisation — but predicated on *total* inference eucalypt lacks).
Borrow representation-selection + the strictness-gates-unboxing discipline; reject
the closed-world/total-inference assumptions.

**Design.** Thread type facts past erasure as a **node-keyed side-table** carrying a
**provenance bit** — `Synthesised` (proven from code) vs `Trusted` (rests on an
annotation crossing `any`). A type-aware specialisation pass in the inliner emits
specialised bodies where a callee carries an actionable signature and arguments are
actionable-concrete: **unboxing + direct intrinsic dispatch** (the ~5–10× on tight
arithmetic — but narrower for string/block-heavy generation), **dead-branch
elimination** on literal types (keyed on the *brancher set*, not the name `if`),
**IO flattening**, and **interpolation specialisation** (the most *pervasive*
generation win). Lens fusion and block-offset specialisation are dropped — their
payoff needs usage patterns that aren't how eucalypt is written.

**The crux:** optimisation may fire **only on proven-concrete (`Synthesised`)
types**, never on a type merely *trusted* across an `any` — because under a naïve
unboxed path a lying `any` would reinterpret e.g. a string pointer as an integer:
**memory corruption, not a type error**. So **W20 cannot land before the
boundary-soundness decision (§4.2) is in force**, and either restricts to
synthesis-only or relies on the opt-in `--strict-boundary` checks at the feeding
sites. Couples tightly to **W11** (a `number` annotation implies the strictness that
makes unboxing legal).

**Implementation/risk.** A checker side-table → an inliner specialisation pass →
an STG unboxed-arithmetic mode bypassing the tag-`case` (additive; the boxed
wrappers remain for every unspecialised call). The high-risk surface is that STG
path — a single optimised node meeting a mis-typed operand is a **P1 memory-safety
bug**, which is why this is a curated post-1.0 fork. Phased and independently
benchmarkable. **Kill switches:** if runtime perf is not a goal, or if real render
workloads show <2× (generation is block/string-heavy, not arithmetic-heavy), it is
shelved — *measure before committing*.

#### W21. Parallel evaluation

**Problem.** No concurrency; the obvious shared-memory route is expensive (the heap
is an `UnsafeCell` deliberately not `Sync`). Real motivating cases: **batches of
files** (embarrassingly parallel) and **data-parallel `map`/`fold`** over large
imported datasets — both safe by construction given purity and IO-isolation.

**Prior art.** Model A: JS Web Workers (isolated heaps, message-passing), Starlark
(parallel loading via immutability), scatter-gather. Model B: GHC Strategies/sparks
on a shared heap (Trinder 1998; "Seq no more" Marlow 2010; Harris/Marlow/Peyton
Jones 2005), OCaml-5 (ICFP 2020 — the GC is the gating cost). Rejected:
async/effects (observable nondeterminism), GPU/SIMD (wrong shape for irregular
graph reduction).

**Design.** Lead with **Model A — isolated workers**: each worker a full evaluator
with its own heap, coordinating by passing *serialised data* (a `par-map`/`par-fold`
prelude combinator that scatters chunks to workers applying a **named** transform
and merges results). It fits because purity makes the merge order-independent,
IO-isolation makes effects independent, and W6's embedded prelude makes workers
cheap to spawn; eucalypt's `eu -e <transform> <chunk>` model *is* a worker, and the
IO driver already spawns subprocesses for `io.exec`. The cost lands at the
**interface** (data-only/strict boundary; addressable transforms, not arbitrary
closures; coarse chunking) rather than the runtime — a far better trade for a data
tool, paid only when you parallelise. **Model B — shared-heap threads** (the
fine-grained, shared-thunk case A can't express) is a **hard fork, maybe-never**: it
needs a `Send`/`Sync` heap with a parallel collector (a superset of W10), an atomic
thunk-claim replacing blackholing, and GC-level spark pruning, all paying an
unpredictable single-threaded tax. Both models stay **advisory and deterministic**
(parallelism, never observable concurrency — relates W17). Model A depends on
**W6**; Model B on **W10**.

**Success.** A: >1.5× on a file batch and one large data-parallel `map` across 4+
cores, **zero** single-threaded cost, identical deterministic results, and an
interface a user reaches for without contortion. B: pursued only if A demonstrably
cannot serve an intra-program compute case **and** a heap prototype shows the
single-threaded tax is acceptable.

#### W22. Host-language & schema interop

**Problem.** Eucalypt cannot exchange schemas with the outside world — no JSON
Schema export of its types, no ingest of external schemas (JSON Schema, Kubernetes
CRDs) as eucalypt contracts.

**Design.** **JSON Schema export/import** as the first increment: emit a schema
from a unit's `type:` annotations (reusing W8's extraction), and **ingest** an
external schema as a W16 contract (an ingested schema *is* a contract). **CRD
import** specifically needs **optional fields (W15)** — Kubernetes schemas are
optional-property-heavy. Codegen is **delegated to existing JSON-Schema
toolchains** rather than built in-house. Builds on W16 (contracts), W8 (doc/schema
extraction) and W15 (presence). Post-1.0; the JSON-Schema core is the natural first
cut.

**Success.** A eucalypt unit's types export to JSON Schema; an external JSON Schema
(incl. a CRD with optional properties) imports as a contract that validates data at
ingress.

---

### Candidates not yet planned

Areas worth a proposal but not yet drafted — recorded so they are not forgotten,
and able to swap into the plan if priorities shift.

- **Alternative backends** — core→WASM compilation and other execution targets. The
  most developed candidate and the **consumer of W5's conformance contract** (a
  genuinely independent backend is what gives cross-backend conformance teeth);
  interacts with W21 and W22.
- **Debugging a lazy language** — a DAP server / value-provenance for stepping lazy
  evaluation.
- **Streaming & large-data scaling** — bounded-memory processing of inputs that
  don't fit the heap.
- **Distribution & packaging** — nixpkgs / Homebrew / a WASM CDN build.
- **Homoiconicity & macros** — compile-time metaprogramming over the core.
- **First-class type values** — types as ordinary values, beyond the `s"…"` surface.
- **Algebraic effect-rows** — typed effects beyond the IO monad.

---

## 8. The critical path

**W1 (versioning) + W2/W3 (cheap latency + the interface) → W6 (latency) → W10 (the
runtime) → W16 + W18 (data-correctness + ecosystem floor, on W12) → W5 green (prove
it) → ship 1.0.** Everything else is a cheap win that slots alongside, or a curated
post-1.0 bet.

## 9. Index

| # | Item | Release |
|---|------|:-------:|
| W1 | Versioning & stability discipline | 0.8 → 1.0 freeze |
| W2 | Eliminate the double STG compile | 0.8 |
| W3 | The Unit Interface (+ cross-import bracket fix) | 0.8 |
| W4 | Resilient parser front-end & error recovery | 0.8 (Ph1) / 0.9 (Ph2) |
| W5 | Conformance corpus, property tests, fuzzing | 0.8 begin → 1.0 bar |
| W6 | Compiled-unit & prelude caching (separate compilation) | 0.9 floor → post-1.0 full |
| W7 | Incremental, query-based core | 0.9 begin → post-1.0 full |
| W8 | `eu doc` — documentation & schema extraction | 0.9 |
| W9 | One demand annotation behind the strictness heuristics | 0.9 |
| W10 | Generational nursery & Immix completion | 0.10 |
| W11 | Strictness & demand analysis | 0.10 |
| W12 | Restore git imports (fetch-under-a-hash) | 0.10 |
| W13 | Persistent O(log n) blocks & the GC-finalisation fix | 0.11 |
| W14 | Generalise `vec` to hold arbitrary values | 0.11 |
| W15 | Optional (presence-annotated) record fields | 0.11 |
| W16 | Structural contracts & runtime validation | 0.12 |
| W17 | Hermetic mode for reproducible rendering | 0.12 |
| W18 | Module & package system (git-only, content-addressed) | 1.0 |
| W19 | Interactive surface — `eu watch` & REPL | 1.0 (Ph1) → post-1.0 |
| W20 | Type-directed compilation | post-1.0 (curated) |
| W21 | Parallel evaluation | post-1.0 (curated) |
| W22 | Host-language & schema interop | post-1.0 |

Settled decisions (§4) and non-goals (§5) are not scheduled work items.

