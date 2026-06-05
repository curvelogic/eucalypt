# 0004 — Compile latency: separate compilation, prelude embedding & (optional) caching

- **Status:** Draft proposal for review
- **Track:** B — performance, runtime & (compiler) architecture
- **Classification:** Stage-C-Fork (foundational; the prelude floor is Medium effort)
- **Suggested horizon:** prelude floor 0.9; general units / cache post-1.0
- **Related:** [0000](0000-priority-fixes.md) F1 (double compile), F2 (bracket bug), F3 (Unit Interface — the foundation); TS-B7 (shipped prelude *type* interface, `src/driver/check.rs`); [0014](0014-incremental-query-core.md) (incremental); [0005](0005-generational-gc.md) (GC, competes for deep-systems effort); [0018](0018-module-package-system.md) (content-addressing); the forthcoming alternative-backends proposal.

---

## Summary

Every `eu` invocation re-parses, re-cooks and re-compiles the entire prelude from
source — ~500–700 ms of fixed pipeline overhead that, for a CLI config tool run
repeatedly in CI and scripts, dwarfs the actual work. The obvious fix — "cache
the prelude's compiled form on disk" — does **not** work as the architecture
stands, because there *is* no per-unit compiled artifact: the prelude, imports
and CLI inputs are spliced into one monolith by `rebody` and compiled in a single
pass. The real fix is therefore **separate compilation**: compile the prelude
*independently* (against a unit interface), **embed the compiled prelude in the
binary**, and link user code against it — eliminating prelude compilation at
runtime with **no on-disk cache and no security surface**. This rests on the
**Unit Interface** refactor ([0000](0000-priority-fixes.md) F3) and pairs with the
**double-compile fix** ([0000](0000-priority-fixes.md) F1). An *optional* on-disk
cache of the whole compiled evaluand (for exact re-runs) and a daemon/server
remain possible, but are demoted, threat-modelled, and post-1.0 — they are not
the win.

> This proposal supersedes and absorbs the earlier "separate compilation & unit
> linking" proposal (formerly 0021); separate compilation is the substance here.

---

## Motivation

### Measured pipeline overhead

The compile pipeline is constant regardless of query complexity
(`docs/development/deep-find-performance-baseline.md`, release build, ~1 MB
fixtures):

| Phase | Time |
|-------|------|
| Parse | ~120 ms |
| Cook | ~190 ms |
| Eliminate | ~110 ms |
| STG compile | ~90 ms |
| **Total** | **~500–700 ms** |

A simple lookup on a 1.15 MB input runs the VM in ~1 ms — so ~500× of the
wall-clock is compile overhead carrying no information, paid on *every* run.

### Why "cache the prelude" doesn't work today — and what does

The prelude (`lib/prelude.eu`, embedded via `include_bytes!`,
`src/driver/resources.rs:16`) is fixed, self-contained, and the dominant cost
(parse + cook over ~2,200 lines). It *looks* like an ideal cache target — and
TS-B7 already caches its **types** (`PreludeSummary`, `src/driver/check.rs:44`).
But the *code* side cannot be cached the same way, because **the prelude has no
independent compiled form**: every input is spliced into one core expression by
`rebody` (`src/core/expr.rs:548`, `:1073`) *before* STG compilation, and
`stg::compile` runs once on the whole merged evaluand (`src/driver/eval.rs:187`).
So caching the prelude's `StgSyn` keyed on prelude bytes is caching something that
doesn't exist; the only thing cacheable as-is is the *whole merged evaluand* keyed
on the entire input set — a hit only on exact re-runs (§ optional cache).

The fix is to give the prelude an independent compiled form — i.e. **separate
compilation** — and then *embed* it. That is the core of this proposal.

### The double-compilation problem (F1)

For plain documents (the common case) `eu` compiles twice — headless to classify
IO-vs-document, then again with `RENDER_DOC` on a fresh machine
(`eval.rs:187`/`:282`), because rendering the headless result in place crashes the
GC on stale string pointers (`eval.rs:271-275`). The ~90 ms second compile is pure
overhead. The fix (make in-place render GC-safe → one compile) is tracked as
[0000](0000-priority-fixes.md) **F1**; it is orthogonal to and complements this
proposal (F1 removes the *second* compile; separate compilation removes the
*prelude* compile).

---

## Prior art & landscape

**Separate compilation (the core).** GHC compiles each module to an object file
plus an **interface (`.hi`)** carrying exports *and unfoldings* for cross-module
inlining — the lesson being that an interface must carry inline-able definitions
or you lose the inlining a monolith gets. ML **functors / 1ML / Backpack** compile
a module as a function of its imports' signatures (separate compilation falls
out). **Unison** identifies definitions by content hash, so linking/caching are
free (radical; ties to [0018](0018-module-package-system.md)). And eucalypt
already has **internal precedents**: TS-B7's `PreludeSummary` + `with_seed`
(`check.rs:197,382`) is a shipped *type* interface, and the desugarer's
`drain`/`seed_monad_namespace_registry` (`desugarer.rs:171`) is a shipped
*monad* interface — exactly the "build a unit interface, seed the dependent"
pattern, proven twice (see [0000](0000-priority-fixes.md) F3).

**Caching (for the optional on-disk part).** rustc's red-green incremental cache
and Bazel's action cache teach *content-addressed keying* (inputs hash + tool
version → outputs); Bloop teaches that a long-running daemon removes the
serialisation round-trip; Unison teaches that a content hash makes hits
*semantically* guaranteed.

**Security (for the threat model).** Python's `.pyc` lives *next to the source*
and is *validated against it* (timestamp, or PEP 552 source-hash) — co-located and
checked, not a central cache of executable blobs. A writable cache of executable
artifacts is an RCE vector; the lesson informs the threat model below.

---

## Proposed design

### 0. Foundation: the Unit Interface ([0000](0000-priority-fixes.md) F3)

Separate compilation needs each unit to expose its cross-unit contributions
(operators, monad registry, bracket modes, type schemes, exported bindings→slots)
as one **interface**. Unifying today's four scattered registries into that
interface is an independently-valuable cleanup tracked as F3; it is the
prerequisite this design builds on.

### 1. Separately compile and embed the prelude (the win)

Compile the prelude *independently* to a compiled form whose cross-unit references
are unresolved externals, and link user code against it. Four mechanisms, floor to
radical:

- **A. Stable-global-prefix (the prelude-only floor).** Keep `rebody` semantics
  but compile the prelude once to a fixed global-slot layout (its de Bruijn
  indices are already body-independent — it is the outermost `let`), and compile
  user bodies to reference those slots. The `Ref::G` global region is already
  position-stable (`syntax.rs:43-58`) — it just holds intrinsics today, not
  prelude bindings.
- **B. Symbolic externals + link** (GHC `.o`+`.hi`): emit named externals
  (today a free var is fatal at compile, `compiler.rs:1031`), link against unit
  interfaces. Carries inline-able defs (the GHC lesson). Generalises A to arbitrary
  units.
- **C. Units-as-functions** (ML functors / lambda-lift): a unit is `λimports.
  body`; composition is application. The most eucalypt-idiomatic (blocks are
  records), at the cost of import-record plumbing and cross-unit recursion.
- **D. Content-addressed definitions** (Unison): hash-identified definitions;
  linking/caching free. Radical; unifies with [0018](0018-module-package-system.md).

**The one genuine wall is operators.** Cook discovers operators from the merged
`let`-nest and mis-parses an out-of-scope operator (`fixity.rs:143`, `fill.rs:56`;
verified: `-Q` + `2 + 3` → "unresolved variable '+'"). There is **no seeding
mechanism today** — so cooking a unit independently of its dependencies' operators
is the load-bearing change. The *other* cross-unit inputs are already handled:
monad namespaces are drainable/seedable (`desugarer.rs:171`), and bracket
content-mode is per-file (modulo the F2 bug). User code uses ~55 prelude operators
pervasively, so this bites even the floor — but there it is bounded: the prelude's
operator set is fixed and **already extracted** on the type side (`check.rs:86`).

**Then embed.** With a separately-compiled prelude, **embed its compiled form in
the binary** — mirroring how `build-meta.yaml` is a generated-by-`eu` artifact
embedded via `include_bytes!` (`resources.rs`), with a build-time hash check that
it matches `lib/prelude.eu`. This delivers the latency win with **no on-disk
cache, no poisoning surface, and automatic versioning** (it *is* the binary's
prelude). The earlier "build-time embedding entangles the build" objection is
minor next to "removes the RCE surface."

**Verdict:** the prelude floor (A, with operator seeding) is **Medium effort,
feasible**; arbitrary-unit separate compilation (B/C/D) is **Hard**, gated on
giving cook a seeded operator interface, and is the post-1.0 fork.

### 2. Eliminate the double compile ([0000](0000-priority-fixes.md) F1)

Land F1 (GC-safe in-place render → one compile) alongside. Independent of the
embed, but together they remove both the prelude compile and the second compile.

### 3. Optional on-disk cache (demoted, threat-modelled)

After F1 + the embedded prelude, the *only* remaining cacheable thing is the
**whole merged evaluand** keyed on the full input set — a hit on **exact re-runs**
(e.g. CI running `eu config.eu` unchanged); it dedupes nothing and is the part
that carries a **security surface**. If pursued: serialise **`StgSyn`** (off-heap,
`Rc`-based, pointer-free; *not* `HeapSyn`, which is GC-heap-bound — `loader.rs`),
keyed `sha256(inputs) ∥ eu_version ∥ edition`, advisory (any miss/corruption →
clean recompile, never wrong output). This is a marginal, post-1.0 add — **only if
exact-re-run caching proves worth the surface.**

#### Threat model (for §3 and §4)

A cache of *compiled* units is **executable content**; loading one trusts whoever
could write it. **Content-hash keying guards staleness, not tampering** — if the
cache value under a key is attacker-controlled, you execute attacker bytes; the
only defences are filesystem permissions or authentication.

- **Assets:** the executable cache blobs; the cache dir; (server) the socket.
- **Trust boundary:** loading an entry trusts whoever can write the cache dir.
  *Stated assumption:* a user-private `0700` dir, and **the cache refuses to load
  from a group/world-writable directory.**
- **Threats:** poisoning → RCE; untrusted deserialisation (serde memory-safety +
  panics/DoS on malformed blobs + malicious-but-valid `StgSyn`); **CI cache
  restore** (a restored cache is untrusted input — the high-value attack);
  multi-user/network-home dirs; (server) unauthenticated local RPC = RCE-as-you,
  DoS, version skew.
- **Mitigations:** user-private permissioned dir (verify before trusting); advisory
  cache (recompile on any mismatch/perm-failure); **never trust a shared/restored
  cache without authentication**; *embedding the prelude (§1) avoids this entirely
  for the dominant cost.* Out of scope: an attacker who can already run as you.

### 4. Daemon/server (deferred)

A long-running process (Bloop-style; the LSP already holds pipeline state) would
remove even deserialisation, but it is a **local RCE-as-you service** if
unauthenticated. If ever pursued: **Unix-domain socket at `0600`** in a
user-private dir (not TCP localhost — any local user reaches it), client auth, a
version handshake, and resource limits. Deferred; see
[0014](0014-incremental-query-core.md) for the incremental vision.

---

## Interaction with the existing roadmap

- **[0000](0000-priority-fixes.md) F1/F3** are the prerequisites: F3 (Unit
  Interface) is the foundation; F1 (double compile) lands alongside. F2 (bracket
  bug) is fixed as a side-effect of F3.
- **TS-B7** is the shipped type-interface precedent and template.
- **[0014](0014-incremental-query-core.md):** unit interfaces are the natural
  incremental-cache unit; the embedded prelude is the base case of its graph.
- **[0005](0005-generational-gc.md):** competes for the same deep-systems effort
  (per `SEQUENCING.md` §4); once compile latency falls, GC cost becomes
  proportionally more visible.
- **[0018](0018-module-package-system.md) / alternative backends:** option D
  (content-addressing) and any independent backend consume exactly this
  interface + separately-compiled artifacts.

## Implementation sketch

**Prelude floor (A), Medium — the recommended target:**
1. **F3 Unit Interface** ([0000](0000-priority-fixes.md)) — the foundation.
2. **Global slots (B1):** a prelude block at fixed `Ref::G` indices
   (`compiler.rs`, `machine/mod.rs:55`, `vm.rs:79`).
3. **Operator seeding (the real work):** cook accepts a seeded operator
   environment (the prelude's fixed, already-extracted table, `check.rs:86`) — the
   one cross-unit input lacking a mechanism; monad seeding already exists.
4. **Deferred externals + link:** emit symbolic externals (`compiler.rs:1031`),
   resolve against the prelude interface (the type checker's seed path,
   `check.rs:269`, is the template).
5. **Embed** the compiled prelude (`resources.rs`) with a build-time source-hash
   check; land **F1** alongside.

**General units (B/C/D), Hard, post-1.0:** export each unit's operators+fixity in
its interface and seed a dependent's cook from its dependencies' — the one change
that unlocks arbitrary-unit separate compilation.

**Optional cache (§3), post-1.0:** `StgSyn` serde + `src/driver/cache.rs`, advisory,
threat-modelled — only if exact-re-run caching is wanted.

## Alternatives considered

- **On-disk cache of the prelude `StgSyn`** (this proposal's original framing):
  rejected as the primary path — there is no standalone prelude `StgSyn` to cache,
  and embedding (§1) is strictly better (no surface, auto-versioned).
- **Whole-evaluand on-disk cache as the headline:** rejected — exact-re-run only,
  carries the security surface; demoted to optional §3.
- **Daemon as the primary deliverable:** rejected — operational + security
  complexity; deferred §4.
- **Among separate-compilation mechanisms:** A is the floor (mechanism-agnostic,
  safe first step); B closest to the shipped precedents; C most idiomatic; D most
  powerful but largest.

## Risks & what would kill this

- **The operator wall doesn't yield cleanly (highest).** If a seeded operator
  interface entangles with the dynamic features, arbitrary-unit separate
  compilation stalls and we land on the prelude floor (the pre-authorised
  fallback). The floor still needs operator seeding — bounded, fixed, already
  extracted.
- **Lost cross-unit inlining (high).** The monolith inlines across the
  prelude/user boundary; separating units makes inline/DCE more conservative (a
  compiled prelude can't be DCE'd against an unknown user). Mitigation: carry
  inline-able defs in the interface (GHC unfoldings); the latency win should
  dominate, but it is the benchmark to watch.
- **Cache/server security (if §3/§4 pursued).** See the threat model; the
  embedded-prelude path avoids it for the dominant cost.
- **Complexity for a small project.** The floor is the proportionate bet; general
  separate compilation and the cache/server are justified only by demonstrated
  need (large multi-file projects, exact-re-run CI, alternative backends).

## Success criteria

- **The prelude is compiled zero times at runtime** for a normal run
  (embedded/linked), output byte-identical across the harness; cold-compile
  latency drops by the prelude's share — **with no on-disk executable cache.**
- User files link against the prelude interface with prelude **operators**
  resolving (the verified failure mode is gone).
- F1 lands: a plain-document run performs exactly one STG compile.
- *If* §3 ships: cache is advisory (recompile on any mismatch), refuses a
  world/group-writable dir, and is never trusted across a restore without
  authentication.

## References

**Compilation/composition:** `src/core/expr.rs:548,1073` (`rebody`);
`src/driver/prepare.rs:24`, `src/driver/eval.rs:187` (one-pass compile of the
merged evaluand), `:163-295`/`:271-275` (double compile + stale-string comment, F1);
`src/eval/stg/compiler.rs:1031` (free var fatal), `:452` (`Context::lookup`);
`src/eval/stg/syntax.rs:43-58` (`Ref::G`); `src/eval/machine/vm.rs:79`,
`mod.rs:55`; `src/eval/intrinsics.rs:45` (stable global region);
`src/core/cook/fixity.rs:143`, `shunt.rs:253`, `fill.rs:56` (operators);
`src/core/typecheck/check.rs:197,269,382,86` (`PreludeSummary`/seed — the shipped
type interface); `src/core/desugar/desugarer.rs:171` (monad seed/drain — the
shipped monad interface); `src/eval/memory/loader.rs` (StgSyn→HeapSyn, why HeapSyn
isn't cacheable); `src/driver/resources.rs:16` (embedded prelude; the
`build-meta.yaml` precedent); `src/driver/check.rs:44` (TS-B7 cache);
`docs/development/deep-find-performance-baseline.md` (timings).

**External:** [GHC separate compilation & `.hi`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/separate_compilation.html) ·
[Unison: the big idea](https://www.unison-lang.org/docs/the-big-idea/) ·
[1ML — first-class modules](https://dl.acm.org/doi/10.1145/2858949.2784738) ·
[Lambda lifting](https://en.wikipedia.org/wiki/Lambda_lifting) ·
[rustc incremental](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html) ·
[Bazel remote caching](https://bazel.build/remote/caching) ·
[Python `.pyc` / PEP 552 hash-based invalidation](https://peps.python.org/pep-0552/).
