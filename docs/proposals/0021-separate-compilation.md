# 0021 — Separate compilation & unit linking (beyond `rebody`)

- **Status:** Draft proposal for review
- **Track:** B — performance, runtime & (compiler) architecture
- **Classification:** Stage-C-Fork (foundational; the prelude floor is smaller)
- **Suggested horizon:** prelude floor 0.9–1.0; general units post-1.0
- **Related:** enables [0004](0004-compiled-unit-caching.md) (caching/embedding) and the embedded-compiled-prelude idea; sibling of [0014](0014-incremental-query-core.md) (incremental); mirrors TS-B7 (shipped prelude *type* interface); [0000](0000-priority-fixes.md) F1 (double compile); the radical option touches [0018](0018-module-package-system.md) (content-addressing) and the forthcoming alternative-backends work.

## Summary

Today every eucalypt program is compiled as one **monolith**: the prelude, all
imports, and all CLI inputs are spliced into a single core expression by
`rebody` *before* STG compilation, so the prelude is recompiled inside every
program and no unit has an independent compiled artifact. This blocks caching,
binary-embedding of the compiled prelude, and incremental/backend reuse. This
proposal evaluates **alternatives to `rebody` that enable separate compilation +
linking** — compiling a unit independently, leaving cross-unit free variables as
*unresolved externals*, and resolving (*linking*) them afterwards — and finds:
the model is already proven internally (the type checker does it), the
**prelude-only floor is a Medium-effort, high-value target**, and **arbitrary-unit
separate compilation is gated almost entirely on one change** — making cook
per-unit via a seeded operator interface — the one cross-unit input that lacks a
seeding mechanism today (monad namespaces already have one; brackets are per-file). The recommendation
is to take the prelude floor (it unlocks the embedded prelude + most of the
compile-latency win with no on-disk security surface) and treat general-unit
separate compilation as a deliberate, well-scoped post-1.0 fork.

## Motivation

The pipeline is strictly: parse → desugar (per unit) → **merge via `rebody`** →
cook → eliminate → inline ×2 → fuse → verify → **`stg::compile` on the single
merged evaluand** (`src/driver/prepare.rs:24`, `src/driver/eval.rs:187`). `rebody`
(`src/core/expr.rs:548`, `:1073`) splices each unit's `let`-nest into another as
its body; the prelude is prepended first, so it becomes the outermost `let` and
user code the innermost body, with ordinary lexical shadowing (innermost wins)
and each unit's *internal* references frozen as resolved de Bruijn vars.

The consequence is decisive: **there is no per-unit compiled artifact.** The
prelude (~228 declarations) is re-parsed, re-cooked, and re-compiled on every
invocation. This is the root cause behind 0004's latency target, and it makes the
two best fixes impossible as the architecture stands:

- **Embedding the compiled prelude in the binary** (the security-clean
  alternative to an on-disk cache) saves nothing if the prelude must still be
  `rebody`-merged with user code and the monolith recompiled.
- **Caching** can only cache the *whole merged* `StgSyn` keyed on the entire
  input set — a hit only on exact re-runs, with the prelude never deduped across
  programs. (0004's "cache the prelude" wording is optimistic: there is no
  standalone prelude `StgSyn` to cache.)

Separate compilation is the enabling change underneath all of it.

## Prior art & landscape

**The internal precedent — we already do this for types.** TS-B7's
`PreludeSummary` (`src/core/typecheck/check.rs:197`) caches the prelude's
*interface*: exported binding schemes, aliases, branch shapes, **and its
operator overloads** (`check.rs:223`). The seed path `type_check_path_with_seed`
(`check.rs:269`) checks a user file by merging it **with itself only** (not the
prelude), then resolves prelude references — left as `Var::Free` — **by name**
against the summary (`with_seed`, `check.rs:382`). That is precisely
"compile the dependency once to an interface; link the dependent's externals
against it," shipped and working for the type stage. The code stage would mirror
it.

**GHC — `.o` + `.hi`.** The canonical separate-compilation model: each module
compiles to an object file *and* an interface (`.hi`) describing its exports.
Crucially the interface carries **unfoldings** so the compiler can still
**inline across module boundaries**. The lesson for eucalypt — which inlines
heavily — is that a unit interface must carry inline-able definitions, or we lose
the cross-unit inlining the monolith currently gets (see Risks).

**Unison — content-addressed definitions.** Every definition is identified by a
hash of its dependency-closed AST; references are hashes; "linking", caching, and
incremental compilation fall out *for free*. The most powerful option and the one
that dovetails with [0018](0018-module-package-system.md); also the largest change
(code identity becomes a hash, names become metadata over hashes).

**ML functors / 1ML / Backpack; lambda-lifting.** A unit can be compiled as a
*function of its imports* (`λimports. body`) — a functor — so composition is
*application*, and a functor compiles against its dependencies' *signatures*, not
their implementations. Lambda-lifting is the mechanism: turn cross-unit free
variables into explicit parameters. This is the most *eucalypt-idiomatic* framing
(blocks are records, functions exist already), at the cost of import-record
plumbing and the known difficulty of mutual recursion across units.

## Proposed design — four options, one common wall

All four must reproduce `rebody`'s exact semantics: innermost-user-wins
shadowing, lexical freeze of each unit's internal references, metadata/target
composition, and laziness. They differ in the *linking mechanism*; they share one
obstacle.

### The wall: declaration-site metadata a dependent needs — and how much is already seedable

The general obstacle is a *class*: **metadata at a unit's declaration site that
changes how a *dependent* unit is processed**, which a dependency can supply
across an import. There are three candidates, but two are already handled —
leaving one real wall.

**Operators (cook) — the one genuine wall.** Cook (`src/core/cook/fixity.rs:143`)
discovers operators by walking the whole merged `let`-nest and resolves uses
against them (`shunt.rs:253`); an operator out of scope mis-parses to catenation
(`fill.rs:56`) or is fatal at STG compile where a surviving `Var::Free` errors
(`compiler.rs:1031`). **Verified:** with `+` out of scope, `2 + 3` fails
"unresolved variable '+'". There is **no seeding mechanism** today — cook learns
operators only from enclosing `let`s — so cooking a unit independently of its
dependencies' operators is the load-bearing change. User code uses ~55 prelude
operators pervasively, so this bites even the prelude floor.

**Monad namespaces (desugar) — cross-unit, but already seedable.** `{ :io … }`
desugars to a bind-chain only if the namespace is registered (`rowan_ast.rs:1348`);
the registry is populated from definition-site `monad:` metadata
(`rowan_ast.rs:2487`, the prelude's `io`/`for`/`let`/`state`/`random`). That *is*
cross-unit — but the desugarer already exposes **`drain_monad_namespace_registry`
/ `seed_monad_namespace_registry`** (`desugarer.rs:171-177`) to carry it between
per-file desugars. So this is not an open wall; it is a **shipped instance** of
exactly the pattern separate compilation needs — export a unit's registry, seed
the dependent. The prelude's monad interface is already drainable.

**Brackets (parse + desugar) — per-file, not cross-unit.** The list-vs-block
content style is a **per-file** `BracketRegistry` built by a pre-scan of *the same
file's* tokens (`prescan_bracket_declarations`, `parse.rs:45`) and consulted at
parse (`parse.rs:516`). A block-mode bracket pair must therefore be declared in
the same file as its uses, and its monad spec is registered and resolved within
that file. So brackets are self-contained per unit — they need **nothing** in a
unit interface. A plain bracket *use* is just `App(Var::Free(pair_name), inner)`
(`rowan_ast.rs:1468`) — an ordinary link-resolved reference. (That per-file
behaviour is itself a bug — [0000](0000-priority-fixes.md) F2 — whose recommended
fix defers the block/soup decision to the seedable desugar stage, keeping brackets
out of this wall.)

**Net:** the genuine remaining wall is **operators**. Monad namespaces are
already seedable and brackets are per-file, so the "export-an-interface,
seed-the-dependent" pattern is proven **twice** in the codebase already (the monad
registry here, and TS-B7 for types). The work narrows to giving *operators* the
same treatment — which both shrinks the problem and de-risks it.

### A. Stable-global-prefix (the prelude-only floor)

Keep `rebody` semantics; compile the prelude **once** to a fixed layout and
compile user bodies to reference it. Concretely: extend the already
position-stable global region (`Ref::G`, `syntax.rs:43`, non-bumping `:53`,
resolved at `vm.rs:79`) — today exclusively the ~190 intrinsics
(`intrinsics.rs:45`) — to include a **prelude global block** at fixed slots;
emit prelude references in user code as those globals; **seed cook with the
prelude's fixed operator table**; link. *Semantic equivalence:* the global-slot
scheme must reproduce the shadow split (a user binding shadowing a prelude name
binds *user* call-sites to the user slot while the prelude's *own* uses stay
bound to the prelude slot) — exactly what frozen de Bruijn vars give today.
Smallest change; prelude only; the conservative floor you pre-authorised.

### B. Symbolic externals + link (the GHC object-file model — your idea)

Generalise A: compile each unit independently, emitting cross-unit references as
**named externals** (a real change — `Var::Free` is currently fatal at compile,
`compiler.rs:1031`), and a **link** pass resolves them against each unit's
**interface** (exported name → slot, exported operators + fixity, optionally
schemes — mirroring `PreludeSummary`). Carries inline-able definitions in the
interface to preserve cross-unit inlining (the GHC lesson). Enables arbitrary
units — *if* the operator wall is solved (per-unit cook).

### C. Units-as-functions (functor / lambda-lift)

Compile a unit to `λimports. body`; composition is application. The most
idiomatic mechanism (eucalypt blocks are records; this is "a unit is a function
of an import record"), and semantically clean (pure λ). Still needs the operator
interface (you must cook the body knowing the imports' operators), and adds
import-record plumbing (overhead unless specialised — where the type system/HKT
could help) plus the mutual-recursion-across-units difficulty.

### D. Content-addressed definitions (Unison)

Identify each binding by a hash of its dependency-closed core; references are
hashes; linking/caching/incremental are free. The radical, highest-payoff option,
naturally unifying this proposal with [0018](0018-module-package-system.md)'s
import/integrity story and the alternative-backends work. Still needs a standalone
cook story (you can't hash a definition you can't resolve), so it sits *on top of*
solving the operator wall, not instead of it. Biggest change; long-horizon.

## Interaction with the existing roadmap

- **[0004](0004-compiled-unit-caching.md):** this proposal is its missing
  foundation. With separate compilation, the prelude can be compiled once and
  **embedded in the binary** (no on-disk cache, no poisoning surface — see 0004's
  threat-model discussion); without it, only a low-value whole-evaluand cache is
  possible. 0004 should be reframed around this.
- **[0000](0000-priority-fixes.md) F1** (in-place render) is orthogonal and
  complementary: F1 removes the *second* compile; this removes the *prelude*
  compile.
- **TS-B7** is the working precedent and the template (`check.rs:269`).
- **[0014](0014-incremental-query-core.md):** unit interfaces are the natural
  query-cache unit; the two reinforce each other.
- **[0018](0018-module-package-system.md) / Unison (option D)** and the
  **alternative-backends** work both want content-addressed, separately-compiled
  units — a shared long-term target.

## Implementation sketch

This builds on the **Unit Interface** unification tracked as
[0000](0000-priority-fixes.md) F3 — an independently valuable cleanup (it removes
the operator-table redundancy and fixes F2) that should land first; separate
compilation is then "build the interface, seed dependents, link" on top.

**Prelude floor (A/restricted-B), Medium:**
1. **Global slots (B1).** Extend the `Ref::G` region with a prelude block at
   fixed indices (`compiler.rs`, `machine/mod.rs:55`, `vm.rs:79`).
2. **Deferred externals (B3).** Let the compiler emit a symbolic external for
   prelude refs instead of erroring (`compiler.rs:1031`); add a link step.
3. **Operator seeding (B2 — the real work).** Teach cook to accept a *seeded
   operator environment* (the prelude's fixed table, already extracted on the type
   side at `check.rs:86`) — the one cross-unit input without a mechanism today.
   Monad-namespace seeding already exists
   (`drain`/`seed_monad_namespace_registry`, `desugarer.rs:171`) and brackets are
   per-file, so neither needs new work.
4. **Compile-then-link driver path (B4).** A parallel loader path that compiles
   the user unit against the cached prelude interface and links — the type
   checker's seed path (`check.rs:269`) is the structural template.
5. Then **embed** the compiled prelude (`resources.rs`, mirroring the generated
   `build-meta.yaml`) with a build-time source-hash check.

**General units (B/C/D), Hard — the gating change:**
6. **Per-unit cook via a seeded operator interface (C1).** Export each unit's
   operators+fixity and seed a dependent's cook from its dependencies' interfaces
   rather than the whole merged tree. (Monad namespaces are already
   drainable/seedable, `desugarer.rs:171`; brackets are per-file.) This is the one
   change that unlocks arbitrary-unit separate compilation; everything else
   (interfaces, externals, link) has working analogues.

## Alternatives considered

- **Do nothing / whole-evaluand cache only** (0004 as written): no separate
  compilation; caches the prelude-merged-with-user monolith; hits only on exact
  re-runs; prelude never deduped. Low value; rejected as the *primary* path.
- **Among the mechanisms:** B (symbolic + link) is closest to the shipped type
  precedent and the smallest conceptual leap; C (functors) is the most idiomatic
  but adds plumbing/recursion concerns; D (content-addressing) is the most
  powerful but the largest change. The *floor* (A) is mechanism-agnostic and is
  the safe first step regardless of which general mechanism is later chosen.

## Risks & what would kill this

- **The operator wall doesn't yield cleanly (highest).** If giving cook a seeded
  operator interface (the one cross-unit input lacking a mechanism today)
  entangles with the dynamic features, *arbitrary* separate compilation stalls —
  and we land on the prelude floor (the pre-authorised fallback). The floor still
  needs operator *seeding*, bounded because the prelude's set is fixed and already
  extracted; monad-namespace seeding is already shipped.
- **Lost cross-unit inlining (high).** Today's whole-program inline/DCE
  (`prepare.rs`) crosses the prelude/user boundary; separating units makes it more
  conservative (a compiled prelude can't be DCE'd against an unknown user — it
  keeps more bindings, as the type cache already does). Mitigation: carry
  inline-able definitions in the interface (GHC unfoldings). Net effect must be
  measured: the latency win from skipping prelude compilation should dominate any
  inlining regression, but this is the key benchmark.
- **Semantic divergence in shadowing/metadata (medium).** A slot/name-based link
  must exactly reproduce frozen-reference shadowing and re-attach per-binding
  metadata (`export:`/`doc:`/`type:`) the interface must carry; operator `type:`
  is destroyed by cook today (`check.rs:377`), so the interface must capture it
  pre-cook (the type side already does).
- **Complexity for a small project (medium).** General-unit separate compilation
  is a real subsystem. The floor is the proportionate first bet; the general
  version should be justified by demonstrated need (large multi-file projects,
  alternative backends, content-addressed imports).

## Success criteria

- **Floor:** the prelude is compiled **zero times at runtime** for a normal run
  (embedded/linked), with byte-identical output to today across the harness;
  cold-compile latency drops by the prelude's share; no on-disk executable cache.
- A user file links against the prelude interface with prelude **operators**
  resolving correctly (the verified failure mode is gone).
- **General (if pursued):** an arbitrary imported unit compiles independently to
  an interface + artifact and links, with operator-defining imports working, and
  a measured inlining/perf delta within budget.

## References

**Eucalypt — composition & compilation:** `src/core/expr.rs:548` (`rebody_int`),
`:1073` (`merge_in`), `:1306` (`bind_free_vars`); `src/driver/prepare.rs:24`
(pipeline), `src/driver/source.rs:451` (cook-after-merge), `:502` (`merge_units`);
`src/driver/eval.rs:187` (compile whole evaluand). **Globals/symbols:**
`src/eval/stg/syntax.rs:43-58` (`Ref::G`, non-bumping), `src/eval/stg/compiler.rs:452`
(`Context::lookup`), `:914/:1225/:1282` (`Ref::G` only for intrinsics), `:1031`
(free var fatal); `src/eval/machine/vm.rs:79`, `mod.rs:55`; `src/eval/intrinsics.rs:45`
(stable global region); `src/eval/memory/symbol.rs`, `loader.rs:90`.
**Cook/operators:** `src/core/cook/fixity.rs:143` (`distribute`), `:22`
(operator `type:` extraction); `src/core/cook/shunt.rs:253`; `src/core/cook/fill.rs:56`.
**The internal precedent:** `src/core/typecheck/check.rs:197` (`PreludeSummary`,
incl. `operator_overloads:223`), `:269`/`:382` (seed path / `with_seed`),
`:86` (prelude operator extraction). **Spec/proposals:**
`docs/development/unit-visibility-spec.md`; `0004`, `0014`, `0018`, `0000`.

**External:** [GHC separate compilation & `.hi` / cross-module inlining](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/separate_compilation.html) ·
[Unison content-addressed code](https://www.unison-lang.org/docs/the-big-idea/) ·
[1ML — first-class modules (Rossberg)](https://dl.acm.org/doi/10.1145/2858949.2784738) ·
[Dreyer, *Understanding and Evolving the ML Module System* (MixML/Backpack lineage)](https://people.mpi-sws.org/~dreyer/thesis/main.pdf) ·
[Lambda lifting](https://en.wikipedia.org/wiki/Lambda_lifting).
