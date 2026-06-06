# 0010 — Hermetic mode for reproducible rendering (H6c)

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Extends-Roadmap
- **Suggested horizon:** 1.0
- **Related:** H6c (type-system-evolution.md §1), sibling proposals
  [0003](0003-conformance-testing-fuzzing.md) (conformance testing),
  [0009](0009-structural-contracts-validation.md) (structural contracts),
  [0018](0018-module-package-system.md) (hermetic imports),
  [0019](0019-host-language-interop.md) (host-language interop)

---

## Summary

Eucalypt's core evaluation is pure and lazy, but four capabilities in its
standard prelude — randomness, time, environment-variable reads, and
`io.shell`/`io.exec` — can make a render non-deterministic without any
static indication that this is happening. For a tool positioned primarily as
a configuration and data generator for CI/CD pipelines, this is a strategic
gap: a render that silently varies between invocations undermines caching,
diffability, and supply-chain trust. H6c in the type-system evolution
document proposes tracking these capabilities as opaque type constructors —
`Random(T)`, `Time(T)`, `Env(T)`, `IO(T)` — mirroring the `IO(T)` monad.
**This proposal declines that route.** It argues for a cheap **hermetic
mode** (`--require-deterministic`) that intercepts the unrepeatable startup
data at its source, and records the capability-**type** layer as
**won't-do**: under eucalypt's gradual, erased typing the types could only
ever be advisory — never a hermeticity *guarantee* — and the guard already
delivers that value without the annotation burden. The rationale is below
(*Capability types — won't-do*).

---

## Motivation

### Reproducibility as a core property for a config tool

A eucalypt render is, conceptually, a function from inputs (`.eu` source
files, YAML/JSON imports, command-line arguments) to output (YAML, JSON,
TOML). If it is a *pure* function of those inputs, then:

- The output is **cacheable**: given the same inputs, any cache keyed on
  them can serve a stored result without re-running `eu`.
- The output is **diffable**: "what changed between this commit and the
  last?" becomes a well-defined question with a deterministic answer.
- CI pipelines can **trust the output**: the same source at the same commit
  hash always generates the same Kubernetes manifest, Terraform plan, or
  Helm values file.
- Supply-chain auditing is coherent: a hash of the output can be reproduced
  from a hash of the inputs and the `eu` binary version.

All four of these properties collapse the moment a render consults the
system clock, reads environment variables that are not declared inputs, or
executes a shell command whose output is not content-addressed.

### The capabilities that break it today

Eucalypt currently provides four sources of external state that can silently
make a render non-deterministic.

**1. Time (`io.epoch-time`, `io.TZ`)**

`io.epoch-time` is injected at startup as a number of seconds since the
Unix epoch (`src/driver/io.rs:31–37`). It is a plain `number` in the
`__io` pseudo-block, indistinguishable at the type level from any other
`number`. A template that writes `generated-at: {io.epoch-time}` will
produce different output on every run.

**2. Randomness (`io.random`, `random.*`)**

`io.RANDOM_SEED` is seeded from `SystemTime::now().duration_since(UNIX_EPOCH)`
as nanoseconds unless `--seed` is passed explicitly (`src/driver/io.rs:66–69`).
`io.random` is then `__STREAM_NEW(io.RANDOM_SEED)` (`lib/prelude.eu:56`).
Random streams are threaded explicitly via the `Native::Prng` opaque state
word (`src/eval/stg/stream_prng.rs`), which is already an opaque value.
The problem is not the stream itself but the seed: unless `--seed N` is
given, the seed varies per run.

**3. Environment variables (`io.env`)**

`io.env` is the block of `std::env::vars()` captured at launch
(`src/driver/io.rs:20–22`). A render that reads `io.env.'DATABASE_URL'`
is parametric on the environment of the machine that ran `eu`. This is
intentional for many use cases but must be declared as such.

**4. Shell and process execution (`io.shell`, `io.exec`)**

`io.shell`/`io.exec` via `IoAction` (`src/eval/stg/io.rs`) return
`IO({..})`. The `IO(T)` wrapper already captures this *as an effect* —
but only at the monad level, not at the *determinism* level. A render that
calls `io.shell("date")` and embeds the result is non-deterministic even if
the rest of the template is pure.

None of these four is flagged in the type system. There is no way to
statically check "this render is hermetic": a reviewer reading a eucalypt
template cannot know without running it.

---

## Prior art & landscape

### Starlark / Bazel: hermeticity by construction

Starlark — the configuration language embedded in Bazel — achieves
hermeticity by *removing* the offending capabilities entirely. Starlark
programs cannot read files, access the network, query the system clock, or
produce random numbers. Determinism is enforced by restriction, not by
typing. The benefit is absolute predictability; the cost is expressiveness:
anything requiring external state must be declared explicitly as a Bazel
rule dependency, not computed inline.

Eucalypt's tool-first positioning intentionally offers more expressiveness
than Starlark: `io.shell` is a legitimate feature for configuration
generators that embed the output of `kubectl` or `vault`. Starlark's
approach of removing capabilities is therefore not directly adoptable, but
the goal — that the *caller* can know statically whether the output is
reproducible — is worth emulating.

### Dhall: totality and integrity without capability typing

Dhall guarantees totality (all expressions terminate), purity (no side
effects), and import integrity via SHA-256-locked imports. A Dhall file
cannot call the system clock or generate random numbers at all; its
non-determinism surface is restricted to remote imports (which can be
integrity-locked). Dhall's safety guarantees are enforced by restriction,
like Starlark, rather than capability tracking. Eucalypt cannot adopt
totality (lazy evaluation and `io.shell` are deliberately in scope), but
Dhall's integrity-locked imports are a direct precedent for
[0018](0018-module-package-system.md), and the general principle — that
*verifiable reproducibility* is a first-class language property, not an
afterthought — applies directly.

### Capability-based security: E, Pony, and object capabilities

The object-capability model (Miller, 1997; the E language) establishes that
authority to perform an action should be mediated by an *unforgeable token*
that a piece of code must receive explicitly, rather than being ambient.
Pony extends this to type-level reference capabilities. The key insight for
eucalypt is that `IO(T)` is *already* an object capability in this sense:
you cannot perform IO without the `io.*` monad namespace, which is the
token. The proposal here extends this pattern to finer-grained capabilities.

A recent strand of PLT work (Lucassen & Gifford 1988; Wadler & Thiemann
1998; "Designing with Static Capabilities and Effects", Brachthäuser et al.
2020) formalises the relationship between capabilities as values and effects
as types: possessing a capability value is what makes the corresponding
effect type available. This is exactly the route taken here: `io.random`
is already "a capability you must thread" (H6c's own phrasing,
`type-system-evolution.md:466`); the proposal is to make the *type* reflect
that threading.

### Koka-style algebraic effect rows: why not

Koka annotates every function arrow with an effect row: `int -> int {time,
random}`. This is precise and compositional, and row polymorphism means
effect rows propagate through generic functions without annotation burden.
H6d considers this route and rejects it for eucalypt (`type-system-evolution.md:469–477`): algebraic effect rows require a pervasive change to every function
type (every arrow gains an effect annotation), a richer kind system (effect
kinds), and a non-trivial row-unification mechanism on top of the existing
checker. For a data-transformation language where the vast majority of
functions are pure, the annotation noise would be high and the expressiveness
benefit low. More importantly, it conflicts with eucalypt's syntactic
conservatism: there is no surface to attach effect annotations to function
arrows without new syntax.

The capability-constructor route — `Random(T)`, `Time(T)`, `Env(T)` as
opaque wrappers, threaded like `IO(T)` — is strictly narrower than effect
rows and consistent with the existing idiom. But it too is **declined**
here (see *Capability types — won't-do*): being advisory and erased it
gives no guarantee, and the guard covers the same ground. It is recorded for
completeness, not adopted.

### The "opaque > transparent" principle

The cross-cutting theme in `type-system-evolution.md §3` is that complex
abstractions should be opaque: the user sees the interface type, not the
encoding. `IO`, `Lens`, `Traversal`, and `Partial` (H6b) follow this
principle, and capability constructors *would* have been the next natural
member of the family — which is why H6c is tempting. The reason this
proposal still declines them (*Capability types — won't-do*) is that the
members above all earn their place as *interfaces you compute with*, whereas
capability brands would only ever be passive, advisory labels.

---

## Proposed design

### Hermetic mode: intercept the unrepeatable startup data

The four unrepeatable sources are all created in **one place** — the `__io`
pseudo-block assembled at launch by `create_io_pseudoblock`
(`src/driver/io.rs:82-88`): `EPOCHTIME` (the wall clock), `RANDOM_SEED`
(system entropy unless `--seed` is given), `ENV`, and `TZ`. They are
*startup data*, not values fetched on demand. So the guard does not need to
*find* their uses after the fact — it intercepts them **at the source**:
under `--require-deterministic`, `create_io_pseudoblock` injects a **poison
thunk** for each unrepeatable field instead of the live value. A poison
thunk renders nothing; it raises an `ExecutionError` the moment it is
**forced**.

```
eu --require-deterministic render.eu
# error: render.eu forced io.epoch-time under --require-deterministic;
#        its output would not be reproducible
```

This is strictly better than a static lint over the inlined tree:

- **No marker-survival problem.** The guard owns the single chokepoint; it
  never has to recognise `__io.EPOCHTIME` after inlining has folded block
  lookups into bare literals. (That fragility is what sank the lint
  formulation — see *Alternatives*.)
- **Laziness flags exactly the output-relevant uses.** A poison thunk fires
  iff it is forced, and a value is forced iff the computation demands it —
  which, for a render, means *it flows into the output*. So the guard
  catches precisely the capability uses that affect output and ignores dead
  or unforced references, which do not affect determinism anyway — *more*
  precise than a reachability lint's over-approximation.
- **Seed-awareness for free.** The chokepoint already knows whether `--seed`
  was explicit (`random_seed(explicit_seed)`, `io.rs:65-73`): it poisons
  `RANDOM_SEED` only when there is **no** explicit seed, and injects the
  seed normally otherwise, because seeded randomness *is* reproducible. A
  post-hoc lint, blind to the runtime flag, could not draw that line.
- **It is a guarantee, not advice.** A render that *completes* under the
  flag provably did not depend, in its output, on unrepeatable startup data
  — any such dependency would have forced a poison thunk and aborted. The
  guarantee is scoped to the render that ran, which is the right scope: you
  render anyway, and the render forces everything its output needs.

**Poison, never substitute.** The injected thunk must *raise*, not stand in
a plausible default (`epoch-time → 0`): a silent default would emit
deterministic-but-wrong output and hide the dependency we are policing.

**On-demand IO is already gated.** `io.shell`/`io.exec` are not in `__io` —
they are `IoAction`s on the IO monad, behind the existing `--allow-io` gate
(off by default, `options.rs:193`). A hermetic render therefore has IO off
already; `--require-deterministic` covers the startup data, `--allow-io`
covers on-demand effects. Clean division, no overlap.

**Granularity is per-boundary, not per-use.** Rather than scattering
opt-out annotations at use sites, you *declare your non-hermetic inputs at
the boundary* — e.g. an env allowlist that injects only the named variables
and poisons the rest. That is the Starlark/Bazel "declare your inputs"
discipline, and it reads better than per-use ignores. The flag is
per-invocation (the whole render is hermetic or not); per-target checks are
just separate invocations.

### What is *not* a capability: `__build`

Eucalypt prepends a second startup block, `__build`
(`src/wasm_pipeline.rs:194-210`), structurally a sibling of `__io` — but a
**different determinism class**, and the guard must leave it alone.
`__build` is **embedded at compile time** from `build-meta.yaml` baked into
the binary: `version`, `commit`, `url`, `banner` (`build.eu:64-72`),
concrete strings fixed at build, with no run-time clock. It is therefore
**identical across every run of a given binary** — reproducible-*given*-the-
binary. Embedding `build.version` or `build.commit` is deterministic, and is
in fact the canonical member of the reproducibility contract this proposal
already states — *output reproduced from inputs **and the eu binary
version***. Pinning the binary pins `__build`.

(A separate axis — *reproducible builds of `eu` itself*, rebuilding from
source to a byte-identical `build-meta` — is out of scope here; it turns on
whether the build pipeline stamps a number/timestamp into `eu-build:`, not
on render determinism. A stricter "output independent of the tool version"
mode could poison `__build` too, but that contradicts the default contract,
so it stays a noted option, not the default.)

### Capability types (the fuller H6c route) — won't-do

H6c's fuller proposal is to add opaque constructors `Random(T)`, `Time(T)`,
`Env(T)` (mirroring `IO(T)`), so a function that touches the clock carries
`Time(...)` in its return type and the taint propagates to call sites. This
proposal **declines** that layer. The reasoning:

- **It cannot give the guarantee that would justify it.** The valuable
  property is *"the checker proves this render is hermetic."* A proof needs
  **soundness** — no `Time(number)` may silently become a plain `number`.
  Eucalypt's gradual typing is deliberately unsound at the boundary
  (`any → T` trusted and erased; [0002]). So capability types could only
  ever be *advisory* — documentation with a checker's blessing, not a
  guarantee — the same degradation `IO(T)` already lives with.
- **The guard already operationalises the same information**, more cheaply:
  it distinguishes the four sources, propagates by reachability from a
  target, and supports binding-level scope via `deterministic:` metadata —
  with no annotation burden and no prelude annotations to keep correct.
- **The one thing types add — capability info in a *published signature***
  — only matters across an **opaque boundary**, where a whole-program check
  cannot see the body (a precompiled module, [0018]; a generated host
  binding, [0019]). Even there it is advisory, and a **capability manifest
  on module exports** (`capabilities: [:time, :env]`, lint-checked at the
  import boundary) would carry the same information without a type-system
  extension. So the only niche that could justify capability types is both
  contingent on [0018]/[0019] *and* arguably better served by metadata than
  by types.

Net: capability types would be annotation burden for advisory documentation
the guard already provides. Should a hard, *enforced* hermeticity guarantee
ever be wanted, that is a different and larger project (sound effect
tracking, which H6d rejects for eucalypt) — not these opaque brands.

### Determinism as a testable property (link to [0003])

A key benefit of formalising this is that **render determinism becomes a
property the conformance suite can test** (see
[0003](0003-conformance-testing-fuzzing.md)). The property is:

> A target annotated `T` (not `Random(T)`, `Time(T)`, `Env(T)`, or `IO(T)`)
> rendered twice with the same input files and the same `--seed` produces
> byte-for-byte identical output.

This is already testable today (the proptest "render determinism" property
in [0003](0003-conformance-testing-fuzzing.md) covers it), but hermetic
mode sharpens it into a *guarantee*: a render that completes under
`--require-deterministic` cannot depend, in its output, on unrepeatable
startup data — so a non-deterministic render that completes under the flag
is a bug in the guard.

### Relation to [0018] hermetic imports and [0019] host-language interop

[0018](0018-module-package-system.md) proposes content-addressed, SHA-256-
integrity-locked module imports — Dhall's integrity mechanism applied to
eucalypt packages. This proposal is the *runtime-value* complement: even
with hermetic imports, a render can be non-deterministic if it calls
`io.epoch-time`. The two proposals together cover the import graph (0018)
and the capability graph (0010), and a CI pipeline that enforces both can
make a strong "this output is reproducible from these inputs" claim.

[0019](0019-host-language-interop.md) covers code generation and schema
interop. If cross-boundary capability information is ever wanted there (a
generated binding advertising that it reads the clock), the right carrier is
a capability *manifest* on the binding — lint-checked at the boundary — not a
type brand; see *Capability types — won't-do*.

---

## Interaction with the existing roadmap

This proposal engages H6c's *goal* (reproducibility) but **declines H6c's
type mechanism**, consistent with H6d's rejection of algebraic effect rows
— eucalypt tracks the determinism capabilities with a runtime guard, not the type
system. H6c placed the capability-type work in Stage C ("radical options,
24+ months", §2, "gather data before committing",
`type-system-evolution.md:1308–1310`); this proposal resolves that fork as
**won't-do** for the types and **1.0** for the guard.

The guard has no dependency on Stage A or B type work — it intercepts the
`__io` pseudo-block at construction (`src/driver/io.rs`), not anything in
the checker. It could be implemented between now and 1.0. There is no
interaction with H8 (MLsub) or the rest of the checker: it never touches
types.

---

## Implementation sketch

### The hermetic-mode guard (1.0)

- **`src/driver/io.rs`** — when `--require-deterministic` is set,
  `create_io_pseudoblock` injects a **poison thunk** (a core expression that
  raises an `ExecutionError` on force) for `EPOCHTIME`, `TZ`, and `ENV`, and
  for `RANDOM_SEED` **only when no explicit `--seed`** is present; otherwise
  it injects the live values as today.
- **`src/driver/options.rs`** — add the `--require-deterministic` / `-D`
  flag (alongside the existing `--allow-io` / `-I` at `options.rs:193`);
  on-demand IO (`io.shell`/`io.exec`) is already blocked by `--allow-io`
  being off.
- **Error quality** — the poison thunk names the capability and says the
  output would not be reproducible (and, for random, that `--seed` makes it
  deterministic). Reuse the `src/eval/error.rs` diagnostic path.
- **Size:** ~50–100 lines in `io.rs` plus the flag. Low risk; no new pass,
  no type-checker or inliner involvement.

### Capability types

Not pursued — see *Capability types (the fuller H6c route) — won't-do*. No
type-checker, `parse.rs`, `types.rs`, or prelude-annotation work is part of
this proposal.

---

## Alternatives considered

**Remove the capabilities entirely (Starlark approach).** Removing
`io.shell`, `io.epoch-time`, and `io.env` *unconditionally* would make
eucalypt deterministic by construction but would break features users rely
on — not viable. The adopted design is the *conditional* form of exactly
this: under `--require-deterministic` the unrepeatable startup data is
withheld (poisoned), and present otherwise. Starlark-when-you-ask-for-it.

**Koka-style algebraic effect rows (H6d).** Considered and rejected in the
evolution document. Full effect rows require annotating every function arrow,
a new kind system, and pervasive checker changes. The cost/benefit ratio is
poor for a data-transformation language.

**A runtime `--check-deterministic` flag that re-runs and compares.**
Running `eu` twice and diffing is a blunt instrument: it catches
non-determinism only when it *happens* to manifest between the two runs
(clocks incrementing, seed varying). The interception guard is strictly
more reliable — it catches the dependency directly, in a single run, the
moment a poisoned value is forced, rather than waiting for chance to expose
it.

**Tagging the `__io` pseudo-block, then scanning for the tags.** A static
cousin of the adopted design: tag each field at injection (e.g. `{:time
1234567890}`) and have a pass scan for the tags. The adopted interception
goes further with less — it withholds the value entirely (poison), so
*forcing* does the detection and no scan-and-survive pass is needed. Tagging
would only be worth revisiting if a *static*, without-rendering pre-check
were ever wanted; the guard already covers the property at render time.

---

## Risks & what would kill this

**The guard errors on benign capability use.** A render that reads
`io.env.'CI'` to choose a log level is technically non-deterministic but in
practice stable across CI runs; under the flag it would abort. Mitigation:
*declare it as an input at the boundary* — an env allowlist that injects the
named variables and poisons the rest — rather than blanket-poisoning `ENV`.
This keeps the "environment-parametric vs non-reproducible" distinction
explicit and at the invocation, not scattered at use sites.

**Low demand.** If eucalypt users primarily run `eu` for one-off renders and
do not use CI-cached outputs, the reproducibility problem is not felt. The
guard is a low-cost probe for exactly that: if nobody ever sets
`--require-deterministic`, the determinism story needs no further investment.

---

## Success criteria

1. **Hermetic mode shipped by 1.0:** under `--require-deterministic` a
   render aborts iff its output depends on unrepeatable startup data
   (`epoch-time`, unseeded `random`, `env`, `TZ`); the existing harness
   tests still pass unchanged under the flag.
2. **Conformance property added (0003):** The property "render with
   `--require-deterministic` and fixed `--seed` produces identical output
   on repeated runs" is added to the property test suite and passes.
3. **Demand evidence:** At least one reported real-world use case (CI
   caching, output diff, supply-chain audit) where the guard caught a
   non-determinism bug — confirming the determinism story earns its keep.

---

## References

**Eucalypt files cited:**

- `src/driver/io.rs:31–37` — `epoch_time()` reads `SystemTime::now()`
- `src/driver/io.rs:66–69` — `random_seed()` from `SystemTime::now()` or `--seed`
- `src/driver/io.rs:82–88` — `create_io_pseudoblock` composes `ENV`, `EPOCHTIME`, `TZ`, `RANDOM_SEED`
- `src/eval/stg/io.rs:25–93` — `IoReturn`, `IoBind`, `IoAction`, `IoFail` STG data constructors
- `src/eval/stg/stream_prng.rs` — `StreamNew`, `StreamFloat`, `StreamInt`, `StreamSplit` PRNG intrinsics operating on `Native::Prng(u64)`
- `src/eval/stg/time.rs` — `Zdt`, `ZdtFromEpoch`, `ZdtParse8601` time intrinsics
- `lib/prelude.eu:40–91` — `io` monad block: `env`, `epoch-time`, `RANDOM_SEED`, `random`, `shell`, `exec`
- `lib/prelude.eu:56` — `io.random: __STREAM_NEW(io.RANDOM_SEED)`
- `src/driver/options.rs:193–194` — `--allow-io` / `-I` flag (parallel to proposed `--require-deterministic`)
- `src/core/typecheck/parse.rs` — opaque constructor parsing (existing `IO`, `Lens`, `Traversal`)
- `docs/development/type-system-evolution.md:§1 H6c` — original H6c sketch, recommendation "gather data before committing"
- `docs/development/type-system-evolution.md:§3` — "opaque > transparent" cross-cutting principle

**Papers and external references:**

- Leijen, D. (2005). "Koka: Programming with Row-polymorphic Effect Types."
  *TLDI 2014 / arxiv:1406.2061.* — effect rows as the alternative route
  (H6d) rejected here.
- Miller, M. (2006). *Robust Composition: Towards a Unified Approach to
  Access Control and Concurrency Control.* PhD thesis, Johns Hopkins. — E
  language and object-capability model.
- Brachthäuser, J., Schuster, P., and Ostermann, K. (2020). "Designing with
  Static Capabilities and Effects: Use, Mention, and Invariants."
  *ECOOP 2020.* — capability-as-value formalisation closest to this proposal.
- Starlark language specification: determinism and hermeticity constraints.
  `github.com/bazelbuild/starlark/blob/master/design.md`
- Dhall safety guarantees: totality, purity, and integrity-locked imports.
  `docs.dhall-lang.org/discussions/Safety-guarantees.html`
- Lucassen, J. and Gifford, D. (1988). "Polymorphic Effect Systems." *POPL
  1988.* — early formalisation of effect typing.
- Wadler, P. and Thiemann, P. (1998). "The Marriage of Effects and Monads."
  *ICFP 1998.* — monads and effects as two views of the same phenomenon.
