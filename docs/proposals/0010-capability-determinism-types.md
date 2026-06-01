# 0010 — Capability & determinism types for reproducible rendering (H6c)

- **Status:** Draft proposal for review
- **Track:** C — type system & language (beyond the roadmap)
- **Classification:** Stage-C-Fork
- **Suggested horizon:** post-1.0
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
`Random(T)`, `Time(T)`, `Env(T)`, `IO(T)` — exactly mirroring the
idiom already in place for the `IO(T)` monad. This proposal argues that a
*lint* (cheaply checkable today) should precede full *capability types*
(heavier, post-1.0), and presents the design fork for the maintainer's
decision.

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
opaque wrappers, threaded like `IO(T)` — is strictly narrower, consistent
with the existing idiom, and requires no new syntax.

### The "opaque > transparent" principle

The cross-cutting theme in `type-system-evolution.md §3` is that complex
abstractions should be opaque: the user sees the interface type, not the
encoding. `IO`, `Lens`, `Traversal`, and `Partial` (H6b) follow this
principle. Capability constructors are the next natural member of this
family. The implementation is free to change; the type contract is not.

---

## Proposed design

### Phase 1 (lint, no new types): `--require-deterministic`

The cheapest useful step is a *lint* that inspects the inlined core
expression and flags uses of the four capability sources before any type
machinery is needed. The lint runs after the `inline` pass
(`src/core/inline/`), where the tree is maximally simplified and uses of
`io.epoch-time`, `io.RANDOM_SEED`, `io.env`, and `IoAction` constructors
are directly visible.

```
eu --require-deterministic render.eu
# error: render.eu uses io.epoch-time — output is not reproducible
# error: render.eu uses io.env — output depends on environment variables
```

The flag can also be expressed as metadata on a target:

```
` { deterministic: true }
my-output: { ... }
```

The linter checks only the reachable expression graph of `my-output`. A
file can contain non-deterministic helpers that are not reachable from the
target.

This lint is implementable entirely in `src/core/` without touching the
type checker. It is cheap, high-value, and appropriate for 1.0.

### Phase 2 (capability types, post-1.0): opaque constructors

The full design adds four opaque type constructors to the type system,
threaded exactly as `IO(T)` is threaded today:

| Constructor | Source | Example annotation |
|-------------|--------|--------------------|
| `Random(T)` | `io.random`, `random.stream` | `"stream -> Random(number)"` |
| `Time(T)` | `io.epoch-time`, `io.TZ` | `"Time(number)"` |
| `Env(T)` | `io.env`, `io.env.'KEY'` | `"Env(string)"` |
| `IO(T)` | `io.shell`, `io.exec` | `"IO({{..}})"` (already present) |

In the type DSL (metadata strings), these are parsed like the existing
`IO(T)` — `src/core/typecheck/parse.rs` already handles one-argument opaque
constructors. Adding four more is a parse-table addition.

The prelude annotations for the non-deterministic bindings would become:

```
` { doc: "Seconds since the Unix epoch at launch time."
    type: "Time(number)" }
epoch-time: __io.EPOCHTIME

` "Opaque random stream seeded from system entropy or --seed flag."
` { type: "Random(stream)" }
random: __STREAM_NEW(io.RANDOM_SEED)

` { doc: "Read access to environment variables at time of launch."
    type: "Env({{string}})" }
env: __io.ENV
```

A function that uses `io.epoch-time` propagates `Time(...)` in its return
type; the type checker then surfaces this at the call site. A render target
annotated `"number"` will generate a type warning if it evaluates to
`Time(number)`. This is identical to the way `IO(T)` already prevents using
an IO action as a plain value.

### Capability-passing idiom: no new syntax

The threading idiom is the same as for `IO(T)`: pass the capability
explicitly. A eucalypt program that needs both a random stream and the
current time would be written as:

```
make-config(rng, ts): {
  id: rng random.next-int(1000000) head
  generated-at: ts
}

config: make-config(io.random, io.epoch-time)
```

The type of `make-config` would be `"stream -> number -> {{..}}"` (where
`rng` carries the `Random` brand through the stream type and `ts` carries
`Time`). There is no new syntax: capability passing is ordinary function
argument passing. The `Random`/`Time`/`Env` constructors appear only in
return-type annotations, not in call syntax.

### Determinism as a testable property (link to [0003])

A key benefit of formalising this is that **render determinism becomes a
property the conformance suite can test** (see
[0003](0003-conformance-testing-fuzzing.md)). The property is:

> A target annotated `T` (not `Random(T)`, `Time(T)`, `Env(T)`, or `IO(T)`)
> rendered twice with the same input files and the same `--seed` produces
> byte-for-byte identical output.

This is already testable today (the proptest "render determinism" property
in [0003](0003-conformance-testing-fuzzing.md) covers it), but the lint and
capability types sharpen it: any render that fails determinism *and* passes
the lint is a bug in the linter.

### Relation to [0018] hermetic imports and [0019] host-language interop

[0018](0018-module-package-system.md) proposes content-addressed, SHA-256-
integrity-locked module imports — Dhall's integrity mechanism applied to
eucalypt packages. This proposal is the *runtime-value* complement: even
with hermetic imports, a render can be non-deterministic if it calls
`io.epoch-time`. The two proposals together cover the import graph (0018)
and the capability graph (0010), and a CI pipeline that enforces both can
make a strong "this output is reproducible from these inputs" claim.

[0019](0019-host-language-interop.md) covers code generation and schema
interop. The capability types are directly useful there: a generated binding
for a time-reading function should carry a `Time(T)` annotation, not `T`,
so that host-language users see the non-determinism.

---

## Interaction with the existing roadmap

This proposal engages H6c directly and follows H6d's rejection of algebraic
effect rows. It is a Stage C item — the type-system evolution document
places H6c in Stage C ("radical options, 24+ months", §2) with the note
"interesting … gather data before committing"
(`type-system-evolution.md:1308–1310`). The classification here as
`Stage-C-Fork` is faithful to that framing.

The lint (Phase 1) has no dependency on Stage A or B type work — it
operates on the inlined core expression tree, not on types. It could be
implemented between now and 1.0 as a standalone `src/driver/` or
`src/core/verify/` check.

The capability types (Phase 2) depend on:
- H6b (`Partial(T)`) landing first — it exercises the opaque-constructor
  path in the checker and validates the pattern.
- The prelude being sufficiently annotated (Stage A) so that capability
  propagation is visible without user annotations.

There is no conflict with H8 (MLsub): the capability constructors are
opaque and do not participate in subtyping beyond the standard
`any`-consistent rule.

---

## Implementation sketch

### Phase 1: lint (1.0 or shortly before)

- **`src/core/verify/`** — new pass `determinism_check.rs`. Walk the inlined
  core expression graph and collect uses of `__io.EPOCHTIME`, `__io.ENV`,
  `__io.RANDOM_SEED`, and `IoAction` data constructors. Emit a `Diagnostic`
  (warning or error per `--require-deterministic`) for each.
- **`src/driver/options.rs`** — add `--require-deterministic` / `-D` flag
  (alongside the existing `--allow-io` / `-I` at `options.rs:193`).
- **`lib/prelude.eu`** — add a `deterministic: true` metadata key that the
  driver reads from a target block before the lint.
- **Size:** ~200–300 lines of Rust in a new file; one new CLI flag. Low risk.

### Phase 2: capability types (post-1.0)

- **`src/core/typecheck/parse.rs`** — add `Random`, `Time`, `Env` to the
  opaque constructor table (same path as `IO`, `Lens`, `Traversal`).
- **`src/core/typecheck/types.rs`** — four new `OpaqueConstructor` variants
  or string-keyed (the latter is already how `IO` is stored — verify).
- **`lib/prelude.eu`** — update the `type:` annotations on `epoch-time`,
  `env`, `RANDOM_SEED`, `random`.
- **`src/core/typecheck/check.rs`** — add subtyping rules: `Random(T) <: any`
  (consistent with existing opaque behaviour), propagation through application.
- **Size:** ~150–200 lines across three files. Medium risk (careful interaction
  with existing opaque-constructor logic needed).

---

## Alternatives considered

**Remove the capabilities entirely (Starlark approach).** This would make
eucalypt deterministic by construction but would remove `io.shell`,
`io.epoch-time`, and `io.env` — features users already rely on. Not viable
without breaking existing use cases and abandoning the tool-first positioning.

**Koka-style algebraic effect rows (H6d).** Considered and rejected in the
evolution document. Full effect rows require annotating every function arrow,
a new kind system, and pervasive checker changes. The cost/benefit ratio is
poor for a data-transformation language.

**A runtime `--check-deterministic` flag that re-runs and compares.**
Running `eu` twice and diffing is a blunt instrument: it catches
non-determinism only when it *happens* to manifest between the two runs
(clocks incrementing between runs, random seed varying). Static analysis is
strictly more reliable.

**Tagging the `__io` pseudo-block structurally.** The `__io` pseudo-block
is injected as a plain core block (`src/driver/io.rs:82–88`). One could add
a structural tag to each field (e.g. `{:time 1234567890}` instead of
`1234567890`) and check for those tags at the lint level. This works for the
lint but does not give the type propagation of Phase 2.

---

## Risks & what would kill this

**The lint finds too many false positives.** A render that reads
`io.env.'CI'` to choose a log level is technically non-deterministic but
in practice stable across all CI runs. If the lint flags this, users will
disable it immediately. Mitigation: allow per-binding suppression via
metadata (`deterministic-ignore: true`); document the distinction between
"environment-parametric" and "non-reproducible".

**Capability types interact poorly with gradual typing.** If `any → Time(T)`
is trusted silently (the current policy for opaque constructors), then a user
who passes `42` where `Time(number)` is expected gets no warning. This is the
same trade-off as `IO(T)` today and is acceptable: the capability types are
documentation and lint triggers, not sound guards. They degrade gracefully
under gradual typing exactly as `IO(T)` does.

**Low demand.** If eucalypt users primarily run `eu` for one-off renders and
do not use CI-cached outputs, the reproducibility problem is not felt. The
maintainer should collect data from real use cases before committing Phase 2.
The lint is a low-cost probe: if nobody ever sets `--require-deterministic`,
capability types are not worth implementing.

**Naming collision.** `Time(T)` is a common type name in other systems.
Within the type-DSL it is unambiguous (it is parsed from a metadata string
and looked up in the opaque-constructor table), but in documentation it may
cause confusion. Alternative names: `Clocked(T)`, `Dated(T)`. This is a
minor decision best deferred to implementation.

---

## Success criteria

1. **Lint shipped by 1.0:** `--require-deterministic` flags all four
   capability sources; zero false positives on the existing harness tests
   (which do not use `io.epoch-time` or `io.env` in their assertions).
2. **Conformance property added (0003):** The property "render with
   `--require-deterministic` and fixed `--seed` produces identical output
   on repeated runs" is added to the property test suite and passes.
3. **Demand evidence:** At least one reported real-world use case (CI
   caching, output diff, supply-chain audit) where the lint caught a
   non-determinism bug. This is the gate for committing Phase 2.
4. **Phase 2 (post-1.0):** `Random(T)`, `Time(T)`, `Env(T)` type-check
   correctly in the existing harness; the prelude annotations propagate
   capability types to callers without user annotation; a render target
   annotated `"{..}"` (not `IO({..})`) generates a type warning when
   `io.shell` is in its transitive use.

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
