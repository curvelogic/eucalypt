# 0002 — Gradual-typing soundness & boundary policy for 1.0

- **Status:** Draft proposal for review
- **Track:** A — v1.0 readiness & process
- **Classification:** Process
- **Suggested horizon:** 0.9
- **Related:** H13 (type-system-evolution.md); §5 open questions 4 & 6; sibling proposals [0009 — structural contracts & runtime schema validation](0009-structural-contracts-validation.md), [0007 — type-directed compilation](0007-type-directed-compilation.md)

## Summary

Eucalypt has a gradual type checker, but it has never *stated* the contract that checker makes with the user at the boundary between typed and untyped code. Today the answer is implicit in the source: when an `any`/untyped value flows into a typed position, **nothing happens** — the value is silently trusted, no runtime cast is inserted, no blame is tracked, and types are erased before code generation. That is a legitimate 1.0 choice (it is TypeScript's choice, and the cheapest one), but it must be a *chosen, documented* policy rather than an accident of implementation. This proposal recommends ratifying the current behaviour as the **permanent default** (H13a), blessing the already-implemented `eu check --strict` as the CI gate, adding an *opt-in* boundary-check mode (H13c) for tests, and explicitly declining whole-program sound casts (H13b). It exists to let the maintainer close open questions 4 and 6 before 1.0 freezes the contract.

## Motivation

The type checker runs, but its semantics at the gradual boundary are documented nowhere a user would look. A 1.0 release is a promise about behaviour, and we cannot ship a promise we have not written down. Five facts about the current implementation define a contract by omission:

**Type issues are advisory warnings, never errors.** `src/core/typecheck/check.rs:24` says so outright: "Type issues are always warnings — they never prevent evaluation." Warnings are accumulated (`check.rs:97-98`) and surfaced, but evaluation proceeds regardless.

**Consistency is symmetric but not transitive, and `any` is consistent with everything.** The gradual relation lives in `is_consistent` (`src/core/typecheck/subtype.rs:179`); its first action is `if matches!(s, Type::Any) || matches!(t, Type::Any) { return true; }` (`subtype.rs:181-183`). The doc-comment is explicit: "consistency is symmetric but NOT transitive. `number ~ any` and `any ~ string` do not imply `number ~ string`" (`subtype.rs:177-178`). This is the textbook Siek–Taha consistency relation, and its non-transitivity is *why* `any` launders any type into any other.

**`any` unifies with everything and produces no binding.** `unify` short-circuits on `(Type::Any, _) | (_, Type::Any) => Ok(())` (`src/core/typecheck/unify.rs:56`); the doc-comment notes "at the gradual boundary, `any` unifies with everything (no binding produced)" (`unify.rs:41-42`). An `any` flowing into a typed position neither constrains the variable nor records an obligation — it simply succeeds.

**Uninformative types suppress warnings.** Even where a mismatch *could* be reported, the checker stays silent if either side is `Any` or `Never`: `is_informative(ty) = !matches!(ty, Type::Any | Type::Never)` (`check.rs:1099-1101`), gating emission throughout (`check.rs:553-554, 784-785, 934, 960`). The boundary is not merely unchecked at runtime — it is deliberately *unwarned* at compile time when the static side is `any`.

**Types are erased before code generation.** The checker is the last thing to touch types. In the binary's flow (`src/bin/eu.rs:122-142`), `--type-check` runs `type_check(&core_expr)`, prints warnings, and — under `--strict` — may abort (`eu.rs:139-141`); then `eval::run` proceeds (`eu.rs:144-146`) and performs STG compilation inside (`src/driver/eval.rs:187`, `stg::compile`). The type environment is never threaded into `stg::compile`; nothing in `src/driver/prepare.rs` carries types forward. Types exist only to produce diagnostics, then vanish.

**What this means for a user, concretely.** Two consequences follow, both surprising to anyone expecting "typed":

1. *A wrong annotation is silent.* Annotate a binding `string -> string`, feed it a number through an `any`-typed pipeline, and the program runs to completion with whatever the intrinsics do to a number — no error, no warning, because the value entered through `any`.
2. *Bad data still renders.* The headline use case is rendering a YAML/JSON/TOML manifest. A manifest assembled from imported data (which enters as `any`) that violates an annotation still renders: the type system does not stand between malformed input and the emitted artefact.

Neither is a bug; both are the designed behaviour of optimistic gradual typing. But a tool whose job is to emit infrastructure manifests *must* tell its users which guarantees it makes and which it withholds, in plain words, before 1.0.

## Prior art & landscape

The gradual-typing literature is, at heart, an argument about exactly this boundary. It spans a spectrum from "trust everything" to "check everything", and the central, well-measured trade-off is **soundness versus runtime cost**.

| Discipline | Boundary behaviour | Blame? | Runtime cost | Exemplar |
|---|---|---|---|---|
| Optional / migratory | trust; erase | none | zero | TypeScript, Flow |
| Sound (guarded / "natural") | proxy + higher-order contract at each crossing | precise | **catastrophic** in the worst case | Typed Racket (deep) |
| Transient / concrete | shallow per-operation checks, proxy-free | collaborative | moderate, tunable | Reticulated Python; shallow Racket |
| Contracts | predicate checks where the programmer writes them | precise | pay-as-you-go | Racket contracts |

The foundations: Siek & Taha, *Gradual Typing for Functional Languages* (2006) gave the consistency relation eucalypt's `is_consistent` implements. Findler & Felleisen, *Contracts for Higher-Order Functions* (2002) introduced **blame** — the bookkeeping that names which side of a boundary violated its contract. Wadler & Findler, *Well-Typed Programs Can't Be Blamed* (2009) turned blame into a calculus and proved the slogan: in a sound gradual system, when a cast fails, the fault always lies with the less-typed side.

The cautionary tale is performance. Takikawa, Feltey, Greenman, New, Vitek & Felleisen, *Is Sound Gradual Typing Dead?* (POPL 2016) measured Typed Racket across the full **performance lattice** — every configuration of typed/untyped module boundaries in a program — and found partially-typed configurations with slowdowns up to roughly **100×** (reported as high as 105×) versus untyped. The cost comes precisely from inserting a checking proxy at every boundary crossing and re-checking values that flow back and forth. This is not a constant factor to be optimised away cheaply: it is the price of soundness under the guarded semantics.

The field's response defines the rest of the spectrum. Bauman, Bolz-Tereick, Siek & Tobin-Hochstadt, *Sound Gradual Typing: Only Mostly Dead* (OOPSLA 2017) showed a tracing JIT (Pycket) recovers >90% of the overhead — soundness is survivable *with a sufficiently sophisticated runtime*. Vitousek, Swords & Siek, *Big Types in Little Runtime* (POPL 2017) introduced **transient** gradual typing: proxy-free, shallow checks at use sites with open-world soundness and blame-without-proxies, trading precision for tractable cost. Greenman et al., *Deep and Shallow Types for Gradual Languages* (PLDI 2022) and the GTP benchmark line characterise the whole spectrum — deep (sound, complete-monitoring, expensive) versus shallow (transient, cheaper, weaker guarantees) — as a design dial, not a binary.

Eucalypt sits today at the optional/migratory end with TypeScript, and the runtime reality argues hard for keeping it there. It is **AOT-compiled to STG and erases types** (`eu.rs:122-146`, `eval.rs:187`), with no JIT to claw back contract overhead the way Pycket does for Racket — so the Takikawa result is the *relevant* warning, not the "only mostly dead" rejoinder. And the VM's costs are already **GC-dominated** (mark cost >95% of traversal-heavy VM time; recon baseline, `_house-style.md` §2): pervasive boundary casts would allocate proxies and run predicate checks against exactly that GC-bound runtime, reintroducing the overhead the literature warns about on the runtime least equipped to absorb it.

Among peers the same split holds. Jsonnet and Starlark are essentially untyped. Dhall is totally *statically* typed — no `any`, so no boundary problem and no gradual escape hatch. CUE folds values and constraints into one lattice, checking at unification rather than via casts. Nickel is the closest analogue worth borrowing from: it pairs static types with **runtime contracts** at the typed/untyped boundary. That is the design sibling [0009](0009-structural-contracts-validation.md) develops for eucalypt, and the insight is that contracts let the user choose *where* validation happens (at data ingress, where they want it) rather than the type system imposing it at *every* boundary. What to borrow: Nickel's instinct that runtime validation belongs at deliberate, user-chosen points. What not to borrow: Typed Racket's deep/guarded semantics as an always-on default — the POPL 2016 numbers and our JIT-less, GC-bound runtime make that a non-starter for production rendering.

## Proposed design — the recommended 1.0 policy

The policy has four parts. It changes almost no code; its substance is a *decision* and its documentation.

**(a) Keep advisory/silent as the permanent default (H13a).** An `any` value flowing into a typed position is trusted at runtime. No cast, no proxy, no blame. This is the current behaviour (`subtype.rs:181`, `unify.rs:56`, `check.rs:24`) and it becomes a stated guarantee, not an accident: *eucalypt is optionally typed and optimistic by default.* The reasons are the runtime reality above — AOT, no JIT, GC-bound — plus the tool-first framing: a manifest renderer must not pay 10–100× to render in production.

**(b) Bless `eu check --strict` as the CI gate.** This already exists and works. `eu check` runs in two phases — annotation *syntax* checking and then the type checker — and tallies issues (`src/driver/check.rs:112`). Under `--strict`, any total issue yields a non-zero exit (`check.rs:114-117`):

```rust
if total_issues == 0 { Ok(0) }
else if opt.check_strict() { Ok(1) }      // --strict: warnings are fatal
else { /* warnings never fail the build */ Ok(0) }
```

So the static contract a project opts into is *exactly* "no type warnings": run `eu check --strict *.eu` in CI and a mistyped manifest fails the build before it ships. (Note `--type-check --strict` on `eu run` also aborts before evaluation — `eu.rs:139-141` — so the same gate is available inline; the blessed, documented surface should be `eu check --strict`, with `eu run --type-check` reserved for ad-hoc local use.) This is the right place to be strict: statically, in CI, at zero production cost. The 1.0 commitment is that `eu check --strict` is a **stable, documented contract** — its exit code is part of the public interface.

**(c) Offer an opt-in `--strict-boundary` / test mode (H13c).** A new flag (it does not exist today — confirmed absent from `src/driver/options.rs`) that inserts **targeted** runtime checks at chosen typed positions — function parameters with concrete annotations, and annotated top-level bindings — emitting a diagnostic naming the offending annotation (lightweight blame) when an actual value fails its declared type. Crucially this is:

- **off by default**, on for `--strict-boundary` and ideally defaulted-on under the test harness;
- **targeted, not pervasive** — checks at explicitly-annotated boundaries only, never the whole-program proxy insertion of H13b. This is transient/shallow in spirit (Vitousek; Greenman shallow), not deep/guarded;
- **shallow** — a predicate check on the value's shape at the crossing, no proxy wrapping, no re-checking on the way back. For higher-order positions (a value typed `a -> b`) the honest shallow check verifies "is a function" and stops; it does not wrap and monitor calls.

The point of (c) is to find latent mistyping in mixed-typed code *during testing*, where a 2–5× slowdown is irrelevant, while never imposing it on production rendering. This is the "test-mode boundary checks" row of the H11/§4 payoff table.

**(d) State the policy in user-facing docs.** One paragraph in the language reference, in plain words: *Eucalypt is optionally typed. Annotations are checked statically and are advisory by default: a value that enters typed code through an untyped (`any`) path is trusted and not re-checked at runtime, and types are erased before execution. Use `eu check --strict` to make type warnings fail your build. Use `--strict-boundary` in tests to catch values that violate their annotations at runtime. Eucalypt does not insert always-on runtime type checks, by design.* Without (d), users will reasonably but wrongly assume "typed" means "checked at runtime" — and discover otherwise when a malformed manifest renders clean.

What we explicitly **do not** do is H13b: whole-program sound casts with guarded semantics. That is the road the POPL 2016 paper measured at up to 100× on a JIT-equipped runtime; on eucalypt's AOT, GC-bound runtime it would be worse, and it serves a soundness goal a config tool does not need when it has (b) for static safety and [0009](0009-structural-contracts-validation.md) for ingress validation.

## Interaction with the existing roadmap

This proposal *decides* H13 rather than extending it: it ratifies H13a as permanent, schedules H13c as the single optional experiment, and rejects H13b. It is deliberately small so that it can land before the type system grows.

It is a **precondition for [0007 — type-directed compilation](0007-type-directed-compilation.md)** (H11). The 0007 optimisations — unboxing numeric pipelines (H11a), direct intrinsic dispatch for typed arithmetic (H11f), dead-branch elimination (H11e) — all *rely on annotations being trustworthy*: if `+ : number -> number -> number` compiles to `__ADDPRIM__` with no tag check, an `any` value that lied about being a number would corrupt the unboxed path or crash the VM. So 0007 cannot stay purely optimistic at the positions it specialises; it needs *either* a proven-concrete call site *or* a guard, and (c)'s targeted-check machinery is the natural home for those guards. The policy must be decided first because it determines whether 0007 may trust annotations or must guard them — this is the bridge from "types are advisory" to "types are a runtime asset", uncrossable without settling the contract here.

It is **complementary to [0009 — structural contracts](0009-structural-contracts-validation.md)**, addressing the same anxiety — "bad data renders" — from the opposite end. This proposal keeps the *type boundary* optimistic; 0009 gives users explicit, runtime-validated **contracts at data ingress** — where a YAML import or external input enters — which is where validation is actually wanted. Contracts validate *where the user writes them* (Findler–Felleisen pay-as-you-go), not at every boundary (Typed Racket's pervasive cost). Together: static checking via `eu check --strict`; optimistic erased boundaries in production; contracts for the few places runtime validation earns its keep. 0009 is the *answer* to "validate my manifest"; this proposal is the *reason* that answer is contracts, not sound casts.

It honours non-negotiable #3 (`_house-style.md` §1): "The default boundary policy is silent (`any → T` trusted), not blame-tracked." This proposal is, in effect, the document that non-negotiable points to.

## Implementation sketch

Parts (a), (b), (d) are **zero code** — (a) and (b) are the current behaviour, and (d) is documentation. The only build is (c):

- **`--strict-boundary` flag** in `src/driver/options.rs` (mirroring the existing `--type-check`/`--strict` plumbing at `options.rs:74-75, 110-111, 169-174`). Small.
- **A boundary-check insertion pass** over core, after type-checking, gated on the flag. At each call to a binding with a concrete (non-`any`, informative) parameter annotation, and at each annotated top-level binding, wrap the argument/value in a shallow runtime predicate that raises an `ExecutionError` naming the annotation on failure. This is a new transform in `src/core/transform/` consuming the checker's inferred-type map (the checker already produces a type environment — `check.rs:154-162` returns warnings *and* the environment). Medium size; the fiddly part is mapping a `Type` to a shallow runtime predicate (reusing the predicate intrinsics — `number?`, `string?`, `block?` — that already exist).
- **Test-harness default.** Have `src/driver/tester.rs` default `--strict-boundary` on, so the harness surfaces annotation violations.

Risk is contained: the pass is off by default, additive, and touches no GC or STG internals. It does *not* require threading types into `stg::compile`; the checks are ordinary core expressions compiled like any other, so erasure (the (a) guarantee) is preserved for the default path.

Rough sequencing: ratify (a)/(b) and write (d) for **0.9** (this is the gating decision); land (c) as a follow-on once 0007 needs guard machinery, or earlier if boundary mistyping proves a real support burden.

## Alternatives considered

**Make warnings errors by default.** Rejected: it breaks the gradual promise that unannotated code "just works", and would make importing real-world `any`-typed data hostile. `--strict` opt-in achieves the same safety for projects that want it without taxing the default.

**Adopt sound (guarded) gradual typing (H13b).** Rejected for 1.0: the POPL 2016 performance lattice shows up to ~100× worst-case overhead, and eucalypt's AOT, JIT-less, GC-bound runtime is the worst case for it. Revisit only if (i) a JIT or type-directed specialisation (0007) makes the cost recoverable *and* (ii) users demonstrably need runtime soundness that contracts (0009) cannot provide.

**Transient/shallow as an always-on default (Vitousek).** Rejected as a default but adopted as the *shape* of the opt-in (c): transient's proxy-free shallow checks are exactly the right mechanism for a test mode, but even their moderate cost is unwanted in production manifest rendering.

**Do nothing — leave the contract implicit.** Rejected: this is the status quo, and the whole point is that 1.0 must not ship an unstated boundary contract. "We never decided" is not a position a 1.0 can take.

## Risks & what would kill this

- **The optimistic default proves too dangerous in practice.** If users routinely ship mis-rendered manifests because the boundary is silent, the calculus shifts toward making (c) the default or toward 0009 contracts being mandatory at import. *Falsifier:* field reports of silent mis-rendering that `eu check --strict` would not have caught (i.e. the bad value genuinely entered via `any` at runtime, not via a statically-visible mismatch).
- **(c) is more expensive than "test-mode" implies.** If shallow checks at annotated boundaries cost more than ~2–5× even in tests, the mode is less attractive. *Falsifier:* benchmark the harness with `--strict-boundary` on a lens/block-heavy corpus; if it dominates, narrow the checked positions further.
- **0007 forces the issue early.** If type-directed compilation is prioritised, the guard machinery of (c) becomes load-bearing sooner than 0.9-follow-on, compressing the schedule.

This proposal is the maintainer's call on **open question 6** ("policy on boundary unsoundness — forever silent, opt-in checks, or eventually mandatory?"): the recommendation is *forever silent by default, opt-in checks via (c), never mandatory*. It informs **open question 4** ("how much do we care about runtime performance?"): by declining always-on casts it keeps the runtime free of contract overhead, which is consistent with — and a precondition for — answering #4 in favour of the 0007 optimisations rather than against them.

## Success criteria

- The language reference contains the (d) paragraph; a new user can correctly answer "does eucalypt check types at runtime?" from the docs alone.
- `eu check --strict` is documented as the CI gate, with its exit-code contract (`check.rs:114-117`) listed among 1.0's stability promises (see [0001 — the 1.0 charter](0001-v1-charter.md)).
- `--strict-boundary`, when delivered, catches a seeded annotation violation in a mixed-typed test that the default run silently renders — demonstrating the test-mode value without affecting the default path.
- No measurable change to default-path render latency or heap behaviour (because (a)/(b) add no code and (c) is off by default).

## References

**Eucalypt source.** `src/core/typecheck/subtype.rs:179` (`is_consistent`), `:181-183` (`any` consistent both ways), `:177-178` (symmetric, non-transitive); `src/core/typecheck/unify.rs:41-42, :56` (`any` unifies, no binding); `src/core/typecheck/check.rs:24` (advisory-only), `:1099-1101` (`is_informative` suppresses on `Any`/`Never`), `:154-162` (checker returns environment); `src/driver/check.rs:112-127` (`--strict` exit logic); `src/driver/options.rs:74-75, 110-111, 169-174` (`--type-check`/`--strict` plumbing); `src/bin/eu.rs:122-146` (type-check then erase then eval); `src/driver/eval.rs:187` (`stg::compile`); `src/driver/prepare.rs` (no type carriage to STG).

**Eucalypt docs.** `docs/development/type-system-evolution.md` H13 and §5 questions 4 & 6; `docs/development/gradual-typing-spec.md` §1 Non-Goals (runtime performance optimisation, runtime dispatch) and §14 (out-of-scope); `docs/proposals/_house-style.md` §1 non-negotiable #3.

**Literature.** Siek & Taha, *Gradual Typing for Functional Languages* (2006); Findler & Felleisen, *Contracts for Higher-Order Functions* (ICFP 2002); Wadler & Findler, *Well-Typed Programs Can't Be Blamed* (ESOP 2009); Takikawa, Feltey, Greenman, New, Vitek & Felleisen, *Is Sound Gradual Typing Dead?* (POPL 2016) — performance lattice, up to ~100× slowdown; Bauman, Bolz-Tereick, Siek & Tobin-Hochstadt, *Sound Gradual Typing: Only Mostly Dead* (OOPSLA 2017); Vitousek, Swords & Siek, *Big Types in Little Runtime* (POPL 2017) — transient, proxy-free, blame without proxies; Greenman et al., *Deep and Shallow Types for Gradual Languages* (PLDI 2022).

**Peer languages.** Nickel (static types + runtime contracts at the boundary); CUE (types-as-values unification); Dhall (total static typing, no gradual escape); TypeScript / Flow (optional, erased).
