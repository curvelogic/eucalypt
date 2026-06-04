# 0001 — Versioning & stability discipline

- **Status:** Draft proposal for review
- **Track:** A — v1.0 readiness & process
- **Classification:** Whitespace
- **Suggested horizon:** 0.8
- **Related:** siblings [0002](0002-gradual-typing-boundary-policy.md) (type-system soundness tier), [0003](0003-conformance-testing-fuzzing.md) (the corpus that pins stable surfaces), [0004](0004-compiled-unit-caching.md) (prelude cache keying), [0011](0011-typeclasses-without-classes.md)/[0012](0012-algebraic-subtyping-fork.md)/[0013](0013-type-dsl-embedding.md) (how experimental the type DSL stays); open-question 1 (`docs/development/type-system-evolution.md` §5)

## Summary

This proposal asks for *proportionate* versioning discipline, not a heavyweight
stability charter. Three pieces, sized for a project with one serious maintainer
and a small user base:

1. **Real semver from 1.0** — replace the current continuous-delivery,
   four-part build number with meaningful `major.minor.patch`, and say what each
   field means for a language-plus-tool.
2. **`requires` as a version guard** — `__REQUIRES` already parses full semver
   ranges; once versions *mean* something, a unit can pin the range it tolerates
   and **fail loud** on a binary that has moved past it, rather than silently
   re-rendering wrong. This is the cheap substitute for the one genuinely useful
   property an editions system would have given us.
3. **Opt-in prelude versioning** — let a frozen prelude v1 coexist with an
   in-development v2, selected per-unit, so the heaviest stable surface can
   *evolve* without a flag-day break.

Plus a lightweight **deprecation lifecycle** for retiring prelude functions in
good order. The explicit non-goal is a Rust-style **editions** mechanism: at
current scale it is over-commitment for a recurring cost (every old semantic
path carried in the front-end forever), and the cheaper pieces above cover the
real need.

## Motivation

The honest framing first. Eucalypt is not yet a language with a large body of
deployed `.eu` files in other people's repositories. It has one serious user.
That changes the calculus from the usual "libraries must not break downstream":
because the maintainer controls essentially every call site, eucalypt currently
has *more* freedom to break things than a typical library — the `cond` rewrite
below is the proof. The risk worth managing is therefore **not** "an
unmodified third-party file breaks" but the narrower, real one: **a file
silently changes behaviour on a binary upgrade and nobody notices**. The
machinery here targets exactly that, and nothing heavier.

There is, today, no statement of what 1.0 means, and the versioning is not what
it appears. `build.eu` derives the version from `Cargo.toml` as
`major.minor.patch`, defaulting `patch` to `:dev` locally (`build.eu:7-13`),
then appends the CI run number as a fourth component to produce the released tag
`major.minor.patch.build` (`build.eu:59-62`). Releases are cut continuously from
`master` as **draft** GitHub releases (`.github/workflows/build-rust.yaml:391-402`).
So eucalypt practises *continuous delivery with a four-part build number*, not
semantic versioning — the `0.7.0` in `Cargo.toml:3` is a human-set prefix and
the fourth field carries no compatibility meaning at all. Fine for a 0.x tool;
not a basis for any 1.0 promise, however modest.

That the version conveys nothing is not academic. **A genuine breaking change
shipped in 0.6.2**: the `cond` multi-way conditional went from
`cond(list_of_pairs, default)` to a clause form — `cond([[c1,v1],[c2,v2]], default)`
"must be rewritten as `cond[c1 => v1, c2 => v2, default]`" — introducing a new
`=>` clause operator and `__COND`/`__CLAUSE` intrinsics (`CHANGELOG.md:57`;
`lib/prelude.eu:1168,1172`). Eucalypt's own callers (`max-of`, `min-of`,
`parse-args`) were fixed by hand (`lib/prelude.eu:895-897,920-922,2226-2227`);
any user file calling the old `cond` simply breaks on upgrade, in a
patch-looking release, with no signal in the version number. (Two more such
changes are on the books: the pending deep-merge-as-default for block catenation
— `docs/welcome/index.md:320` — and the already-shipped 0.5.3 lookup/scoping
change — `CHANGELOG.md:190-192`.)

The language already ships the primitive that makes the proportionate fix
possible: `requires` (`lib/prelude.eu:15-17`) calls `__REQUIRES`
(`src/eval/stg/version.rs`), which parses a semver constraint and raises
`VersionRequirementFailed` (`src/eval/error.rs:679`) if the running `eu` does
not satisfy it. Crucially it parses with **`semver::VersionReq`**
(`version.rs:31`) — the full constraint grammar, so `requires("^0.7")` or
`requires(">=0.7, <0.8")` already work today. What is missing is not the
mechanism but the *meaning*: with no real semver, a `requires` upper bound
guards nothing.

## Prior art & landscape

Eucalypt's peers are configuration/data languages, so the references are theirs.

**Semantic Versioning 2.0.0** and **Rust RFC 1105 (API evolution).** The
baseline: a version that actually predicts breakage, and the useful nuance that
"all major changes are breaking, but not all breaking changes are major" — a
change that only affects an explicitly-not-covered surface is not MAJOR.

**Dhall's standard versioning.** A sharp three-field rule — MAJOR
backwards-incompatible, MINOR backwards-compatible feature, PATCH "no semantic
change" — and the discipline that an implementation interprets a unit "as a
whole" under one version, never mixing. The right mental model for what a
eucalypt version number should promise.

**CUE's `language.version`.** CUE records a version *inside the module file* and
**errors** when a module declares a version the evaluator does not understand.
That is exactly the read-side of `requires`, and it validates putting the
version contract in the artefact rather than in a flag.

**Rust editions** are the obvious thing to reach for and the one to **not**
adopt — see Alternatives. They earn their keep when a single compiler must keep
*many third-party crates* working across breaking changes. Eucalypt has neither
the third-party crates nor the appetite to carry every old semantic lowering in
the desugarer indefinitely.

## Proposed design

### 1. Stable surfaces and their tiers

State, per surface, how much 1.0 promises. Three tiers: **Stable** (won't break
except in a MAJOR, with a changelog entry and — for the prelude — a deprecation
path), **Experimental** (may change in a MINOR; opt-in or advisory), **Not
covered** (no promise). This is documented *intent*, not a frozen legal
contract — appropriate to the scale.

| Surface | Tier at 1.0 | Notes |
|---|---|---|
| Core syntax (catenation, blocks, lists, operators, anaphora, metadata) | **Stable** | The conservatism guarantee. New syntax is additive. |
| Prelude **v1** API | **Stable (frozen)** | Frozen at 1.0; *evolves via opt-in v2*, not in place (§4). |
| Block-merge / lookup semantics | **Stable** | The deep-merge flip becomes a MAJOR (or a v2-prelude/​`requires`-guarded change), not a silent default flip. |
| CLI surface (`eu` subcommands + flags) | **Stable** | `run`, `test`, `dump`, `version`, `explain`, `list-targets`, `fmt`, `lsp`, `check` (`src/driver/options.rs:85-105`). |
| Import/export formats | **Stable** | The format contracts (YAML/JSON/TOML/EDN/XML/CSV in; YAML/JSON/TOML/EDN/text/eu out). |
| WASM / embedding API (`src/wasm.rs`) | **Stable** | `evaluate`, `evaluate_expr`, `formats`, the `EvalResult` envelope. |
| **Type-annotation DSL & checker** (`type:` strings) | **Experimental** | *Provisional tier.* The DSL grammar is still growing fast — `forall`/`Kind`/`Con`/`App`/structural constraints all landed across 0.6.2–0.7.0 — and several open questions (boundary policy [0002], MLsub fork [0012], embedding [0013], constraint maturation [0011]) are live. Types are advisory, so this tier does **not** gate lang/prelude 1.0. Promote to Stable only once those settle. |
| LSP behaviours; per-file `type-extensions`/`type-profile` (H18) | **Experimental** | Protocol conformance stable; specifics best-effort. |
| Internal IR / STG / GC / heap layout / `dump` output | **Not covered** | Debugging aids, not a contract. |
| Performance; exact error/warning *prose* | **Not covered** | Error *codes/locations* and exit codes are stable; wording is not. |

The point of decoupling the type system (its own row, Experimental) is that
**lang + prelude can reach 1.0 without the type system being "done"** — which it
is not, and need not be, since it is advisory. That removes the single biggest
false blocker to a 1.0.

### 2. Semver for a language-plus-tool

Adopt true semver at 1.0 (semver.org), moving the build number to metadata
(`1.2.3+build.456`, which semver ignores for precedence). Interpret the fields
against eucalypt's real API — syntax + prelude + rendered output:

| Bump | Means | Examples |
|---|---|---|
| **MAJOR** | A change an *unmodified* file can observe in its output, or a removal from the Stable surface. | Deep-merge as the default; removing/renaming a Stable prelude function past deprecation; changing a calling convention (the 0.6.2 `cond` rewrite, had it been post-1.0); changing default render shape. |
| **MINOR** | Backwards-compatible addition. | A new prelude function; a new export format; a new CLI flag/subcommand; **a new opt-in prelude version**; a change to an Experimental surface. |
| **PATCH** | No observable semantic change. | Bug fixes, performance, error-text wording, internal refactors. |

This is Dhall's rule, sharpened by RFC 1105: a change touching only Not-covered
surfaces is not MAJOR.

### 3. `requires` as a version guard

`requires` already does more than it is used for. Because it parses
`semver::VersionReq`, a unit can pin not just a floor but a *range*:

```eu,notest
{ requires: ">=0.7, <0.8" }   # written against 0.7 semantics; refuse 0.8+
```

Once §2 makes versions meaningful, this is the proportionate replacement for
editions' one real benefit. When deep-merge-default lands in (say) 0.8, a unit
that pinned `^0.7` **fails loud** with `VersionRequirementFailed` instead of
silently producing different YAML. You then edit the file and re-pin — which, at
this scale, you would do anyway.

Concretely:

- **Convention:** shared/published units declare a `requires` range. (The
  library units — `lens.eu`, `state.eu` — can model it.)
- **Tooling (small):** `eu` can report the minimum version a file needs, and
  the LSP/`eu check` can hint when a version-sensitive feature is used without a
  `requires` pin. No new runtime mechanism — `__REQUIRES` already raises.
- **Honest limitation:** this is *fail-loud*, not *keeps-working*, and only
  protects files that opted in. Both are the right trade here; "keeps-working
  forever for files I can't touch" is the property we are consciously declining.

### 4. Opt-in prelude versioning ("prelude swap")

The prelude is the heaviest Stable surface (~228 declarations) and the one most
likely to want cleanup — renamed functions, fixed signatures, dropped
misfeatures. Freezing it shouldn't mean it can never improve. The mechanism is
cheap because **the prelude is already a named, embedded, swappable resource**:
`resources.rs:14-18` holds it in a `HashMap` keyed `"prelude"`, it is already
suppressible (`-Q`/`--no-prelude`, `options.rs:58`), and it is injected by the
same unit-metadata-driven path that handles `import:` (`source.rs`).

So: ship a frozen **prelude v1** and an in-development **prelude v2** as two
embedded resources, and let a unit select:

```eu,notest
{ prelude: :v2 }     # opt in; default (absent) is the frozen v1
```

- **Default is v1.** Existing files are untouched.
- **v2 develops opt-in** across MINOR releases (a new prelude version is a MINOR
  addition, §2). When/if it stabilises, a future MAJOR may make it the default,
  or it may stay opt-in indefinitely. `requires` (§3) lets a v2 file pin the
  range where v2 has the shape it expects.
- **Why this is *not* editions:** a prelude is a *unit merged in*, not semantic
  lowering rules in the desugarer. Shipping two preludes is two `include_bytes!`
  + a selector keyed off metadata the loader already reads. There is no forked
  desugarer, no per-construct semantic gating, no migration-lint engine. New
  intrinsics a v2 needs are additive to the binary; functions v2 drops still
  exist in v1 — which is the whole point.
- **One real cost:** the prelude type-summary cache (TS-B7,
  `src/driver/check.rs:51,70` `get_or_build_prelude`/`build_prelude`, the
  `PRELUDE_CACHE` `OnceLock`) must key on the selected version rather than
  assume one prelude. Small, and noted for [0004](0004-compiled-unit-caching.md).

This is deliberately narrower than general editions: it buys "evolve the big
library surface without breaking v1" — the one place the evolve-without-breaking
problem actually bites — and nothing else.

### 5. Deprecation lifecycle

For retiring a Stable prelude function or operator in good order (no editions
required):

1. **Annotate** with `deprecated:` metadata — a reason and a replacement, a new
   symbol-keyed field, no new syntax
   (`` ` { deprecated: "use `window-all`", replaced-by: :window-all } ``).
2. **Warn** on use — `DiagnosticSeverity::WARNING` in CLI and LSP, never an
   error under the advisory default; `eu check` reports it, `--strict` promotes
   it.
3. **Quick-fix** — the LSP offers "replace with `<replacement>`", reusing the
   existing `WorkspaceEdit` code-action machinery (`src/driver/lsp/actions.rs`).
4. **Remove** in a MAJOR (with a changelog entry) — or, better, simply omit it
   from **prelude v2** while v1 keeps it (§4), so removal never breaks a v1 file
   at all.

The `deprecated:`/`replaced-by:` metadata + quick-fix is a small, self-contained
feature worth shipping on its own merits, independent of the rest.

## Interaction with the existing roadmap

Process whitespace — there is no roadmap entry for versioning. The prelude-swap
selector shares the unit-metadata-driven loading path with `import:` and, like
H18's `type-extensions`, lives in metadata. It interacts most with
[0004](0004-compiled-unit-caching.md) (the prelude cache must key on prelude
version) and with the type-system proposals
([0002](0002-gradual-typing-boundary-policy.md),
[0011](0011-typeclasses-without-classes.md),
[0012](0012-algebraic-subtyping-fork.md),
[0013](0013-type-dsl-embedding.md)): the **Experimental** tier for the type DSL
is provisional, and those proposals decide when (if ever) it is promoted to
Stable. It supersedes nothing and does not touch the runtime (non-negotiable #4).

## Implementation sketch

Front-end/loader and policy only; low runtime risk.

1. **Semver policy + build plumbing (`build.eu`, release flow, `docs/`, ~S).**
   Decide the field meanings (§2), publish them, and move the build number to
   `+build.N` metadata at 1.0.
2. **`requires` usage + tooling (`docs/`, small `src/`, ~S).** Document the
   range-pin convention; optionally add an `eu`/LSP "minimum version / missing
   pin" hint. The runtime path already exists.
3. **Prelude versioning (`resources.rs`, `source.rs`, `check.rs`, ~M).** Add a
   second embedded prelude resource; read `prelude:` from unit metadata in the
   loader (beside `import:`); default to v1; key the prelude type cache on the
   selection.
4. **Deprecation metadata (`lib/prelude.eu`, diagnostics, LSP, ~S).**
   `deprecated:`/`replaced-by:` fields, a use-site warning, and the quick-fix.

## Alternatives considered

- **Rust-style editions (metadata-gated semantic lowering).** *Rejected as
  premature.* Editions let one binary interpret old files under old *semantics*
  forever — valuable with many untouchable third-party files, but it forces the
  desugarer to carry every historical semantic path indefinitely. At current
  scale the cost is real and the benefit hypothetical. §3 (`requires` guard) +
  §4 (prelude swap) cover the concrete needs (don't misbehave silently; evolve
  the prelude) far more cheaply. Revisit only if a real third-party ecosystem of
  deployed `.eu` files appears.
- **Newest-version-by-default (CUE-style).** Rejected: defaulting changed
  semantics onto existing files is the silent-misbehaviour failure we want to
  avoid. Defaults stay conservative; change is opt-in (`prelude:`/`requires`).
- **Freeze the prelude outright.** Rejected: it makes the largest surface
  unimprovable. §4 is the escape valve.
- **Do nothing; stay perpetually 0.x.** Legitimate, and cheaper still — but it
  forgoes any version signal at all, and the `cond`-in-a-patch-release problem
  recurs every time.

## Risks & what would kill this

- **Two preludes drift / double maintenance.** v1 frozen + v2 evolving means
  fixes may need porting. Mitigation: v1 is *frozen* (security/critical fixes
  only); feature work happens only in v2; keep v2 a strict evolution, not a fork
  of philosophy.
- **Premature 1.0.** Declaring stability before the Stable surfaces have
  actually settled burns the (small) credibility there is. Mitigation: the gate
  below, and especially keeping the type system Experimental so it is not mistaken
  for promised.
- **`requires` pins rot.** Convention-dependent guards are only as good as the
  discipline of adding them. Mitigation: the optional "missing pin" hint; model
  it in the shipped library units.

## The 0.x → 1.0 gate

1.0 ships when:

- **G1 — Versioning in place.** Real semver adopted (§2); the `requires`-range
  convention documented (§3); the prelude-version selector working with v1 as
  default (§4).
- **G2 — Stable surfaces documented.** The §1 table ratified, including the
  prelude tiering and the **Experimental type system** call.
- **G3 — Conformance bar met ([0003](0003-conformance-testing-fuzzing.md)).** A
  corpus pins the Stable surfaces (and proves a v1-prelude file is unchanged by a
  binary that also ships v2).
- **G4 — Boundary policy fixed ([0002](0002-gradual-typing-boundary-policy.md)).**
  What soundness 1.0 claims (or explicitly does not) is decided — even if the
  answer is "the type system is Experimental and makes no 1.0 promise."
- **G5 — Performance/UX floor ([0004](0004-compiled-unit-caching.md),
  [0005](0005-generational-gc.md)).** A regression-gated baseline (notably the
  ~500–700 ms cold-compile latency 0004 targets), without promising specific
  numbers.

Type-system *completeness* is deliberately absent from the gate: it is
advisory and Experimental, and does not block a lang/prelude 1.0.

## Success criteria

- A published `1.0.0` with real semver (`+build.N` as metadata) and a written,
  one-page statement of the §1 tiers.
- The deep-merge default, when it lands, ships either as a MAJOR or behind
  `prelude: :v2` — never as a silent default flip; a `requires("^0.7")` file
  errors clearly on the binary that changes it.
- Prelude v2 exists and is opt-in, with v1 frozen and unaffected.
- Every Stable prelude element has a deprecation→removal path; the type DSL is
  labelled Experimental and no one mistakes it for a 1.0 promise.

## References

**Eucalypt:** `CHANGELOG.md:57` + `lib/prelude.eu:1168,1172,895-897,920-922`
(0.6.2 breaking `cond` change); `CHANGELOG.md:190-192` (0.5.3 lookup change);
`docs/welcome/index.md:320` (pending deep-merge default); `build.eu:7-13,59-62`
+ `.github/workflows/build-rust.yaml:391-402` (version scheme & release flow);
`lib/prelude.eu:15-17` + `src/eval/stg/version.rs:31` + `src/eval/error.rs:679`
(`requires`/`__REQUIRES`/`semver::VersionReq`/`VersionRequirementFailed`);
`src/driver/resources.rs:14-18` (embedded prelude resource);
`src/driver/options.rs:58,85-105` (`-Q/--no-prelude`, CLI surface);
`src/driver/check.rs:51,70` (`get_or_build_prelude`/`build_prelude`, prelude
cache); `src/driver/lsp/actions.rs` (code-action machinery); `src/wasm.rs`
(embedding API); `docs/development/type-system-evolution.md` §H18, §5.

**External:** [Semantic Versioning 2.0.0](https://semver.org/);
Rust RFC 1105, *API evolution*
(https://rust-lang.github.io/rfcs/1105-api-evolution.html);
Dhall standard *versioning.md*
(https://github.com/dhall-lang/dhall-lang/blob/master/standard/versioning.md);
[CUE language version](https://cuelang.org/docs/concept/cue-language-version/).
