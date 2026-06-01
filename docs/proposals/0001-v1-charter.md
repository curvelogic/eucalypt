# 0001 — The eucalypt 1.0 charter: stability, semver, editions, deprecation

- **Status:** Draft proposal for review
- **Track:** A — v1.0 readiness & process
- **Classification:** Whitespace
- **Suggested horizon:** 0.8
- **Related:** ADR-001 (`docs/development/architectural-decisions.md`), open-question 1 (`docs/development/type-system-evolution.md` §5); H18 per-file profiles (ibid. §H18); sibling proposals [0002](0002-gradual-typing-boundary-policy.md), [0003](0003-conformance-testing-fuzzing.md), [0004](0004-compiled-unit-caching.md), [0005](0005-generational-gc.md), [0011](0011-typeclasses-without-classes.md)

## Summary

1.0 is not a feature set; it is a **commitment**. This document proposes what
eucalypt 1.0 would guarantee, and — more importantly — the machinery that keeps
those guarantees *keepable* as the language keeps moving. The central proposal
is a Rust-style **editions** mechanism that lives in **unit metadata**
(`{ edition: "2026" }`), so that necessary breaking changes — there is already
one pending — can land as new defaults without invalidating files written
against an older edition. Around it sit three supporting pieces: an enumeration
of **stable surfaces** and their tiers; a **semver interpretation** suited to a
language-plus-tool whose "API" is its syntax, prelude and output rather than a
set of function signatures; and a **deprecation lifecycle** built on the
warning/diagnostic and LSP code-action infrastructure that already exists. The
thesis throughout: config and `.eu` files are durable infrastructure artefacts,
checked into other people's repositories and CI; backward compatibility matters
*more* here than for a typical library, and the cost of getting it wrong is paid
by users who did not change anything.

## Motivation

Eucalypt is in "early phase of development and subject to change"
(`docs/welcome/index.md:33`), yet it is already used the way config languages
are used: a `.eu` or annotated `.yaml` file is committed, rendered in CI, and
left alone for months. The people who feel a breaking change are not following
the changelog. They are the ones for whom *nothing changed* except the version
of `eu` their pipeline picked up.

There is no document today that says what 1.0 will *not* break. Worse, the
present versioning is not what it appears. `build.eu` derives the version from
`Cargo.toml` as `major.minor.patch`, defaulting `patch` to the symbol `:dev`
locally (`build.eu:7-13`), then appends the CI run number as a fourth component
to produce the released tag `major.minor.patch.build` (`build.eu:59-62`,
`build-meta.yaml`). Releases are cut continuously from `master` as **draft**
GitHub releases (`.github/workflows/build-rust.yaml:391-402`); the tag is
whatever `eu -e eu.build.version` reports at build time. So eucalypt practises
*continuous delivery with a four-part build number*, not semantic versioning —
the `0.6.1` in `Cargo.toml:3` is a human-set prefix, and the fourth field
carries no compatibility meaning at all. That is fine for a 0.x tool. It is not
a 1.0 promise.

Meanwhile breaking changes are demonstrably real and *already happening*:

- **A pending default change.** "It is possible that a deep merge will become
  the default for block catenation in future" (`docs/welcome/index.md:320`).
  Block catenation `{a:1} {b:2}` is one of the first things every user learns
  (ibid. §Example 2). Flipping shallow→deep silently would change the output of
  files that nobody edited — the canonical infrastructure-artefact failure.
- **A precedent already shipped.** 0.5.3 changed simple-lookup semantics so that
  `.name` is "consistently key lookup restricted to block bindings, never
  extending to outer scope. Previously, `.name` on a static block literal
  resolved through the block scope and fell through to outer scope"
  (`CHANGELOG.md:128`), and changed monadic-block scoping and associativity in
  generalised lookup (`CHANGELOG.md:129-130`). These are exactly the kind of
  *semantic* change an edition is meant to gate.

The language even ships a primitive that anticipates this need: `requires`
(`lib/prelude.eu:15-17`) calls the `__REQUIRES` intrinsic
(`src/eval/stg/version.rs`), which parses a semver constraint and asserts the
running `eu` satisfies it, raising `VersionRequirementFailed`
(`src/eval/error.rs:677`) otherwise. A file can already say "I need `eu >=
0.6.0`". What it cannot say is "interpret me under the *2026* semantics" — which
is the strictly more useful guarantee.

## Prior art & landscape

Three reference points, each chosen because eucalypt's peers are
configuration/data languages, not general-purpose ones.

**Rust editions (RFC 2052).** The load-bearing precedent. A crate opts into an
edition in its manifest (`edition = "2021"`); the *same compiler* supports all
editions and a dependency graph "may involve several different editions
simultaneously. Thus, editions do not split the ecosystem nor do they break
existing code." The hard invariant — "Warning-free code on edition N must
compile on edition N+1 and have the same behavior" — is precisely the property
config files need. Crucially, an edition is *crate-local*: it "cannot impose new
requirements or semantics on external crates", which maps cleanly onto
eucalypt's per-unit model. Migration is mechanical: `rustfix` applies edition
lints' suggestions, while the RFC is honest that it "will never be perfect".

**Dhall's standard versioning.** Dhall versions the *language standard*
separately from any implementation, with a sharp rule: a MAJOR bump is
backwards-incompatible, a MINOR adds a backwards-compatible feature, and a PATCH
is "no semantic change" (documentation, grammar-terminal renames). An
implementation "MUST support the latest version" and, if it supports an older
one, "MUST do so 'as a whole' … MUST NOT mix and match functionality from
different releases." That whole-or-nothing rule is the right discipline for an
edition: a unit is interpreted entirely under one edition's semantics, never a
pick-and-mix.

**CUE's `language.version`.** The closest structural analogue to this proposal.
CUE records a mandatory `language.version` *inside the module file*, which lets
language designers "support old syntax and repurpose it" and makes the evaluator
*error* when a module declares a version newer than the evaluator understands —
because it "cannot know what semantics apply to the newer version." This is the
read-side of what eucalypt's `requires` already does on the write-side, and it
validates putting the version declaration in the artefact itself rather than in
a flag.

What eucalypt should borrow: per-unit opt-in (Rust/CUE), one binary supporting
all editions (Rust), whole-edition interpretation (Dhall), and the
artefact-embedded version (CUE). What it should *not* borrow: Rust's full
multi-edition trait/coherence machinery (eucalypt has no traits), and Dhall's
separate-standard-document overhead (premature for one implementation).

## Proposed design

### 1. Stable surfaces and their tiers

1.0 commits, per surface, to one of three tiers: **Stable** (breaking changes
only across an edition boundary, with migration support), **Unstable**
(explicitly experimental; may change in a MINOR), and **Not covered** (no
compatibility promise at all).

| Surface | Tier at 1.0 | Notes |
|---|---|---|
| Core syntax (catenation, blocks, lists, operators, anaphora, metadata) | **Stable** | The non-negotiable conservatism guarantee. New syntax is itself an edition-gated event. |
| Prelude API (~200 functions, `lib/prelude.eu`) | **Stable** | The largest and heaviest commitment — see below. |
| Block-merge / lookup *semantics* | **Stable, edition-gated** | The deep-merge flip and the 0.5.3 lookup change are the motivating cases. |
| Type-annotation DSL (`type:` strings, `src/core/typecheck/parse.rs`) | **Stable (advisory)** | Annotations are documentation; their *grammar* is stable, but a type warning appearing/disappearing is **not** a breaking change (it has no runtime effect). |
| CLI surface (`eu` subcommands + flags) | **Stable** | `run`, `test`, `dump`, `version`, `explain`, `list-targets`, `fmt`, `lsp`, `check` (`src/driver/options.rs:85-105`); flags such as `-x/-j/-o/-t/-e/-Q` (ibid. `RunArgs`). |
| Import/export formats (YAML, JSON, TOML, EDN, XML, CSV, text in; YAML/JSON/TOML/EDN/text/eu out) | **Stable** | The *contract* of these formats; see open-question 1 below for the export-shape nuance. |
| WASM / embedding API (`src/wasm.rs`) | **Stable** | `evaluate`, `evaluate_expr`, `formats` and the `EvalResult` JSON envelope. |
| LSP behaviours | **Unstable** | Protocol conformance is stable; *which* hovers/actions/hints appear is best-effort and will keep improving. |
| Per-file `type-extensions` / `type-profile` (H18) | **Unstable** | Explicitly the opt-in experimental channel (`type-system-evolution.md:1159-1163`). |
| Internal representations: core IR, STG, GC behaviour, heap layout, `dump` output | **Not covered** | These are debugging aids; `eu dump` is for us, not a contract. |
| Performance, exact error *text*, exact warning *wording* | **Not covered** | Error *codes/locations* and exit codes are stable; prose is not. |

The prelude commitment deserves a flag of its own. ~200 annotated functions
(`CHANGELOG.md`, 0.6.0; ≈229 top-level declarations in `lib/prelude.eu`) is a
*large* stable surface — far larger than most config languages expose. Freezing
it forbids freely renaming a function, re-ordering arguments (as 0.6.0 did for
`arr.slice`/`arr.neighbours`), or deleting a misfeature without an
edition + deprecation cycle. This is a real, ongoing tax the maintainer should
accept deliberately. Pragmatic softener: tier the prelude itself — a **core**
subset (arithmetic, comparison, string, list, block, `io`) at Stable, newer
namespaces (`arr`, `set`, `state`, lens internals) held at Unstable until 1.1 —
shrinking the frozen footprint without weakening the promise where it matters.

### 2. Semver for a language-plus-tool

Adopt true semver from 1.0 (semver.org), dropping the fourth build component
from the *public* version (it can persist as build metadata, `1.2.3+build.456`,
which semver ignores for precedence). Interpret the three fields against
eucalypt's actual API — syntax + prelude + rendered output:

| Bump | Means | Examples |
|---|---|---|
| **MAJOR** | A change to *default* semantics or a removal that an unmodified file can observe. | Making deep-merge the default outside an edition; removing a prelude function past its deprecation window; changing default render shape. |
| **MINOR** | Backwards-compatible addition. | A new prelude function; a new export format; a **new edition**; a new CLI flag/subcommand; a new advisory type-warning. |
| **PATCH** | No observable semantic change. | Bug fixes (including diagnostics), performance, error-text wording, internal refactors. |

This mirrors Dhall's rule and is sharpened by RFC 1105: "all major changes are
breaking, but not all breaking changes are major" — a change that breaks only
code relying on Not-covered surfaces (`dump` output, exact error prose) is *not*
MAJOR. The edition mechanism is what lets a genuinely-breaking *language* change
ship in a MINOR: the new behaviour exists only for units that opt in, so the
binary stays backwards-compatible for everyone else — the bargain Rust strikes
(RFC 2052: editions ship in normal releases yet "do not break existing code").

### 3. Editions in metadata (the key proposal)

A unit declares its edition in its **unit metadata** — the first block
expression at the top of the file, the same channel that already carries
`import:` (`docs/appendices/syntax-gotchas.md:439-457`; e.g.
`tests/harness/027_unit_import.eu:2`):

```eu,notest
{ edition: "2026"
  import: ["lens.eu", "state.eu"] }

# … unit body. Block catenation here uses 2026 semantics (deep merge),
# the 2025 lookup rules, and whatever else the 2026 edition pins.
result: defaults config   # deep merge under edition 2026
```

Semantics:

- **Default is the *oldest* edition.** A unit with no `edition:` is interpreted
  under edition `"2025"` (the pre-editions baseline — current shallow-merge,
  current lookup rules). Existing files therefore never change behaviour.
  This is the inverse of CUE's "newest by default" and is the safer choice for
  artefacts already in the wild.
- **One binary, all editions.** The `eu` binary supports every published
  edition; the desugarer selects edition-specific lowerings from the unit
  metadata. Per Rust's invariant, a clean file valid under edition N is valid
  with identical output under the binary that also supports N+1.
- **Whole-unit, never mixed.** Following Dhall, a unit is interpreted entirely
  under one edition. Editions compose at unit boundaries only: an imported unit
  carries *its own* edition, so a 2026 file may import a 2025 library safely —
  which matters because imports are how eucalypt reuses code today.
- **Forward-reference is an error.** A unit declaring an edition newer than the
  binary knows fails fast with a clear diagnostic — exactly CUE's behaviour, and
  trivially built on the existing `VersionRequirementFailed` path.

**Why metadata, not a flag day or a CLI switch.** Three reasons, in priority
order. (i) *Syntactic conservatism* (non-negotiable #1): editions add **no new
syntax** — `edition:` is an ordinary symbol-keyed entry in an ordinary metadata
block, no keyword, no pragma form. H18 already proposes exactly this shape for
`type-extensions: [:hkt, :rec, :flow]` (`type-system-evolution.md:1159-1163`),
so editions reuse an *already-blessed* pattern rather than inventing one. (ii)
*The artefact is the unit of truth.* A CLI flag (`--edition`) cannot express
that *this file* wants new semantics while *that one* keeps the old — and config
repos routinely mix old and new files. CUE's rationale applies verbatim: putting
the version in the artefact lets the evaluator apply the right semantics
per-file. (iii) *No flag day.* A flag day (bump the binary, everyone's output
changes at once) is precisely the infrastructure-artefact catastrophe this
charter exists to prevent. Editions let the deep-merge default *land in the
language* — new files written with `{ edition: "2026" }` get it immediately —
while every existing file is untouched until its author opts in and migrates.

**Migration assistance.** The breaking delta of an edition is published as a set
of edition lints, surfaced through the existing warning/diagnostic
infrastructure (`Diagnostic::warning()`, `DiagnosticSeverity::WARNING`,
`CHANGELOG.md` 0.6.0). The LSP already emits structural rewrites as
`WorkspaceEdit`/`TextEdit` code actions — "wrap as namespace", "promote/demote
metadata shortcuts", "add metadata fields"
(`src/driver/lsp/actions.rs:27,86,151-165`). An "Upgrade unit to edition 2026"
code action is the same machinery: insert/update the `edition:` key and apply
the mechanical fixes (e.g. rewrite an affected `{...} {...}` to an explicit
shallow `merge(...)` where the author wants the *old* behaviour under the new
edition). As with `rustfix`, this will not be perfect — laziness and dynamic key
construction defeat static rewriting — and the migration UX should say so.

### 4. Deprecation lifecycle

Deprecation is how a Stable surface element reaches an edition boundary in good
order. The lifecycle for a prelude function, operator, or default behaviour:

1. **Annotate.** Mark the element with `deprecated:` metadata carrying a reason
   and a suggested replacement — a new symbol-keyed metadata field, no new
   syntax (`` ` { deprecated: "use `window-all`", replaced-by: :window-all } ``).
2. **Warn.** Using a deprecated element emits a `DiagnosticSeverity::WARNING`
   (CLI and LSP) — never an error under the default advisory policy
   (non-negotiable #3), so nothing that worked stops working. `eu check`
   reports it; `--strict` promotes it (`CHANGELOG.md` 0.6.0).
3. **Code-action.** The LSP offers a "replace with <replacement>" quick-fix,
   reusing `actions.rs`.
4. **Remove at an edition boundary only.** The element remains callable in every
   edition that predated its removal; it is absent only from the edition that
   removes it (and later). Removal that an *unmodified* file could observe is a
   MAJOR change; gated behind an edition it is a MINOR.

This ties deprecation timelines to editions rather than to wall-clock: an
element deprecated during the 2026 line is removable no earlier than the 2027
edition, giving users a full edition's notice and a mechanical path forward.

### 5. The 0.x → 1.0 gate

1.0 ships when the following are all true. Several are owned by sibling
proposals; this charter sets the bar, they do the work.

- **G1 — Editions live.** At least the `2025` baseline and one successor edition
  exist, the deep-merge default has *landed under the successor* edition (closing
  `docs/welcome/index.md:320`), and the "upgrade edition" code action ships.
- **G2 — Stable surfaces frozen and documented.** The table in §1 is ratified,
  the prelude tiering decided, and a `requires`-style declaration is the
  sanctioned way to pin a minimum `eu`.
- **G3 — Conformance bar met ([0003](0003-conformance-testing-fuzzing.md)).** A
  conformance suite pins the stable surfaces, including a *per-edition* golden
  corpus proving edition N output is unchanged by a binary that also supports
  N+1 — the executable form of the Rust invariant.
- **G4 — Type-system completeness ([0011](0011-typeclasses-without-classes.md),
  roadmap Stage A).** Stage A "close the existing system"
  (`type-system-evolution.md:1263-1282`) is delivered: the advisory checker is
  coherent enough that its annotation DSL can be declared Stable. 1.0 does **not**
  wait on Stage B/HKT — types are advisory, so an incomplete-but-sound checker
  does not block a language-stability promise.
- **G5 — Boundary policy fixed ([0002](0002-gradual-typing-boundary-policy.md)).**
  The answer to open-question 6 (silent / opt-in / mandatory boundary checks) is
  chosen and documented, because "what soundness 1.0 guarantees" is part of the
  commitment.
- **G6 — Performance & UX baselines
  ([0004](0004-compiled-unit-caching.md), [0005](0005-generational-gc.md)).** A
  published, regression-gated baseline (notably the ~500–700 ms cold-compile
  latency that 0004 targets) so 1.0 has a *floor* it will not silently drop
  below — without promising specific numbers (performance stays Not-covered as a
  *compatibility* surface).

## Interaction with the existing roadmap

This proposal is **process whitespace** — there is no roadmap entry for
stability. It depends on, and amplifies, the H18 per-file-profile idea
(`type-system-evolution.md:1148-1163`): editions and `type-extensions` share the
unit-metadata channel and should share a parser/representation. It is the
*precondition* the other Track-A proposals assume: [0002](0002-gradual-typing-boundary-policy.md)
and [0003](0003-conformance-testing-fuzzing.md) define *what* is guaranteed and
*how* it is tested; this defines the *contract and the lever* (editions) those
guarantees hang from. It supersedes nothing. It does not touch the
single-threaded lazy-pure runtime (non-negotiable #4) or ADR-001 — editions are
a front-end/desugar concern.

On **open-question 1** ("is 'no nominal' inviolable?", `type-system-evolution.md:1383`),
light touch only — [0011](0011-typeclasses-without-classes.md) owns it. The
stability angle: a 1.0 *export contract* (the shape a unit promises to render) is
a structural promise that structural typing already expresses. Were a small
opt-in nominal newtype ever admitted for export contracts, it would itself be an
edition-gated change — so editions *contain* that decision rather than give a
reason to pre-empt it here.

## Implementation sketch

Phased, front-end-only, low runtime risk.

1. **Edition plumbing (`src/core/`, ~M).** Read `edition:` from unit metadata in
   desugar; thread an `Edition` enum through the desugar/cook passes; default to
   `2025`; forward-reference error via the existing `VersionRequirementFailed`
   path. No STG, VM, GC, or type-checker changes.
2. **First edition delta (`src/core/desugar/`, ~M).** Implement deep-merge-as-
   default *gated on edition ≥ 2026* for block catenation lowering; leave 2025
   shallow. Re-home the 0.5.3 lookup change retroactively as a 2025-vs-pre rule
   if desired (it shipped without an edition; documenting it as the 2025 baseline
   costs nothing).
3. **Deprecation metadata (`lib/prelude.eu`, diagnostics, ~S).** `deprecated:` /
   `replaced-by:` fields + a warning when a flagged name resolves.
4. **LSP migration actions (`src/driver/lsp/actions.rs`, ~M).** "Upgrade unit to
   edition" and "replace deprecated" code actions, reusing the existing
   `WorkspaceEdit` machinery.
5. **Charter + conformance (`docs/`, [0003](0003-conformance-testing-fuzzing.md),
   ~M).** Write the stable-surface table as ratified policy; add the per-edition
   golden corpus.

Risk concentrates in (2): block-merge semantics are pervasive, and proving the
2025 path is bit-identical post-change is exactly what G3's corpus is for.

## Alternatives considered

- **CLI flag (`--edition 2026`).** Rejected: cannot vary per-file in a mixed
  repo; the artefact, not the invocation, must carry the contract (CUE's
  reasoning).
- **Newest-edition-by-default (CUE-style).** Rejected for eucalypt: with files
  already deployed, defaulting to *newest* changes their behaviour on a binary
  upgrade — the opposite of the goal. Default to *oldest*.
- **Flag day with a major bump.** Rejected: a synchronous output change for
  every unmodified file is the precise failure mode this charter exists to
  prevent.
- **Separate language-standard document (Dhall-style).** Deferred: valuable once
  there is a *second* implementation; overhead without payoff for one.
- **Do nothing; stay perpetually 0.x.** A legitimate option — but it forecloses
  the adoption that depends on a stability promise, and leaves the pending
  deep-merge change with no safe way to ship at all.

## Risks & what would kill this

- **Edition proliferation / fragmentation.** Too many editions, or large deltas,
  recreate the ecosystem split editions exist to prevent. Mitigation: editions
  are *rare* and *small*; an edition with no breaking delta is not minted.
- **Migration tooling underdelivers.** If laziness/dynamic keys defeat the code
  actions for common patterns, "automated migration" is hollow. Mitigation:
  measure code-action coverage on the conformance corpus as a G3 sub-bar; be
  honest, as RFC 2052 is, that it is best-effort.
- **The prelude freeze is too expensive.** If freezing ~200 functions blocks
  needed evolution, the promise becomes a millstone. Mitigation: the core/Unstable
  prelude tiering in §1; revisit at 1.1.
- **Premature 1.0.** Declaring stability before the surfaces are actually stable
  (e.g. while the type DSL is still churning) burns credibility. The G1–G6 gate,
  and especially G4's "Stage A done", is the guard.

Falsifier: if, on a representative corpus, the deep-merge edition delta cannot be
migrated mechanically for a clear majority of affected files, the
metadata-edition story is weaker than advertised and a flag day with a long
deprecation window should be reconsidered.

## Success criteria

- A binary upgrade across an edition boundary produces **byte-identical** output
  for every unit in the conformance corpus that does not opt in (G3 corpus
  green).
- The deep-merge default ships **without a single** unmodified-file behaviour
  change reported.
- "Upgrade unit to edition" mechanically migrates a clear majority of corpus
  files; the remainder get a precise, located diagnostic.
- Every Stable-tier element has a defined deprecation→removal path, and no
  Stable element is ever removed except across an edition.
- A published `1.0.0` (true semver, build metadata as `+build.N`) with a written
  charter a downstream user can rely on.

## References

**Eucalypt:** `docs/welcome/index.md:320` (pending deep-merge default);
`CHANGELOG.md:128-130` (0.5.3 lookup/scoping change); `build.eu:7-13,59-62` and
`.github/workflows/build-rust.yaml:391-402` (current version scheme & draft
release flow); `lib/prelude.eu:15-17` + `src/eval/stg/version.rs` +
`src/eval/error.rs:677` (`requires`/`__REQUIRES`/`VersionRequirementFailed`);
`src/driver/options.rs:85-105` (CLI surface); `src/wasm.rs` (embedding API);
`src/driver/lsp/actions.rs:27,86,151-165` (code-action infrastructure);
`docs/appendices/syntax-gotchas.md:439-457` (unit-metadata mechanics);
`docs/appendices/migration.md` (existing v0.2→v0.3 migration note);
`docs/development/type-system-evolution.md` §H18 (`:1148-1163`, per-file
profiles/extensions), §5 (`:1383`, open-question 1), Stage A (`:1263-1282`);
ADR-001 (`docs/development/architectural-decisions.md:6`).

**External:** [Semantic Versioning 2.0.0](https://semver.org/);
Rust RFC 2052, *Epochs/Editions*
(https://rust-lang.github.io/rfcs/2052-epochs.html) — opt-in, one-compiler-all-
editions, "do not split the ecosystem nor break existing code"; Rust RFC 1105,
*API evolution* (https://rust-lang.github.io/rfcs/1105-api-evolution.html) —
"all major changes are breaking, but not all breaking changes are major";
[The Rust Edition Guide](https://doc.rust-lang.org/edition-guide/editions/);
Dhall standard *versioning.md*
(https://github.com/dhall-lang/dhall-lang/blob/master/standard/versioning.md) —
MAJOR/MINOR/PATCH rules; whole-version interpretation;
[CUE language version](https://cuelang.org/docs/concept/cue-language-version/) —
per-module `language.version`, error on newer-than-evaluator.
