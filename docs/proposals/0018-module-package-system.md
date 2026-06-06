# 0018 — Module & package system: versioned, content-addressed, hermetic imports

- **Status:** Draft proposal for review
- **Track:** E — ecosystem, interop & safety
- **Classification:** Whitespace
- **Suggested horizon:** 1.0
- **Related:** sibling proposals [0001](0001-v1-charter.md) (editions &
  stability), [0010](0010-capability-determinism-types.md) (hermetic
  reproducibility), [0019](0019-host-language-interop.md) (host-language &
  schema interop); [0000](0000-priority-fixes.md) F3 (the Unit Interface —
  the export/link contribution) and [0004](0004-compiled-unit-caching.md) /
  [0014](0014-incremental-query-core.md) (the shared content hash);
  `docs/development/unit-visibility-spec.md`
  (`export: :internal`); ADR-001 (`docs/development/architectural-decisions.md`)

## Summary

Eucalypt's import system is file- and (notionally) git-based, with diamond
deduplication added in 0.5.3. That is enough for a single project laid out on
one author's disk. It is *not* enough for a 1.0 ecosystem in which authors
publish and consume each other's libraries: there is **no dependency
management, no dependency versioning, no namespace isolation beyond the
planned-but-unshipped `export: :internal`, and no story for trusting a
third-party import**. This proposal sequences a module/package system whose
1.0-essential core is small and conservative — **integrity-checked imports**
(Dhall's semantic-hash model), a **manifest plus lockfile expressed in
eucalypt's own block syntax**, and **namespace isolation** — followed by
**versioned dependencies** and an optional **hermetic mode**. A hosted
**registry is explicitly won't-do** — eucalypt has no ecosystem to centralise,
and a registry is a standing social-and-infra liability; **git repositories
(GitHub/GitLab) are the sole distribution mechanism**, made safe by content
hashes rather than by a trusted host. The thesis: integrity + manifest +
namespacing over plain git is the minimal cut that makes third-party `.eu`
libraries safe and reproducible — no registry, now or later.

## Motivation

### What the import system is today

Imports are resolved entirely at **compile time** by the `SourceLoader`, not
by any runtime IO. A relative string import is read from disk by
`read_fs_input` (`src/driver/source.rs:585`), which searches the lib-path then
falls back to baked-in resources; the importing file's own directory is pushed
onto the lib-path so resolution is relative to the importer
(`source.rs:259-270`). The import graph is a `petgraph` DAG built in
`src/syntax/import.rs`; cycles are rejected by `check_for_cycles`
(`import.rs:83-90`). **Diamond deduplication** (0.5.3, `CHANGELOG.md:108`) is
keyed on the *locator*: `encounter_input`/`find_index`
(`import.rs:109-123`) treat two imports of the same path as one node, so the
same file pulled in by two routes is desugared once. Named imports
(`cfg=config.eu`) contribute a single namespacing binding via `apply_name`
(`src/core/expr.rs:527`).

This is a clean, compile-time, deterministic model — a genuine asset. But it
is built for *local* code.

### Exactly what is missing for multi-author use

| Need | Status today | Evidence |
|---|---|---|
| **Versioning of dependencies** | none | `requires`/`__REQUIRES` pins the *binary* version, not deps (`lib/prelude.eu:15-17`, `src/eval/stg/version.rs`) |
| **Integrity** (this import is the bytes I reviewed) | none for FS; partial-but-**unshipped** for git | see below |
| **Namespace isolation** (a library's helpers don't leak) | partial; full form unshipped | named imports isolate; `export: :internal` is specced only (`docs/development/unit-visibility-spec.md`) |
| **Trust** (safely consume untrusted code) | none | no hash, no hermetic mode |
| **Dependency resolution** across a graph | none | `ImportGraph` records edges; nothing selects versions |

Two of these deserve correction of the record.

**Git imports are documented but not implemented.** The guide and reference
describe `{ git: …, commit: …, import: … }` (`docs/guide/imports-and-modules.md:183-207`, `docs/reference/import-formats.md:87-114`) and claim a pinned
`commit` makes the import "repeatable and cacheable". In the current code this
form does **nothing**: there is no `Git` variant in the `Locator` enum
(`src/syntax/input.rs:17`), no git dependency in `Cargo.toml`, no `git
clone`/`git2`/`gix` call anywhere in `src/`, and the import scraper
`scrape_rowan_imports` (`src/syntax/import.rs:259-293`) only matches string
literals (`Element::Lit`) and lists — a `{ git: … }` block is an
`Element::Block` and is silently ignored. There are no git-import harness
tests. So the *partial content-addressing* that a pinned commit SHA would give
is real in principle but **aspirational, not shipping**. (Remote `Url`
locators are likewise unwired in the loader — `source.rs` has no `Locator::Url`
arm.) This matters: the design below must decide whether to *finish* git
imports or to leapfrog them with content hashes; it argues the latter, with git
support as a fetch backend underneath a hash.

**`requires` is a version *assertion*, not dependency management.** It checks
the running `eu` against a semver constraint and raises
`VersionRequirementFailed` (`src/eval/error.rs:677`) otherwise. A file can say
"I need `eu >= 0.6`"; it cannot say "I depend on `acme/json-tools` at a version
compatible with 1.2". The intrinsic and its `semver` plumbing are nonetheless a
useful, *already-shipping* building block to reuse (it proves `semver` is
already a dependency and that constraint strings round-trip).

The strategic gap is therefore not "imports don't work" — they work well for
one repo — but "there is no safe, versioned way for one person's `.eu` to
depend on another's." For a tool whose artefacts are checked into other
people's CI ([0001]'s framing), that is the missing half of the ecosystem.

## Prior art & landscape

### Dhall — semantic integrity hashes (borrow the model)

Dhall lets you "'freeze' the imported expression, pinning its value with a
cryptographic hash" (SHA-256), after which "an import frozen in this way can
never successfully return a different expression." Crucially the hash is
**semantic, not textual**: "Dhall's integrity checks are 'semantic' integrity
checks, meaning that they are hashes of an expression's normal form", so
reformatting or refactoring a dependency without changing its meaning does not
break the hash. The security claim is sharp and exactly what a config tool
needs: against a malicious provider "the worst they can do is cause the program
to fail loudly if they tamper with the import in any way" — they cannot serve a
function "with the same type but different behavior." And the hash doubles as a
cache key: "when you protect an import with a semantic integrity check the
import is permanently locally cached after the first request."

**Borrow:** the freeze-and-verify discipline, and the hash as the trust
boundary. **Don't borrow wholesale:** Dhall hashes a *β-normal form*, which
presupposes strong-normalisation; eucalypt is lazy and deliberately non-total
(`io.shell` is in scope), so a semantic hash is not available for a general
expression. The honest adaptation is a **source-content hash** (hash the
canonical parsed-and-pretty-printed form of the imported unit): integrity and
caching, but *not* Dhall's insensitivity to whitespace. An acceptable 1.0
trade; a semantic variant could follow for the pure subset.

### Go modules — registry-free, VCS-direct: MVS + `go.sum` (the primary model)

Go's design (Russ Cox, "Go & Versioning", research.swtch.com, 2018) is the
reference for *boring, reproducible* dependency management. **Minimal Version
Selection (MVS)** "always selects the minimal (oldest) module version that
satisfies the overall requirements of a build", so "the release of a new
version has no effect on the build" — there is no SAT-style solver and builds
are stable by construction. **`go.sum`** records cryptographic hashes of each
dependency's content and `go.mod`, verified on download; the **checksum
database** (`sum.golang.org`) is a global transparency log so everyone observes
the same hash for a given version; **`GOPROXY`** decouples fetching from the
origin VCS.

**Borrow — and Go is the *primary* model precisely because it is
registry-free:** a module *is* its VCS path, fetched **directly from
GitHub/GitLab**; there is no central package registry. Take MVS's philosophy
(deterministic, no surprise upgrades), the `go.sum`-style lockfile as the
integrity ledger, and the VCS-direct fetch. **Reject outright** (not defer):
the module proxy and the global checksum-transparency-log — *registry-scale*
services eucalypt will not run. Git plus the lockfile's hashes give the
integrity a transparency log would attest, without the service.

### Unison Share — content-addressed code (instructive, not adoptable)

Unison identifies every definition by the hash of its implementation; code is
stored as hashed ASTs, so there are "no builds, no dependency conflicts", two
versions of a type coexist, and `lib.install` pulls a release from Unison
Share. The radical end-state: conflicts cease to exist because names are not
the identity. **Instructive** — but **not adoptable**, since it requires a
codebase database and a different editing model (`ucm`) that collides with
eucalypt's plain-`.eu`-files-on-disk identity. We take the *idea*
(hash-as-identity for integrity) without the *codebase-manager*.

### Starlark / Bazel & Nix/Nickel (boundary references)

Starlark achieves hermeticity by **removing** ambient capability: `load()`
imports are pure and a Starlark program cannot touch the filesystem or network
([0010] covers this). Eucalypt keeps `io.shell`, so it cannot copy that
"remove the capability" stance — but Starlark's *goal*, fully-declared build
inputs, is what the hermetic mode below targets. Nix (with Nickel as its config
language) makes the *whole world* content-addressed and builds in a sandbox:
the maximalist position, far beyond a 1.0 config tool, but the existence proof
that "declared inputs + content addressing → reproducibility" scales.

## Proposed design

Staged, each stage independently shippable, each living in **metadata, symbols,
blocks and strings** — no new syntax (non-negotiable #1). Dependencies are
*data*, exactly as `import:` already is.

### (a) Integrity hashes on imports — the cheapest high-value win (1.0)

Extend the existing import metadata with an optional `sha256:` (a string).
A hashed import is verified at load time; on mismatch the load fails loudly.

```eu,notest
{ import: { from: "acme/json-tools.eu"
            sha256: "e3b0c44298fc1c149afbf4c8996fb924..." } }
```

Mechanically: `scrape_rowan_imports` (`src/syntax/import.rs:259-293`) is taught
to read the **block** form (today it ignores `Element::Block`), extracting the
target and the optional hash into the `Input`. The `SourceLoader`, after
reading the bytes in `read_fs_input` (`source.rs:585`), canonicalises (parse →
pretty-print to a stable form) and hashes; a present `sha256:` that disagrees
raises a new `EucalyptError`. An `eu freeze <file>` subcommand (mirroring
`dhall freeze`) rewrites bare imports to hash-pinned ones. This is the
Dhall-style "pin + verify content hash" applied to the bytes; it gives
tamper-evidence and a content-addressed cache key with no version solver and no
registry. It is the highest safety-per-line change available.

**Scope of the hash — per-file, so transitivity is an open sub-decision.**
A `sha256:` hashes *one unit's* canonical source: tamper-evidence for that
file, but **not** for what it transitively imports. Dhall's guarantee is
transitive only because its hash is *semantic*, over the fully-resolved
normal form; a per-file source hash is not — a pinned `json-tools.eu` that
does `import: "helper.eu"` leaves `helper.eu` unverified, so an author
pinning only direct imports has a shallow guarantee. End-to-end integrity
needs one of two routes (not mutually exclusive), and this proposal must
pick:

- **(i) Merkle hash over the import DAG** — a unit's hash folds in its
  imports' hashes, so one pin covers the whole closure (Dhall-like;
  `eu freeze` walks the already-acyclic, diamond-deduped DAG). One opaque
  number, re-frozen on any deep change.
- **(ii) closure-complete lockfile** — every unit in the closure carries its
  own `sha256:` in (b), Go/Cargo-style: an explicit, diffable ledger; then
  (a)'s inline pin is per-file convenience and the *security* boundary is the
  lockfile.

This bears directly on the '(a)+(b)+(c) minimal safe cut' below: whichever
route is chosen is what actually carries transitivity — `(a)`'s inline pins
alone do not.

### (b) Manifest + lockfile in eucalypt block syntax (1.0)

Declare a project's dependencies as **a eucalypt block** — deps are data, and
eucalypt already reads its own blocks. A manifest `project.eu` (conventional
name; nothing magic) contributes a `__deps`-style block that the driver reads
*before* assembling the import graph:

```eu,notest
` { target: :package }
package: {
  name:    "my-config"
  edition: "2026"               # ties to [0001]
  requires: ">=0.7"             # the binary pin, via existing __REQUIRES
  deps: {
    json-tools: { from: "git:github.com/acme/json-tools"
                  version: "^1.2"
                  sha256:  "…" }
    aws:        { from: "aws/cloudformation.eu" }   # vendored / local
  }
}
```

The **lockfile** is the same shape, fully resolved and committed: every dep
pinned to an exact commit/path *and* a `sha256:`. It is a generated `.eu`
block — readable, diffable, parseable by eucalypt itself — so no new format is
introduced, honouring syntactic conservatism completely. The driver resolves
`deps` names to import roots, so `{ import: "json-tools" }` in source resolves
through the manifest rather than the bare lib-path.

**Discovery — how the driver finds the manifest when you run an arbitrary
file.** This is the project-root problem (`Cargo.toml` / `go.mod` /
`package.json` all solve it). For `eu sub/foo.eu`, the driver searches
**upward from the invoked file's directory** for the conventional manifest
name (`project.eu`), falling back to the CWD for stdin / `-e` — anchored on
the *file* because `eu` is invoked with explicit paths and already resolves
imports importer-relative (`source.rs:259-270`). The first manifest found
defines the project root; its `deps` govern named-import resolution.
Crucially, this **degrades to today's behaviour**: with no manifest in the
ancestry, resolution is exactly the current lib-path/relative model — so a
one-off `.eu` in a folder with no project runs unchanged, and the manifest
takes effect only when present. An explicit `--manifest <path>` overrides
discovery, and `--hermetic` (e) *requires* a manifest and makes it the
closure root. The convention is opt-in: no filename is load-bearing until a
project chooses to have one.

### (c) Namespace isolation — building on `export: :internal` (1.0)

A library that exposes a curated surface and hides its helpers is the
precondition for *anyone* depending on it without namespace pollution — the
exact problem `unit-visibility-spec.md` opens with. The per-declaration
`export: :internal` mechanism specified there — a binding that stays live
within its unit but is masked from importers/mergers and from rendered output,
via a position-preserving capture mask in `rebody_int` (`src/core/expr.rs:548`)
— is **the namespace-isolation primitive this proposal needs**, and this
proposal is a primary motivation for shipping it. On top of it, a manifest may
declare its public surface as an explicit allowlist, so "what this package
exports" is a reviewable data declaration, not an accident of which helpers
clash. Named imports (`cfg=config.eu`) remain the per-import namespacing tool;
`:internal` is the per-*library* one. No new syntax: one metadata value plus a
manifest key. This package export surface is also exactly the *Link*
contribution in [0000](0000-priority-fixes.md) F3's Unit-Interface table
(exported binding names → slots): the allowlist is what a unit *publishes* —
the same data a separate-compilation interface records.

### (d) Versioned dependencies — start minimal (1.0-if-affordable, else 1.1)

Begin with the **least machinery that is still real**: a dep is a git ref (or
path) + a `sha256:` + an optional `version:` constraint string, reusing the
**already-shipping `semver` plumbing** behind `__REQUIRES`
(`src/eval/stg/version.rs`) to parse and match constraints. Resolution uses
**MVS**, not a solver: across the dependency graph, for each module pick the
*lowest* version satisfying all stated constraints, exactly Go's rule — which
needs only a graph walk over `ImportGraph` and gives stable, surprise-free
builds. The lockfile records the outcome. This deliberately omits SemVer
*range solving* with backtracking; if two deps demand incompatible majors, the
tool reports it rather than searching. Honest scope: even "minimal" versioning
needs a fetch-and-cache step for git refs — but that **git-fetch backend is
restored ahead of (d) as the prioritised regression-fix [0000] F5** (the docs
already promise git imports; they worked in Haskell, PR #115, and were dropped
in the rewrite), already content-hash-verified per (a). So (d) layers MVS over
a backend that already exists; the residual risk that may slip to 1.1 is the
version *solving*, not the fetch.

### (e) Hermetic mode — declared inputs only (1.0 flag, ties to [0010])

A `--hermetic` flag (and a `hermetic: true` manifest key) makes the build
**refuse any input not reachable from the manifest**: no ambient lib-path
fallback, no resource fallback, no undeclared filesystem read, and — composed
with [0010]'s `--require-deterministic` — no ambient `io.env`/clock/`io.shell`.
The two proposals are complementary: 0018 closes the *import* graph (every byte
that enters the build is declared and hashed); [0010] closes the *capability*
graph (no non-deterministic runtime value). A CI job that sets both can make
the strong claim "this output is a pure function of this manifest and this `eu`
binary" — the supply-chain property [0010] §Summary motivates. Implementation
is mostly *subtraction* in the `SourceLoader` resolution path (disable the
fallbacks in `read_fs_input`/`resolve_fs_path` under the flag), so it is cheap.

### No registry — git repositories are the distribution mechanism

Eucalypt will **not** build a hosted registry — not in 1.0, not later. There
is no ecosystem to centralise, and a registry (discovery, publication, a
`sum.golang.org`-style transparency log, an `eu add …` UX) is a *standing
social and infrastructural liability* — hosting, naming policy, moderation,
availability SLAs, security response — irreversible in a way language features
are not. **Distribution is plain git: GitHub/GitLab (or any) repositories,
referenced by URL + ref, made safe by content hashes — not by trusting a
host.** This is Go's registry-free, VCS-direct model (above), and it suffices:
integrity hashes make *any* git fetch tamper-evident (per unit, and
transitively via the (a) decision), and the manifest/lockfile (b) makes the
dependency set explicit and reproducible. The lockfile already carries every
hash a transparency log would attest — so the safety a registry would add is
obtained without the service. The complete distribution story is therefore
**(a) integrity + (b) manifest/lockfile + (c) namespacing, over git** — and
that is the whole of it, not a stepping stone to a registry.

## Interaction with the existing roadmap

This is **whitespace** — the README names "no package/module system beyond file
imports" as a top-tier 1.0 gap (`docs/proposals/README.md:40`).

- **[0001] (editions & stability).** The manifest's `edition:` and `requires:`
  keys are the *same* unit-metadata channel 0001 defines; a lockfile pins not
  just dep versions but the *edition* each dep is interpreted under, which is
  precisely 0001's "whole-unit, never mixed" rule applied across a dependency
  graph. Dependency versioning and the stability/editions charter are two faces
  of one promise: 0001 governs how *one* unit evolves compatibly; 0018 governs
  how a *graph* of units pins what it depends on. They must share the
  metadata-schema parser.
- **[0010] (capability/determinism).** Hermetic mode (e) is the import-graph
  half of the reproducibility story; [0010]'s **hermetic mode** (capability
  interception — its capability *types* were dropped as won't-do) is the
  capability-graph half. 0010 already names 0018 as its complement; this
  proposal reciprocates. Neither subsumes the other.
- **[0004] (compiled-unit caching) and [0014] (query core).** 0004 already
  points at 0018 (`0004-…:167`): a content hash (a) is the natural,
  collision-free cache key for a compiled unit; [0014]'s query graph keys on
  the same content hash. Integrity (0018), the compile cache (0004), and the
  query keys (0014) all want the *same* digest — one shared canonicalisation,
  whose substrate is `eu fmt`'s reformat-**idempotence**
  (`src/syntax/export/format.rs`). Get it byte-stable once, for all three.
- **[0019] (host-language interop).** Schema/CRD ingest produces `.eu`
  artefacts that want to be *published* like any library; the manifest format
  is what they would be published *as*.

It supersedes the documentation-only git-import promise by **subsuming** it:
git becomes one fetch backend beneath a hash, rather than a standalone trusted
mechanism. It touches neither the runtime (non-negotiable #4) nor ADR-001 —
this is a front-end / driver concern.

## Implementation sketch

Front-end and driver only; no STG/VM/GC/type-checker change.

1. **Integrity (a) — `src/syntax/import.rs`, `src/driver/source.rs`, ~M.** Teach
   `scrape_rowan_imports` the block form (read `from:`/`sha256:`); add a
   canonicalise-and-hash step in the loader; new `EucalyptError::IntegrityMismatch`;
   `eu freeze` subcommand. Low risk, high value. **The 1.0 minimum starts here.**
2. **Manifest/lockfile (b) — `src/driver/`, ~M.** Parse a `:package` block;
   resolve `deps` names to import roots ahead of `ImportGraph` construction;
   read/write the lockfile as a generated `.eu` block. New CLI: `eu lock`.
3. **Namespacing (c) — depends on `export: :internal` landing**
   (`unit-visibility-spec.md`); add a manifest export-allowlist consult. ~S on
   top of that spec.
4. **Versioning (d) — `src/syntax/import.rs` + a fetch/cache module, ~L.** MVS
   over the import graph reusing `semver` from `version.rs`; a git/tarball
   fetch-and-cache directory. The heaviest stage; candidate to slip to 1.1.
5. **Hermetic (e) — `src/driver/source.rs`, ~S.** Gate the lib-path/resource
   fallbacks (`read_fs_input`, `resolve_fs_path`) behind `--hermetic`; compose
   with [0010]'s flag.
6. **Registry — post-1.0, separate effort.** Not sized here.

Risk concentrates in (d)'s fetch/cache (network, on-disk cache invalidation)
and in getting canonicalisation stable enough that a hash is reproducible
across platforms — the same problem (a) and [0004] both depend on, so it is
worth solving once, carefully.

## Alternatives considered

- **Finish git imports as specced, stop there.** Rejected as the *end*: a
  pinned commit gives repeatability but no integrity against a force-pushed tag
  or a compromised host, no isolation, and no version resolution. It is a fetch
  backend, not a package system. (It should still be *finished*, beneath a hash.)
- **Adopt Unison-style content-addressing wholesale.** Rejected: requires a
  codebase database and a non-text editing model incompatible with eucalypt's
  plain-`.eu`-files identity.
- **Full SemVer range solver (npm/Cargo style).** Rejected for 1.0 in favour of
  MVS: solvers bring backtracking, lockfile churn, and "minimal version
  selection avoids" the surprise-upgrade failure mode. MVS is a graph walk.
- **A registry, ever.** Won't-do (not merely "not first"): largest cost,
  irreversible, and unnecessary — git repos + integrity + manifest/lockfile
  deliver trust and reproducibility with no host to run. Eucalypt has no
  ecosystem to justify centralising, and would not take on the standing
  liability if it did.
- **Do nothing (status quo file/git imports).** Forecloses a library ecosystem
  and leaves third-party imports untrustworthy at exactly the moment 1.0 invites
  people to depend on them.

## Risks & what would kill this

- **Canonicalisation instability.** If the parse→pretty-print hash is not
  byte-stable across platforms/versions, integrity (a) is worthless. Mitigation:
  hash a defined canonical form and pin it in the conformance corpus ([0003]);
  share the digest with [0004].
- **Scope creep into a registry.** The gravity toward "just build the registry"
  is strong; succumbing turns a 1.0 feature into a multi-year service. The
  discipline is to ship (a)+(b)+(c) and *stop*.
- **Low demand.** If real users vendor everything and never consume third-party
  `.eu`, the versioning machinery (d) is over-built. The probe is cheap: ship
  integrity + manifest first; build versioning only when a real multi-author
  dependency appears. (This mirrors [0010]'s "lint first, types later" gate.)
- **Semantic vs source hash confusion.** Users may expect Dhall's
  whitespace-insensitive semantic hash; ours is source-content. Document the
  difference; consider a semantic hash for the pure subset later.

Falsifier: if, on a representative multi-author corpus, source-content hashing
churns on benign reformatting badly enough that authors disable it, the
integrity model needs the heavier semantic-hash route before 1.0 — and the
"cheapest high-value win" framing is wrong.

## Success criteria

1. **Integrity shipped by 1.0:** a hash-pinned import that has been tampered
   with fails loudly — **including a tampered *transitive* import** (via the
   Merkle root or the closure-complete lockfile, per the (a) sub-decision); an
   untampered one loads and populates a content-addressed cache; `eu freeze`
   round-trips on the harness corpus.
2. **Manifest/lockfile shipped:** a project with a `:package` manifest resolves
   and pins its deps; the lockfile is a committable `.eu` block; re-resolution
   with no change is a no-op (byte-identical lockfile).
3. **Namespacing:** an importer of a packaged library sees only its declared
   surface; `:internal` helpers are unreachable (the `unit-visibility-spec.md`
   test plan, exercised through a *package* import).
4. **Hermetic:** `--hermetic` rejects an undeclared filesystem read; composed
   with [0010]'s flag, a build is provably a function of manifest + binary.
5. **Versioning (if in 1.0, else 1.1):** MVS selects the minimal satisfying
   version across a two-level dependency graph; a conflicting-major graph is
   reported, not silently resolved.
6. **No registry** — git (GitHub/GitLab) plus the lockfile is the complete
   distribution story; the lockfile carries every hash a transparency log would
   attest, proving a registry is unnecessary, not merely deferred.

## References

**Eucalypt files cited:**

- `src/driver/source.rs:585` (`read_fs_input` — compile-time FS read),
  `:259-270` (importer-relative resolution), `:223-239` (`resolve_fs_path`),
  `:382-440` (`translate`; no `Locator::Url` arm)
- `src/syntax/import.rs:109-123` (locator-keyed diamond dedup),
  `:83-90` (cycle rejection), `:259-293` (`scrape_rowan_imports` — string/list
  only; ignores `Element::Block`)
- `src/syntax/input.rs:17` (`Locator` enum — no `Git` variant),
  `:210-226` (`Input::from_str` name/format parsing)
- `lib/prelude.eu:15-17` (`requires: __REQUIRES`),
  `src/eval/stg/version.rs:14-57` (`Requires` — pins the *binary* version),
  `src/eval/error.rs:677` (`VersionRequirementFailed`)
- `src/core/expr.rs:527` (`apply_name` — named-import namespacing),
  `:548` (`rebody_int` — the splice point `export: :internal` masks)
- `docs/development/unit-visibility-spec.md` (`export: :internal` design)
- `docs/guide/imports-and-modules.md:183-207`,
  `docs/reference/import-formats.md:87-114` (git imports — **documented, not
  implemented**)
- `CHANGELOG.md:108` (0.5.3 diamond-import dedup)
- `Cargo.toml` (no git dependency; `semver` present via `__REQUIRES`)
- sibling proposals `0001-v1-charter.md`,
  `0010-capability-determinism-types.md:284-292`,
  `0004-compiled-unit-caching.md:167`, `0019-host-language-interop.md`

**External references:**

- Dhall, *Safety guarantees* — semantic integrity hashes, `dhall freeze`,
  "the worst they can do is cause the program to fail loudly", hash-keyed cache.
  https://docs.dhall-lang.org/discussions/Safety-guarantees.html
- G. Gonzalez, *Semantic integrity checks are the next generation of semantic
  versioning* (2017).
  https://www.haskellforall.com/2017/11/semantic-integrity-checks-are-next.html
- R. Cox, *Minimal Version Selection* / *Go & Versioning* (2018) —
  "selects the minimal (oldest) module version that satisfies the overall
  requirements"; "release of a new version has no effect on the build".
  https://research.swtch.com/vgo-mvs
- *Go Modules Reference* — `go.sum`, MVS, module proxy, checksum database.
  https://go.dev/ref/mod
- Unison, *The big idea* (content-addressed code; no dependency conflicts) and
  *Unison Share* / `lib.install`.
  https://www.unison-lang.org/docs/the-big-idea/
- Starlark — hermeticity & pure `load()` (boundary reference; see [0010]).
  https://github.com/bazelbuild/starlark/blob/master/design.md
