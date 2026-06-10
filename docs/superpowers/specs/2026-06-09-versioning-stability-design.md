# W3: Versioning & Stability Discipline (0.8.0 Scope)

**Bead:** eu-yhk0.1
**Target:** 0.8.0

## Problem

Eucalypt uses a four-part build number (`0.8.0.1685`) that is not
valid semver. A breaking change shipped in patch-looking 0.6.2 with
no mechanism to detect or prevent it. There is no deprecation
mechanism, no stability policy, and no way to evolve the prelude
without breaking existing files.

## Deliverable 1: Semver-compliant build versioning

### Current state

`build.eu` (lines 59-62) constructs the version:

```eu,notest
new-version: [version.major,
              version.minor,
              version.patch,
              build.number] map(str.of) str.join-on(".")
```

This produces `0.8.0.1685`. The `semver` crate rejects this as
invalid.

`version.rs:39` works around a `.dev` suffix:
```rust
let version_str = env!("CARGO_PKG_VERSION").replace(".dev", "");
```

### Changes

**File:** `build.eu` (line 59-62)

Replace `new-version` with:
```eu,notest
new-version:
  "{version.major}.{version.minor}.{version.patch}+{build.number str.of}"
```

This produces `0.8.0+1685` — valid semver where `+1685` is build
metadata (ignored by `VersionReq::matches()`).

**File:** `src/eval/stg/version.rs` (line 39)

Remove the `.replace(".dev", "")` hack. `CARGO_PKG_VERSION` is
`0.8.0` in Cargo.toml (no suffix). In CI builds where
`build-meta.yaml` carries the `+N` version, `semver::Version::parse`
handles `+` metadata natively.

**File:** `.github/workflows/build-rust.yaml`

The release tag currently uses the four-part version string. Verify
that the `+` character works in GitHub release tags (it does — GitHub
URL-encodes it as `%2B` in URLs but accepts it in the API).

### Acceptance criteria

1. `eu version` outputs a string matching `\d+\.\d+\.\d+(\+\d+)?`.
2. `eu.requires(">=0.8")` passes against `0.8.0+1685`.
3. `eu.requires("=0.8.0")` passes against `0.8.0+1685` (build
   metadata is ignored per semver).
4. CI produces a GitHub release with a `+N` tag.
5. The `.replace(".dev", "")` hack is gone.

## Deliverable 2: Stability tier table

### Changes

**New file:** `docs/development/stability-policy.md`

A one-page document defining three tiers:

| Tier | Contents | Promise |
|------|----------|---------|
| **Stable** | Core syntax, prelude API, block-merge/lookup semantics, CLI surface, import/export formats, error codes/locations, exit codes | Won't break except in a MAJOR, with deprecation path |
| **Experimental** | Type-annotation DSL and type checker | May change in a MINOR; opt-in/advisory |
| **Not covered** | Internal IR/STG/GC, `dump` output, exact error prose | No promise |

The document also states the semver field meanings:
- **MAJOR**: a change an unmodified file can observe in its output, or
  a removal from the Stable surface.
- **MINOR**: backwards-compatible addition.
- **PATCH**: no observable semantic change.

This is ratified at 0.8.0. The enumerated surface is frozen at 1.0.

### Acceptance criteria

1. `docs/development/stability-policy.md` exists and is linked from
   the main documentation.
2. The tier table covers syntax, prelude, CLI, exports, types, and
   internals.

## Deliverable 3: `requires` documentation and modelling

### Current state

The runtime mechanism works: `__REQUIRES` in `version.rs` parses
`semver::VersionReq` and checks against `CARGO_PKG_VERSION`. The
`eu.requires(constraint)` prelude function calls it. But no shipped
library uses it, and it is not documented as a convention.

### Changes

**File:** `docs/reference/agent-reference.md` (or appropriate guide
chapter)

Document the convention: every `.eu` file that depends on specific
language features should include `eu.requires(">=0.8")` (or
appropriate range) near the top.

**Files:** `lib/lens.eu`, `lib/state.eu`, `lib/markup.eu`

Add `eu.requires(">=0.8")` to the shipped library units as exemplars.
These are the files users will see when looking for the pattern.

### Acceptance criteria

1. `lib/lens.eu`, `lib/state.eu`, and `lib/markup.eu` each call
   `eu.requires(">=0.8")`.
2. Running them on 0.7.x fails with `VersionRequirementFailed`.
3. The convention is documented in the reference.

## Deliverable 4: Prelude selection mechanism

### Current state

The prelude is an embedded resource loaded by name:

```rust
// src/driver/resources.rs:15-16
("prelude".to_string(), include_bytes!("../../lib/prelude.eu").to_vec()),
```

`SourceLoader::load_source()` (`src/driver/source.rs:342-351`)
dispatches on `Locator::Resource(name)`. The prelude type cache
(`PRELUDE_CACHE` in `check.rs:44`) is a `OnceLock<UnitInterface>` —
exactly one prelude, cached forever.

There is no mechanism for a unit to specify an alternative prelude.

### Changes

**File:** `src/core/metadata.rs`

Add `prelude` as a recognised **block-level** metadata key in
`DesugarPhaseBlockMetadata` (lines 161-169). Extract it as
`Option<String>` (the symbol name, e.g. `"v2"` or a file path).

Strip the key after extraction.

**File:** `src/driver/source.rs`

After parsing the unit's metadata block (the first block in a file,
which already provides `import:` and `doc:`), read the `prelude` key.
If present, use it to select which resource to load as the prelude.

For 0.8.0, support two forms:
- A symbol: `{ prelude: :name }` → look up `"name"` in `Resources`.
- A file path: `{ prelude: "path/to/prelude.eu" }` → load from
  filesystem.

If no `prelude:` metadata, use `"prelude"` (the default).

**File:** `src/driver/resources.rs`

No change to the resource map itself — it still embeds only the
default prelude. Alternative preludes are loaded from the filesystem
or, in future, from additional embedded resources.

**File:** `src/driver/check.rs`

The `PRELUDE_CACHE` (`OnceLock<UnitInterface>`, line 44) can only
hold one prelude. Change to a `Mutex<HashMap<String, UnitInterface>>`
(or `OnceLock<HashMap<...>>` built lazily) keyed on the prelude
identifier (resource name or file path hash).

`get_or_build_prelude_interface()` (lines 52-63) takes a prelude
identifier parameter and looks up or builds the interface for that
specific prelude.

### Acceptance criteria

1. A file with no `prelude:` metadata loads the default prelude
   (no regression).
2. A file with `{ prelude: "tests/fixtures/alt-prelude.eu" }` loads
   that file as its prelude instead of the built-in one.
3. A file with `{ prelude: :nonexistent }` produces a clear error
   message.
4. Two files with different `prelude:` selections can coexist in the
   same `eu` invocation (e.g. via imports) without cache corruption.
5. `eu check` works correctly with alternative preludes (the type
   cache is keyed per prelude).

## Deliverable 5: Deprecation metadata

### Current state

There is no deprecation mechanism. Type warnings exist
(`src/core/typecheck/error.rs:13-24`, `TypeWarning`) and are rendered
with `DiagnosticSeverity::WARNING`. The binding verification pass
(`src/core/verify/content.rs`) collects `CoreError`s (hard errors),
not warnings. `eu check --strict` turns type warnings into exit
code 1 (`check.rs:192`).

### Changes

**File:** `src/core/metadata.rs`

Add `deprecated` as a recognised declaration metadata key in
`DesugarPhaseDeclarationMetadata`. Add `:deprecated` as a special
symbol in `normalise_metadata()` (alongside `:suppress`,
`:internal`, `:target`, `:trace`):

```rust
"deprecated" => core::block(
    *smid,
    [("deprecated".to_string(), core::bool(*smid, true))].iter().cloned(),
),
```

Also recognise `replaced-by` as a metadata key (string value).

Extract both via `ReadMetadata`, producing:
```rust
pub struct DeprecationSpec {
    pub message: Option<String>,   // from { deprecated: "msg" }
    pub replacement: Option<String>, // from { replaced-by: "new-fn" }
}
```

Strip both keys after extraction.

**File:** `src/core/desugar/rowan_ast.rs`

During declaration processing (where `record_doc()`, `record_target()`
etc. are called), extract `DeprecationSpec` and store it in a new
`HashMap<String, DeprecationSpec>` on the `Desugarer` struct (same
pattern as `docs`, `targets`, `monad_registry`).

**File:** `src/core/verify/content.rs` (or a new
`src/core/verify/deprecation.rs`)

Add a deprecation check pass. After desugaring, walk the core
expression looking for references to deprecated names. For each
reference, emit a warning.

The warning type reuses `TypeWarning` (which already renders as
`DiagnosticSeverity::WARNING`) or introduces a parallel
`DeprecationWarning` type with the same rendering. The message format:

```
warning: 'old-fn' is deprecated
  --> file.eu:12:5
  = help: use 'new-fn' instead
```

The `help:` line appears only when `replaced-by:` is present.

**Integration with `--strict`:**

**File:** `src/driver/check.rs`

The existing logic (lines 188-202) counts type warnings and sets
exit code 1 under `--strict`. Deprecation warnings feed into the same
count. No special handling needed — they are warnings, and `--strict`
makes all warnings into failures.

**File:** `src/driver/lsp/actions.rs`

Add a quick-fix code action for deprecation warnings. When the cursor
is on a deprecated name and `replaced-by:` metadata exists, offer a
`CodeAction` with `CodeActionKind::QUICKFIX` that replaces the name
with the replacement. Use the existing `TextEdit` / `WorkspaceEdit`
pattern (modelled on `collect_qualify_name_actions()`, lines 857-902).

The quick-fix is triggered by matching the diagnostic source (e.g.
`"eucalypt-deprecation"`) and extracting the replacement from the
diagnostic's data or related information.

### Acceptance criteria

1. `` ` :deprecated f(x): x `` — referencing `f` produces a warning
   on stderr (non-strict mode: exit 0; strict mode: exit 1).
2. `` ` { deprecated: "use g instead" } f(x): x `` — warning
   includes the message text.
3. `` ` { deprecated: "use g", replaced-by: "g" } f(x): x `` —
   warning includes `help: use 'g' instead`.
4. `eu check --strict` fails (exit 1) when deprecated names are
   referenced.
5. `eu check` (non-strict) succeeds (exit 0) with deprecation
   warnings on stderr.
6. LSP offers a quick-fix to replace `f` with `g` when
   `replaced-by: "g"` is present.
7. Deprecated declarations still **work** — deprecation is a warning,
   never a hard error. The declaration's value is unaffected.
8. Harness test covering all forms: bare `:deprecated`, with message,
   with `replaced-by`.

## Scope Exclusions

- No alternate prelude *content* — the selection mechanism is built
  but no second prelude ships.
- No `@since` annotations.
- No runtime deprecation warnings (all checking is compile-time).
- No `eu check` hint for missing `requires` pins (deferred).
- No automatic migration tooling beyond the LSP quick-fix.
