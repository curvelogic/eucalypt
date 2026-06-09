# W3: Versioning & Stability Discipline

**Bead:** eu-yhk0.1
**Target:** 0.8.0 (begin), 1.0 (freeze)
**Roadmap ref:** W3 (Section 7, ROADMAP.md)

## Problem

Eucalypt practises continuous delivery with a four-part build number
(`0.8.0.1685`); the version in Cargo.toml is human-set and carries no
compatibility meaning. A genuine breaking change shipped in
patch-looking 0.6.2 (the `cond` rewrite). There is no stability
discipline, no deprecation mechanism, and no way to evolve the prelude
without breaking existing files.

## Five Deliverables

### 1. Semver plumbing

**Current:** `build.eu` joins `[major, minor, patch, build_number]`
with `.` producing `0.8.0.1685`.

**Change:** Move build number to `+build.N` metadata per semver.org.

In `build.eu`, change `new-version` to:

```eu,notest
new-version:
  "{version.major}.{version.minor}.{version.patch}+{build.number str.of}"
```

The `semver` crate already ignores `+build` metadata during
`VersionReq::matches()`, so `eu.requires(">=0.8")` works unchanged.

**Field meanings** (from ROADMAP.md Section 4.1): **MAJOR** = a change
an unmodified file can observe in its output, or a removal from the
Stable surface. **MINOR** = backwards-compatible addition (including a
new opt-in prelude version). **PATCH** = no observable semantic change.

**CI impact:** GitHub release tag changes from `0.8.0.1685` to
`0.8.0+1685`. GitHub tags allow `+` characters.

**`eu.requires()` cleanup:** Remove the `.replace(".dev", "")` hack in
`version.rs:39` — `semver::Version` handles `+` metadata natively.

### 2. Stability tier table

Publish `docs/development/stability-policy.md` — a one-page statement
of what 1.0 commits to:

| Tier | Contents | Promise |
|------|----------|---------|
| **Stable** | Core syntax, prelude v1 API, block-merge/lookup semantics, CLI surface, import/export formats, WASM/embedding API, error codes/locations, exit codes | Won't break except in a MAJOR, with a deprecation path for the prelude |
| **Experimental** | Type-annotation DSL and checker | May change in a MINOR; opt-in/advisory |
| **Not covered** | Internal IR/STG/GC, `dump` output, exact error prose | No promise |

The type system being **Experimental** is what lets language + prelude
reach 1.0 without the type system being "done" (it is advisory, so it
need not be).

The tier table is **ratified at 0.8** and **frozen at 1.0**.

### 3. `requires` as a guard

The runtime path already exists (`__REQUIRES` / `semver::VersionReq`).
This deliverable is about making it real:

- **Document** the range-pin convention in the language reference.
- **Model** it in shipped library units (`lib/lens.eu`,
  `lib/state.eu`) so users see the pattern.
- **Optional hint:** `eu check` / LSP emits a "missing version pin"
  hint for units that import but don't `requires`.

### 4. Prelude selection mechanism

Build the **ability to specify and use an alternate prelude** — the
mechanism, not the content. The prelude is already an embedded,
swappable resource (`src/driver/resources.rs:14-18`); the missing
piece is a way for a unit to select which one.

**Selection metadata:** Read `{ prelude: <value> }` from unit metadata
in the loader (beside `import:`). The default (no metadata) uses the
built-in prelude as today. The exact vocabulary of `<value>` is TBD —
possible forms include a symbol (`:v2`), a file path, or a block with
options. For 0.8.0, supporting at least a symbol selector for an
alternative embedded prelude is sufficient.

**Cache key:** The prelude type cache (`PreludeSummary` /
`PRELUDE_CACHE`) must be keyed on the prelude selection, not just the
prelude bytes. (Noted for W6.)

**No second prelude ships yet.** The v1/v2 split happens at or near
1.0 when the prelude is frozen. This deliverable builds the
infrastructure so that transition is straightforward.

**The deep-merge-as-default change** (when it lands) ships as a MAJOR
or behind an alternate prelude — never as a silent default flip.

### 5. Deprecation lifecycle

Three metadata keys:
- `` ` :deprecated `` / `` ` { deprecated: true } `` — marks a
  declaration deprecated.
- `` ` { deprecated: "message" } `` — with an explanation.
- `` ` { replaced-by: "new-fn" } `` — pointer to the replacement
  (co-occurs with `deprecated:`).

**Metadata normalisation:** Handle `:deprecated` as a special symbol
in `normalise_metadata()` (same pattern as `:suppress`, `:trace`):

```rust
"deprecated" => core::block(
    *smid,
    [("deprecated".to_string(), core::bool(*smid, true))].iter().cloned(),
),
```

**Compile-time behaviour:** During desugaring, extract the
`deprecated` and `replaced-by` keys. During binding verification
(`src/core/verify/`), check references against the deprecated set and
emit warnings:

```
warning: 'old-fn' is deprecated: use new-fn instead
  --> file.eu:12:5
```

Warnings are **never errors under the advisory default**. Under
`eu check --strict`, deprecation warnings become errors.

**LSP quick-fix:** Reuse the existing `WorkspaceEdit` machinery
(`src/driver/lsp/actions.rs`) to offer a quick-fix that replaces a
deprecated name with its `replaced-by:` value.

**Lifecycle:** `deprecated:` + WARNING → LSP quick-fix → removal in a
MAJOR or simply omitted from prelude v2.

## Implementation Notes

All work is front-end/loader/docs — no runtime risk.

- `build.eu` + release flow (Small)
- `requires` docs + optional hint (Small)
- Prelude selection mechanism in `resources.rs`/`source.rs`/`check.rs` (Medium)
- Deprecation metadata + diagnostics + LSP quick-fix (Small)
- Tier table document (Small)

## Testing

- `eu version` outputs semver-compliant string with `+` build metadata
- `eu.requires(">=0.8")` works with `+build` metadata versions
- `{ prelude: <selector> }` metadata is read and changes which
  prelude is loaded; default (no metadata) uses the built-in prelude
- The prelude type cache is correctly keyed on the selection
- `` ` :deprecated `` emits a warning when the declaration is
  referenced
- `{ deprecated: "msg", replaced-by: "new" }` includes both in the
  warning
- `eu check --strict` fails on deprecation warnings
- LSP quick-fix replaces deprecated name with replacement

## Scope Exclusions

- No automatic migration tooling beyond the LSP quick-fix
- No `@since` version annotations
- No runtime deprecation warnings (deferred)
- No alternate prelude *content* ships yet — this deliverable builds
  the selection mechanism; the v1/v2 split happens at or near 1.0
