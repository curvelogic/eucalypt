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

**CI impact:** GitHub release tags use the base semver version only
(e.g. `0.8.0`). The `+build` metadata appears in `eu version` output
but is stripped from the tag — `+` characters break GitHub download
URLs and build numbers don't distinguish semantic versions.

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

### 4. Opt-in prelude versioning

Ship a **frozen v1** and an in-development **v2** as two embedded
resources. The prelude is already an embedded, swappable resource
(`src/driver/resources.rs:14-18`); two preludes is two `include_bytes!`
plus a selector.

**Selection:** Read `{ prelude: :v2 }` from unit metadata in the
loader (beside `import:`), default to v1.

**Cache key:** The prelude type cache (`PreludeSummary` /
`PRELUDE_CACHE`) must be keyed on the prelude selection, not just the
prelude bytes. (Noted for W6.)

**v1 is frozen:** Security-only fixes. Feature work happens in v2.

**The deep-merge-as-default change** (when it lands) ships as a MAJOR
or behind `prelude: :v2` — never as a silent default flip.

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
- Prelude versioning in `resources.rs`/`source.rs`/`check.rs` (Medium)
- Deprecation metadata + diagnostics + LSP quick-fix (Small)
- Tier table document (Small)

## Testing

- `eu version` outputs semver-compliant string with `+` build metadata
- `eu.requires(">=0.8")` works with `+build` metadata versions
- `{ prelude: :v2 }` selects the v2 prelude; default selects v1
- Two units with different prelude selections both work in the same
  import DAG
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
- No prelude v2 *content* — this deliverable creates the versioning
  mechanism; populating v2 is ongoing work across releases
