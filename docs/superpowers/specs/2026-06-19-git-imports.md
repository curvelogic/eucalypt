# eu-9tah.3 (W12): Restore Git Imports

**Date**: 2026-06-19
**Bead**: eu-9tah.3
**Agent**: Lantern
**Parent spec**: `2026-06-19-0.10.0-release-design.md`

## Motivation

Git imports were available in the original Haskell eucalypt but were dropped
during the Rust rewrite. The syntax is documented in `docs/reference/
import-formats.md` (lines 87-119) and `docs/guide/imports-and-modules.md`
(lines 182-213). Currently, `{ git: ... }` import blocks are silently ignored
because the import parser only processes string literals.

Restoring git imports enables sharing eucalypt libraries across projects via
git repositories with commit-pinned reproducibility.

## Documented Syntax (Restoring Prior Behaviour)

```text
{ import: { git: "https://github.com/gmorpheme/eu.aws"
            commit: "0140232cf882a922bdd67b520ed56f0cddbd0637"
            import: "aws/cloudformation.eu" } }
```

- `git`: repository URL (any format git CLI accepts)
- `commit`: SHA — mandatory for reproducibility and cacheability
- `import`: path to file within the repository

Multiple git imports mix freely with local imports:

```text
{ import: [
  "local.eu",
  { git: "https://github.com/user/lib"
    commit: "abc123"
    import: "helpers.eu" }
] }
```

## Current Infrastructure

### Locator enum (`src/syntax/input.rs:16-36`)

Eight existing variants: `Url`, `Fs`, `Resource`, `Pseudo`, `StdIn`, `Cli`,
`Literal`, `Buffer`. No `Git` variant.

### Import parsing (`src/syntax/import.rs`)

`read_rowan_soup_imports()` extracts imports from unit metadata. Currently only
processes **string literals** — structured blocks `{ git: ..., commit: ...,
import: ... }` fall through with no handling.

### Source loading (`src/driver/source.rs`)

`SourceLoader::load()` dispatches on format:
- `.eu` → `load_tree()` (recursive, handles imports)
- data formats → `load_simple()`

Path resolution via `resolve_fs_path()` searches `lib_path` directories then
CWD. Once a git import is fetched to a local path, the existing pipeline
handles it.

### Import graph (`src/syntax/import.rs`)

`petgraph::Graph<Input, ()>` with cycle detection via `toposort()`. Transitive
imports work correctly — `load_tree()` extends `lib_path` with the importing
file's directory.

### Error types

`ImportError` enum: `ImportSyntax`, `LoadFailure`, `Cycle`, `UnknownInput`.
`EucalyptError` wraps all error types including `Io`.

## Design

### 1. Add `Locator::Git` variant

In `src/syntax/input.rs`:

```rust
Git {
    url: String,
    commit: String,
    path: String,
}
```

### 2. Parse git import blocks

In `src/syntax/import.rs`, extend `read_rowan_soup_imports()` to handle
structured import blocks:

- When an import list entry is a block (not a string), extract `git`, `commit`,
  and `import` keys
- Validate: `commit` is mandatory — error if missing
- Create `Input` with `Locator::Git { url, commit, path }`

### 3. Create git fetch/cache module

New file: `src/import/git.rs`

**Cache layout:**
```
~/.eu/cache/git/
  <url-hash>/            # bare clone of repository
    HEAD, refs/, ...
  <url-hash>/<commit>/   # extracted files for a specific commit
    <path>               # the imported file
```

**Resolution function:**
```rust
pub fn resolve_git_import(
    url: &str,
    commit: &str,
    path: &str,
) -> Result<PathBuf, GitImportError>
```

Logic:
1. Compute `url_hash` (e.g. SHA-256 of URL, truncated)
2. If `<cache>/<url_hash>/<commit>/<path>` exists, return it (cache hit)
3. If bare clone doesn't exist, run `git clone --bare <url> <cache>/<url_hash>/`
4. Extract file: `git -C <repo_dir> archive <commit> -- <path> | tar x -C <extract_dir>`
5. Return the extracted file path

**Cache properties:**
- Keyed by `(url, commit)` — immutable, never re-fetched or invalidated
- Bare clone is shared across commits from the same repo
- Uses `dirs::home_dir()` (already a dependency) for `~/.eu/`

**Error type:**
```rust
pub enum GitImportError {
    CommitRequired,                    // missing commit field
    CloneFailed { url: String, stderr: String },
    CommitNotFound { commit: String, stderr: String },
    PathNotFound { path: String, commit: String },
    IoError(std::io::Error),
}
```

### 4. Integrate into source loader

In `src/driver/source.rs`, handle `Locator::Git` in the loading path:

1. Call `resolve_git_import(url, commit, path)` to get a local `PathBuf`
2. Create a new `Input` with `Locator::Fs(resolved_path)` and inferred format
3. Feed into existing `load()` / `load_tree()` / `load_simple()`

### 5. Transitive imports from git-fetched files

When a git-imported `.eu` file itself contains imports:
- Relative imports resolve relative to the extracted file's directory
  (the `<cache>/<url_hash>/<commit>/` directory)
- This works automatically because `load_tree()` extends `lib_path` with the
  importing file's parent directory
- Git imports within git-imported files work recursively (same fetch/cache logic)

### 6. Error integration

Add `GitImport(GitImportError)` variant to `ImportError` or `EucalyptError`.
Ensure errors include:
- The URL and commit for context
- The git CLI stderr for clone/archive failures
- Clear message for missing commit: "git imports require a commit SHA for
  reproducibility"

### 7. Offline/network-down behaviour

- If cache hit: succeed silently (no network needed)
- If cache miss and network unavailable: fail with clear error mentioning the
  URL and suggesting the user check connectivity
- No retry logic — fail fast

## Acceptance Criteria

1. `{ import: { git: "url", commit: "sha", import: "path" } }` works end-to-end
   with a real public repository
2. Cached imports do not re-fetch (verify with network-down test or timing)
3. Missing `commit` field produces clear error mentioning "commit SHA"
4. Bad URL produces error with URL and git stderr
5. Missing path within repo produces error naming the path and commit
6. Works with `.eu` files and data format imports (YAML, JSON, etc.)
7. Transitive imports from git-fetched `.eu` files resolve correctly
8. Full test suite passes (`cargo test`)
9. `cargo clippy --all-targets -- -D warnings` clean
10. `cargo fmt --all` clean

## Out of Scope

- Branch or tag references (commit SHA only, per documented design)
- Authentication (public repos only for now)
- Cache eviction or garbage collection
- Lock files or dependency resolution
- Shallow clones (use full bare clone for simplicity)
