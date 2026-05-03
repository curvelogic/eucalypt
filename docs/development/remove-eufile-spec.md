# Remove Eufile and Ergonomic/Batch CLI Mode

**Status**: Spec  
**Bead**: eu-8ea2  
**Date**: 2026-05-03

## 1. Overview

Remove the Eufile project file mechanism and the
`Ergonomic`/`Batch` command-line mode distinction.  These are
barely documented, more often surprising than useful, and add
complexity to the CLI option handling.

After this change, `eu` behaves as current `-B` (batch) mode
always — no implicit file loading from the filesystem.

## 2. What Gets Removed

### 2.1 Eufile

`project::eufile()` walks up from the current directory looking
for a file named `Eufile` (case-insensitive).  When found, it is
prepended as an input.  This means running `eu` in different
directories can produce different results depending on whether
an ancestor has a Eufile.

### 2.2 Dotfile (.eucalypt)

`project::dotfile()` looks for `~/.eucalypt` in the user's home
directory.  Only loaded in `Ergonomic` mode.

### 2.3 CommandLineMode enum

```rust
pub enum CommandLineMode {
    Ergonomic,  // loads Eufile + dotfile
    Batch,      // skips both
}
```

The `-B` / `--batch` flag switches from `Ergonomic` to `Batch`.
After removal, there is only one mode.

## 3. Implementation

### 3.1 Remove project.rs

Delete `src/driver/project.rs` entirely — it only contains
`eufile()` and `dotfile()`.

### 3.2 Remove CommandLineMode

Remove the enum and the `mode` field from `EucalyptOptions`.
Remove the `-B` / `--batch` CLI flag from all subcommand
definitions.

### 3.3 Simplify process_defaults

Remove the Eufile and dotfile prepend blocks from
`process_defaults()`:

```rust
// REMOVE: Prepend project Eufile (unless in batch mode)
if self.mode != CommandLineMode::Batch {
    if let Some(eufile) = project::eufile() {
        self.prepend_input(Input::new(Locator::Fs(eufile), None, "eu"));
    }
}

// REMOVE: In ergonomic mode prepend user .eucalypt
if self.mode == CommandLineMode::Ergonomic {
    if let Some(dotfile) = project::dotfile() {
        self.prepend_input(Input::new(Locator::Fs(dotfile), None, "eu"));
    }
}
```

### 3.4 Keep -B as deprecated no-op (one release)

To avoid breaking scripts that pass `-B`, keep the flag but make
it a silent no-op for one release cycle.  Remove entirely in 0.7.

Alternatively, remove immediately if we judge that `-B` usage is
negligible.

## 4. Acceptance Criteria

1. `eu` with no args shows help (as per the recent fix)
2. `eu -e '{}'` works without loading any Eufile
3. `eu file.eu` works without loading any Eufile
4. `-B` flag is accepted without error (deprecated no-op) or
   removed entirely — decision at implementation time
5. No reference to `Eufile`, `dotfile`, or `CommandLineMode` in
   the codebase after removal
6. `eu check lib/prelude.eu` — zero warnings
7. All existing harness tests pass
8. Documentation updated: remove any Eufile references from
   docs/, README, help text

## 5. Files Changed

| File | Change |
|------|--------|
| `src/driver/project.rs` | Delete |
| `src/driver/mod.rs` | Remove `project` module |
| `src/driver/options.rs` | Remove `CommandLineMode`, `-B` flag, Eufile/dotfile logic |
| `docs/` | Remove any Eufile references |
| `CLAUDE.md` | Remove Eufile references if any |

## 6. Dependencies

None.

## 7. Risk

Low.  The Eufile mechanism is undocumented and unused in the test
suite.  The `-B` flag is used in CLAUDE.md examples and test
commands but only defensively (to avoid Eufile interference).
Those references become unnecessary after removal.
