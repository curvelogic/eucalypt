# Change: Add Command Line Argument Access

## Why
Eucalypt programs cannot currently access command line arguments, limiting their use for scripting and CLI tools. Users need both simple list access (like Python's `sys.argv`) and potentially structured argument parsing.

## What Changes
- Capture arguments after `--` separator in CLI
- Create `__ARGS` pseudo-input following the `__io` pattern
- Expose arguments as a list of strings accessible in eucalypt code

## Impact
- Affected specs: `command-line-args` (new capability)
- Affected code:
  - `src/driver/options.rs` - Capture args after `--`, inject pseudo-input
  - `src/driver/io.rs` - Create args pseudo-block
  - `src/driver/source.rs` - Handle args pseudo-input loading

## Success Criteria
- `eu script.eu -- arg1 arg2` makes args available as `__ARGS`
- `__ARGS` is a list of strings `["arg1", "arg2"]`
- Works with all subcommands (`run`, `test`, etc.)
- Existing behaviour unchanged when no `--` arguments provided

## Scope
This proposal covers Phase 1 (basic access) only. Structured argument parsing (Phase 2) can be built on top as a library or future enhancement.
