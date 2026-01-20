# Change: Add Eucalypt Source Formatter

## Why
Eucalypt lacks a code formatter, making it difficult to maintain consistent code style across projects and teams. Users must manually fix spacing, alignment, and line breaking issues. A formatter would improve developer experience and code quality.

## What Changes
- Add `eu fmt` subcommand for formatting eucalypt source files
- Implement two formatting modes: conservative (fix violations only) and full reformatting
- Extend the existing `ToSourceDoc` trait with proper pretty-printing logic
- Support configurable line width and indentation settings

## Impact
- Affected specs: `source-formatting` (new capability)
- Affected code:
  - `src/syntax/export/pretty.rs` - Extend ToSourceDoc implementations
  - `src/driver/` - Add format command handling
  - `src/main.rs` / CLI - Add `fmt` subcommand

## Success Criteria
- `eu fmt file.eu` produces well-formatted output
- Formatting preserves semantic meaning (round-trip safe)
- Conservative mode preserves intentional formatting choices
- Tab characters are converted to spaces with alignment preservation
