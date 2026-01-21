# Changelog

All notable changes to eucalypt are documented here.

## [Unreleased]

### Added

- **Source Code Formatter** (`eu fmt`) - New subcommand for formatting eucalypt source files
  - Conservative mode preserves original formatting choices where possible
  - Full reformat mode (`--reformat`) applies consistent style throughout
  - Configurable line width and indent size
  - Check mode (`--check`) for CI integration
  - In-place modification (`--write`)

- **Emacs Major Mode** (`eucalypt-mode.el`) - Syntax highlighting and editing support for `.eu` files

- **CLI Rationalization** - Restructured command line with clap v4 subcommand architecture
  - `eu run` - Evaluate eucalypt code (default when no subcommand)
  - `eu test` - Run tests
  - `eu dump` - Dump intermediate representations
  - `eu fmt` - Format source files
  - `eu version` - Show version information
  - `eu explain` - Explain what would be executed
  - `eu list-targets` - List targets defined in source

### Changed

- **Parser Rewrite** - Complete replacement of LALRPOP parser with Rowan-based implementation
  - Better error recovery and diagnostics
  - Foundation for IDE tooling support
  - Improved performance

### Fixed

- Large object allocation sizing for improved memory efficiency
- Object alignment for double-word boundaries
- Array bounds checking guards
- Deprecated chrono API usage
- YAML tag output handling

## [0.2.0] - Rust Implementation

### Added

- Complete rewrite in Rust (from Haskell)
- Immix-style garbage collector with:
  - Block hierarchy (32KB blocks, 128B lines)
  - Size class allocation
  - Fragmentation detection and collection strategy selection
  - Comprehensive performance metrics and telemetry
  - Graceful OOM handling
  - Per-heap mark state architecture

- New prelude functions:
  - `group-by(k, xs)` - Group elements by key function
  - `qsort(lt, xs)` - Quicksort with custom comparison
  - `discriminate(pred, xs)` - Split list by predicate
  - Modulus operator `%`

- EDN (Extensible Data Notation) import and export support
- Heap statistics output with `-S` flag

### Fixed

- `head-or` function behavior
- Missing `__UPPER` and `__LOWER` string intrinsics
- YAML tag output formatting
- Large object allocation

## [0.1.x] - Haskell Implementation

Initial implementation in Haskell. See git history for detailed changes.
