# Changelog

All notable changes to eucalypt are documented here.

## [Unreleased]

### Added

- **Named Monadic Blocks** (eu-ekph) — Five new syntactic forms for specifying the monad
  in bracket block expressions:
  - Bare symbol namespace reference: `{ :name ... }.expr`
  - Structured metadata: `{ { monad: name } ... }.expr`
  - `:monad namespace:` inline marker form
  - Inline `:monad bind: return:` form with explicit bind and return
  - Bracket def namespace delegation
  - Bracket block definitions now **require** a `:monad` block metadata marker;
    missing marker produces a `MonadSpecMissingMarker` error

- **`deep-merge-at`** — New prelude function to merge a value into a nested path

- **`✓` Postfix Non-nil Predicate Operator** — Asserts a value is non-nil; replaces
  the previous `‼` operator (which has been retired)

### Changed

- **`assert` Refactoring** — `assert` now accepts a predicate; moved adjacent to
  assertion operators in the prelude

- **Declaration metadata** — `bind`/`monad_return` fields removed from declaration
  metadata; monad configuration is exclusively block metadata now

- **`‼` Operator Renamed to `✓`** — Assertions namespace reorganised and non-nil
  postfix operator renamed

### Performance

- **NdArray arithmetic dispatch** (eu-76sv) — Array type dispatch moved from
  interpreted prelude `is-array?` checks to Rust intrinsics, giving approximately
  10× performance improvement for array operations

### Fixed

- **Depth-aware `beta_reduce`** (eu-5pe9) — Substitution in `beta_reduce` now
  tracks binder depth correctly for destructuring lambdas, fixing incorrect
  variable capture in certain patterns

- **`deep-find` symbol keys** (eu-9vzc) — `deep-find`, `deep-find-first`, and
  `deep-find-paths` now accept symbol keys only (previously accepted strings,
  which was inconsistent with the block key model)

- **Deep-query prelude** — Fixed nested conditional in `match-sym`; refactored
  deep-fold abstraction to unify deep-query functions

- **Emacs mode** — Corrected indentation of backtick metadata at top-level
  declarations

### Removed

- **Persistent block infrastructure** — Removed `pb.*` persistent block API and
  supporting runtime code. An Architecture Decision Record (ADR-001) documents
  the deferral. The `im-rc` dependency removed.

## [0.4.0] - Destructuring, Monadic Blocks, Arrays, Error Messages

### Added

- **Destructuring Parameters** - Pattern matching in function parameters
  - Block destructuring: `f({x y}): x + y`
  - Fixed-length list destructuring: `f([a, b]): a + b`
  - Head/tail cons destructuring: `f([h : t]): h` with `‖` operator
  - Juxtaposed call syntax: `f{x: 1 y: 2}` and `f[1, 2]`
  - Juxtaposed definition syntax: `f{x y}: x + y` and `f[a, b]: a + b`
  - Destructure fusion pass to elide intermediate allocations

- **Monadic Blocks** - Do-notation via bracket metadata
  - `⟦ a: ma  b: mb ⟧.expr` desugars to bind/return chains
  - User-defined bracket pairs with `:monad` marker
  - Paren-free bracket pair declarations

- **Idiot Brackets** - User-defined Unicode bracket pairs
  - Unicode Ps/Pe category detection for bracket support
  - Custom evaluation semantics via bracket metadata

- **N-dimensional Arrays** - `arr.*` namespace for tensor operations
  - `arr.from-flat`, `arr.reshape`, `arr.map`, `arr.fold`, `arr.neighbours`
  - Polymorphic arithmetic: `+`, `-`, `*`, `/` work element-wise on arrays
  - Heap-backed `HeapNdArray` with GC integration

- **Persistent Blocks** - Functional map data type
  - `pb.from-block`, `pb.lookup`, `pb.to-list`, `pb.merge`, `pb.merge-with`
  - O(log n) lookup backed by `im-rc` persistent data structures

- **Expression Anaphora Scoping** - Refined `_` semantics
  - Multiple `_` creates multiple parameters (`_ + _` means `_0 + _1`)
  - Parentheses are opaque boundaries for anaphora scoping
  - Propagation through paren-free constructs

- **Relative Imports** - Resolve imports relative to source file directory

- **Power Operator** - `^` for exponentiation with `pow` intrinsic

- **Division Semantics** - `/` is floor division, `÷` is exact division

- **Unicode Operator Aliases** - `≤`, `≥`, `≠` for comparison operators

- **`product`** - Multiply elements in a list (complement to `sum`)

- **Contextual Error Messages** - Comprehensive error improvement programme
  - Did-you-mean suggestions for key lookup failures
  - Expected vs actual type in type mismatch errors
  - Structured error output with `--error-format=json`
  - Multiple parse/translate errors reported in one pass
  - Hints for common mistakes: `==` → `=`, `->` is const not lambda,
    `import`/`use`/`require` as unresolved variables, camelCase function
    names, missing parity functions, string method patterns
  - Source locations in parse errors and BlackHole (infinite loop) errors
  - Contextual help for operator precedence issues

- **Documentation**
  - Eucalypt Guide (15 tutorial chapters)
  - Eucalypt by Example (15 worked examples)
  - FAQ (20 common questions)
  - Agent reference page for AI coding agents
  - Syntax cheat sheet
  - `llms.txt` and `llms-full.txt` for LLM context
  - mdBook migration with syntax highlighting
  - Documentation example testing (`scripts/test-doc-examples.py`)

- **Editor Support**
  - Unified Emacs mode with tree-sitter and traditional variants
  - Tree-sitter grammar updates for 0.4.0 syntax
  - VS Code extension improvements

- **Platform**
  - aarch64-linux release binary
  - Curl-installable install script
  - WASM compilation gate in CI

### Changed

- **Default heap limit** reduced from 64 GiB to 32 GiB managed heap
- **Assertion operators** reorganised; falsy variants deprecated
- **Stack traces** - Lazy iterators, pre-allocated buffers, auto-filtered
  intrinsic frames, suppressed empty traces

### Performance

- **VM execution hot loop** optimisation
- **Continuations** stored inline in Vec, off the eucalypt heap
- **Boolean returns** use pre-allocated global closures
- **`return_native`** reuses existing Atom closures
- **Thunk memoisation** preserved through shared constructor env backing
- **Update accumulation** prevented in IF branches via `suppress_update`
- **`str_arg_ref()`** zero-copy string borrowing for intrinsics

### Fixed

- Expression anaphor scoping for `_`, `_0`, `_1` in arg positions
- Cons pattern mangling in EU formatter
- `HeapNdArray` GC evacuation correctness
- `u8` overflow for large frames in constructor env
- `set.add` for computed values
- Deep merge handling of boxed symbols in dynamic blocks
- Unclosed string interpolation converted from panic to proper error
- `::` converted from panic to proper error
- `#` inside string literals no longer parsed as comment (tree-sitter, Emacs)
- Rainbow-delimiters restored with string-aware syntax-propertize
- Stray debug `println` corrupting CI version output
- Block-level DCE handling of dynamise fallbacks

## [0.3.0] - Runtime v2, GC, Random Numbers, Streaming Imports

### Added

- **Random Number Facilities** - Pure functional SplitMix64 PRNG
  - `__PRNG_NEXT` / `__PRNG_FLOAT` built-in functions
  - Seed from environment (`RANDOM_SEED`) or default
  - No mutable state - deterministic and reproducible

- **Streaming File Imports** - Eager drain via `__STREAM_NEXT` intrinsic
  - Thread-local `StreamTable` for import stream management
  - Process large files without loading entirely into memory

- **Sets Data Type** - New `Native::Set` with heap-backed storage
  - Set literal syntax and set operations in prelude
  - Deterministic output via sorted rendering

- **Symbol Interning** - Runtime `SymbolPool` for efficient symbol handling
  - `Native::Sym` changed from `RefPtr<HeapString>` to `SymbolId`

- **Deep-Query DSL** - Pattern-based data querying
  - `deep-find` functions and type predicates (`ISBLOCK`, `ISLIST`)
  - Path wildcard support for nested data access

- **Block Indexing** - O(1) key lookup for blocks
  - `Native::Index` variant for block index storage
  - `LOOKUPOR` modified for lazy block indexing
  - Block-level dead code elimination

- **RAWMETA Intrinsic** - Non-recursive metadata access
  - Doc metadata preserved at runtime for doc generation library

- **GC Evacuation** - Full Immix evacuating collector
  - `scan_and_update` for pointer fixup during evacuation
  - Lazy sweeping optimisation
  - O(1) block lookup for line marking
  - GC stress benchmarks and Criterion benchmarks
  - GC benchmarking script (`scripts/gc-bench.sh`)

- **Command-Line Args** - `__args` pseudo-block and `io.args` in prelude

- **C-String Literals** - `c"..."` syntax with escape sequences

- **ZDT Literal Syntax** - `t"..."` for zoned datetime values

- **Polymorphic Comparison** - Equality and ordering for strings, symbols, datetimes

- **Cartesian Product** - `cross` function in prelude

- **SHA-256 Hash Intrinsic** and **Base64 Encode/Decode**

- **Version Assertion Intrinsic**

- **Self-Referential Thunk Detection**

- **Tight-Binding Head Operator** - `↑` for list head access

- **JSONL Import** - JSON Lines format support

- **Nullary Operators**

- **YAML Import Enhancements** - Anchor/alias resolution, merge keys, timestamp-to-ZDT

- **Source Code Formatter** (`eu fmt`)
  - Conservative and full reformat modes
  - Configurable line width and indent size
  - Check mode (`--check`) for CI, in-place modification (`--write`)

- **LSP Server** - Language Server Protocol implementation
  - Diagnostics, completion, hover, go-to-definition
  - Find references, semantic tokens, formatting
  - Code actions, inlay hints, rename, document symbols

- **Editor Support**
  - Emacs major mode (`eucalypt-mode.el`) with tree-sitter variant
  - VS Code extension with TextMate grammar
  - Tree-sitter grammar for Eucalypt

- **CLI Rationalisation** - clap v4 subcommand architecture
  - `eu run`, `eu test`, `eu dump`, `eu fmt`, `eu version`, `eu explain`, `eu list-targets`

- **CI/CD Improvements**
  - Consolidated lint job, `cargo audit` security audit
  - SHA-based concurrency groups for builds
  - Error test `.expect` sidecar validation

- **Testing Expansion** - 106 harness tests (up from ~50)
  - GC stress tests, error tests, benchmark validation

- **Prelude Extensions**
  - List sorting (`sort`, `sort-by`), string comparison
  - Streaming string intrinsic returns
  - Extended block and list functions

### Changed

- **Parser Rewrite** - Complete replacement of LALRPOP parser with Rowan-based implementation
  - Better error recovery and diagnostics
  - Foundation for IDE tooling support

- **Runtime v2** - STG machine rewrite with new BIF system
  - Case-of-known-constructor folding
  - O(1) tag dispatch for case branches
  - Redundant branch elimination
  - Performance: lazy stack trace iterators, pre-allocated trace buffers, reduced Vec cloning

- **Dependencies** - Security updates, replaced unmaintained crates, clap v4 migration

### Fixed

- Large object allocation sizing for improved memory efficiency
- Object alignment for double-word boundaries
- Array bounds checking guards
- Deprecated chrono API usage
- YAML tag output, merge key spans, scalar conversion error handling
- Unicode support in TextMate grammar and VS Code
- UTF-16 offset handling in LSP
- Formatter unary operator spacing and tab-to-space alignment
- `-e` evaluand blocking on stdin in non-TTY contexts

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
