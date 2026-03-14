# Change log

All notable changes to eucalypt are documented here.

## [0.5.0] - 2026-03-13

### Added

- **IO Monad** — Execute shell commands and system processes from eucalypt
  - `{ :io r: io.shell("cmd") }.(r.stdout)` — monadic block syntax with bind/return desugaring
  - `io.shell(c)`, `io.shell-with(opts, c)` — run shell commands via `sh -c`
  - `io.exec([cmd : args])`, `io.exec-with(opts, [cmd : args])` — exec processes directly
  - `io.check(r)`, `io.checked`, `io.bind`, `io.return`, `io.map`, `io.and-then`, `io.fail`
  - `--allow-io` / `-I` CLI flag required to enable IO operations
  - Results as `{stdout, stderr, exit-code}` blocks
  - Spawn failures return result blocks (exit-code 127) rather than hard errors

- **`render-as(fmt, value)`** — Serialise any eucalypt value to a string at runtime
  - Supports `:json`, `:yaml`, `:toml`, `:text`, `:edn`, `:html`

- **`parse-as(fmt, str)`** — Pure inverse of `render-as`; converts strings to eucalypt data structures
  - Supports `:json`, `:yaml`, `:toml`, `:csv`, `:xml`, `:edn`, `:jsonl`
  - Data-only mode: untrusted input (e.g. shell output) never executes embedded code

- **`monad(m)`** — Derive standard monad combinators from a block with `bind` and `return` fields
  - Returns a block with `map`, `and-then`, `then`, `join`, `sequence`, `map-m`, `filter-m`
  - `and-then(f, action)` — bind with flipped args for pipeline use
  - Compose with `{ ... }` to build monadic namespaces: `monad(m) { extra-field: ... }`

- **Monadic `random:` namespace** — State-monad interface to the PRNG
  - `random.stream(seed)` — create initial stream; each action is a function `stream → {value, rest}`
  - `random.float`, `random.int(n)`, `random.choice(xs)`, `random.shuffle(xs)`, `random.sample(n, xs)`
  - `random.map`, `random.sequence`, `random.map-m`, `random.bind`, `random.return`
  - Legacy `random-stream`, `random-int`, `random-choice`, `random-shuffle`, `random-sample` retained

### Fixed

- **`render-as` argument order** — Changed from `render-as(value, fmt)` to
  `render-as(fmt, value)` for pipeline-friendly partial application
  (e.g. `data render-as(:json)`)
- **`render` / `render-as` nested block null** — Nested block values inside
  `render` or `render-as` were serialised as null; now correctly traverses
  unevaluated Let/LetRec thunks in the heap walk
- **GC correctness** — Multiple garbage collector fixes addressing crashes on aarch64-linux and macOS ARM:
  - 16-byte alignment for evacuation allocations
  - Per-heap mark state (global `MARK_STATE` moved into `Heap` struct), fixing parallel test crashes
  - Backing arrays of `Cons`/`App`/`Bif`/`Case` nodes now evacuated correctly
  - Line marking extended to evacuation target blocks, preventing lazy sweep from recycling live data
  - Full allocation span (header + object) now marked when straddling Immix line boundaries
  - Heap string corruption in release builds prevented
  - Cross-line array backing mark coverage corrected

- **Error diagnostics** — Comprehensive error message improvements:
  - Source locations for operator errors, function calls, lookup failures, dot-on-non-block, intrinsic type mismatches, comparison errors, datetime/timezone errors, regex errors, and base64 decode errors
  - Available keys shown on lookup failure when no close match exists
  - Contextual hints: `map(.field)` for dot on list of blocks, `count` for list-used-as-block, block/list in string interpolation
  - Panics replaced with structured errors: `ComparisonTypeMismatch`, `BitshiftRangeError`, `BitwiseIntegerRequired`, `AssertionFailed`, `VersionRequirementFailed`, dotted metadata keys
  - Improved messages for partial application, datetime components, numeric format types, symbol-vs-string mismatches

- **Block-dot metadata** — `AllocationPruner` letrec strip index adjustment corrected, fixing incorrect evaluation when accessing fields of metadata-tagged blocks

- **Emacs mode** — Backtick auto-pairing, closing brace indentation, docstring indentation, smartparens compatibility, new Unicode chars in Quail input method and transient menu

## [0.4.0] - Destructuring, Monadic Blocks, Arrays, Error Messages

### Added

- **`deep-merge-at`** — New prelude function to merge a value into a nested path

- **`✓` Postfix Non-nil Predicate Operator**
  - Also useful for expanding scope of expression anaphora eg. filter(\_0✓ && fn(\_0))

- **Destructuring Parameters** - Pattern matching in function parameters
  - Block destructuring: `f({x y}): x + y`
  - Fixed-length list destructuring: `f([a, b]): a + b`
  - Head/tail cons destructuring: `f([h : t]): h` with `‖` operator
  - Juxtaposed call syntax: `f{x: 1 y: 2}` and `f[1, 2]`
  - Juxtaposed definition syntax: `f{x y}: x + y` and `f[a, b]: a + b`
  - Destructure fusion pass to elide intermediate allocations

- **N-dimensional Arrays** - `arr.*` namespace for tensor operations
  - `arr.from-flat`, `arr.reshape`, `arr.map`, `arr.fold`, `arr.neighbours`
  - Polymorphic arithmetic: `+`, `-`, `*`, `/` work element-wise on arrays
  - Heap-backed `HeapNdArray` with GC integration

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

- **(Experimental) Idiot Brackets** - User-defined handling for Unicode bracket pairs
  - Unicode Ps/Pe category detection for bracket support
  - Custom evaluation semantics via bracket metadata
  - Allows "customising catenation"

- **(Experimental) Monadic Blocks** - Blocks as a "do notation" for monads
  - Several syntactic forms all desugar to `bind` / `return` expressions
  - Inline monad definition `{ { :monad bind: ... return: ...} declarations... }.expr`
  - Referencing a namespace with `bind` and `return`: `{ { :monad namespace: ns } declarations... }.expr`
  - Via monad key in block metadata: `{ { monad: ns } ... }.expr`
  - Short form using only symbol metadata for namespace reference: `{ :ns ... }.expr`
  - User-defined bracket pairs with allowing `⟦ a: ma  b: mb ⟧.expr` 

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

- **`assert` Refactoring** — `assert` now accepts a predicate; moved adjacent to
  assertion operators in the prelude
- **Default heap limit** reduced from 64 GiB to 32 GiB managed heap
- **Assertion operators** reorganised; falsy variants deprecated
- **Stack traces** - Lazy iterators, pre-allocated buffers, auto-filtered
  intrinsic frames, suppressed empty traces

### Performance

- **NdArray arithmetic dispatch** (eu-76sv) — Array type dispatch moved from
  interpreted prelude `is-array?` checks to Rust intrinsics, giving approximately
  10× performance improvement for array operations
- **VM execution hot loop** optimisation
- **Continuations** stored inline in Vec, off the eucalypt heap
- **Boolean returns** use pre-allocated global closures
- **`return_native`** reuses existing Atom closures
- **Thunk memoisation** preserved through shared constructor env backing
- **Update accumulation** prevented in IF branches via `suppress_update`
- **`str_arg_ref()`** zero-copy string borrowing for intrinsics

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
