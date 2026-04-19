# Change log

All notable changes to eucalypt are documented here.

## [0.5.4] - 2026-04-19

### Added

- **`EU_GC_VERIFY=2`** — full multi-checkpoint structural verification of the GC heap: header validity, pointer validity, line consistency, forwarding pointer lifecycle, and block list integrity across four checkpoints during collection. Level 1 unchanged
- **Ceiling/floor bracket notation** — `⌈n⌉` for ceiling and `⌊n⌋` for floor, using idiot bracket pairs. `⌈3.2⌉` → `4`, `⌊3.8⌋` → `3`
- **Debugging tools guide** — `docs/development/debugging.md` documenting all debug environment variables, dump commands, and recommended debugging workflows
- **Editor Unicode coverage** — `⌈⌉⌊⌋` (ceiling/floor) and `‹›` (lens brackets) added to Emacs quail/transient and VS Code quick-pick. Bracket pairs group in Emacs transient menu with auto-pairing

### Changed

- **Unified stream producers** — `StreamProducer`/`StreamTable`/`STREAM_NEXT` replaced by `LazyProducer` trait, `ProducerTable`, and `PRODUCER_NEXT` BIF. All handle-based lazy producers (file imports, future IO streams) use one registration mechanism. `Native::Stream(u64)` renamed to `Native::Prng(u64)`, `Native::Producer(u32)` added for all producer handles

### Fixed

- **Lens brackets `‹›` in tree-sitter grammar** — added to `BRACKET_OPEN_RE`/`BRACKET_CLOSE_RE` so lens path expressions parse and highlight correctly
- **`▶` and `⊝` in tree-sitter** — added to `OPER_CHARS` for correct operator highlighting
- **`•` in VS Code TextMate grammar** — added to unicode-operator character class

## [0.5.3] - 2026-04-17

### Added

- **State monad library** — optional import (`{ import: "state.eu" }`) providing `{ :state ... }` monadic blocks for threading block-valued state. Includes `state.get`, `state.put`, `state.lift`, `state.query`, `state.modify`, `state.run`/`eval`/`exec`, and lens-based operators `=!` and `%!`
- **Eucalypt export format** — `eu -x eu` and `render-as(:eu)` preserving type distinctions: symbols as `:name`, strings quoted, numbers bare. Uses `pretty` crate for layout
- **Import guards** — include-once deduplication for diamond imports. Same file imported multiple times is desugared once; different-name imports of the same file emit alias bindings
- **Deep nested destructuring** — list and block destructuring now supports arbitrary depth: `f([[a, b], [c, d]])`, `g({x: {y: inner}})`
- **IntrinsicMachine thunk-forcing** — `Machine::evaluate_to_whnf` enables Rust intrinsics to force lazy thunks. `MachineCore`/`MachineBifContext` split eliminates aliased `&mut` UB. `evaluate_to_whnf_for_io` refactored to delegate
- **`deep-transform(rule, data)`** — recursive structural rewrite: apply `rule` at each node, return non-null to replace or null to recurse into children
- **`map-elements(f, block)`** — apply `f([key, value])` to each element of a block, returning a new block
- **`unzip(pairs)`** — list of pairs to pair of lists, inverse of `zip`
- **`interleave(a, b)`** — alternate elements from two lists, appending remainder when one is exhausted
- **`window-all` / `partition-all`** — like `window`/`partition` but include trailing incomplete chunks
- **`random.run` / `random.eval` / `random.exec`** — convenience methods matching the state monad API
- **Set membership operators** — `v ∈ s` and `v ∉ s` at precedence 40; sections work as predicates: `when(∈ ks, f)`
- **Editor Unicode support** — `∈`, `∉`, `⊝`, `▶` added to Emacs quail/transient and VS Code quick-pick
- **List monad (`:for` blocks)** — `{ :for x: xs y: ys [x, y] }` for list comprehensions with monadic bind over lists
- **`parts-of(traversal)`** — lens combinator turning a traversal into a lens on the list of all foci. `view` collects; `over` applies list transforms and distributes back
- **`each-element` / `filtered-elements(p?)`** — block traversals for the lens library, analogous to `each`/`filtered` for lists. Traverse all or matching kv pairs with `to-list-of`, `over`, and composition with `_key`/`_value`
- **`EU_IO_TRACE=1`** — debug environment variable tracing all `io.shell` and `io.exec` commands to stderr, showing command strings, stdin piping, and exit codes

### Changed

- **Unified assertion/expectation operators** — `//=` and `//!` now use a single `__EXPECT` BIF with stderr diagnostics on failure, replacing separate `__ASSERT` and `__CHECK` implementations
- **`dbg` and `▶` rendering** — switched from `render-as(:json)` to `render-as(:eu)`, preserving symbol syntax in debug output
- **Simple lookup semantics** — `.name` is now consistently key lookup restricted to block bindings, never extending to outer scope. Previously, `.name` on a static block literal resolved through the block scope and fell through to outer scope
- **Monadic blocks in generalised lookup** — `A.{ :monad ... }` now evaluates the monadic block in `A`'s scope with implicit return. Previously the monadic block was desugared independently, losing the lookup scope
- **Monadic return left-associativity** — `{ :monad ... }.{ block }.(expr)` now parses as `(({ :monad }).{ block }).(expr)`. The return expression is a single element; subsequent `.` operations are separate lookups on the result
- **State monad representation** — state actions return `{ value: v, state: s }` blocks (matching random monad's `{ value: v, rest: stream }` pattern) instead of `[v, s]` lists
- **Quoted identifier handling** — `NormalIdentifier::value()` strips single quotes from quoted names like `'x/z'` in all desugarer paths

### Fixed

- **Compiler panic on consecutive metadata blocks** — `{ a: 1 } { b: 2 } c: 3` no longer panics. Root cause: `mark_body` in prune pass now marks metadata as reachable. Belt-and-braces `InternalCompilerError` diagnostic replaces the `expect()` panic
- **Deep merge panic with list-valued keys** — `{a: 1, b: []} << {a: 2}` no longer panics. `deconstruct` uses `HeapNavigator::resolve_in_closure` which handles `Ref::G` (global constants like `[]`) and `Ref::V` (inline natives)
- **Merge with dynamically-created symbol keys** — `[[:b str.of sym, 0]] block` now merges correctly. `ExtractKey` handles raw symbol atoms; `BlockPair` normalises keys via `ExtractKey`
- **`set.contains?` with non-primitives** — returns `false` instead of panicking when given lists, blocks, or other non-primitive arguments
- **`head`/`tail` error messages** — now show the actual argument: `"head requires a list argument, got 42"` instead of a generic message
- **TypeMismatch error values** — error messages include the actual runtime value with char-safe truncation
- **Parser stray colon recovery** — `f(2, 2:)` produces a parse error instead of an assertion panic
- **`str.of` on quoted symbols** — symbols with special characters (e.g. `'x/z'`) no longer include quotes in string conversion

### Performance

- **Env-var lookup caching** — `EU_STACK_DIAG` and `EU_GC_STRESS` checked once at startup instead of every VM step / GC cycle
- **Exact-arity fast path** — `saturate_with_array` avoids an `Array` copy for exact-arity function application
- **Native arithmetic fast path** — binary operators skip unbox/force chains when both arguments are already unboxed natives

## [0.5.1] - 2026-03-24

### Added

- **Vec type** — `vec.of`, `vec.len`, `vec.nth`, `vec.slice`, `vec.sample`, `vec.shuffle`, `vec.to-list` for O(1) indexed access on large primitive collections
- **Set sampling** — `set.sample(k, s, stream)` for random monad sampling from sets
- **Unified test expectations** — `//=` and `//!` now emit stderr diagnostics on failure via `__EXPECT` BIF; `__DBG_REPR` renders values for diagnostic output
- **Debug tracing** — `dbg(opts, v)` function and `▶` prefix operator for stderr debug output
- **Structured argument parsing** — `parse-args(defaults, args)` with short flags, type coercion, combined options, and `--help` generation
- **Monadic blocks** — `monad: true` metadata registers namespaces; implicit return from non-underscore bindings; identity monad (`:let` blocks)
- **Bracket registry** — proper content-type registry replaces parser heuristic for bracket pairs
- **Idiot brackets collect as list** — `⟦ a b c ⟧` now collects items as `[a, b, c]`; bracket parameter definitions support destructuring patterns
- **Multi-label diagnostics** — secondary source labels from env trace; stack trace reversed to read top-down with name-first formatting
- **Error source locations** — all `ExecutionError` variants now carry `Smid` for source location
- **WASM API** — `evaluate(source, format)` via wasm-bindgen for browser/Node.js use
- **Browser playground** — CodeMirror-based eucalypt playground (separate repo)
- **Markdown docstrings** — tree-sitter grammar identifies docstrings; Emacs mode highlights inline markdown
- **VS Code extension** — feature parity with Emacs mode: improved highlighting, Unicode input, snippets, render command
- **Windows support** — crash handler gated with `cfg(unix)`, PowerShell shell dispatch, Windows CI and release binary
- **Deep merge metadata** — merge and deep merge now preserve block metadata (RHS wins)
- **`coalesce(xs)`** — return first non-null element from a list
- **`update-nth(n, f, l)`** — apply function to element at index n in a list
- **`update-first(p?, f, l)`** — apply function to first matching element in a list
- **`eu.os` / `eu.arch`** — platform constants for cross-platform test portability
- **Nested list destructuring** — `f([a, [b: c]])` now works in function parameters
- **AddressSanitizer CI** — ASAN job (continue-on-error) for catching memory safety issues

### Changed

- **Moniker dependency removed** — replaced with custom binding module; simplified type signatures throughout core pipeline (22 files)
- **`:suppress` documentation** — clarified as data-only; not needed on functions
- **Type predicates** — `number?`, `string?`, `symbol?`, `bool?` intrinsics added
- **`list-update` removed** — replaced by `update-nth` with pipeline-friendly arg order

### Fixed

- **Array growth bug** — `default_array_growth(1)` returned 1 (no growth) due to integer division truncation; caused heap-buffer-overflow on Linux (detected by ASAN)
- **Monadic implicit return self-reference** — synthesised `{a: a}` used `Expr::Block` (letrec) causing self-reference; fixed with manual `Let + Block` scope
- **`-e` monad registry** — `monad: true` registrations from prelude now persist across translation units so `-e` expressions see them
- **`split-after` no-match crash** — crashed when predicate never matched; separated nil check from predicate check
- **`head`/`tail` on empty list** — now panics with "head of empty list" / "tail of empty list" instead of cryptic type mismatch (fixed `Panic.global` to use `BoxedString`)
- **Stack traces show function names** — `new_smid` inherits declaration name from desugarer stack; `intrinsic_display_name` catch-all no longer masks user function names
- **Graceful cwd error** — warns instead of panicking when current directory is inaccessible
- **String interpolation** — pipelines inside interpolation braces were silently producing wrong results; fixed in prelude
- **Windows c-string test** — newline comparison uses c-string instead of literal to avoid `\r\n` conversion

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
