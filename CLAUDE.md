# Beads / Project Management

Run `bd prime` for instructions on using `bd` to manage beads

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Eucalypt is a Rust-based tool and language for generating, templating, rendering and processing structured data formats like YAML, JSON and TOML. The project is implemented in Rust using Cargo as the build system.

## Build and Development Commands

### Basic Development
- `cargo build` - Build the project
- `cargo test` - Run all tests (includes unit tests and harness tests)
- `cargo run` - Run the eucalypt binary
- `cargo install --path .` - Install local `eu` binary

### Testing
- `cargo test` - Run all tests including the comprehensive harness test suite
- `cargo test test_harness_001` - Run a specific harness test (e.g., test 001)
- `cargo test --test harness_test` - Run only the harness tests
- `cargo bench` - Run benchmarks (located in `benches/`)

### Build System
- The project uses LALRPOP for parser generation (configured in `build.rs`)
- Grammar files are in `src/syntax/` with `.lalrpop` extension
- Build metadata is generated via `eu build.eu -t build-meta`

## Architecture

### Core Components

**Core Pipeline (`src/core/`)**:
- `syntax/` - Lexing, parsing, and AST representation
- `desugar/` - AST transformation and desugaring
- `cook/` - Operator precedence and fixity resolution
- `verify/` - Binding and content verification
- `simplify/` - Expression simplification and optimization
- `transform/` - Various AST transformations
- `inline/` - Inlining and reduction passes

**Evaluation (`src/eval/`)**:
- `machine/` - Virtual machine implementation with garbage collection
- `memory/` - Memory management, heap, and GC
- `stg/` - STG (Spineless Tagless G-machine) backend
- `intrinsics.rs` - Built-in functions and primitives

**I/O (`src/import/` and `src/export/`)**:
- Import: CSV, EDN, TOML, XML, YAML, text
- Export: EDN, HTML, JSON, markup, table, text, TOML, YAML

**Driver (`src/driver/`)**:
- `eval.rs` - Main evaluation pipeline
- `tester.rs` - Test harness execution
- `options.rs` - Command-line option handling

### Test Architecture

**Harness Tests (`tests/harness/`)**:
- Comprehensive test suite with 50+ test files
- Tests cover language features, edge cases, and error conditions
- Test files use `.eu` extension for eucalypt source
- Also includes YAML, TOML, CSV, XML, and EDN test files
- Error tests in `tests/harness/errors/` directory
- Benchmark tests in `tests/harness/bench/` directory

**Test Execution**:
- Tests are run via `tests/harness_test.rs`
- Each test corresponds to a file in `tests/harness/`
- Tests can be run individually or as a complete suite
- Uses the `tester` module for test execution

### Memory Management

The project includes a sophisticated garbage collector:
- Generational GC with multiple generations
- Bump allocation for young generation
- Mark-and-sweep for older generations
- Custom memory layout for eucalypt values

### Debug Environment Variables

| Variable | Effect |
|----------|--------|
| `EU_GC_POISON=1` | Fill swept memory with `0xDE` poison pattern. Detects use-after-free by checking for poison in `mark()`. Also enables hole verification in the bump allocator. |
| `EU_GC_VERIFY=1` | After each GC mark phase, re-traverse from roots and verify no reachable objects were missed. Panics if any unmarked-but-reachable objects are found. |
| `EU_STACK_DIAG=1` | Log continuation stack composition to stderr whenever a new max stack depth is reached. |
| `EU_ERROR_TRACE_DUMP=1` | Dump full env trace and stack trace Smid details as diagnostic notes on every execution error. Useful for debugging which source locations are available at error time. |

The crash signal handler (SIGSEGV/SIGBUS diagnostics) is always active and has no environment variable — it installs unconditionally in `main()`.

### Dump Commands for Debugging

Use `eu dump <phase>` to inspect intermediate representations at each pipeline stage. These are the **primary tool** for investigating compiler and core expression issues — do NOT add temporary debug prints.

| Command | Shows |
|---------|-------|
| `eu dump ast <file>` | Parsed syntax tree |
| `eu dump desugared <file>` | Core expression after desugaring |
| `eu dump cooked <file>` | Core expression after operator precedence resolution |
| `eu dump inlined <file>` | Core expression after inlining |
| `eu dump pruned <file>` | Core expression after dead code elimination |
| `eu dump stg <file>` | Compiled STG syntax |
| `eu dump runtime <file>` | Runtime globals |

Add `--debug-format` for the Rust Debug representation (shows full structure including de Bruijn indices), or `--embed` for eucalypt source representation.

Use `-B` (batch mode) to skip `.eucalypt.d` config files.

### Language Features

- Functional programming with lazy evaluation
- Built-in support for structured data (YAML, JSON, TOML)
- Import system for external data files
- Metadata system for attaching information to values
- String interpolation and templating
- Comprehensive numeric operations
- Time and date handling

## Development Notes

- The project uses custom memory management - be careful when modifying anything in `src/eval/memory/`
- Grammar changes require rebuilding due to LALRPOP code generation
- Test files in `tests/harness/` serve as both tests and examples
- The `lib/` directory contains eucalypt library code (prelude, markup, test utilities)
- Build metadata is embedded in the binary via `build-meta.yaml` (generated by `eu build.eu -t build-meta`)

## Writing Eucalypt Code

**MANDATORY: Read the documentation BEFORE writing any eucalypt (`.eu`) code.**

### Required Reading

When writing or modifying `.eu` files, you MUST read these files first:
- `docs/reference/agent-reference.md` — dense syntax reference, prelude functions, pipeline patterns, and common pitfalls (READ THIS FIRST)
- `docs/appendices/syntax-gotchas.md` — critical precedence and syntax traps
- `docs/appendices/cheat-sheet.md` — quick syntax and operator reference

For specific topics, also consult:
- `docs/guide/expressions-and-pipelines.md` — pipeline style and catenation
- `docs/guide/anaphora.md` — `_`, `•`, and string anaphora usage
- `docs/guide/functions-and-combinators.md` — function definition and partial application
- `docs/reference/prelude/` — full prelude reference by category

### Critical Rules (Most Common Agent Mistakes)

1. **Catenation precedence is LOW (20)**: ALL infix operators bind tighter. `xs f(a) + 1` parses as `xs(f(a) + 1)` NOT `(xs f(a)) + 1`. Fix: use parentheses or split into named bindings.
2. **Dot `.` binds tighter (90) than catenation (20)**: `list head.name` parses as `list (head.name)`. Fix: `(list head).name`.
3. **NO lambda/arrow syntax**: `->` is the `const` operator, NOT lambda. Use sections `(+ 1)`, expression anaphora `(_ + 1)`, or named functions.
4. **Each `_` creates a new param**: `_ + _` means `_0 + _1` (two params). Use `_0 * _0` to reference the same param twice.
5. **Backtick is metadata, not comment**: `` ` "text" `` attaches to the NEXT declaration. Use `#` for comments.
6. **`/` is floor division**: Use `÷` for exact division.
7. **Many "obvious" functions don't exist**: No `str.replace`, `str.trim`, `str.contains?`, `flatten`, `abs`, `even?`. Check agent-reference.md section 5.11.
8. **`has` takes a symbol, not a string**: `has(:key)` not `has("key")`.
9. **`str.split-on` uses regex**: `"a.b" str.split-on(".")` matches any char. Use `"[.]"`.
10. **No whitespace before `(`**: `f(x)` is a call, `f (x)` is catenation.
11. **Multiple imports go in one block**: `{ import: ["a.eu", "b.eu"] }` — do NOT write separate `{ import: "a.eu" }` and `{ import: "b.eu" }` blocks. Only the first block is unit metadata; the second becomes a separate expression.
12. **`keys` returns symbols**: do NOT `map(sym)` over `keys` output — they are already symbols.
13. **`if` with `_` anaphora doesn't make a rule**: `if(_ symbol?, x, null)` doesn't create a function — `if` evaluates its condition. Use a named function.
14. **Use interpolation, not `str.join-on`**: `"{pfx}{name}"` not `[pfx, name] str.join-on("")`. Interpolation auto-converts values.
15. **Use `deep-transform` for recursive rewrites**: return non-null to replace, null to recurse. Avoids nested `if(block?, ..., if(list?, ...))`.
16. **Read `docs/eucalypt-style.md`** for idiomatic patterns: `when` over `if`, `bimap` for point-free, scope capture in blocks, sets for membership.

## Panics Are Critical

- **Panics must NEVER be deferred.** If you encounter a panic (thread panicked, assertion failure, etc.), stop what you are doing, reproduce a minimal case, and investigate the root cause immediately.
- **Use `eu dump` commands** (see above) to inspect the pipeline stage where the panic occurs — do NOT add temporary `eprintln!` debug statements.
- **File a P1 bug** and fix it before moving on to other work.
- **Convert panics to proper errors** as a first step if the root cause fix is complex — a `panic!` or `expect()` in user-reachable code should become an `ExecutionError` or `CoreError` diagnostic.
- **Every panic fix must include a regression test.**

## Running the `eu` Binary (Process Safety)

- The `eu` binary defaults to a 32 GiB managed heap limit (`--heap-limit-mib 32768`). Use `--heap-limit-mib 0` for unbounded.
- **Always wrap `eu` in `timeout`** to guard against divergent programs: `timeout 60 ./target/release/eu ...`
- **Benchmark verification**: Agent-reported benchmark numbers MUST be independently verified before acceptance. Always do a clean build (`cargo clean && cargo build --release`) when verifying.

## Code Quality Rules

- **ABSOLUTELY NEVER allow clippy warnings unless explicitly permitted by the user**
- **ALL clippy issues must be fixed, not suppressed with `#[allow()]` attributes**
- **Fix EVERY SINGLE clippy warning without exception**
- Maintain strict code quality standards throughout the codebase
- When asked to fix clippy issues, fix ALL of them systematically, not selectively

## Language and Style

- **Use UK English spelling**: optimisation (not optimization), utilisation (not utilization), colour (not color), etc.
- **Comments and documentation**: Follow UK English conventions throughout

## Development Workflow

### Pre-Commit Checklist
**ALWAYS run these commands before committing to avoid CI failures:**
1. `rustup update stable` - Ensure latest stable Rust to match CI (run weekly)
2. `cargo fmt --all` - Fix formatting issues for all targets
3. `cargo clippy --all-targets -- -D warnings` - Fix ALL lint warnings (matches CI exactly)
4. `cargo test --lib` - Verify tests pass (when appropriate)  
5. **Check and fix dependabot security alerts** - Address vulnerabilities immediately
6. `git commit` and `git push`

**CRITICAL Rust Version Matching**: 
- CI uses `dtolnay/rust-toolchain@stable` (latest stable Rust)
- Local development MUST use the same Rust version as CI to avoid clippy discrepancies
- Different Rust versions have different clippy rules - this causes the "local passes, CI fails" cycle
- Run `rustup update stable` regularly to stay current with CI

**CRITICAL Clippy Targeting**: Use `--all-targets` for clippy to match CI behaviour exactly. Local `--lib` checks miss test and bench targets that CI validates.

### Security and Dependencies
- **MANDATORY: Fix dependabot security alerts immediately** - treat them like clippy warnings
- **NEVER ignore security vulnerabilities** - address them with the same urgency as build failures
- Monitor GitHub security alerts and resolve them as part of standard development workflow
- Use `cargo update` to update dependencies within semver constraints
- Replace deprecated dependencies with modern alternatives (e.g., `atty` → `std::io::IsTerminal`)
- Fix deprecation warnings to maintain compatibility with newer dependency versions
- **Security alerts should block PRs** - just like clippy and rustfmt failures

## Development Standards (Based on Common Mistakes)

### Technical Analysis Standards  
- **Read relevant documentation FIRST** - always check `docs/` directory before making assumptions about language syntax or behaviour (see "Writing Eucalypt Code" section above)
- **Understand root causes, not just symptoms** - investigate HOW things work, not just WHETHER they work
- **Respect architectural boundaries** - only modify components within the defined scope (e.g., don't modify STG compiler when implementing Rowan parser)
- **Track your own changes** - don't assume existing working code is broken without verifying what you changed

### Communication and Progress Standards
- **Be precise about what "working" means** - distinguish between "not crashing" and "producing correct results"
- **Avoid flip-flopping** - gather solid evidence before changing diagnosis or approach
- **Don't repeat failed investigations** - if an approach isn't  yielding insights, step back and ask to try a different angle
- **Follow the scope of assigned tasks** - complete what's asked without expanding scope unnecessarily
