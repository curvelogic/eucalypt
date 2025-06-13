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
- Build metadata is generated from `Eufile` configuration

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

**Harness Tests (`harness/test/`)**:
- Comprehensive test suite with 50+ test files
- Tests cover language features, edge cases, and error conditions
- Test files use `.eu` extension for eucalypt source
- Also includes YAML, TOML, CSV, XML, and EDN test files
- Error tests in `harness/test/errors/` directory
- Benchmark tests in `harness/test/bench/` directory

**Test Execution**:
- Tests are run via `tests/harness_test.rs`
- Each test corresponds to a file in `harness/test/`
- Tests can be run individually or as a complete suite
- Uses the `tester` module for test execution

### Memory Management

The project includes a sophisticated garbage collector:
- Generational GC with multiple generations
- Bump allocation for young generation
- Mark-and-sweep for older generations
- Custom memory layout for eucalypt values

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
- Test files in `harness/test/` serve as both tests and examples
- The `lib/` directory contains eucalypt library code (prelude, markup, test utilities)
- Build metadata is embedded in the binary via `build-meta.yaml` and `Eufile`

## Code Quality Rules

- **ABSOLUTELY NEVER allow clippy warnings unless explicitly permitted by the user**
- **ALL clippy issues must be fixed, not suppressed with `#[allow()]` attributes**
- **Fix EVERY SINGLE clippy warning without exception**
- Maintain strict code quality standards throughout the codebase
- When asked to fix clippy issues, fix ALL of them systematically, not selectively

## Development Workflow

### Pre-Commit Checklist
**ALWAYS run these commands before committing to avoid CI failures:**
1. `cargo fmt` - Fix formatting issues
2. `cargo clippy --lib -- -D warnings` - Fix all lint warnings
3. `cargo test --lib` - Verify tests pass (when appropriate)
4. `git commit` and `git push`

### Security and Dependencies
- Monitor dependabot alerts and fix security vulnerabilities promptly
- Use `cargo update` to update dependencies within semver constraints
- Replace deprecated dependencies with modern alternatives (e.g., `atty` → `std::io::IsTerminal`)
- Fix deprecation warnings to maintain compatibility with newer dependency versions

## Project Management

### GitHub Project
- **Eucalypt Backlog**: Private GitHub project containing development plans, roadmap, and work items
- Access via: `gh project list --owner curvelogic` (shows project ID: `PVT_kwDOAG6azc4AAShI`)
- **Privacy Note**: The repository is public but the project is private - do not reference specific project items in public commits

### GC Implementation
- See `docs/gc-implementation.md` for comprehensive garbage collector documentation
- **Phase 1 COMPLETE**: Comprehensive GC test suite with 10 test scenarios covering all major functionality
- **GC Tests Location**: `harness/test/gc/gc_001_*.eu` through `gc_010_*.eu`
- **Test Integration**: Added to `tests/harness_test.rs` with functions `test_gc_001()` through `test_gc_010()`
- **Current Status**: Feature-complete implementation, all tests passing, ready for production hardening
- **Next Phase**: Error handling, long-running stability, performance monitoring