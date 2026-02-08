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
- **Read relevant documentation FIRST** - always check `docs/` directory before making assumptions about language syntax or behaviour
- **Understand root causes, not just symptoms** - investigate HOW things work, not just WHETHER they work
- **Respect architectural boundaries** - only modify components within the defined scope (e.g., don't modify STG compiler when implementing Rowan parser)
- **Track your own changes** - don't assume existing working code is broken without verifying what you changed

### Communication and Progress Standards
- **Be precise about what "working" means** - distinguish between "not crashing" and "producing correct results"
- **Avoid flip-flopping** - gather solid evidence before changing diagnosis or approach
- **Don't repeat failed investigations** - if an approach isn't  yielding insights, step back and ask to try a different angle
- **Follow the scope of assigned tasks** - complete what's asked without expanding scope unnecessarily
