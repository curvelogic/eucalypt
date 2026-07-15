# Beads / Project Management

Run `bd prime` for instructions on using `bd` to manage beads

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Eucalypt is a Rust-based tool and language for generating, templating, rendering and processing structured data formats like YAML, JSON and TOML. It is a lazily-evaluated functional language with a gradual type system, built in Rust with Cargo.

## Writing Eucalypt Code

**MANDATORY: Read the documentation BEFORE writing any eucalypt (`.eu`) code.** Agents that skip this produce incorrect code and waste cycles — catenation's pipeline semantics, lambda syntax, and prelude contents in eucalypt are all unlike mainstream languages.

### Required Reading

When writing or modifying `.eu` files, you MUST read these files first:
- `docs/reference/agent-reference.md` — dense syntax reference, prelude functions, pipeline patterns, and common pitfalls (READ THIS FIRST)
- `docs/appendices/syntax-gotchas.md` — critical precedence and syntax traps
- `docs/appendices/cheat-sheet.md` — quick syntax and operator reference
- `docs/eucalypt-style.md` — idiomatic style: pipeline/catenation over nested calls, juxtaposed `f[...]` / `g{...}` calls, `then` over `if`, `.result` over `.(...)`, point-free composition

For specific topics, also consult:
- `docs/guide/expressions-and-pipelines.md` — pipeline style and catenation
- `docs/guide/anaphora.md` — `_`, `•`, and string anaphora usage
- `docs/guide/functions-and-combinators.md` — function definition and partial application
- `docs/reference/prelude/` — full prelude reference by category

`agent-reference.md` §5 ("Common Pitfalls") and `syntax-gotchas.md` already cover the specific traps (catenation precedence, dot-vs-catenation, anaphora scoping, functions that don't exist, etc.) in more depth and nuance than a condensed list here could — read them rather than relying on a summary.

## Build and Development Commands

### Basic Development
- `cargo build` - Build the project
- `cargo test` - Run all tests (includes unit tests and harness tests)
- `cargo run` - Run the eucalypt binary
- `cargo install --path .` - Install local `eu` binary
- `cargo xtask prelude-compile` - Regenerate `lib/prelude.blob` (the pre-compiled prelude) after changing prelude source; the build falls back to compiling from source when the blob is absent or stale
- `eu doc <file>` - Extract documentation from eucalypt source (used to regenerate `docs/reference/prelude/`)

### Type Checking

Type checking runs **unconditionally** on every `eu` invocation (evaluate, dump, test) — it is not gated by a flag. Warnings go to stderr and never affect stdout or exit code unless `--strict` is used.

- `eu check file.eu` - check-only: validate type annotations and run the checker without evaluating; reports warnings
- `eu check --strict file.eu` - same, but promote warnings to errors and exit 1 (use for CI gates)
- `eu file.eu --strict` / `eu run --strict file.eu` - evaluate, but abort before evaluation with exit 1 if there are type warnings
- `eu file.eu --suppress-type-warnings` - evaluate normally, silencing warning output (the checker still runs)
- `--type-check` is a **deprecated no-op** kept for backwards compatibility — type checking used to be opt-in via this flag; it always runs now

### Testing
- `cargo test` - Run all tests including the comprehensive harness test suite
- `cargo test test_harness_001` - Run a specific harness test (e.g., test 001)
- `cargo test --test harness_test` - Run only the harness tests
- `cargo bench` - Run benchmarks (located in `benches/`)

### Build System
- Parsing uses a hand-written, lossless Rowan-based parser (`src/syntax/rowan/`); there is no separate grammar file or parser generator
- Build metadata is generated via `eu build.eu -t build-meta` and embedded in the binary via `build-meta.yaml`

## Architecture

### Core Components

**Parsing (`src/syntax/rowan/`)**: hand-written, lossless Rowan-based parser — no grammar file or generator.

**Core Pipeline (`src/core/`)**:
- `desugar/` - AST transformation and desugaring
- `cook/` - Operator precedence and fixity resolution
- `verify/` - Binding and content verification
- `simplify/` - Expression simplification and optimisation
- `transform/` - Various AST transformations including namespace lambda hoisting (`hoist.rs`)
- `inline/` - Inlining and reduction passes
- `typecheck/` - Gradual type system: type representation, parser, subtyping, bidirectional checker, polymorphic instantiation
- `analyse/` - Demand analysis (strictness/usage), feeding codegen decisions

**Evaluation (`src/eval/`)**:
- `machine/` - Virtual machine implementation with garbage collection
- `memory/` - Memory management, heap, and GC
- `stg/` - STG (Spineless Tagless G-machine) backend
- `intrinsics.rs` - Built-in functions and primitives

**I/O (`src/import/` and `src/export/`)**:
- Import: CSV, EDN, TOML, XML, YAML, text, JSONL, git refs, streaming sources
- Export: EDN, HTML, JSON, markup, table, text, TOML, YAML

**Driver (`src/driver/`)**:
- `eval.rs` - Main evaluation pipeline
- `options.rs` - Command-line option handling
- `check.rs` - `eu check` driver; also caches the prelude's type-check result once per process
- `tester.rs` - Test harness execution
- `doc/` - `eu doc` reference-doc extraction
- `lsp/` - Language Server Protocol server (stdio, via `lsp-server`)
- `statistics.rs` - `-S`/`--statistics` timing and pipeline stats reporting

### Test Architecture

**Harness Tests (`tests/harness/`)**:
- Large test suite covering language features, edge cases, and error conditions
- Test files use `.eu` extension for eucalypt source, plus YAML, TOML, CSV, XML, and EDN fixtures
- Error tests in `tests/harness/errors/` directory
- Benchmark tests in `tests/harness/bench/` directory

**Test Execution**:
- Tests are run via `tests/harness_test.rs`, one test per file in `tests/harness/`, using the `tester` module

#### Writing harness tests that gate

A harness test must genuinely gate: an assertion that fails must fail `cargo test`. How `lib/test.eu` turns a target's output into a PASS/FAIL verdict — the `RESULT` key, all-values-true inference, and `//=>` inline assertions — is documented in `docs/guide/testing.md` ("How the default verdict is computed"). Compute a target's `RESULT` from its checks so each one is in the verdict, following `tests/harness/189_r9oy_union_as_spec.eu` and `tests/harness/182_typedata_alias_resolution.eu`.

Every regression test must be **fault-injection verified**: break the code under test, confirm the harness test FAILs, restore it, confirm it PASSes — and the PR must state this was done. Further tester improvements are tracked by **eu-ntwg.2**.

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
| `EU_GC_VERIFY=2` | Full multi-checkpoint structural verification: header validity, pointer validity, line consistency (post-mark), forwarding pointer lifecycle (post-evacuate/update), and block list integrity (post-sweep). Level 2 implies level 1. |
| `EU_STACK_DIAG=1` | Log continuation stack composition to stderr whenever a new max stack depth is reached. |
| `EU_ERROR_TRACE_DUMP=1` | Dump full env trace and stack trace Smid details as diagnostic notes on every execution error. Useful for debugging which source locations are available at error time. |
| `EU_IO_TRACE=1` | Trace all `io.shell` and `io.exec` commands to stderr before execution. Shows the command string and whether stdin is piped. |
| `EU_HEAPSYN=1` | Select the legacy HeapSyn tree-walk engine instead of the default bytecode engine (BV1). Retained as the performance baseline and differential-testing engine; Phase 4 collapse is deferred pending an A/B perf study. |
| `EU_BYTECODE=1` | Now redundant — the bytecode engine is the default. Still accepted as an explicit opt-in; `EU_HEAPSYN=1` takes precedence over it. |

The crash signal handler (SIGSEGV/SIGBUS diagnostics) is always active and has no environment variable — it installs unconditionally in `main()`.

`--suppress-demand-analysis` (an `eu run` flag) disables the demand analysis pass for debugging, leaving all demands at `Unknown`.

### Dump Commands for Debugging

Use `eu dump <phase>` to inspect intermediate representations at each pipeline stage. These are the **primary tool** for investigating compiler and core expression issues — do NOT add temporary debug prints.

| Command | Shows |
|---------|-------|
| `eu dump ast <file>` | Parsed syntax tree |
| `eu dump desugared <file>` | Core expression after desugaring |
| `eu dump cooked <file>` | Core expression after operator precedence resolution |
| `eu dump split <file>` | Core expression after SCC-based LetRec splitting |
| `eu dump inlined <file>` | Core expression after namespace hoisting + inlining |
| `eu dump pruned <file>` | Core expression after dead code elimination |
| `eu dump demands <file>` | Core expression annotated with demand analysis results |
| `eu dump reflatten <file>` | Core expression after re-flattening nested Let scopes |
| `eu dump stg <file>` | Compiled STG syntax |
| `eu dump runtime <file>` | Runtime globals |

Add `--debug-format` for the Rust Debug representation (shows full structure including de Bruijn indices), or `--embed` for eucalypt source representation.

## Development Notes

- The project uses custom memory management - be careful when modifying anything in `src/eval/memory/`
- Parser changes are made directly in `src/syntax/rowan/` (hand-written; no code-generation step)
- Test files in `tests/harness/` serve as both tests and examples
- The `lib/` directory contains eucalypt library code (prelude, markup, test utilities)

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

## Engine Performance Claims — Mandatory Protocol

Any bytecode-vs-HeapSyn performance number that enters CHANGELOG, ROADMAP, a
release gate, a bead, or a report **must** be produced under the checked-in
measurement standard and cite a dated report or a ledger row:

- **Protocol:** `docs/superpowers/engine-ab/PROTOCOL.md` — interleaved bc/hs
  pairs on a quiet machine, ticks-first, wall median-of-N with spread, one
  binary/one blob for both engines, nothing under ~200 ms in ratio analysis,
  and a confidence label (measured-verified / measured-single / projected) on
  every figure. Read it before quoting any engine number.
- **Canonical suite:** the eight `tests/harness/bench/{015..022}` benches (run
  under `cargo test` and `EU_HEAPSYN=1 cargo test`).
- **Runner + ledger:** `cargo xtask engine-ab` runs the suite interleaved and
  appends to `docs/superpowers/engine-ab/results.jsonl`; `cargo xtask engine-ab
  --check` flags regressions vs the previous run.

## Code Quality, Style, and Security

- **Never allow clippy warnings**: fix every one (don't suppress with `#[allow(...)]`) unless the user explicitly permits it. Use `cargo clippy --all-targets -- -D warnings` — `--all-targets` matters, since `--lib` alone misses tests and benches that CI validates.
- **Use UK English spelling** throughout code, comments, and documentation: optimisation, utilisation, colour, etc.
- **Fix dependabot/security alerts immediately**, with the same urgency as a build failure or clippy warning — treat them as blocking a PR. Prefer replacing deprecated dependencies over suppressing warnings (e.g. `atty` → `std::io::IsTerminal`).

## Development Workflow

### Pre-Commit Checklist
**ALWAYS run these commands before committing, in order, to avoid CI failures:**
1. `rustup update stable` - CI uses `dtolnay/rust-toolchain@stable` (latest stable), so a stale local toolchain causes clippy discrepancies ("local passes, CI fails"); run this weekly
2. `cargo fmt --all`
3. `cargo clippy --all-targets -- -D warnings`
4. `cargo test` - run the full suite (not just `--lib`) when the change might affect harness tests
5. `git commit` and `git push`


<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:970c3bf2 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   bd dolt push
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->
