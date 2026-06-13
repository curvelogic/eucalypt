# W5 Phase 3: Fuzz Targets (cargo-fuzz)

- **Date:** 2026-06-13
- **Status:** Draft
- **Bead:** eu-kgsi.8
- **Roadmap:** W5 — Conformance corpus, property tests & fuzzing (Phase 3)

---

## 1. Problem

The hand-written parser (`src/syntax/rowan/`), the type-DSL parser
(`src/core/typecheck/parse.rs`), and the import loaders (YAML, JSON, TOML,
CSV, XML) process untrusted input. Any panic in these paths is a
denial-of-service in the LSP and a crash in the CLI. There is no fuzz
testing to discover panics, hangs, or memory issues on adversarial input.

## 2. Goal

Three `cargo-fuzz` targets with seed corpora, run to exhaustion before
the 1.0 freeze. Every panic discovered becomes a diagnostic with a
regression test.

## 3. Design

### 3.1 Targets

**`fuzz_type_dsl`** (highest priority)

- Input: arbitrary bytes interpreted as a type-DSL string.
- Entry point: `typecheck::parse::parse_type(&input)`.
- The type-DSL parser is hand-written and processes untrusted annotation
  strings (any user can write `{ type: "..." }`). Panics here crash the
  LSP on hover or the CLI on `eu check`.
- Expected behaviour: returns `Ok(Type)` or `Err(ParseError)`, never
  panics.

**`fuzz_parser`**

- Input: arbitrary bytes interpreted as eucalypt source.
- Entry point: `syntax::rowan::parse::parse(&input)`.
- The Rowan parser is the largest hand-written parser. With Phase 2 error
  recovery (W4), it must handle any input without panicking.
- Expected behaviour: returns a `SyntaxTree` (possibly with error nodes),
  never panics.

**`fuzz_loader`**

- Input: arbitrary bytes with a format tag prefix byte selecting the
  loader (0=YAML, 1=JSON, 2=TOML, 3=CSV, 4=XML).
- Entry point: the relevant import loader in `src/import/`.
- These wrap third-party crates (`serde_yaml`, `serde_json`, `toml`,
  `csv`, `roxmltree`), but our wrapper code can panic on unexpected
  shapes from the underlying parsers.
- Expected behaviour: returns imported data or an error, never panics.

### 3.2 Seed corpora

Each target gets a seed corpus from existing test data:

| Target | Seed source |
|---|---|
| `fuzz_type_dsl` | All `type:` annotation strings extracted from `lib/prelude.eu` and `tests/harness/typecheck/*.eu` |
| `fuzz_parser` | All `.eu` files in `tests/harness/` and `lib/` |
| `fuzz_loader` | All `.yaml`, `.json`, `.toml`, `.csv`, `.xml` files in `tests/harness/` |

A script or xtask extracts the seeds and populates `fuzz/corpus/`.

### 3.3 Panic policy

Every panic discovered by fuzzing is handled as:

1. Add the crashing input to the regression corpus.
2. Write a minimal reproduction as a harness test.
3. Convert the panic to a proper error (diagnostic with source location).
4. Verify the fix doesn't regress.

This aligns with the project's panic policy (CLAUDE.md): panics in
user-reachable code must become `ExecutionError` or `CoreError` diagnostics.

### 3.4 Infrastructure

```
fuzz/
├── Cargo.toml              # cargo-fuzz workspace member
├── fuzz_targets/
│   ├── fuzz_type_dsl.rs
│   ├── fuzz_parser.rs
│   └── fuzz_loader.rs
├── corpus/
│   ├── fuzz_type_dsl/      # seed corpus
│   ├── fuzz_parser/
│   └── fuzz_loader/
└── artifacts/              # crash artifacts (gitignored)
```

The `fuzz/` directory follows the standard `cargo-fuzz` layout. It is a
separate workspace member that depends on the `eucalypt` library crate.

### 3.5 CI integration

Fuzz targets are **not** run in CI on every push — fuzzing is
long-running and non-deterministic. Instead:

- Seed corpus is checked in and regression-tested via a simple
  `cargo test` that runs each seed through the target function (ensuring
  no regressions on known inputs).
- A periodic CI job (weekly or before releases) runs each target for a
  bounded duration (e.g. 10 minutes per target).
- Pre-1.0: a 24-hour fuzz run on each target, per the roadmap.

### 3.6 Hang detection

`cargo-fuzz` detects hangs via a timeout (default 60s per input). For the
parser and type-DSL targets, a 5-second timeout is appropriate — any
input that takes longer than 5 seconds to parse is a bug (algorithmic
complexity issue).

## 4. Implementation sketch

### Phase 1: Infrastructure
- `cargo install cargo-fuzz`.
- Create `fuzz/` directory structure.
- Add `fuzz/Cargo.toml` depending on `eucalypt` library.

### Phase 2: `fuzz_type_dsl` target
- Write the fuzz target.
- Extract seed corpus from prelude and typecheck test annotations.
- Run initial fuzzing session (1 hour).
- Fix any panics found.

### Phase 3: `fuzz_parser` target
- Write the fuzz target.
- Copy `.eu` test files as seeds.
- Run initial fuzzing session.
- Fix any panics found.

### Phase 4: `fuzz_loader` target
- Write the fuzz target with format-tag dispatch.
- Copy test data files as seeds.
- Run initial fuzzing session.
- Fix any panics found.

### Phase 5: Regression test integration
- A `#[test]` in `tests/fuzz_regression_test.rs` that runs all seed
  corpus entries through the target functions.
- Crash artifacts from fuzzing are added as regression seeds.

## 5. Dependencies

- **W4 Phase 2 (parser recovery)**: the parser should have error recovery
  before fuzzing it — otherwise every malformed input is a "bug" when
  it's really just missing recovery. Fuzz after recovery is implemented
  to find genuine panics.

## 6. Test plan

- Each target runs for at least 1 hour with no panics on initial setup.
- All seed corpus entries pass as regression tests in CI.
- Every panic found is converted to a diagnostic with a regression test.
- Pre-1.0: 24-hour run per target with zero panics.

## 7. Success criteria

- **Zero panics** after a 24-hour fuzz run on each target.
- Every discovered panic has been converted to a diagnostic.
- Regression tests prevent re-introduction.
- The seed corpus covers the project's existing test data.
