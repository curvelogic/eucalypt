---
name: eucalypt-frontend
description: Working on eucalypt's frontend — the Rowan parser, desugaring, cooking (operator precedence), verification, simplification, core transformations and inlining, the type checker, or the prelude and other library code in lib/. Use before changing, reviewing or debugging src/syntax/, src/core/ or lib/*.eu, and before writing any .eu code.
---

# eucalypt frontend: syntax, core pipeline, type checker, prelude

## Scope

- `src/syntax/` — the hand-written Rowan parser (`src/syntax/rowan/`; no
  grammar file or generator), lexer and AST
- `src/core/` — desugar, cook, verify, simplify, transform, inline,
  typecheck, analyse
- `src/driver/` — driver options and the evaluation pipeline
- `lib/` — prelude and library code

## Read first

- `CLAUDE.md` — "Writing Eucalypt Code", pre-commit checklist, `eu dump`
  phases
- `docs/reference/agent-reference.md` and
  `docs/appendices/syntax-gotchas.md` — before **any** `.eu` edit
- `docs/eucalypt-style.md` — idiomatic style for library code
- `docs/development/gradual-typing-spec.md` — before type-checker work
- The bead (`bd show <id>`) and any spec it references in
  `docs/superpowers/specs/`: every phase and success criterion must be met
  before the work is called complete

## Rules

- Every change carries harness tests in `tests/harness/`, and they must
  gate and be fault-injection verified (`CLAUDE.md` "Writing harness tests
  that gate"); say in the PR that you did it.
- Changed behaviour ships with its documentation; a prelude change
  regenerates `docs/reference/prelude/` (`eu doc --prelude --output-dir
  docs/reference/prelude/`, which CI checks) and, when it changes the
  prelude's inputs, `lib/prelude.blob` (`cargo xtask prelude-compile`).
- A new language feature, or a deliberate change to what a construct means,
  is a change the owner reviews (`REVIEW.md`). Bug fixes proceed;
  intentional semantic changes stop.
- Prefer the general mechanism: flag and avoid hard-coded special cases for
  particular types or functions, metadata-driven side channels that bypass
  the type system, and duplicated logic that should be unified.
- Challenge an instruction that looks architecturally wrong rather than
  implementing it.
- Use `eu dump <phase>` to inspect the pipeline, not temporary debug
  prints; wrap every `eu` run in `timeout`.
- Work in a worktree branched from `master`, one bead per PR, PR to
  `master`.
