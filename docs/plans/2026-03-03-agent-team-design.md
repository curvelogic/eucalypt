# 0.4.0 Agent Team Design

**Status:** Design approved

## Overview

Six named agents plus a human coordinator deliver eucalypt 0.4.0.
All work accumulates on a `0.4.0` integration branch. Nothing
touches master until the project owner reviews the branch as a whole.

| Agent | Role | Scope |
|-------|------|-------|
| **Quill** | Frontend | Syntax, parsing, desugar, cook, core transforms |
| **Furnace** | Backend | STG compiler, VM, GC, memory, intrinsics |
| **Lantern** | Tooling | Emacs mode, tree-sitter, LSP, editor integration |
| **Wicket** | Gatekeeper | Validates and merges PRs into `0.4.0` branch |
| **Stopwatch** | Performance | Ongoing performance improvements |
| **Clarion** | Errors | Ongoing error message improvements |
| Coordinator | Team lead | Routes work, resolves blockers, does not code |

The coordinator manages agent interaction and escalates decisions to
the project owner. The project owner makes final decisions on master
merges and the persistent blocks experiment.

---

## Section 1: Branch Strategy

```
master (frozen during 0.4.0 work)
  └── 0.4.0 (Wicket's integration branch)
        ├── feat/quill-<name>      (Quill's feature branches)
        ├── feat/furnace-<name>    (Furnace's feature branches)
        ├── feat/lantern-<name>    (Lantern's feature branches)
        ├── perf/stopwatch-<name>  (Stopwatch's proposals)
        ├── errors/clarion-<name>  (Clarion's proposals)
        └── feat/furnace-persistent-blocks  (experimental, late-stage)
```

- Wicket creates `0.4.0` from master at project start
- All agents branch from `0.4.0` and create PRs targeting `0.4.0`
- Wicket merges PRs after validation (see Section 4)
- The project owner reviews `0.4.0` and merges to master when
  satisfied

### Persistent blocks exception

The persistent blocks work (eu-m59i, eu-hmji) is experimental. Furnace
builds it on a long-lived feature branch
(`feat/furnace-persistent-blocks`), keeping it rebased on `0.4.0`.
This branch includes benchmark comparison against the current
cons-list representation.

Wicket does NOT merge persistent blocks autonomously. When benchmarks
are ready, the coordinator presents results to the project owner, who
decides whether to include them in 0.4.0 or defer. No other 0.4.0
work may depend on persistent blocks being present.

---

## Section 2: Agent Interaction Model

```
Project owner (human)
  ↕ (directs work, decides persistent blocks, reviews 0.4.0)
Coordinator (me)
  ↕ (routes tasks, manages dependencies, escalates)
  ├── Quill ──→ PRs ──→ Wicket
  ├── Furnace ──→ PRs ──→ Wicket
  ├── Lantern ──→ PRs ──→ Wicket
  ├── Stopwatch ──→ PRs ──→ Wicket
  └── Clarion ──→ PRs ──→ Wicket
                           ↓
                      0.4.0 branch
                           ↓
                  Project owner reviews & merges to master
```

### Communication rules

- **Quill, Furnace, Lantern** message the coordinator when they
  complete a task, hit a blocker, or need a design decision
- **Wicket** messages the coordinator when a PR fails tests, has
  conflicts, or raises concerns
- **Stopwatch and Clarion** produce PRs autonomously. Wicket handles
  them without coordinator involvement unless there are concerns
- **Lantern** watches for PRs from Quill and Furnace that affect the
  language surface and creates follow-up beads for editor support
- The coordinator ensures agents respect dependency ordering and do
  not start blocked work

---

## Section 3: Work Sequencing

### Quill (frontend)

```
Phase 1 (bugs, no deps):   eu-mi8d, eu-dobu        (parallel)
Phase 2 (small features):  eu-jrrt, eu-v1dr         (parallel)
Phase 3 (large feature):   eu-spqy (destructuring)
Phase 4 (depends on spqy): eu-wenf (idiot brackets)
Phase 5 (depends on wenf): eu-1x9a (monadic blocks)
Phase 6 (if plan exists):  eu-0i4b (assertion operators)
```

### Furnace (backend)

```
Phase 1 (bugs, no deps):   eu-xxrx, eu-nqde         (parallel)
Phase 2 (infrastructure):  eu-skjg (pinning)
Phase 3 (depends on skjg): eu-7flw (str_arg_ref)
Phase 4 (investigation):   eu-cb4k (DCE)             (independent)
Phase 5 (new type):        eu-atzn (tensors)          (independent)
Phase 6 (build):           eu-raaa (WASM target)      (independent)
Phase 7 (experimental):    eu-hmji + eu-m59i          (late-stage)
```

Phases 4-6 can run in any order once Phase 3 completes. Phase 7
stays on its own branch and is not merged until the project owner
decides.

### Lantern (tooling)

```
Phase 1 (after eu-jrrt):   Unicode input, transient menu
Phase 2 (core mode):       Rename, keymap, commands, project detection
Phase 3 (LSP + bugs):      eglot integration, # in strings, indent fix
Phase 4 (after spqy/wenf): Update highlights, bracket colouring
Phase 5 (cleanup):         Delete legacy mode, final verification
```

### Stopwatch and Clarion

Continuous throughout. No phasing — they pick targets and produce
PRs as they go.

### Wicket

Reactive — processes PRs as they arrive.

---

## Section 4: Wicket's Merge Criteria

### Code quality gate

1. All tests pass: `cargo test`
2. No clippy warnings: `cargo clippy --all-targets -- -D warnings`
3. No conflicts with current `0.4.0` head
4. Change is within scope of the bead it claims to address

### Documentation gate

Before merging any PR, Wicket verifies that documentation has been
updated appropriately:

| Change type | Required documentation |
|-------------|----------------------|
| New syntax / operator | `doc/reference/syntax.md`, `doc/appendices/cheat-sheet.md` |
| New prelude function | Relevant `doc/reference/prelude/*.md` |
| Changed error message | `doc/reference/error-messages.md` if listed |
| New intrinsic | Relevant reference section |
| New import/export format | `doc/reference/import-formats.md` or `export-formats.md` |
| New CLI flag | `doc/reference/cli.md` |
| New language feature | Guide section in `doc/guide/` |
| Changed behaviour | `doc/appendices/syntax-gotchas.md` if relevant |

If a PR lacks required documentation, Wicket sends it back to the
originating agent with a specific request. The PR is not merged
until documentation is present.

### Surge agent criteria

- **Stopwatch PRs:** Net positive performance impact across the full
  test suite. No significant regressions. Change must be an engine
  improvement, not moving eucalypt logic into Rust intrinsics.
- **Clarion PRs:** Genuine improvement to error message quality.
  Existing error expectation tests pass.

### Persistent blocks exception

Wicket does NOT merge the persistent blocks branch. The coordinator
presents benchmark results to the project owner, who decides.

---

## Section 5: Common Agent Instructions

All 0.4.0 agents share these rules:

- Work on branches from `0.4.0`, create PRs targeting `0.4.0`
- Follow `CLAUDE.md` conventions (clippy, UK English, pre-commit
  checklist)
- Read `doc/appendices/syntax-gotchas.md` before writing eucalypt
  code
- Include documentation updates with every PR
- Never merge your own PRs — Wicket merges
- Use `bd update` to mark beads `in_progress` when starting work
  and `bd close` when the PR is created and accepted by Wicket
- Message the coordinator on completion, blockers, or design
  questions

---

## Section 6: Agent-Specific Instructions

### Quill (frontend)

- Read `src/syntax/`, `src/core/` thoroughly before starting
- Key files: `src/syntax/parser.rs`, `src/core/desugar/rowan_ast.rs`,
  `src/core/cook/mod.rs`, `src/core/verify/`, `src/core/simplify/`
- For large features (destructuring, brackets, monadic blocks), read
  the design doc AND implementation plan before starting
- Respect phasing: bugs first, small features, then large features
  in dependency order

### Furnace (backend)

- Read `src/eval/` thoroughly before starting
- Key files: `src/eval/stg/compiler.rs`, `src/eval/machine/vm.rs`,
  `src/eval/memory/`, `src/eval/stg/support.rs`,
  `src/eval/stg/block.rs`
- Be careful with memory management code — the GC is subtle
- Respect dependency chain: pinning before str_arg_ref, persistent
  sets before persistent blocks
- Keep the persistent blocks branch rebased on `0.4.0`

### Lantern (tooling)

- Primary workspace is Emacs lisp, tree-sitter queries, and editor
  configuration — not the Rust codebase
- Watch PRs from Quill and Furnace for language surface changes
  (new syntax, operators, error types, CLI flags)
- Create follow-up beads for editor support when new syntax lands
- Ensure tree-sitter grammar, highlights, and Emacs mode stay in
  sync with all language changes

### Wicket (gatekeeper)

- Create and maintain the `0.4.0` branch
- Process PRs in arrival order unless dependencies dictate otherwise
- Apply all merge criteria (code quality + documentation gates)
- Send back inadequate PRs with specific, actionable feedback
- Message the coordinator if PRs conflict, raise architectural
  concerns, or if Stopwatch/Clarion changes seem risky
- Do NOT merge `feat/furnace-persistent-blocks` — escalate to
  coordinator

### Stopwatch (updated for 0.4.0)

- Branch from `0.4.0`, PR against `0.4.0`
- Same workflow as standalone surge (profile, hypothesise, implement,
  validate, propose)
- Wicket decides whether to merge (not the project owner directly)

### Clarion (updated for 0.4.0)

- Branch from `0.4.0`, PR against `0.4.0`
- Same workflow as standalone surge (provoke, evaluate, implement,
  propose)
- Wicket decides whether to merge (not the project owner directly)
