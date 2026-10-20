---
name: eucalypt-tooling
description: Working on eucalypt's editor and developer tooling — the Emacs mode, the VS Code extension, the tree-sitter grammar, the LSP server, and the WASM target and its JS API. Use before changing or reviewing editors/, src/driver/lsp/ or src/wasm*.rs, and after any merged change to syntax, operators, error types, CLI flags or prelude functions, which the tooling must follow.
---

# eucalypt tooling: editors, LSP, tree-sitter, WASM

## Scope

- `editors/emacs/` — Emacs major mode
- `editors/vscode/` — VS Code extension
- `editors/tree-sitter-eucalypt/` — tree-sitter grammar and corpus
- `src/driver/lsp/` — LSP server (stdio, via `lsp-server`)
- `src/wasm.rs`, `src/wasm_pipeline.rs` — WASM target and JS API

## Read first

- `CLAUDE.md` — pre-commit checklist
- `docs/reference/agent-reference.md` and
  `docs/appendices/syntax-gotchas.md` — before any `.eu` edit, grammar
  change or highlighting rule
- `.github/workflows/tree-sitter.yaml` and
  `scripts/check-tree-sitter-corpus.py` — what CI checks for the grammar

## Keeping tooling in step with the language

When a change lands that affects the language surface, follow it here:

- New syntax or operator → tree-sitter grammar and highlights, Emacs and
  VS Code modes
- New error type → LSP diagnostics
- New CLI flag → VS Code extension
- New prelude function → keyword lists and snippets

## Rules

- Tests that gate, fault-injection verified, as `CLAUDE.md` "Writing harness
  tests that gate" describes; say in the PR that you did it.
- The bead (`bd show <id>`) and any spec it references must be met in full
  before the work is called complete.
- Work in a worktree branched from `master`, one bead per PR, PR to
  `master`.
- Review and merge: a fresh `rig-review` against `REVIEW.md` on `master`.
