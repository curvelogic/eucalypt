# AGENTS.md — eucalypt

<!-- bd-doctor-divergence: ok -->
<!-- A pointer to CLAUDE.md by design; the two are not meant to match. -->

`CLAUDE.md` is this repository's canonical agent instruction file, for every
agent, not only Claude Code. Read it in full before starting work; this file
is only a pointer, plus the minimum an agent needs if it reads nothing else.

- **What it is:** eucalypt, a lazily evaluated functional language and tool
  for generating, templating and processing YAML, JSON and TOML, written in
  Rust.
- **Before writing any `.eu` code:** read `docs/reference/agent-reference.md`
  and `docs/appendices/syntax-gotchas.md`. Eucalypt's catenation, lambda
  syntax and prelude are unlike mainstream languages.
- **Check before every commit** (CI runs the same):

  ```bash
  cargo fmt --all
  cargo clippy --workspace --all-targets -- -D warnings
  cargo test
  ```

- **Review:** every PR is reviewed against `REVIEW.md`, which also lists
  what only the owner merges. Never merge your own PR.
- **Work tracking:** beads (`bd ready`, `bd show <id>`, `bd update <id>
  --claim`). See `CLAUDE.md` "Beads Issue Tracker" for syncing, including
  from cloud sessions.
- **Areas:** `.claude/skills/eucalypt-*/SKILL.md` hold the read-first lists
  and rules for the backend, frontend, diagnostics, tooling, docs and
  performance work. Read the one for the area you touch.
- Use UK English.
