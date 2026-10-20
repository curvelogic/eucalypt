---
name: eucalypt-docs
description: Writing or changing eucalypt's user documentation — the guide, reference, prelude reference, appendices and llms.txt files under docs/, and the code examples in them. Use before editing docs/, before regenerating docs/reference/prelude/, and whenever a code change alters documented behaviour.
---

# eucalypt documentation

## Scope

- `docs/` — the mdBook (`book.toml`, `docs/SUMMARY.md`): guide, reference,
  appendices, development notes
- `docs/reference/prelude/` — generated from `lib/prelude.eu` doc strings;
  hand-written additions live in `docs/reference/prelude/supplements/`
- `docs/llms.txt`, `docs/llms-full.txt` — agent-facing summaries
- `docs/reference/agent-reference.md`, `docs/appendices/syntax-gotchas.md`,
  `docs/appendices/cheat-sheet.md` — the references agents read before
  writing `.eu`

## Read first

- `docs/reference/agent-reference.md` and
  `docs/appendices/syntax-gotchas.md` — before writing any `.eu` example
- `docs/eucalypt-style.md` — examples should show idiomatic style

## Rules

- Changed behaviour ships with its documentation in the same PR; a new
  feature is not done without docs.
- Never hand-edit generated prelude pages: change the doc string in
  `lib/prelude.eu` (or a supplement) and regenerate with
  `./target/release/eu doc --prelude --output-dir docs/reference/prelude/`.
  CI fails on stale prelude docs.
- Code examples are tested: `python3 scripts/test-doc-examples.py --eu
  ./target/release/eu --timeout 30` (CI runs it). Tag a block that cannot
  run as `eu,notest`; prefer `//=>` assertions so an example checks itself.
- Regenerate `docs/llms-full.txt` with `scripts/generate-llms-full.sh` when
  pages are added, removed or reordered in `docs/SUMMARY.md`.
- Use UK English.
- Analysis, measurements and investigation narrative belong in
  `docs/superpowers/reports/` or `docs/superpowers/plans/`, linked from the
  PR, not in user documentation or the PR body.
- A PR from an open-ended "find things to improve" dispatch is one the
  owner reviews (`REVIEW.md`); directed work is reviewed normally.
- Run the `CLAUDE.md` pre-commit checklist (`cargo fmt --all`, `cargo clippy
  --workspace --all-targets -- -D warnings`, `cargo test`) before every PR.
- Review and merge: `REVIEW.md` on `master`. Until it lands, the same rules
  are in `CLAUDE.md` ("Recorded review before merge", "Owner-hold PRs") and
  the current gatekeeper's owner review filter.
