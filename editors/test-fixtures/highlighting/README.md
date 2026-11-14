# Highlighting fixtures

Small eucalypt files covering the language's lexical and declaration
syntax, shared by the editor grammars:

- `strings.eu`: plain, raw, c-, t- and s-strings; interpolation
  (`{x}`, `{a.b}`, `{ x }`, `{0}`, `{}`, `{x:%06d}`) and its edge cases
  (`{{`/`}}`, `"{{{x}}}"`, `"{x}}}"`, c-string `\{`/`\}`); multi-line strings
- `declarations.eu`: property, function, juxtaposed (`f[a, b]:`,
  `f{x y}:`) and destructuring (`([a, b])`, `({x y})`, `([h : t])`)
  heads; quoted names; operator heads (binary, prefix, postfix, nullary);
  backtick metadata (doc strings, metadata blocks); nested declarations
- `calls-and-anaphora.eu`: `f(...)`, `f[...]`, `f{...}` calls,
  pipelines, sections, expression and block anaphora, generalised and
  namespace lookup, prelude names, language constants, intrinsics
- `brackets-and-blocks.eu`: idiot bracket declarations and uses (`⟦ ⟧`,
  `« »`, `⌈ ⌉`, `⌊ ⌋`, `‹ ›`, a monadic `⟪ ⟫`), `{ :for ... }`,
  `{ :let ... }` blocks
- `operators-and-assertions.eu`: numbers, symbols, ASCII and Unicode
  operators, `//=>`, `//=`, `//!`, `//=?`, `//=?>`, `//` and `//<<`

Every file parses and evaluates with `eu` (check with
`eu <file>.eu`). The VS Code extension's tests
(`editors/vscode/test/grammar.test.js`) assert the TextMate scopes of
tokens on these files; the rowan LSP semantic tokens are the reference
classification. Follow `docs/eucalypt-style.md` when adding to them.

## Consistency check

`scripts/check-highlighting-consistency.js` classifies every token of
these fixtures, `lib/*.eu` (not `prelude.eu`), `tests/harness/**/*.eu`
outside `errors/` and `examples/aoc25/**/*.eu` with the TextMate grammar,
the tree-sitter `highlights.scm` and the LSP's semantic tokens, and fails
on any disagreement with the semantic tokens that
`consistency-allowlist.json` does not accept. Each allow-list rule gives
its reason and a reference. To run it:

```sh
cargo build --release
(cd editors/vscode && npm ci)
(cd editors/tree-sitter-eucalypt && npm ci)
node scripts/check-highlighting-consistency.js            # the check
node scripts/check-highlighting-consistency.js --summary  # grouped counts
```

`--only textmate` or `--only tree-sitter` checks one grammar; files on
the command line replace the corpus. A rule that no longer matches
anything is reported as a warning: remove it once the defect it accepts
is fixed. The comparison is on coarse classes where the highlighters are
meant to agree (every kind of name reference compares as `name`); see
`docs/superpowers/reports/2026-11-14-highlighting-consistency-check.md`.
