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
