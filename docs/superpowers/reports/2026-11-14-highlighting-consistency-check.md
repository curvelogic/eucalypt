# Highlighting consistency check (eu-ioz21.1)

Date: 2026-11-14. Builds on
`2026-11-13-textmate-vs-rowan-highlighting.md` (eu-ioz21.2), whose groups
the allow-list cites.

## What it checks

`scripts/check-highlighting-consistency.js` runs `eu lsp` and requests
`textDocument/semanticTokens/full` for each corpus file (the reference),
tokenises it with the VS Code grammar through vscode-textmate, and runs
`tree-sitter query --captures queries/highlights.scm`. Each highlighter
paints every UTF-16 code unit with a fine class; the comparison projects
the classes onto a coarse set (comment, string, number, symbol,
operator, declaration, parameter, metadata, punctuation, name), because
rowan classifies names by symbol-table lookup rather than scope and does
not distinguish calls, anaphora, constants or intrinsics (report groups
1, 2 and 4). Two disagreements are reported: a rowan token whose extent
the other highlighter does not paint wholly with the same coarse class
(`mismatch`), and a lexical or declaration run the other highlighter
paints where rowan has no token (`missing`).

Tree-sitter captures are applied as `tree-sitter-highlight` 0.25 (used
by `cargo xtask highlight-html`) applies them: the inner node wins, and
for the same node the later pattern wins. Files in
`editors/tree-sitter-eucalypt/test/corpus-expected-failures.txt` are not
compared with tree-sitter. Over 420 files the run takes about 3 s.

## Defects found and fixed (highlights.scm)

The query was ordered as if the first pattern won. With the later
pattern winning:

- the prelude-name `@keyword` pattern overrode declarations, parameters
  and symbol names (`value:`, `count:`, `:count`);
- `":" @punctuation.delimiter` overrode a symbol's colon;
- the namespaced-prelude pattern lost its captures to `(operator)`, and
  `@_key` captures in the doc-value patterns left `doc` keys unstyled
  (tree-sitter-highlight drops a match when a later pattern claims one of
  its nodes);
- quoted declaration names, `f({x y})` shorthand fields, renamed field
  bindings and juxtaposed `f{..}`/`f[..]` parameters had no capture;
- metadata block keys were `@property`; they are now `@attribute`, as
  TextMate's metadata names, and an operator declaration's operator is
  `@operator` (it was `@function`).

`xtask` test `contexts_override_general_captures` pins the main cases.

## Accepted differences

`editors/test-fixtures/highlighting/consistency-allowlist.json` holds 13
rules: rowan defects filed as eu-oaz72.115 (block-in-call declarations
as parameters, operator-declaration operands as declarations,
block-destructuring parameters as metadata, `s"..."` without a token);
rowan's metadata classification (group 6); TextMate's metadata
operators and unit-metadata limits; tree-sitter leaving references
unstyled; and two tree-sitter grammar defects needing `grammar.js`
changes, listed below.

## Follow-ups (grammar.js)

- **Symbol lexing.** `symbol` is `seq(':', identifier)` without
  `token.immediate`, so `a: x` followed by a declaration whose name
  starts with a non-ASCII letter (`β: y`) parses as the soup `x β :y` of
  the first, with no ERROR node; `{x: number}` in a string becomes
  `x :number`; `{x:badfmt}`'s spec is read as a symbol; and plain strings
  treat `\"` as an escape. Making the name immediate fixes most of it but
  breaks `⌈ [f: lists] ⌉:` in `097_idiot_brackets.eu`, which parses only
  through this defect and needs cons patterns in bracket declaration
  heads (GLR conflicts). 38 files are allow-listed for tree-sitter
  wholesale until then.
- **Bracket delimiters** are anonymous regex tokens, so the query can only
  capture the whole `bracket_expr`: its delimiters cannot be `@operator`
  and uncaptured names inside it are `@punctuation.special`. Aliasing
  them to named nodes would fix both.
