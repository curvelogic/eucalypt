# TextMate grammar against rowan semantic tokens (eu-ioz21.2)

Date: 2026-11-13. Grammar: `editors/vscode/syntaxes/eucalypt.tmLanguage.json`
as updated by eu-ioz21.2. Reference: the LSP's semantic tokens
(`src/driver/lsp/semantic.rs`) from `eu lsp`.

## Method

An ad hoc script (not committed; eu-ioz21.1 builds the real check)
tokenised each file with vscode-textmate and requested
`textDocument/semanticTokens/full` from `eu lsp`. For every rowan token
it mapped the innermost meaningful TextMate scope to a rowan class
(`string.*` → string, `keyword.operator.*` → operator,
`entity.name.function` → function.declaration, `variable.parameter` →
parameter.declaration, `support.function.prelude` →
function.defaultLibrary, `support.variable.prelude` →
variable.defaultLibrary, and so on) and compared classes and extents. It
also listed TextMate tokens with a class where rowan emits nothing.

Corpora: the fixtures in `editors/test-fixtures/highlighting/`, then
`lib/*.eu` (without `prelude.eu`, where every prelude name is a local)
and `tests/harness/*.eu`.

## Grammar defects found and fixed this way

- `(x !?):` split `!?` into `!` and a `?` parameter (backtracking);
  identifier and operator quantifiers are now possessive.
- Nullary operator declarations `(∅): 42` were not declarations.
- Bracket declarations with structured contents (`⌈{}⌉:`, `⌈[x]⌉:`,
  `⌈ [f: lists] ⌉:`) were not declarations.
- Doc strings carried no string scope on their quotes.

## Accepted differences

On the fixtures and the corpus, every remaining disagreement falls in
one of these groups.

1. **Rowan classifies by name, not scope.** The symbol table holds the
   unit's and the prelude's top-level names and is looked up by name
   wherever an identifier appears: a local `first:` or `value:` in a
   nested block is marked `defaultLibrary`, a block member after `.`
   (`str.split-on`, `x.count`) takes the prelude or local kind, and a
   local `note:` is a function because a function `note` exists
   elsewhere. TextMate is lexical: a name after `.` is never a prelude
   name.
2. **Call position.** TextMate scopes `name(`, `name[` and `name{` as a
   call (`entity.name.function.call`); rowan marks a parameter, nested
   function or namespace member there as a variable.
3. **Nested function declarations.** Rowan marks `inner(z):` inside a
   block as a property declaration; TextMate as a function declaration.
4. **Language constants and intrinsics.** `true`, `false` and `null` are
   prelude properties to rowan; TextMate scopes them
   `constant.language`. `__NAME` intrinsics are variables to rowan and
   `support.function.builtin` to TextMate.
5. **Rowan structural quirks** (reported for follow-up on the rowan side):
   - block-destructuring parameters (`f{x y}:`, `f({x y}):`) are marked
     as metadata (`decorator`): the leading soup of a block is read as
     block metadata;
   - declarations in a block passed as a call argument
     (`merge({ a: 1 })`, `matches({y: 2008})`) are marked parameters;
   - operands of operator and bracket declarations (`(l +++ r):`,
     `⟦ xs ⟧:`) are property declarations, not parameters;
   - `s"..."` type-data literals get no semantic token;
   - a multi-line string becomes one token spanning lines, which LSP
     clients cannot place on later lines;
   - interpolation braces and the format-spec colon get no token
     (TextMate scopes them inside the string).
6. **Metadata.** Rowan marks every identifier in a metadata expression
   as `decorator`, including a unit's leading metadata block
   (`{ import: ... }`) and values inside backtick metadata
   (`` ` { requires-io: true } ``). TextMate marks backtick metadata
   block keys as metadata names (`entity.other.attribute-name.metadata`)
   and leaves the rest, and cannot recognise a unit's leading block.
