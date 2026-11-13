# Evidenced diagnostic hints — audit (eu-oaz72.28)

- **Date:** 2026-11-12
- **Bead:** eu-oaz72.28 (AoC 2026 preparation, principle 4 of
  `docs/superpowers/plans/2026-11-12-aoc-2026-preparation.md`)

Rule: a hint is shown only when the source gives evidence for it, or when
it states a fact about what was parsed or called.

## Mechanism

`src/common/evidence.rs` reads the source line under a diagnostic's
primary label. `ExecutionError`, `CompileError` and `EucalyptError` gain
`to_diagnostic_with_source(source_map, files)`; the CLI render paths
(`eu.rs`, `prepare.rs`, `eval.rs` human and JSON) and the WASM pipeline
use it. The plain
`to_diagnostic(source_map)` is unchanged in signature and shows no
evidence-gated hint (no source, no evidence) — the LSP uses it.
When a fixture has no primary label (some `--source-prelude` renderings),
gated hints are likewise not shown.

The error harness gains `stderr-absent:` in `.expect` sidecars (repeatable)
so a fixture can gate that a hint is *not* shown.

## Dispositions

G = gated on source evidence, R = rewritten, X = removed, K = kept (fact,
or keyed on the exact name or key the user wrote).

### `src/eval/error.rs`

| site | hint | disposition |
|---|---|---|
| `format_not_callable` | "only functions and blocks can be called" | K (fact) |
| `format_not_callable` | "this often means too many arguments were passed" | X (guess). A replacement keyed on `f(a, b)` was tried and dropped in review: `foldl(5, 0, xs)` raises the same error for a non-function argument, so the call shape is no evidence |
| `not_callable_notes` true/false | "if this result of a boolean expression, check operator precedence" | X (guess, ungrammatical) |
| `not_callable_notes` true/false | "'and' and 'or' are identifiers …" | G: `and`/`or` a whole word on the label's line; R: "ordinary functions" |
| `not_callable_notes` number/string/… | "'f(a) + 1' binds as 'f(a + 1)'" | X: the precedence guess is removed, since the diagnostic has no fixity information and a copied precedence table would drift from the prelude (owner review). K: "the value called here is the result of '{op}'" when the label is an operator; a called literal juxtaposed after an expression gets a factual catenation note |
| `not_callable_notes` number/string/… | "'f(a) + 1' binds as 'f(a + 1)'" | X: the precedence guess is removed, since the diagnostic has no fixity information and a copied precedence table would drift from the prelude (owner review). K: "the value called here is the result of '{op}'" when the label is an operator; a called literal juxtaposed after an expression gets a factual catenation note |
| `not_callable_notes` list | nth/head/tail | K (fact about the called value) |
| `unexpected_function_notes` | "this can happen when … under-applied, or when low-precedence catenation …" | X |
| `unexpected_function_notes` | "this can happen when … under-applied, or when low-precedence catenation …" | X |
| `format_cannot_return_fun` | "this often happens due to operator precedence" (headline) | X |
| `CannotReturnFunToCase` bool | "'_' anaphora … two-argument function" | G: `_` on the label's line |
| `type_mismatch_notes` / `data_tag_mismatch_notes` | "to concatenate strings/lists … instead of '+'" | G: `+` on the line |
| `data_tag_mismatch_notes` | "'str' is a namespace … not a type conversion function" | G: bare `str` (not `str.…`) on the line, as in `str(x)`, `x str` or `x.str` |
| `type_mismatch_notes` / `data_tag_mismatch_notes` / `BadNumberFormat` | "use 'str' to convert a number to a string" | R: `str.of` (`str` is a namespace) |
| `NoBranchForNative` string | "if you called 'head' or 'tail' on an empty list …" + `nil?` guard | G: `head`/`tail` on the line |
| `NoBranchForNative` string | "'++' is for list append" | G: `++` on the line |
| `HeadOfNonList` / `TailOfNonList` string | "not '++' (which is for list append)" | G: `++` on the line |
| `HeadOfEmptyList` / `TailOfEmptyList` | "use 'nth(0, xs)' or pattern matching if the list may be empty" | R: removed — `nth(0, [])` fails too |
| `lookup_failure_notes` `trim`/`strip` | "eucalypt has no 'trim'/'strip' function" | R: false — `str.trim` exists |
| `lookup_failure_notes` other keys | Python/JS method names → `str.*` | K (keyed on the key written) |
| `type_mismatch_notes` / `data_tag_mismatch_notes` `.`-on-list/number/string | "the '.' operator performs key lookup …" and its remedies | G: the labelled value is looked up with `.`: the label holds `x.name`, or is followed by `.name` on its line or a continuation line (`"abc".upper`; `[1, 2]` then `  .name`). A namespace call elsewhere on the line (`str.of`) is not evidence |
| `type_mismatch_notes` / `data_tag_mismatch_notes` symbol/string, null, block | language facts about the found type | K |
| `format_not_value`, `format_lookup_on_function`, datetime, timezone, format-string, division, export-format messages | facts about the operation | K |
| `format_not_value` "a function application" | "this often means a function received fewer arguments" | K for now — see open items |
| `BlackHole`, `ListIndexOutOfBounds` | facts and remedies | K |

### `src/eval/stg/compiler.rs` (`CompileError::FreeVar`)

| hint | disposition |
|---|---|
| "check that the variable is defined and in scope" | K |
| `export: :internal` note | K (fact from the name) |
| `_` out of scope: friendly message and scope note | K (fact from the mangled name) |
| "'f(_)' does not create a section" | G: the `_` is written as a call argument |
| "eucalypt has no arrow functions" (any single-letter name) | G: the name is followed by `->` (after optional further parameters) and either opens a call argument or parameter list (`map(x -> y + 1)`, `(x, y) -> 1`) or is used again after the arrow, as a lambda body would; `xs foo -> 1` is a correct use of the const operator |
| `==`, `=`, `+=`…, `import`, `fold`, `sort`, `True`, `None`, `then`, `lambda`, `for`, … | K (keyed on the exact name written) |

## Open items

- `format_not_value` "a function application" still says "this often
  means a function received fewer arguments"; its message is built before
  any label is known, so gating it needs the note moved out of the
  headline. Recorded on the bead.
- Evidence is read from the raw line, so text inside a string or comment
  on that line counts. Acceptable for hints; a token-level reading would
  need the parser.
- The LSP does not show gated hints; it would need the file database
  passed to its diagnostic conversion.

## Evidence is read through the lexer

After owner review, every evidence check reads tokens from the eucalypt
lexer (`syntax::rowan::lex::Lexer`) rather than matching characters, so a
name, an operator, a literal and a `.` lookup are what the parser reads,
and nothing inside a string or a comment counts as evidence. The
catenation-precedence note was removed rather than kept on a copied
precedence table; showing it exactly would need the cooked operator
fixities carried into diagnostics.
