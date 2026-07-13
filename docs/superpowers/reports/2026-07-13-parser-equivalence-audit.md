# Parser equivalence audit: tree-sitter grammar vs Rowan (eu-wrf7)

- **Date:** 2026-07-13
- **Scope correction:** the bead title references "legacy kind.rs", but that
  parser was deleted in 0.10.1 (#902). This audit compares the **tree-sitter
  grammar** (`editors/tree-sitter-eucalypt/`, used by VS Code, Emacs, and the
  playground) against the **sole production parser**, the hand-written
  lossless Rowan parser (`src/syntax/rowan/`).
- **Grammar state:** `grammar.js` (512 lines) + committed `src/parser.c`
  (generated, ABI 14). Last touched 2026-06-23 (`182df1a8`, "add bracket_expr
  as juxtaposed call target" — fixed the exact gap this bead's description
  cites; that fix is confirmed merged and covered by corpus tests). All 40
  existing `tree-sitter test` corpus cases pass. Regenerating with the
  currently-installed `tree-sitter-cli` 0.26.11 reproduces the committed
  `parser.c` byte-for-byte modulo ABI/schema metadata noise (no drift from
  `grammar.js`) — the checked-in parser is not stale relative to its own
  source.
- **Method:** report only, no grammar or parser changes made. Worktree
  `/tmp/eu-parseraudit`, branch `audit/parser-equivalence`.

## 1. Corpus and method

521 `.eu` files under `tests/harness/`, `lib/`, `examples/aoc25/`. Each file
was run through both engines:

- **Rowan**: `eu dump ast <file>` (release build, rustc 1.97.0 stable-aarch64).
- **tree-sitter**: `tree-sitter parse -q --stat <file>` (tree-sitter-cli
  0.26.11 via Homebrew, against the committed `parser.c`).

**Important methodological correction, discovered mid-audit:** Rowan is a
*lossless, error-tolerant* parser — it always produces a CST and `eu dump
ast` exits 0 even for source with genuine syntax problems (e.g. unterminated
blocks); those are only rejected later, in `verify`/`cook` during a real `eu
run`/`eu check`. Confirmed directly: `tests/harness/errors/160_unclosed_block.eu`
dumps a full AST with exit 0 and no ERROR nodes, but `eu tests/harness/errors/160_unclosed_block.eu`
fails at runtime with "unterminated block (missing '}')". So comparing
`dump ast` exit codes against `tests/harness/errors/*.eu` (176 of the 521
files) is apples-to-oranges — those fixtures intentionally test
later-pipeline diagnostics, not parseability, and tree-sitter (no later
pipeline) rejecting them is not evidence of a grammar gap.

Excluding `tests/harness/errors/`, the corpus is **345 files of syntax that
Rowan accepts as intentionally-valid eucalypt**. That's the honest
equivalence set:

| | Count |
|---|--:|
| Non-error-test corpus files | 345 |
| Rowan-accepts / tree-sitter-rejects (genuine divergence) | **34** |
| tree-sitter-accepts / Rowan-rejects | 0 |
| Divergence rate | **9.9%** |

(Two further apparent divergences, `tests/harness/errors/172_yaml_complex_key.eu`
and `tests/harness/errors/error_167.eu`, are Rowan-side *import/YAML-key
validation* errors triggered incidentally by `dump ast` resolving embedded
imports — not parser divergences at all.)

Full per-file results: `corpus_results.csv` (521 rows) — reproducible via the
method above; not attached to this PR, regenerate by re-running the
commands in §1 from the worktree.

## 2. Divergence catalogue (ranked)

### 2.1 [HIGH] Duplicated/drifted operator-character regex — 3 chars silently dropped

`grammar.js` defines `OPER_CHARS` (line 19, a documented constant, comment
claims it enumerates all operator characters including "✓ (postfix
non-nil check)") but **`OPER_CHARS` is never referenced anywhere in the
file** — dead code. The actual `operator:` token rule (line 459) is a
**hand-duplicated copy** of that character class that has drifted: it is
missing `✓` (U+2713 CHECK MARK), `▶` (U+25B6, debug-trace triangle), and `⊝`
(U+229D, bitwise NOT), all three of which *are* present in the unused
`OPER_CHARS` constant one line away.

Effect: any use of `✓`, `▶`, or `⊝` as an operator — declaration *or*
usage — is unparseable. Confirmed with minimal repros (`(x ✓)`, `▶ 42`,
`(⊝ n)` all produce an `ERROR` node standalone). Breaks:
`tests/harness/084_bitwise.eu`, `tests/harness/101_non_nil_postfix.eu`,
`tests/harness/131_debug.eu`, and is one of the contributing causes of
`lib/prelude.eu` (§2.4) failing to parse at all, since the prelude declares
`(x ✓)` and `(⊝ n)`.

**Fix is trivial**: make the `operator:` rule reference `OPER_CHARS`
directly instead of maintaining a second copy, then `tree-sitter generate`.
Ranked #1 because it's high-impact (breaks real, shipped prelude syntax) and
essentially zero-risk to fix (deletes a duplicate, doesn't change grammar
shape).

### 2.2 [HIGH] Gradual-typing syntax (0.10–0.12) has no grammar coverage at all

Two related surfaces, both entirely unrecognised:

- **Binding-scope type keywords**: `{ :for x: 42 }`, `{ :let x: 42 }`,
  `{ :io x: io.shell(...) }` (block-metadata-style keyword immediately after
  `{`, before the first declaration). Confirmed unsupported standalone.
  Breaks `tests/harness/typecheck/010_for_number_binding.eu`,
  `011_for_string_binding.eu`, `012_for_correct_binding.eu`,
  `013_io_number_binding.eu`, `023_for_map_no_warning.eu`,
  `024_for_bind_no_warning.eu`, `014_let_any_binding.eu` (7 files, likely
  many more not yet in the corpus — this is a whole syntax category, not an
  edge case).
- **Inline type ascription**: `expr : "type-string"`, e.g. `"hello" :
  "string"` as a widening annotation. Confirmed unsupported standalone.
  Breaks `038_literal_string_annotation_widens.eu`,
  `037_union_absorbs_literal_string.eu`, `035_literal_string_dsl.eu`,
  `034_literal_string_synthesis.eu`, `054_io_elem_type_correct.eu`.

This is the single largest identifiable category (12 of the 34 genuine
divergences) and represents an entire language-feature generation that
postdates the grammar's last substantive syntax update. It's exactly the
"stale grammar rules for syntax that changed 0.10→0.12" the task asked to
catalogue.

### 2.3 [HIGH] Bracket-expr declaration heads unsupported (custom bracket/monad literals)

Declaring a name via a `bracket_expr` pattern as the declaration head —
`⟦ x ⟧: x` (identity/"idiot" bracket), `⟦{}⟧: { :monad bind(m, f): ... }`
(inline monad-bracket definition), `⌊xs⌋: xs head`, `⌈[x]⌉: x ceiling` — is
not in `declaration_head`'s grammar at all; only `identifier` /
`quoted_identifier` (optionally with `parameter_list`/`block_param`/
`list_param`) are accepted as declaration heads. Confirmed standalone
(`⟦ x ⟧: x` alone → ERROR).

Breaks: `tests/harness/097_idiot_brackets.eu`,
`tests/harness/149_bracket_inline_monad.eu`,
`tests/harness/096_monadic_blocks.eu`,
`tests/harness/129_monadic_implicit_return.eu`,
`tests/harness/172_idiot_bracket_juxtaposed_call.eu`,
`tests/harness/testdata/155_cross_import_bracket_lib.eu`, and is a
contributing cause of `lib/prelude.eu` failing (it declares `⌈[x]⌉` and
`⌊[x]⌋` for ceiling/floor notation) and of `lib/lens.eu` (custom lens
brackets). 6+ files; like §2.2, an entire feature is invisible to the
grammar, not a one-off gap.

### 2.4 [HIGH] `lib/prelude.eu` — the shipped standard library — does not parse at all

Practical consequence of the above, plus one more standalone bug: declaring
an operator whose character sequence is a **strict prefix of a
previously-declared operator's** characters in the same scope — e.g. `(l <
r): ...` followed later by `(l <= r): ...`, or `(l + r): ...` followed by
`(l ++ r): ...` — corrupts the parse from that point on. Minimal repro:

```
(l < r): l
(l <= r): l
```
→ `ERROR [1, 3] - [1, 7]` (order matters: shorter-then-longer triggers it;
`(l ++ r)` then `(l + r)` also triggers it. Declaring two operators that
don't share a character prefix, e.g. `&&` then `∧`, is fine.)

`lib/prelude.eu` hits this at `(l < r)` / `(l <= r)` (around line 706/720 of
2347), and separately hits §2.1 (`✓`, `⊝`) and §2.3 (`⌈[x]⌉`, `⌊[x]⌋`)
later in the file. The combined effect: the entire top-level unit collapses
into a single `ERROR` node spanning the whole file — tree-sitter cannot
produce *any* usable partial tree for the prelude source today. Any
tooling that wants to offer completions/hover/highlighting sourced from
`lib/prelude.eu` itself (rather than a hardcoded keyword list) is currently
unable to.

`lib/lens.eu` and `lib/markup.eu` also fail (see §2.7, §2.8).

### 2.5 [MEDIUM] Prefix-operator usage followed by a prefix-operator declaration, same block

Independent of §2.4's prefix-collision bug: using a custom prefix operator
as an *expression* earlier in a block, then declaring a *different* prefix
operator later in the same block, breaks the second declaration:

```
b: {
  f: ¬true
  (▶x): x     # ERROR — regardless of which operator, or whether ¬ was used as decl or expr
}
```
Declaring first then using is fine; using-then-declaring is not. Found via
`tests/harness/020_op_precedence.eu`'s `unary-ops` block (`¬true` used, then
`(⊙foo)` declared → error). Likely a GLR conflict-resolution artefact in how
`declaration_head` vs `soup` compete once the parser has committed to an
interpretation from the earlier usage.

### 2.6 [MEDIUM] List-destructuring pattern gaps

Two distinct, confirmed-standalone gaps in list-destructuring parameter
patterns:

- **Multi-head cons patterns**: `[a, b : rest]` (two or more names before
  the cons-tail binding) fails; single-head `[x : xs]` works fine. Breaks
  `tests/harness/092_destructure_list.eu`,
  `tests/harness/095_destructuring_integration.eu`.
- **Nested-list patterns**: `[a, [b, c]]` as a parameter pattern fails
  standalone. Breaks `tests/harness/141_deep_destructuring.eu`,
  `tests/harness/102_destructure_list_in_block.eu`. (The 141 test's own
  comment notes a simpler two-level case "already worked before" — i.e. this
  looks like a regression point the test itself is guarding, on the Rowan
  side; tree-sitter never had it.)

### 2.7 [MEDIUM] Nested braces inside `s"..."` type-literal strings

`s"symbol → Lens({..r}, a)"` (row-polymorphic type syntax inside an s-string)
and `s"{ users: [{ name: string }] }"` (doubly-nested block/list content)
both break the string scanner's brace handling — it appears to not balance
more than one level of `{…}` inside interpolation-eligible string content.
Breaks `tests/harness/175_sv2_to_spec.eu`, `lib/lens.eu`, and contributes to
`lib/prelude.eu` (many `s"... {..} ..."` type annotations throughout
`monad()`'s doc block). This is a variant of the documented "curly braces in
doc strings parsed as block literals" gotcha, but specifically about
*nesting depth* rather than presence.

### 2.8 [LOW] Chained multi-arg call `f(a)(b, c)` where the second call has 2+ args

`g(1)(2, 3)` fails; `g(1)(2)` (single arg second call) and `g(1, 2)(3)`
(single arg *chained* call) both succeed. Breaks
`tests/harness/010_prelude.eu` (`flip(__SUB)(1, 3)`). Narrow but real —
looks like the `application` production's arg-list handling doesn't
correctly repeat for a chained callee once the first argument list has 2+
elements.

### 2.9 [LOW] Identifier lexing excludes the Letterlike Symbols Unicode block

`identifier: $ => /[•$?_a-zA-ZÀ-Ö...]/ ` is a hardcoded set of
Unicode ranges (Latin-1 Supplement, Latin Extended, Greek, Cyrillic,
Greek Extended) and does not include U+2100–U+214F (Letterlike Symbols),
which contains `ℕ` (U+2115), used in `examples/aoc25/day08.eu` and
`day10.eu` as a prelude-defined identifier (the naturals stream). Rowan's
lexer uses dynamic Unicode general-category detection for identifiers, so
`ℕ` works there. Breaks `examples/aoc25/day08.eu`, `examples/aoc25/day10.eu`
(confirmed root cause via minimal repro: `x: ℕ` alone fails; `x: N` (ASCII)
succeeds).

This is architecturally the same disease as **eu-fbyk** (hardcoded operator
character list vs Rowan's dynamic Unicode-category scanning) but on the
*identifier* rule rather than the *operator* rule. eu-fbyk's fix (an
external scanner mirroring `is_operator_char()`) should be scoped to cover
`is_identifier_start`/`is_identifier_continue` too, or a companion bug filed
— not fixing here per the "don't fix, reference" instruction, but flagging
the scope gap since eu-fbyk's current description only mentions operators.

`examples/aoc25/day12.eu` also diverges but its root cause wasn't isolated
(reported error line didn't reproduce standalone in the time available;
likely a downstream artefact of one of the above rather than a new class —
flagged for follow-up, not counted as a distinct category).

## 3. Bonus finding: `lib/markup.eu` has dead code from a real bug (not a parser issue)

While isolating divergences, `lib/markup.eu`'s parse failure
(`tag = head`, `attrs = second` — using `=` instead of `:` to declare) led
to a genuine functional bug, unrelated to the grammar audit: Rowan's
lossless/lenient parser accepts `tag = head` at the top of the file as
**block-level metadata** (`BLOCK_META`, not a `DECLARATION`) rather than
raising a syntax error, silently discarding the intended binding. Verified
at runtime:

```
$ eu -e 'import "lib/markup.eu" tag([:a, {}, "hi"])'
error: unresolved variable 'tag'
```

`tag` and `attrs` (and likely other `X = Y` lines in `markup.eu`, worth a
full grep) are **not actually exported/defined** by the shipped
`lib/markup.eu`. This is a latent library bug independent of tree-sitter;
recommend filing separately (Quill/library-content territory, not this
bead) rather than folding into the parser-equivalence work.

## 4. Does any of this affect editor tooling / playground in practice?

- **VS Code extension / Emacs mode**: primarily use tree-sitter for syntax
  *highlighting*, which degrades gracefully on ERROR nodes (surrounding
  text still highlights; the erroring span falls back to plain text). Low
  practical severity for everyday user code — most divergences are in
  edge-case operator/bracket-declaration syntax, not everyday expressions.
  The **exception is real**: any user writing gradual-typing annotations
  (`:for`/`:let`/`:io`, inline `: type`) — a *current, actively-developed*
  0.13 feature area per the epic this bead sits under — gets broken
  highlighting for that syntax today, with no editor feedback that it's a
  tooling gap rather than a real error.
- **Playground/WASM**: if it uses tree-sitter for anything beyond
  highlighting (e.g. structural editing, fold ranges), same caveat.
- **`lib/prelude.eu` unparseable (§2.4)** is the one finding with a
  plausible *maintenance* impact: any future tooling idea that wants to
  derive keyword lists, hover docs, or signatures from the prelude source
  directly (rather than the current hand-maintained keyword lists) is
  blocked until §2.1/§2.3/§2.4's prefix-collision bug are fixed.

## 5. Recommendations (ranked)

1. **Fix §2.1** (operator regex drift) — one-line change (reference
   `OPER_CHARS` instead of duplicating it), trivially verified against the
   existing corpus. Do this first; it partially un-blocks `lib/prelude.eu`
   too.
2. **Fix the operator-prefix-collision bug** (§2.4) — needed to fully
   unblock `lib/prelude.eu`; likely a lexer/precedence issue in how the
   `operator` token's maximal-munch interacts with declaration-head
   disambiguation across sequential declarations. Needs grammar-internals
   investigation, not just a data fix.
3. **Add grammar support for §2.2** (gradual-typing keywords + inline type
   ascription) — highest user-facing value given it's the active 0.13
   feature area; also the largest single divergence category.
4. **Add grammar support for §2.3** (bracket-expr declaration heads) —
   needed for full prelude/lens coverage and any user code defining custom
   bracket pairs or inline monads.
5. §2.5–2.9 — lower priority; file as follow-up beads (P3/P4) rather than
   fixing ad hoc, since several (§2.5, §2.8) look like they need real GLR/
   precedence-table debugging rather than character-class patches.
6. **Broaden or companion-file eu-fbyk** to explicitly cover identifier
   lexing (§2.9), not just operators — same root architecture problem,
   currently scoped out of the existing bug's description.

### Maintenance strategy going forward

The grammar has no CI check against the Rowan parser or against real
source (`lib/`, `tests/harness/`) at all today — it was only caught up to
`182df1a8` because the bead's owner manually diagnosed the bracket-target
gap. Recommend:

- **A corpus-parse CI job**: run `tree-sitter parse -q` over
  `lib/*.eu`, `tests/harness/*.eu` (excluding `tests/harness/errors/`,
  which is out of scope per §1's methodology), and `examples/aoc25/*.eu`
  on every PR that touches `editors/tree-sitter-eucalypt/` *or* the
  language surface (`src/syntax/rowan/`, `src/core/desugar/`). Fail (or at
  minimum diff-report) on ERROR-node count regressions. This would have
  caught §2.1 (a one-character regex drift) immediately.
- **A lighter-weight tripwire for everyone else**: since `src/syntax/rowan/`
  changes are the leading indicator of grammar drift, consider a `bd`
  reactive-duty note (already exists in Lantern's brief) plus a periodic
  (not per-PR) full corpus-equivalence run — the full audit here took
  under two hours including root-cause isolation, so a monthly or
  per-milestone cadence is cheap.
- **Retire the unused `OPER_CHARS` constant** as part of the §2.1 fix, or
  make it the single source of truth referenced by both the `operator` rule
  and the (currently separate, also-hardcoded) bracket-detection regexes —
  the same "two copies drift apart" failure mode could recur for brackets.

## 6. What this audit did *not* cover

- WASM binding correctness (only the grammar/parser.c was exercised, not
  the compiled `.wasm` artifact or JS bindings).
- tree-sitter *query* files (`queries/highlights.scm` etc.) — a construct
  parsing correctly doesn't guarantee it's highlighted correctly; that's a
  separate query-coverage audit.
- Emacs `eucalypt-mode`'s own (non-tree-sitter) indentation/font-lock rules.
- Performance of the tree-sitter parse (not in scope; note only the
  incremental "Slow parse rate" warning on corpus test 1, unrelated to
  correctness).
