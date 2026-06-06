# 0015 — Parser error-recovery & resilient front-end (Rowan)

- **Status:** Draft proposal for review
- **Track:** D — tooling
- **Classification:** Whitespace
- **Suggested horizon:** 0.8
- **Related:** TS-A7 (alias-reference tooling), ADR-001,
  [0000](0000-priority-fixes.md) F2 (block-bracket content-mode — couples with
  Phase 2 bracket recovery), sibling proposals
  [0014 — incremental-query-core](0014-incremental-query-core.md),
  [0016 — eu-doc](0016-eu-doc.md)

---

## Summary

Eucalypt already has a Rowan-based lossless-syntax-tree parser
(`src/syntax/rowan/`) that is the **sole** front-end path for the LSP and
for all compilation; the earlier LALRPOP infrastructure has been fully
retired.  However, the all-or-nothing compatibility shim (`src/syntax/parser.rs:22`)
discards the tree and returns `Err(ParserError::ParseErrors(…))` the moment a
parse error appears — but only **on the pipeline path**. The LSP's
*syntactic* features (completion, hover, folding, go-to-definition, semantic
tokens) call the Rowan parser directly (`src/driver/lsp/mod.rs:805` ff.) and
already get a best-effort tree, so they keep working mid-edit. What the shim
suppresses is everything behind the pipeline: **type diagnostics, type-based
inlay hints, and import resolution** abort at the first parse error — exactly
while the user is typing. The proposal is to promote the front-end to
**genuinely resilient**: always return a best-effort lossless tree and a
structured error list, never a hard failure, so the *type-aware* features
keep working on incomplete code too.  This is a pre-requisite for the
incremental query core (0014) and directly enables precise alias spans for
TS-A7.

---

## Motivation

### The empirical state of the two parsers

The house-style guide (`docs/proposals/_house-style.md:168`) mentions "a
newer Rowan parser" alongside LALRPOP.  Investigation reveals the situation
is better than that summary suggests, and worse in a different dimension.

**LALRPOP is gone in all but name.**  `src/syntax/parser.rs:1-4` reads:

```rust
// Re-export the Rowan parser interface as the main parser interface
pub use crate::syntax::rowan::{
    ast, parse_expr, parse_unit as rowan_parse_unit, Parse, ParseError as RowanParseError,
};
```

`parse_unit` and `parse_expression` in `src/syntax/parser.rs:11-46`
delegate directly to `crate::syntax::rowan::parse_unit` and
`crate::syntax::rowan::parse_expr`.  There is no `.lalrpop` file anywhere
in the tree; no LALRPOP build dependency exists in `Cargo.toml`.  The
legacy `SyntaxError` enum in `src/syntax/error.rs` retains variants with
names like `UnrecognisedToken` and `InvalidToken` — relics of the LR era
— but they are only constructed via the compatibility shim, not by any
active grammar.  **The Rowan parser is already the single primary path.**

**The desugarer consumes the Rowan tree.**  `src/core/desugar/rowan_ast.rs`
implements `Desugarable` directly for `rowan_ast::Unit` and the other Rowan
AST types; there is also `src/core/desugar/rowan_disembed.rs` for embedding
constructs.  The driver at `src/driver/source.rs:316-320` calls
`parser::parse_unit` (which delegates to Rowan) and stores the result as
`ParsedAst::Unit(unit)`, which is then handed to the desugarer.

**Error recovery in the parser is partial and ad-hoc.**  The Rowan parser
(`src/syntax/rowan/parse.rs`) already produces an `ERROR_STOWAWAYS` node
for surplus tokens (`read_surplus`, line 743), wraps `::` in an error node
(lines 715-722), and performs token-scanning recovery when an
`OPEN_PAREN_APPLY` is not properly closed (lines 609-631 of
`src/syntax/rowan/parse.rs`).  The `BlockEventSink` handles a bare colon
before EOF rather than panicking (tested by `bare_colon_in_block_does_not_panic`
in `src/syntax/rowan/mod.rs:374`).  These are point-fixes, not a
systematic strategy.

**The blocking problem is in the shim, not the parser.**
`src/syntax/parser.rs:21-26` rejects the tree and returns an error the
moment `parse_result.errors()` is non-empty.  The LSP
(`src/driver/lsp/mod.rs:343,800,812,825,837,852,869,887,907,942,961,979,993,1011`)
calls `crate::syntax::rowan::parse_unit(text)` directly for symbol tables,
folding, selection, hover, definition, references, completion, semantic
tokens, code actions, inlay hints, highlights, and rename.  On those paths
the tree is always available.  But wherever the pipeline runs — triggered by
`on_document_changed` at `src/driver/lsp/mod.rs:727` — errors abort with
`Err(ParserError::ParseErrors(…))` before desugar begins, so type
diagnostics, import resolution, and inlay hints from the pipeline are
suppressed while the user is mid-edit.

**Tech-debt summary.**  The real duplication is not two live parsers but a
shim layer (`src/syntax/parser.rs`) that wraps Rowan in an all-or-nothing
`Result` — a pattern from the LALRPOP era that no longer fits.  The residual
`SyntaxError` variants in `src/syntax/error.rs` are dead code.

### Why this matters for the LSP

The LSP specification requires servers to "tolerate incomplete programs and
return best-effort results."  Eucalypt advertises `TextDocumentSyncKind::INCREMENTAL`
(`src/driver/lsp/mod.rs:133`) — a commitment to per-keystroke change events.
Users write declarations incrementally: `x: {` before adding the closing
brace, or `foo(a, b` before closing the tuple.  At every such transient
state any LSP feature depending on a successful pipeline run is unavailable.

Parse-error diagnostics (`src/driver/lsp/diagnostics.rs:14-19`) are
published immediately from the Rowan tree — error *reporting* already works.
The gap is that desugar, cook, type-check, and import resolution abort at
the first parse error, leaving the cached type environment stale during
editing, exactly when it matters most.

### Why LR/LALRPOP struggles, and why Rowan is well-suited

Classical LR parsers detect errors at the token where no valid transition
exists, and standard recovery (panic-mode, error productions, Burke-Fisher
token insertion/deletion) requires either grammar annotations or global
backtracking.  Merlin (the OCaml LSP) extends Menhir's LR tables with
synthesised completions — a sophisticated technique described in Bour et al.
(ICFP 2018) — but the authors note that recovery "works best on the prefix
of the buffer before the first error" and that each grammar construct needs
individually tuned recovery rules.  The "Don't Panic!" paper (Landman et al.
2018, arxiv:1804.07133) surveys the structural difficulty: LR error recovery
requires inserting tokens chosen to re-enter a valid state, which is
grammar-specific and fragile.

Rowan's design philosophy, and the approach taken in rust-analyzer, is
different in kind.  Alexander Nazarov (matklad), in his "Resilient LL
Parsing Tutorial" (2023), codifies the rule: every parse function must
consume at least one token and must not leave the parser in a structurally
worse state than before the call.  Error nodes are first-class tree citizens;
the tree is always complete and lossless regardless of input.  Because Rowan
already materialises every token (including erroneous ones) in the green
tree, and because the eucalypt parser is an LL-style hand-written descent
— not an LR table — applying these techniques requires adding explicit
recovery strategies per construct, not altering a generated table.

---

## Prior art & landscape

**Rowan / rust-analyzer.**  Rowan (crates.io, github.com/rust-analyzer/rowan)
is a persistent red-green syntax tree library.  The key property is that
`GreenNode` is an immutable, internable tree; `SyntaxNode` (the red layer)
is a view with parent pointers and offsets computed on the fly.  Because the
tree is always constructed — even for broken input — the parser accumulates
`ParseError` structs with `TextRange` positions rather than aborting.
Rust-analyzer's parser uses a layered approach: the lexer tokenises fully;
the parser emits an event stream (`StartNode`, `Token`, `Finish`) that the
tree builder assembles; errors are attached to the `Parse<T>` result, not
to individual nodes.  Eucalypt's Rowan parser (e.g. `src/syntax/rowan/parse.rs`)
already follows this architecture — the `ParseEvent` enum, `GreenNodeBuilder`,
and `Parse<T>` struct are direct equivalents.

**Resilient LL parsing (matklad 2023).**  The tutorial
(`matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html`)
gives three principles: (1) every parse function returns, even on error;
(2) error nodes wrap unincorporated tokens; (3) recovery sets terminate a
failed sub-parse so the parent can continue.  The eucalypt parser's
`read_surplus` / `ERROR_STOWAWAYS` mechanism is a partial implementation
of (2) and (3); it needs to be applied consistently throughout.

**Merlin (OCaml).**  Bour et al. (ICFP 2018, arxiv:1807.06702;
`let-def/ocaml-recovery-parser`) instrument the Menhir LR automaton to
synthesise tokens when stuck, completing partial parses at the cost of
semantically nonsensical subtrees.  The technique suits ML-family languages
with keywords and explicit delimiters; eucalypt's keywordless, layout-free
grammar makes it harder to adapt.

**Other peer languages.**  Dhall (Rust, hand-written PEG), CUE (Go, `bailout`
on first error), and Nickel (Lalrpop LR) all abort on parse failure.
Pkl (JetBrains, ANTLR) has built-in single-token insertion/deletion recovery
and is the closest peer with serious IDE tooling; eucalypt's Rowan foundation
is architecturally better suited to full-tree recovery than ANTLR's approach.

**Classic techniques.**  Panic-mode recovery (skip to a synchronisation
point), error productions, and phrase-level recovery (token insertion/deletion)
are surveyed in Aho et al. (Dragon Book §4.4).  For a hand-written descent
parser the most applicable is phrase-level: skip tokens up to a fixed
recovery set, wrap them in an `ERROR` node, and return to the parent.

---

## Proposed design

### Phase 1 — Dissolve the all-or-nothing shim (0.8)

This has two sub-steps of different risk, which earlier drafts conflated.

**Phase 1a — stop aborting; expose tree + errors (low risk).** Change
`src/syntax/parser.rs:parse_unit`/`parse_expression` to return the Rowan
`Parse<T>` directly, preserving errors alongside the tree, and update
`src/driver/source.rs:316-320` to keep the partial tree and route the error
list to the diagnostic channel instead of aborting. The syntactic LSP
features already hold the tree (direct Rowan calls), so 1a's specific win is
that the *pipeline* no longer hard-stops at the shim boundary. Removing the
residual dead code — the `SyntaxError` variants in `src/syntax/error.rs`
never constructed by the live parser — belongs in this batch.

**Phase 1b — desugar the best-effort tree (medium risk).** Letting the
pipeline *continue* into desugar/type-check on a partial tree is what
actually delivers type diagnostics and type-based inlay hints mid-edit — but
it feeds malformed nodes to the desugarer, which today pattern-matches on
well-formed AST (see Risks, and Phase 2). The desugarer already tolerates
some stubs (the `BlockEventSink::complete` orphaned-declaration path,
`src/syntax/rowan/parse.rs:1198-1203`), but 1b is **gated on the desugar
panic-conversion below**. If 1b proves hard, **1a stands alone** — better
diagnostics, with the pipeline still skipping desugar/type-check while parse
errors are present — which is the safe fallback.

### Phase 2 — Systematic error recovery in the Rowan parser (0.8, medium risk)

Apply the resilient-LL principles to the construct types that currently lack
recovery:

| Construct | Current behaviour | Recovery target |
|-----------|------------------|-----------------|
| Unclosed block `{ x: 1` | `UnterminatedBlock` error, no tree below this point | Emit remaining tokens as `ERROR_STOWAWAYS` inside the block; close the `BLOCK` node |
| Unclosed list `[1, 2` | Same: no list node | Wrap remaining tokens; close `LIST` node |
| Unclosed paren `(f x` | Same | Close `PAREN_EXPR` node |
| Unexpected token in soup | Push-back and give up | Emit `ERROR_STOWAWAYS` token; advance one token; retry |
| Missing declaration body `x:` (at EOF) | `EmptyDeclarationBody` error | Already handled partially; emit empty `DECL_BODY` and `SOUP` |
| Double colon `::` | Wrapped in `ERROR_STOWAWAYS` | Already handled at `src/syntax/rowan/parse.rs:710-722` |

The key invariant to maintain: every path through the parser must account
for every token exactly once.  The `build()` function at line 209 asserts
this with `assert_eq!(event_tokens, …)`.  New recovery paths must not break
this accounting.

**Recovery sets.** Block content is delimited by `CLOSE_BRACE`; list content
by `CLOSE_SQUARE`; soup by any closing bracket or end-of-input.  Each
recovery loop should be bounded by the appropriate set so it does not consume
a token that terminates a parent construct.

**Coupling with [0000](0000-priority-fixes.md) F2 (block-style idiot
brackets).** A user bracket pair `⟦ … ⟧` whose *content-mode* (block vs soup)
is decided at parse time depends on the per-file `BracketRegistry`, which
today does not compose across imports (F2, a live P1 bug). 0015's bracket
recovery and F2's fix touch the same parse path: if F2 is fixed by deferring
the block/soup decision past parse to a seedable desugar registry (F2 fix
(b)), the parser will parse bracket contents *generically* — which
*simplifies* recovery here (one generic bracket-content rule, not two
mode-specific ones). Sequence the two together; prefer F2 (b).

### Phase 3 — Span-enriched diagnostics feeding the Clarion programme (0.9)

Once recovery is systematic, each `ParseError` variant already carries a
`TextRange` (all variants in `src/syntax/rowan/error.rs` do so).  The
`diagnostics_from_parse_errors` function (`src/driver/lsp/diagnostics.rs:14`)
already converts these to LSP `Diagnostic` objects with line/column spans.
The upgrade path is:
- Add a `hint` field to `ParseError` variants where helpful (e.g.
  `UnterminatedBlock` could carry the opening brace range to suggest the
  missing close);
- Surface these as LSP `DiagnosticRelatedInformation` entries;
- Feed precise spans into the Clarion error-quality programme referenced in
  the main type-system roadmap.

### Phase 4 — Precise spans for TS-A7 and 0016 (0.9)

TS-A7 (`docs/development/alias-reference-tooling-spec.md`) needs byte spans
for alias-name tokens inside `type:` strings.  The type-DSL parser
(`src/core/typecheck/parse.rs`) currently returns bare `Type` values with no
attached spans.  Phase 1 of this proposal (always returning the Rowan tree
and not aborting) is a pre-requisite: TS-A7's go-to-definition and hover
features query the LSP symbol table, which is rebuilt from the Rowan tree on
every document change.  If the tree is truncated by a parse error,
alias-reference spans inside a type string that appears after the error
point are invisible to the LSP.  Full resilient parsing ensures the complete
tree is always available.

Similarly, `eu doc` (proposal [0016](0016-eu-doc.md)) extracts metadata
blocks from the syntax tree.  A partially-typed source file with an open
block should not suppress all doc extraction from declarations that precede
the error.

### Consolidating the two-parser debt

The only remaining duplication is the `src/syntax/parser.rs` shim and the
dead `SyntaxError` variants.  Once Phase 1 is complete, the shim can be
dissolved: callers can use `crate::syntax::rowan::{parse_unit, parse_expr}`
directly, and `ParserError::ParseErrors` can either be dropped or kept as a
thin wrapper for the diagnostic pathway.  The `SyntaxError` enum should be
removed in the same commit.  This is a naming and module-boundary tidy, not
a behaviour change.

---

## Interaction with the existing roadmap

**0014 — incremental-query-core.**  Salsa-style incremental evaluation
requires that each query return a stable, comparable result.  If parse
queries can return `Err` for erroneous documents, the dependent queries
(desugar, type-check) must either handle the error case or be skipped,
complicating the dependency graph.  If the parse query always returns a
`Parse<T>` with attached errors — never `Err` — the incremental core can
treat the tree as always-present and propagate parse errors as structured
data rather than query failures.  This proposal is therefore a pre-requisite
for 0014's design, not just a compatible adjacent piece.

**TS-A7.**  As described above, Phase 1 is a pre-requisite for reliable
alias-span coverage.

**0016 — eu-doc.**  Doc extraction should be tree-based (it already would
be, operating on the Rowan AST), but only if the tree is complete for a
partial file.  Phase 2 makes this reliable.

**Stage A / B type system.**  No interaction with type-checker internals.
The type-checker (`src/core/typecheck/`) receives core expressions after
desugar; making the parser more resilient means it receives slightly
malformed core expressions in error cases, which it already handles
(advisory mode, no panics expected from ill-typed input).

---

## Implementation sketch

| Phase | Components changed | Size estimate | Risk |
|-------|-------------------|---------------|------|
| 1 — Remove shim | `src/syntax/parser.rs`, `src/driver/source.rs`, `src/syntax/error.rs` | ~150 lines removed / changed | Low |
| 2 — Recovery | `src/syntax/rowan/parse.rs` | ~300 lines added/changed | Medium |
| 3 — Diagnostic hints | `src/syntax/rowan/error.rs`, `src/driver/lsp/diagnostics.rs` | ~80 lines | Low |
| 4 — Span wiring (TS-A7 handoff) | `src/core/typecheck/parse.rs`, `src/driver/lsp/hover.rs` | ~100 lines | Low |

Phase 1 should be a standalone PR; Phases 2–4 can be sequenced across 0.8.
The `assert_eq!` token-accounting assertion in `build()` is a useful
regression guard; any recovery path that loses a token will trigger it in
tests.  Each new recovery production should be accompanied by a harness test
in `tests/harness/` exercising the specific incomplete construct.

---

## Alternatives considered

**Reintroduce LALRPOP with error productions.**  LALRPOP supports `!` error
tokens.  This would require re-introducing a grammar file, reconciling it
with the Rowan tree representation, and accepting the same LR-specific
fragility that makes Merlin's approach labour-intensive.  Rejected: the
existing Rowan parser already has the right architecture.

**Rely on tree-sitter for LSP resilience.**  Tree-sitter is designed for
error-resilient incremental parsing and the project already has a tree-sitter
grammar.  However, all LSP semantic operations (symbol table, type env, inlay
hints) consume the Rowan tree; rewriting `src/driver/lsp/` to consume the
tree-sitter CST instead is disproportionate to Phase 1.  A future incremental
core (0014) might revisit if tree-sitter becomes the canonical front-end.

**Panic-mode recovery.**  Recovering to the next top-level declaration
boundary is simple but coarse: hover and completion inside a partially typed
block would still return nothing.  Rejected in favour of phrase-level
recovery, which preserves inner structure at the cost of more implementation
effort.

---

## Risks & what would kill this

**Token-accounting breakage.**  The `assert_eq!` in `build()` at
`src/syntax/rowan/parse.rs:209` is a hard correctness invariant.  Any
recovery path that emits the wrong number of token events will panic.
Mitigation: run the full harness test suite (`cargo test --test harness_test`)
after each recovery addition; the assertion fires in debug builds.

**Desugarer assumptions about well-formed trees.**  The desugarer
(`src/core/desugar/rowan_ast.rs`) traverses the Rowan AST by pattern-matching
on typed AST nodes (e.g. `rowan_ast::Declaration`, `rowan_ast::Block`).
If recovery produces structurally unexpected nodes — an empty `DECL_BODY`
where a `SOUP` is expected — the desugarer may panic or produce incorrect
core expressions.  Mitigation: per the project panic policy (a `panic!` /
`expect()` in user-reachable code is a P1 bug, never deferred), **convert**
desugar node-accessor `unwrap()`/`expect()` calls into proper `CoreError`
diagnostics, each with a regression test exercising the malformed construct —
not merely audit them. This conversion is the **gating work for Phase 1b and
Phase 2**: until a malformed tree can reach desugar without panicking, the
pipeline stays on the 1a path — expose the tree and errors, but skip
desugar/type-check while parse errors are present.

**Stale type environment from partial trees.**  The LSP caches the type
environment in `CachedPipeline` (`src/driver/lsp/mod.rs:65`); inlay hints
from a partial type environment could be wrong rather than absent.
Mitigation: tag `CachedPipeline` with `parse_errors_present: bool` and
suppress hints requiring a complete type environment when errors exist.

**What would kill this.**  If the desugarer routinely panics on best-effort
trees and each fix is non-trivial, the proposal stalls.  The safe fallback
is Phase 1 alone — remove the shim, always return a partial tree alongside
errors, leave the pipeline abort — which still improves diagnostics without
requiring desugar-on-error.

---

## Success criteria

1. **LSP completion and hover work on a file with one unclosed block.**
   Specifically: `x: { y: 1` (missing `}`) — completion at line 1 col 15
   returns the prelude symbol table, hover on `y` returns its type.

2. **Parse errors are published with byte-precise spans.**  `src/driver/lsp/diagnostics.rs`
   already achieves this for existing errors; success is that the set of
   errors covers recovery-wrapped tokens and not just the first error site.

3. **No regression in the existing harness.**  `cargo test --test harness_test`
   passes without change; error-case tests in `tests/harness/errors/` still
   fail as expected (recovery produces a tree, but evaluation still fails).

4. **TS-A7 alias spans cover type strings after error points.**  A file with
   a syntax error on line 5 and a `type: "Person"` annotation on line 10
   should still make `Person` navigable via go-to-definition.

5. **Pipeline cost does not increase measurably.**  Recovery adds token
   scanning loops, not algorithmic overhead.  The Rowan parser is not on
   the hot path for batch compilation (compile latency is dominated by STG
   evaluation and the ~500–700 ms pipeline, not by parsing).

---

## References

**Eucalypt files cited:**

- `src/syntax/rowan/parse.rs` — Rowan parser: `ErrorStowaways`, recovery
  productions, token-accounting assertion
- `src/syntax/rowan/mod.rs` — `parse_unit`, `parse_expr`, `Parse<T>`,
  `bare_colon_in_block_does_not_panic` test
- `src/syntax/rowan/error.rs` — `ParseError` variants with `TextRange`
- `src/syntax/parser.rs` — compatibility shim; the all-or-nothing `Result`
  boundary
- `src/syntax/error.rs` — legacy `SyntaxError` enum; `ParserError::ParseErrors`
- `src/core/desugar/rowan_ast.rs` — `Desugarable` impls for Rowan AST
- `src/driver/source.rs:316-320` — pipeline parse/desugar entry point
- `src/driver/lsp/mod.rs` — LSP server; `on_document_changed`; direct Rowan
  calls for all LSP features
- `src/driver/lsp/diagnostics.rs` — `diagnostics_from_parse_errors`;
  `LineIndex`
- `docs/development/alias-reference-tooling-spec.md` — TS-A7 spec

**Papers and external references:**

- Bour, Refis, Pédrot, Scherer. "Merlin: A Language Server for OCaml
  (Experience Report)." ICFP 2018. arxiv:1807.06702.
  `dl.acm.org/doi/10.1145/3236798`
- Landman, Vinju, Smienk. "Don't Panic! Better, Fewer, Syntax Errors for
  LR Parsers." arxiv:1804.07133.
- Nazarov (matklad). "Resilient LL Parsing Tutorial." 2023.
  `matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html`
- `github.com/rust-analyzer/rowan` — Rowan library
- `github.com/let-def/ocaml-recovery-parser` — Merlin's recovery-augmented
  OCaml parser
