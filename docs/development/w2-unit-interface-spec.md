# W2: Unit Interface and cross-import bracket fix

## Status: Spec — ready for implementation
## Release: 0.7.1
## Agent: Quill (front-end architecture)
## Bead: eu-u2vn.2

## Problem

A dependency makes several source-level contributions to a dependent
unit's compilation, handled today by four separate, inconsistent
mechanisms:

1. **Bracket content-modes** — per-file `BracketRegistry`, no
   cross-file seeding (`src/syntax/rowan/parse.rs:45`)
2. **Monad-namespace registry** — seedable via drain/seed pattern
   (`src/core/desugar/desugarer.rs:171`)
3. **Operator table** — rediscovered from merged tree at cook, no
   seeding (`src/core/cook/fixity.rs:143`)
4. **Type schemes** — `PreludeSummary`/`with_seed` in the type checker
   (`src/core/typecheck/check.rs:197,382`)

The bracket mechanism has an outright bug: a block-style bracket pair
defined in an imported file is invisible to the importing file's
pre-scan. Uses of the pair default to soup mode — a program that works
in one file silently mis-parses when the definition moves to a library.

## Design

### Bracket fix: colon heuristic

Remove the `BracketRegistry` from the parser. Instead, determine block
vs soup mode from the bracket content itself:

- **Colons at top level** → block mode (declarations)
- **No colons** → soup mode (catenated elements)
- **Empty** → soup mode

This works because monad brackets are restricted:
- **No empty monad brackets** — an empty monadic block is meaningless
- **No block metadata in monad brackets** — the monad tag (`:for`,
  `:io`) goes on the outer block declaration, not inside the brackets

These restrictions are safe — no existing code in the codebase uses
empty block-mode brackets or block metadata inside bracket content.

### UnitInterface struct

Unify the four cross-unit mechanisms into a single `UnitInterface`:

```rust
pub struct UnitInterface {
    /// Bracket pair content modes (block vs expression)
    pub bracket_modes: HashMap<char, BracketContentType>,

    /// Monad namespace specs (bind/return shapes)
    pub monad_specs: HashMap<String, MonadSpec>,

    /// Monad wrapper type hints (for LSP element-type display)
    pub monad_type_hints: HashMap<String, String>,

    /// Operator fixity, precedence, and type schemes
    pub operators: HashMap<String, OperatorInfo>,

    /// Type schemes, aliases, and branch shapes
    pub type_summary: PreludeSummary,

    /// Declaration visibility (internal vs exported)
    pub visibility: HashMap<String, Visibility>,
}
```

Built incrementally as each pipeline phase completes:
1. After parse: bracket modes extracted (local declarations)
2. After translate/desugar: monad registries drained, bracket modes
   finalised with imported seeds
3. After cook: operator metadata extracted (with imported seeds)
4. After typecheck: type schemes extracted

`PreludeSummary` is absorbed — it becomes the `type_summary` field.
The `PRELUDE_CACHE` in `check.rs` caches the prelude's complete
`UnitInterface` rather than just the type summary.

### Operator extraction

Extract operator metadata once, seed to cook via the UnitInterface.
Eliminates the current redundancy where operators are discovered
both by cook (`fixity.rs:143`) and by typecheck
(`extract_operator_type_strings`).

## Code locations

- `src/syntax/rowan/parse.rs:45` — `prescan_bracket_declarations`,
  `BracketRegistry` (to remove)
- `src/core/desugar/desugarer.rs:160-177` — monad registry
  drain/seed (to consolidate)
- `src/core/cook/fixity.rs:143` — operator rediscovery (to replace
  with seeded table)
- `src/core/typecheck/check.rs:197,382` — `PreludeSummary` (to
  generalise into UnitInterface)
- `src/driver/source.rs` — `SourceLoader` (to carry UnitInterface)
- `src/driver/check.rs:51-96` — `PRELUDE_CACHE` (to cache
  UnitInterface)

## Acceptance criteria

1. `BracketRegistry` removed from parser
2. Colon heuristic correctly determines block vs soup for all
   existing bracket usages
3. Bracket pair defined in imported file parses correctly in
   importing file (regression test)
4. `UnitInterface` struct replaces the four separate mechanisms
5. Operators extracted once, seeded to cook
6. `PreludeSummary` absorbed into `UnitInterface`
7. All existing harness tests pass
8. `eu check lib/prelude.eu` exits 0
