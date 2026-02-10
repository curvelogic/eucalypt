# Improve Dump and Quote Formats Across Pipeline Phases

**Parent epic**: eu-vt15
**Date**: 2026-02-09

## Overview

Improve the dump output formats for debugging and complete the
quote/embed system for directly targeting core and STG phases in
test code.

## Agreed Decisions

- **Core dump format**: Compact symbolic notation shared across all
  core phases (desugared, cooked, inlined, pruned). Use symbols for
  lambda, case, application etc. Keep intrinsic names honest. Improve
  indentation and grouping.
- **STG dump format**: Same philosophy — tighter, better structured,
  improved indentation for nested expressions.
- **AST dump**: Keep as-is, low priority, useful for agents debugging
  parse issues.
- **Quote/embed**: Fix completeness for all expression types (including
  Let, Lam, Block) for both core and STG. Stabilise format by
  convention (documented, no snapshot tests).
- **Embed purpose**: Inline ASM equivalent — write syntactically valid
  eucalypt that directly constructs core/STG expressions, can reference
  outer scope variables. Primarily for testing.

## Tasks

### 1: Redesign core pretty-printer

Redesign the `ToPretty` implementation for `RcExpr` in
`src/core/export/pretty.rs` to produce compact symbolic output.

Design goals:
- Use symbolic notation: `λ` for lambda, `→` for case branches,
  `@` for application (or similar concise conventions)
- Clean indentation with proper nesting for let-bindings, case
  expressions, and nested applications
- Intrinsic names shown as-is (e.g. `ADD`, `LOOKUP`, `HEAD`)
- Compact — minimise vertical space while keeping readability
- Single format used for all core phases (desugared through pruned)
- 80-character line width target (already in prettify infrastructure)

**Files**: `src/core/export/pretty.rs`, `src/common/prettify.rs`

**Testing**: Manually verify output is readable across all four core
phases. Run `eu dump desugared`, `eu dump cooked`, `eu dump inlined`,
`eu dump pruned` on representative test files and confirm output is
useful for debugging.

### 2: Redesign STG pretty-printer

Redesign the `ToPretty` implementation for `StgSyn` and `LambdaForm`
in `src/eval/stg/pretty.rs`.

Design goals:
- Tighter output — reduce vertical space
- Better indentation and grouping for deeply nested CASE expressions
- Clear visual structure for lambda forms, closures, and thunks
- Intrinsic names shown as-is
- Compact representation of data constructors and tag matching

**Files**: `src/eval/stg/pretty.rs`

**Testing**: Run `eu dump stg` and `eu dump runtime` on representative
files and confirm output is useful for debugging.

### 3: Complete core embed for all expression types

Fix the `Embed` implementation for `CoreExpr` in
`src/core/export/embed.rs` so that all expression types can be
embedded, including:
- `Let` bindings (currently returns `None`)
- `Lam` (lambda expressions, currently returns `None`)
- `Block` (currently returns `None`)
- Any other variants that currently return `None`

Each needs a stable tag-based representation (e.g. `[c-let, bindings,
body]`, `[c-lam, params, body]`, `[c-block, pairs]`) that can be
parsed back via the `parse-embed` mechanism.

Ensure embedded expressions can reference variables from the enclosing
eucalypt scope.

**Files**: `src/core/export/embed.rs`, potentially
`src/core/metadata.rs` for parse-embed handling

**Testing**: Create test files that embed core expressions inline and
verify they produce the same results as the equivalent compiled
eucalypt. Test each expression type (Let, Lam, Block, App, List, etc.)
in isolation and in combination.

### 4: Complete STG embed for all expression types

Create or complete an `Embed` implementation for `StgSyn` so that STG
expressions can be represented as syntactically valid eucalypt with a
`parse-embed: :STG` block.

This needs a stable tag-based representation for:
- Lambda forms
- Case expressions and branches
- Data constructor applications
- Let/LetRec bindings
- Primitive operations
- Annotations

Ensure embedded STG can reference variables from the enclosing scope.

**Files**: `src/eval/stg/` (new or existing embed module),
`src/core/metadata.rs` for parse-embed handling

**Testing**: Create test files embedding STG expressions. Verify they
execute correctly and produce expected results.

### 5: Document the embed format

Document the stable embed format for both core and STG:
- Complete tag vocabulary with examples
- Scoping rules for referencing outer variables
- How parse-embed blocks are structured
- Limitations and edge cases

This serves as the stability contract — changes to the format should
be made deliberately and documented.

**Files**: Internal documentation (e.g. comments in embed.rs or a
doc in docs/)

## Dependency Graph

```
[1: Core pretty-printer]     [2: STG pretty-printer]
                \                    /
                 \                  /
[3: Core embed complete] → [5: Document format]
[4: STG embed complete]  ↗
```

Tasks 1-4 are largely independent and can be parallelised. Task 5
depends on 3 and 4 being complete so the format is settled.
