## Context

Eucalypt uses a Rowan-based AST with an existing `ToSourceDoc` trait that currently just returns the original source text. The `pretty` crate is already a dependency and provides document combinators for width-aware formatting.

### Stakeholders
- Eucalypt users who want consistent code formatting
- Tool authors who may integrate formatting into editors/CI

### Constraints
- Must preserve semantic meaning (e.g., `f(x)` vs `f (x)` distinction)
- Must handle string literals, comments, and metadata without corruption
- Should integrate with existing Rowan AST infrastructure

## Goals / Non-Goals

**Goals:**
- Implement a working formatter with configurable line width
- Support both conservative and full reformatting modes
- Handle all eucalypt syntax elements correctly
- Preserve intentional alignment where detectable

**Non-Goals:**
- Editor integration (IDE plugins) - future work
- Formatting other representations (Core, STG) - separate proposal
- Automatic formatting on save - user/editor responsibility

## Decisions

### Decision 1: Two-Mode Architecture
Use a dual-mode approach:
1. **Conservative mode** (default): Fix spacing violations while preserving good formatting
2. **Full reformatting mode**: Generate well-formatted output from scratch

**Rationale**: Conservative mode respects user formatting choices. Full reformatting provides a canonical style when code is badly formatted or contains tabs.

### Decision 2: Pretty Printer Algorithm
Use Wadler-Lindig style pretty printing via the `pretty` crate.

**Alternatives considered**:
- Custom line-breaking logic: More work, less proven
- Prettier-style algorithm: More complex, not needed for eucalypt's syntax

**Rationale**: The `pretty` crate is already a dependency and provides proven document combinators.

### Decision 3: Tab Handling Strategy
Tabs trigger mode switching:
- If tabs appear to represent alignment (assuming 2 or 4 char width), convert to equivalent spaces
- If tabs cannot be legitimised as alignment, trigger full reformatting of that AST node

**Rationale**: Tabs are always wrong in eucalypt source, but we should try to preserve apparent intent before reformatting.

### Decision 4: Whitespace Preservation
Use the Rowan green tree to access original whitespace tokens, enabling:
- Preservation of blank lines
- Detection of intentional alignment
- Identification of spacing violations

**Rationale**: Rowan preserves trivia (whitespace, comments) in the syntax tree.

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| Semantic changes during formatting | Comprehensive test suite with round-trip verification |
| Loss of comments | Rowan preserves trivia; test comment preservation explicitly |
| Performance on large files | Benchmark and optimise if needed; likely not an issue |
| Complex nesting edge cases | Start with common patterns; iterate based on real usage |

### Decision 5: I/O Behaviour
Default to reading from file and writing to stdout. Provide `--write` flag for in-place modification.

**Rationale**: Safer default; users can preview changes before modifying files. Matches behaviour of tools like `rustfmt --check`.

### Decision 6: Check Mode
Provide `--check` flag that exits with non-zero status if formatting would change the file.

**Rationale**: Essential for CI integration; allows enforcing formatting standards in pipelines.

### Decision 7: Default Line Width
Default line width is 80 characters, configurable via `--width` option.

**Rationale**: 80 is a widely accepted default; configurability allows project-specific preferences.
