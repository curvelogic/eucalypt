# Documentation Refresh Implementation Plan

**Parent epic**: eu-wma8 (Documentation refresh for 0.4.0)
**Date**: 2026-02-09

## Overview

Complete documentation refresh: switch to mdBook, restructure content
following Diataxis framework, write new tutorial and example content,
auto-generate prelude reference, add documentation testing, and create
agent-friendly documentation.

## Agreed Decisions

- **Platform**: Switch from MkDocs to mdBook
- **Structure**: Welcome / Guide / Reference / Understanding / FAQ /
  Appendices. No Cookbook or Contributors sections.
- **Prelude reference**: Auto-generated from source via mdBook
  preprocessor, split by category
- **Agent docs**: All four — llms.txt, llms-full.txt, expanded
  AGENTS.md, dedicated agent reference page
- **Eucalypt by Example**: Included in 0.4.0 scope (synergy with AoC
  2025 bead eu-zuif)
- **Documentation testing**: Built into mdBook pipeline for 0.4.0
- **Architecture/GC docs**: Stay in repo but not published to
  user-facing site

## Site Structure

```
src/SUMMARY.md
├── Welcome
│   ├── What is Eucalypt?
│   ├── Quick Start (install + first program)
│   └── Eucalypt by Example (10-15 worked examples)
│
├── The Eucalypt Guide (tutorial, read in order)
│   ├── 1. Blocks and Declarations
│   ├── 2. Expressions and Pipelines
│   ├── 3. Lists and Transformations
│   ├── 4. String Interpolation
│   ├── 5. Functions and Combinators
│   ├── 6. Operators
│   ├── 7. Anaphora (Implicit Parameters)
│   ├── 8. Block Manipulation
│   ├── 9. Imports and Modules
│   ├── 10. Working with Data (JSON, YAML, TOML, CSV, XML)
│   ├── 11. The Command Line
│   ├── 12. YAML Embedding
│   ├── 13. Testing with Eucalypt
│   ├── 14. Date, Time, and Random Numbers
│   └── 15. Advanced Topics (metadata, sets, deep queries, formatting)
│
├── Reference
│   ├── Language Syntax Reference
│   ├── Operator Precedence Table
│   ├── Prelude Reference (auto-generated from source)
│   │   ├── Lists
│   │   ├── Blocks
│   │   ├── Strings (str.*)
│   │   ├── Numbers and Arithmetic
│   │   ├── Booleans and Comparison
│   │   ├── Combinators
│   │   ├── Calendar (cal.*)
│   │   ├── Sets (set.*)
│   │   ├── Random Numbers
│   │   ├── Metadata
│   │   └── IO
│   ├── CLI Reference
│   ├── Import Formats
│   ├── Export Formats
│   └── Error Messages Guide
│
├── Understanding Eucalypt
│   ├── Design Philosophy
│   └── Lazy Evaluation
│
├── FAQ
│
└── Appendices
    ├── Syntax Cheat Sheet
    └── Migration from v0.2 to v0.3
```

## Tasks

### Phase 1: Infrastructure (blocks everything else)

#### 1A: Switch to mdBook

Create `book.toml` configuration and `src/SUMMARY.md` with the agreed
structure. Migrate existing markdown files into the new layout,
converting MkDocs-specific syntax (admonitions `!!! note` → mdBook
equivalents). Update `.github/workflows/docs.yaml` to build with
mdBook and deploy to GitHub Pages. Remove `mkdocs.yml` and
`docs/requirements.txt`.

**Files**: `book.toml`, `src/SUMMARY.md`, `.github/workflows/docs.yaml`,
existing `docs/*.md` files (move to `src/`), `mkdocs.yml` (delete),
`docs/requirements.txt` (delete)

**Testing**: `mdbook build` succeeds, `mdbook serve` renders correctly,
GitHub Pages deployment works.

#### 1B: Custom eu syntax highlighting

Create a custom language definition for eucalypt so that `eu` code
blocks render with proper syntax highlighting in mdBook.

**Files**: custom highlight.js definition or mdBook theme extension

**Testing**: Code blocks in docs render with highlighting for keywords,
strings, operators, comments.

### Phase 2: Content Migration and Fixes (depends on 1A)

#### 2A: Fix stale content and migrate

- Update version references (0.2.0 → current)
- Fix UK English inconsistencies ("colorful" → "colourful")
- Update outdated CLI flag references (`-T` → `eu test`,
  `-p`/`--dump-xxx` → `eu dump`)
- Fix broken links (README prelude link)
- Ensure all migrated content is correctly placed in new structure
- Remove `implementation.md` (outdated, not user-facing)
- Move architecture/GC docs out of published site

**Files**: All existing doc files

**Testing**: All links work, no stale references remain, `mdbook build`
clean.

#### 2B: Document undocumented features

Add documentation for the 12 features identified as having no
user-facing docs:

1. Random number generation (`random-stream`, `shuffle`, `sample`,
   `io.random`, `--seed`)
2. Streaming imports (`jsonl-stream@`, `csv-stream@`, `text-stream@`)
3. Sets (`set.from-list`, `set.union`, `set.intersect`, etc.)
4. ZDT literals (`t"2023-01-15T10:30:00Z"`)
5. Deep find / deep query (`deep-find`, `deep-query`, patterns)
6. Block indexing (`!!` operator)
7. Sorting (`qsort`, `sort-nums`, `sort-by`, etc.)
8. Base64 and SHA-256 (`str.base64-encode`, `str.sha256`)
9. Doc metadata system (`raw-meta`, doc-gen)
10. LSP server (`eu lsp`)
11. Formatter (`eu fmt`)
12. Version assertions (`eu.requires(">=0.3.0")`)

These feed into both the Guide chapters and the Reference section.

**Files**: New pages in reference section, updates to prelude source
doc strings

**Testing**: Each feature has at least one runnable example.

### Phase 3: Build Pipeline (can run parallel with content work)

#### 3A: Prelude reference auto-generation

Build an mdBook preprocessor (or build script) that extracts
documentation from `lib/prelude.eu` backtick doc strings using
`lib/doc-gen.eu` (or a Rust-based extractor) and produces markdown
pages split by category (Lists, Blocks, Strings, Numbers, Booleans,
Combinators, Calendar, Sets, Random, Metadata, IO).

The generated pages should include function signatures, descriptions,
and examples extracted from the source.

**Files**: mdBook preprocessor script/binary, `lib/prelude.eu`
(ensure all functions have doc strings)

**Testing**: `mdbook build` produces prelude reference pages that match
`lib/prelude.eu` content. Adding a new prelude function with a doc
string automatically appears in the reference.

#### 3B: Documentation testing

Create a mechanism to extract code blocks from documentation, run them
through `eu`, and verify the output matches expected results. This
could be an mdBook preprocessor, a CI step, or a standalone test
script.

Code blocks marked with a convention (e.g. ` ```eu,test `) would be
extracted and executed. Expected output would follow in a separate
block or annotation.

**Files**: Test extraction script/preprocessor, CI integration

**Testing**: Deliberately break an example and verify the build fails.

### Phase 4: New Content (depends on 2A, can run parallel with 3)

#### 4A: Write the Eucalypt Guide

15 progressive tutorial chapters, each building on the previous.
Each chapter should include:
- Clear learning objectives
- Explanation with examples
- Runnable code samples with expected output
- Key concepts highlighted

Chapters:
1. Blocks and Declarations
2. Expressions and Pipelines
3. Lists and Transformations
4. String Interpolation
5. Functions and Combinators
6. Operators
7. Anaphora (Implicit Parameters)
8. Block Manipulation
9. Imports and Modules
10. Working with Data (JSON, YAML, TOML, CSV, XML)
11. The Command Line
12. YAML Embedding
13. Testing with Eucalypt
14. Date, Time, and Random Numbers
15. Advanced Topics (metadata, sets, deep queries, formatting)

**Testing**: All code examples pass documentation testing.

#### 4B: Write Eucalypt by Example

10-15 worked examples showing real-world problems solved in eucalypt.
Each example includes:
- Problem statement
- Input data (JSON/YAML)
- Eucalypt code
- Output
- Brief explanation of key concepts

Synergy with AoC 2025 bead (eu-zuif) — solutions can feed into
examples.

**Testing**: All examples pass documentation testing.

#### 4C: Write FAQ

20 questions covering:
- Getting started (4 questions: install, convert formats, pipelines,
  supported formats)
- Language (6 questions: functions, catenation, anaphora, parsing,
  merging blocks, lambdas)
- Data processing (5 questions: filter/transform, nested lookup, deep
  search, sorting, dates)
- Advanced (5 questions: metadata, imports, testing, random numbers,
  sets)

**Testing**: All code examples in answers pass documentation testing.

#### 4D: Write syntax cheat sheet

Dense single-page reference covering all syntax forms, operators with
precedence, common patterns, and key prelude functions. Designed to
be useful for both humans and AI agents.

**Files**: Single markdown page in appendices

### Phase 5: Agent-Friendly Documentation (depends on content existing)

#### 5A: Create llms.txt and llms-full.txt

- `llms.txt` — Markdown index of all documentation sections with brief
  descriptions, placed at site root
- `llms-full.txt` — Auto-generated concatenation of all user-facing
  docs into a single markdown file, produced as part of the build

**Files**: `llms.txt` (hand-written), build step for `llms-full.txt`

**Testing**: Both files are valid, `llms-full.txt` contains all doc
content.

#### 5B: Expand AGENTS.md

Add to the existing AGENTS.md:
- Language syntax quick reference
- Top 20 common patterns and idioms
- Common pitfalls (from syntax-gotchas)
- Key prelude functions cheat sheet
- Error handling guidance

**Files**: `AGENTS.md`

#### 5C: Create agent reference page

Purpose-built dense, example-heavy single page for AI coding agents.
Covers:
- Complete syntax reference
- All operators with precedence
- Top 30 prelude functions with signatures and examples
- Pipeline patterns and idioms
- Common pitfalls
- Disambiguation of similar concepts (map vs mapcat, etc.)

Published as part of the docs site and referenced from `llms.txt`.

**Files**: New page in docs

**Testing**: Examples pass documentation testing.

## Dependency Graph

```
[1A: mdBook setup] → [1B: syntax highlighting]
        |
        ├──→ [2A: fix/migrate content]
        |         |
        |         ├──→ [2B: document missing features]
        |         |
        |         ├──→ [4A: Guide chapters]
        |         |
        |         ├──→ [4B: Eucalypt by Example]
        |         |
        |         ├──→ [4C: FAQ]
        |         |
        |         └──→ [4D: Cheat sheet]
        |
        ├──→ [3A: prelude auto-generation]
        |
        └──→ [3B: doc testing]
                    |
                    └──→ [5A: llms.txt] ← needs content
                         [5B: AGENTS.md]
                         [5C: agent reference]
```

## Synergies with Other 0.4.0 Beads

- **eu-zuif (AoC 2025)** — Solutions feed into Eucalypt by Example
  content
- **eu-fti1 (Error messages)** — Error message improvements feed into
  the Error Messages Guide in the reference section
- **eu-epe4 (aarch64 binary)** and **eu-1j9u (install script)** — Quick
  Start page needs updated install instructions
- **eu-vt15 (dump/quote formats)** — Improved dump output is
  documented in CLI Reference

## Testing Strategy

Every content task MUST:
1. Include runnable code examples with expected output
2. Use the documentation testing mechanism (once 3B is complete)
3. Pass `mdbook build` cleanly
4. Follow UK English conventions throughout
