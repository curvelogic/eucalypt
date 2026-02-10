# Documentation Refresh Research Report

**Date**: 2026-02-09
**Bead**: eu-wma8

---

## 1. Current State: Complete Documentation Inventory

### 1.1 Documentation Site Infrastructure

Eucalypt's documentation is currently served via **MkDocs** with the **Read the Docs** theme, deployed to GitHub Pages at `https://curvelogic.github.io/eucalypt/` via `.github/workflows/docs.yaml`. The site is triggered on release publication or manual workflow dispatch.

**Configuration** (`mkdocs.yml`):
- Uses `readthedocs` theme (dated, minimal customisation)
- Markdown extensions: `admonition`, `footnotes`, `tables`
- Python dependencies: `PyYAML>=6.0`, `mkdocs>=1.5` (from `docs/requirements.txt`)
- No search configuration, no syntax highlighting for `eu` code blocks, no versioning

### 1.2 Documentation Files Inventory

| File | Topic | Lines | Quality | Freshness |
|------|-------|-------|---------|-----------|
| `docs/index.md` | Landing page / lightning tour | ~439 | Good — engaging, well-written | Partially stale (mentions "0.1.x Haskell" era, some language choices like "colorful" not UK English) |
| `docs/getting-started.md` | Installation guide | ~90 | Adequate | Stale — shows version `0.2.0`, missing aarch64-linux, missing install script, missing `eu run` subcommand examples |
| `docs/syntax.md` | Language syntax reference | ~324 | Good — thorough | Reasonably current |
| `docs/syntax-gotchas.md` | Common pitfalls | ~128 | Good — practical | Current |
| `docs/operators-and-identifiers.md` | Operator/identifier naming | ~145 | Adequate | Current |
| `docs/anaphora-and-lambdas.md` | Implicit parameters | ~230 | Good — thorough with examples | Current |
| `docs/command-line.md` | CLI reference | ~455 | Good — comprehensive | Current (updated for subcommands) |
| `docs/imports.md` | Import system | ~303 | Good — covers git imports, YAML features | Current (includes anchors, merge keys, timestamps) |
| `docs/yaml-embedding.md` | YAML `!eu` tags | ~105 | Adequate | Current |
| `docs/prelude.md` | Prelude function reference | ~387 | Good — comprehensive tables | Mostly current (includes sets, cal, sorting, streams missing) |
| `docs/tester.md` | Test framework | ~88 | Thin — basic coverage | Stale (mentions `-T` flag, missing `eu test` subcommand) |
| `docs/implementation.md` | Implementation overview | ~45 | Thin | Stale (mentions "deliberately leaks memory", outdated diagnostics flags) |
| `docs/architecture.md` | Detailed architecture | ~710 | Excellent — deep and well-structured | Current |
| `docs/gc-implementation.md` | GC documentation | ~399 | Excellent — detailed technical reference | Current |
| `docs/gc-benchmarking.md` | GC benchmarking procedures | ~165 | Good — practical workflow guide | Current |
| `docs/deep-find-performance-baseline.md` | Performance analysis | ~118 | Good — thorough investigation | Current |
| `docs/philosophy-lang.md` | Language design philosophy | ~155 | Good — thoughtful and distinctive | Current |

**Total documentation**: ~4,286 lines across 17 files (excluding plans/).

### 1.3 README.md

The `README.md` (77 lines) is minimal but functional. It provides a quick example, feature list, installation instructions (Homebrew + source), and links to documentation. The prelude reference link points to `docs/prelude.md` (a local path) rather than the deployed site.

### 1.4 Library Documentation

| File | Purpose | Lines | Documentation Quality |
|------|---------|-------|-----------------------|
| `lib/prelude.eu` | Standard library (~1100 lines) | ~1100 | Good — every public function has a backtick doc string |
| `lib/test.eu` | Test framework library | ~266 | Adequate — structural docs, report generation |
| `lib/markup.eu` | Hiccup-style markup utilities | ~42 | Minimal |
| `lib/doc-gen.eu` | Documentation generator (meta!) | ~110 | Good — self-documenting |

The prelude has approximately 176 documented items with backtick doc strings. The `doc-gen.eu` tool can extract these into Markdown, but this pipeline is not integrated into the documentation build.

### 1.5 CLAUDE.md and AGENTS.md

- `CLAUDE.md` (project root): Comprehensive (~200 lines) with build commands, architecture overview, testing guidance, code quality rules, and development workflow. Well-maintained.
- `AGENTS.md` (project root): Brief (~40 lines) focusing on `bd` (beads) issue tracking workflow and session completion protocol.

### 1.6 Help Text (CLI)

The `eu --help` output and subcommand help text is generated via `clap v4` derive macros in `src/driver/options.rs`. This is adequate but sparse — no examples in help text, no man page generation.

---

## 2. Content Gaps

### 2.1 Critical Gaps (Features with No Documentation)

1. **Random number generation** — `random-stream`, `random-int`, `random-choice`, `shuffle`, `sample`, `io.random`, `--seed` flag. Implemented in prelude (harness test 078) but completely undocumented.

2. **Streaming imports** — `jsonl-stream@`, `csv-stream@`, `text-stream@` import formats for lazy streaming. Tested in harness test 079, not documented anywhere.

3. **Sets** — `set.from-list`, `set.to-list`, `set.add`, `set.remove`, `set.contains?`, `set.union`, `set.intersect`, `set.diff`, `set.size`, `set.empty?`, and `∅` (empty set literal). Comprehensive implementation (harness test 074), zero documentation.

4. **ZDT (Zoned DateTime) literals** — `t"2023-01-15T10:30:00Z"` syntax for datetime literals. Tested in harness test 068, not mentioned in any documentation.

5. **Deep find / deep query** — `deep-find`, `deep-find-first`, `deep-find-paths`, `deep-query`, `deep-query-first`, `deep-query-paths`. Pattern-based recursive data querying (harness tests 072, 075). Not in user docs (only in `docs/deep-find-performance-baseline.md` which is a technical note).

6. **Block indexing** — `!!` operator for block indexing by numeric index (harness test 073). Not documented.

7. **Sorting** — `qsort`, `sort-nums`, `sort-strs`, `sort-zdts`, `sort-by`, `sort-by-num`, `sort-by-str`, `sort-by-zdt`, `sort-keys`. Comprehensive sorting (harness tests 069, 071), only partially in prelude reference.

8. **Base64 and SHA-256** — `str.base64-encode`, `str.base64-decode`, `str.sha256`. Tested (harness tests 066, 067), listed in prelude.md but with no usage examples.

9. **Doc metadata system** — `raw-meta`, metadata-driven documentation generation (`lib/doc-gen.eu`). A sophisticated self-documentation system exists but is not explained to users.

10. **LSP server** — `eu lsp` subcommand exists but has no documentation whatsoever.

11. **Formatter** — `eu fmt` is documented in `command-line.md` but deserves its own guide with examples and integration advice.

12. **Version assertions** — `eu.requires(">=0.3.0")` for semver constraints (harness test 065). Undocumented.

### 2.2 Significant Gaps (Incomplete Documentation)

1. **Error messages and debugging** — No guide on understanding error messages, debugging strategies, or common runtime errors. The `syntax-gotchas.md` is a good start but covers only three issues.

2. **Recipes / cookbook** — No practical examples showing real-world use cases: processing AWS responses, generating Kubernetes configs, transforming CI/CD files, querying JSON APIs, etc.

3. **How blocks actually work** — The dual nature of blocks (binding vs. data structuring) is mentioned in `architecture.md` but never explained to users in accessible terms.

4. **Metadata system** — Beyond the prelude reference table, the metadata system (`:target`, `:suppress`, `:main`, `:doc`, assertion operators) is scattered and not comprehensively explained.

5. **Interoperability** — How to use eucalypt with `jq`, shell pipelines, `yq`, scripting, CI/CD tools.

6. **Eufile** — Mentioned in `index.md` but never documented. What goes in a `Eufile`? How does it work?

7. **~/.eucalypt configuration** — Mentioned as providing "user-specific declarations" but never explained.

8. **Performance guidance** — When is eucalypt fast? When is it slow? What are the practical limits?

### 2.3 Quality Issues

1. **Stale version references** — `getting-started.md` shows `eu 0.2.0`; current version is `0.3.0`.
2. **Inconsistent UK English** — `index.md` uses "colorful" (American) rather than "colourful".
3. **Outdated CLI flags** — `implementation.md` references `-p` and `--dump-xxx` flags that have been replaced by `eu dump` subcommands. `tester.md` references `-T` flag replaced by `eu test`.
4. **No cross-linking** — Documents rarely link to each other. The prelude reference does not link to syntax docs for operators.
5. **No runnable examples** — Code blocks cannot be executed or verified. There is no playground.
6. **Inconsistent heading structure** — Some files use H1 for sections, others use H2.

---

## 3. Structure Problems

### 3.1 Navigation Issues

The current MkDocs nav is flat — a single level of items with no logical grouping except a collapsed "Implementation" section. A new user encountering this list of 17 items has no sense of learning path or progression.

**Current nav structure**:
```
Home → Getting Started → Syntax → Syntax Gotchas → Operators → Anaphora →
Command Line → Imports → YAML Embedding → Prelude → Testing →
Implementation (Overview, Architecture, GC, GC Benchmarking) → Philosophy
```

### 3.2 Missing User Journey

There is no clear learning path. A new user might read the lightning tour on the home page, then jump to "Getting Started" for installation, but then faces a wall of reference documentation with no guided tutorial.

The documentation conflates three distinct audiences:
- **New users** who want to learn eucalypt from scratch
- **Practitioners** who need reference material for daily use
- **Contributors** who need to understand the implementation

### 3.3 Internal vs. External Documentation

Design documents (`docs/plans/`), performance baselines (`deep-find-performance-baseline.md`), and GC benchmarking workflows are mixed in with user-facing documentation. These are valuable but belong in a separate section or repository.

### 3.4 The Prelude Reference Problem

The prelude reference (`docs/prelude.md`) is the single most important reference document but it is:
- Manually maintained (prone to drift from `lib/prelude.eu`)
- A single massive page with no inter-linking
- Missing examples for most functions
- Not generated from source (despite `lib/doc-gen.eu` existing!)

---

## 4. Modern Best Practices

### 4.1 How Modern Languages Structure Documentation

The best programming language documentation follows the **Diataxis** framework, which identifies four types of documentation:

1. **Tutorials** — Learning-oriented, step-by-step guides ("The Rust Book", "The Elm Guide", "Tour of Go")
2. **How-to Guides** — Task-oriented, practical recipes ("Rust Cookbook")
3. **Reference** — Information-oriented, exhaustive and accurate ("Rust std library docs")
4. **Explanation** — Understanding-oriented, conceptual discussion ("Rustonomicon")

**The Rust Book** is widely considered the gold standard:
- Progressive structure: basics → ownership → structs → enums → modules → etc.
- Each chapter builds on the previous
- Practical examples woven throughout
- Exercises at the end of sections
- Separate "Rust by Example" companion for example-first learning

**The Gleam Guide** is excellent for a small language:
- Concise, focused chapters
- Every concept illustrated with code + output
- Online playground for trying examples
- "Language Tour" as primary learning path

**Key patterns observed**:
- Separate "Guide" (tutorial) from "Reference" (API docs)
- Online playground or REPL for immediate experimentation
- Search that understands code (function names, operators)
- Syntax highlighting for the language itself
- Versioned documentation matching releases

### 4.2 Documentation Site Generators

| Generator | Technology | Best For | Eucalypt Fit |
|-----------|-----------|----------|-------------|
| **mdBook** | Rust | Programming language guides, book-style docs | Excellent — Rust ecosystem, fast, simple, The Rust Book uses it |
| **Starlight** (Astro) | JavaScript/Astro | Modern docs with rich features | Good — beautiful, full-featured, but heavier toolchain |
| **Docusaurus** | JavaScript/React | Large projects with versioning, i18n | Overkill — too heavy for eucalypt's needs |
| **MkDocs** (current) | Python | Simple docs | Adequate but outdated feel |
| **MkDocs Material** | Python | Enhanced MkDocs | Good upgrade path from current setup |

**Recommendation**: **mdBook** is the strongest choice for eucalypt:
- Written in Rust, aligning with eucalypt's toolchain
- Used by The Rust Book, mdBook itself, and dozens of Rust projects
- Built-in search, syntax highlighting (custom languages possible)
- Fast build times, simple Markdown-based authoring
- Supports custom preprocessors (could auto-generate prelude docs)
- `cargo install mdbook` — one command to install
- Active development, well-maintained by the Rust project

**Second choice**: **Starlight** if a more modern visual design is desired, though it adds a JavaScript dependency.

---

## 5. Agent-Friendly Documentation

### 5.1 The llms.txt Standard

The [llms.txt specification](https://llmstxt.org/) is an emerging standard (adopted by 600+ websites including Anthropic, Stripe, Cloudflare) for making documentation accessible to LLMs and coding agents. It consists of:

- **`/llms.txt`** — A concise, Markdown-formatted index of documentation with links and descriptions
- **`/llms-full.txt`** — Complete documentation content in a single Markdown file for full-context ingestion

For eucalypt, creating these files would make the language immediately usable by AI coding agents helping users write eucalypt code.

### 5.2 The AGENTS.md Convention

The [AGENTS.md format](https://agents.md/) (launched late 2025 by Google, OpenAI, Factory, Sourcegraph, and Cursor) provides structured guidance for coding agents. Eucalypt already has an `AGENTS.md` but it only covers issue tracking workflow. It should be expanded to include:

- Language syntax quick reference
- Common patterns and idioms
- Error handling guidance
- Prelude function cheat sheet

### 5.3 What Makes Documentation Agent-Friendly

Based on research into how projects like Deno, Bun, and Astro structure their docs for AI consumption:

1. **Concise, self-contained examples** — Each concept should have a complete, runnable example that includes input, code, and expected output. Agents can copy-paste these directly.

2. **Structured metadata** — Consistent formatting so that function signatures, parameters, return values, and examples can be parsed programmatically.

3. **Single-page references** — A single comprehensive reference page (like `llms-full.txt`) that an agent can ingest entirely rather than navigating links.

4. **Disambiguation of similar concepts** — Explicit "X vs Y" sections (e.g., "when to use `map` vs `mapcat`", "expression anaphora vs block anaphora").

5. **Error catalogue** — Mapping error messages to causes and solutions, so agents can diagnose errors.

6. **Idiom library** — Common patterns expressed in eucalypt, since agents cannot infer eucalypt idioms from other languages (the catenation/pipeline style, anaphora, block merge patterns).

### 5.4 Specific Recommendations for Eucalypt

1. **Create `/llms.txt`** — A concise markdown index of all documentation sections with brief descriptions.

2. **Create `/llms-full.txt`** — Concatenation of all user-facing documentation into a single file, automatically generated during doc build.

3. **Expand `CLAUDE.md`** — Add a "Language Quick Reference" section with the 20 most common patterns, so any coding agent working on the codebase can also help users write eucalypt.

4. **Create a "Eucalypt for AI Agents" reference** — A dense, example-heavy single page specifically designed for agents helping users write eucalypt code. This would include:
   - Syntax cheat sheet (declarations, expressions, blocks, lists)
   - All operators with precedence
   - Top 30 prelude functions with signatures and examples
   - Common pitfalls (the syntax gotchas)
   - Pipeline patterns

---

## 6. Recommended New Documentation Structure

### 6.1 Proposed Table of Contents

```
eucalypt Documentation
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
├── Cookbook (task-oriented recipes)
│   ├── Processing JSON API Responses
│   ├── Generating Kubernetes Manifests
│   ├── Templating Configuration Files
│   ├── Data Transformation Pipelines
│   ├── Working with CSV Data
│   ├── Shell Pipeline Integration
│   └── CI/CD Integration
│
├── Understanding Eucalypt (explanatory)
│   ├── Design Philosophy
│   ├── How Blocks Work (binding vs. data)
│   ├── Lazy Evaluation
│   ├── The Metadata System
│   ├── Syntax Gotchas and Pitfalls
│   └── Performance Characteristics
│
├── For Contributors
│   ├── Architecture Overview
│   ├── GC Implementation
│   ├── GC Benchmarking
│   ├── Adding Intrinsics
│   └── Development Workflow
│
└── Appendices
    ├── Syntax Cheat Sheet (single page)
    ├── Migration from v0.2 to v0.3
    └── FAQ
```

### 6.2 Key Structural Decisions

1. **Separate "Guide" from "Reference"** — The guide is a progressive tutorial; the reference is exhaustive and alphabetical.

2. **Auto-generate prelude reference** — Use `lib/doc-gen.eu` (or an enhanced version) as a build step to generate the prelude reference from source metadata. This eliminates drift.

3. **Cookbook section** — Real-world, copy-paste-ready recipes. This is what draws users and what AI agents find most useful.

4. **Contributors section** — Move `architecture.md`, `gc-implementation.md`, `gc-benchmarking.md`, and `deep-find-performance-baseline.md` here.

5. **Single-page cheat sheet** — A dense, one-page reference for both humans and agents.

---

## 7. Tooling Recommendation

### 7.1 Primary Recommendation: mdBook

**Switch from MkDocs to mdBook.** Rationale:

1. **Rust ecosystem alignment** — eucalypt is a Rust project; mdBook is a Rust tool. Contributors already have `cargo` installed, making `cargo install mdbook` trivial.

2. **Proven for language documentation** — The Rust Book, rustc dev guide, Cargo Book, and many other language docs use mdBook. It is designed for exactly this purpose.

3. **Custom syntax highlighting** — mdBook supports custom language definitions, enabling proper `eu` syntax highlighting in code blocks.

4. **Preprocessor system** — mdBook's preprocessor architecture allows inserting a build step that runs `eu lib/doc-gen.eu` to auto-generate the prelude reference from source.

5. **Built-in search** — Full-text search out of the box, much better than MkDocs readthedocs theme.

6. **Simpler deployment** — Static HTML output, trivially deployable to GitHub Pages. The current Python dependency (`requirements.txt`) is eliminated.

7. **Performance** — mdBook builds are extremely fast.

### 7.2 Migration Path

1. Create `book.toml` configuration
2. Create `src/SUMMARY.md` with the new structure
3. Move and reorganise existing `.md` files
4. Replace MkDocs admonition syntax (`!!! note`) with mdBook equivalents
5. Add custom `eu` syntax highlighting definition
6. Create mdBook preprocessor for prelude doc generation
7. Update `.github/workflows/docs.yaml` to use mdBook
8. Remove `mkdocs.yml`, `docs/requirements.txt`

### 7.3 Alternative: MkDocs Material (Upgrade Path)

If the migration to mdBook is too disruptive, upgrading to **MkDocs Material** theme would provide significant improvements with minimal structural change:
- Modern, responsive design with dark mode
- Built-in search with better indexing
- Code block copy buttons
- Tabs, admonitions, icons, annotation support
- Content tabs for showing the same concept in different formats

This would require only changing the theme in `mkdocs.yml` and adding `mkdocs-material` to `requirements.txt`.

---

## 8. FAQ Proposals

Based on analysis of language features, harness tests, and common pitfalls:

### Getting Started
1. **How do I install eucalypt?** — Homebrew, binary download, cargo install, install script
2. **How do I convert JSON to YAML (and vice versa)?** — `eu input.json -o output.yaml`
3. **Can I use eucalypt in a shell pipeline?** — Yes, with stdin/stdout and `-j` for JSON
4. **What file formats does eucalypt support?** — YAML, JSON, JSONL, TOML, EDN, XML, CSV, text

### Language
5. **How do I define a function?** — Named functions, anaphora, sections
6. **What is catenation / pipeline syntax?** — `value function` = `function(value)`
7. **How do anaphora work?** — Expression `_`, block `•`, string `{}`
8. **Why does my expression parse differently than expected?** — Operator precedence, lookup vs catenation
9. **How do I merge two blocks?** — Catenation, `merge`, `deep-merge`, `<<`
10. **How do I write a lambda / anonymous function?** — You don't (directly). Use anaphora or named functions.

### Data Processing
11. **How do I filter and transform a list of objects?** — `list filter(pred?) map(transform)`
12. **How do I look up nested values?** — `block.key.subkey` or `lookup-path([:key, :subkey], block)`
13. **How do I search for a key at any depth?** — `deep-find("key", data)` or `deep-query("**.key", data)`
14. **How do I sort data?** — `qsort(<)`, `sort-nums`, `sort-by(key-fn, <)`
15. **How do I work with dates and times?** — `cal.parse`, `cal.format`, `cal.fields`, ZDT literals `t"..."`

### Advanced
16. **What is the metadata system?** — Backtick annotations, `:target`, `:suppress`, `:doc`
17. **How do I import other files?** — `{ import: "file.eu" }` in metadata
18. **How do I write tests?** — `eu test file.eu`, RESULT key pattern
19. **How do random numbers work?** — `io.random` stream, `random-int`, `random-choice`, `shuffle`
20. **How do I use sets?** — `set.from-list`, `set.add`, `set.union`, etc.

---

## 9. Radical Ideas

### 9.1 Eucalypt Playground

Build an online playground (similar to the Rust Playground or Go Playground) where users can type eucalypt code and see the output immediately. This could be:
- A WASM compilation of `eu` running in the browser
- A server-side execution endpoint
- Even a simple JavaScript-based approximation for basic expressions

This would transform the documentation from passive reading to active experimentation.

### 9.2 "Eucalypt by Example" as Primary Entry Point

Rather than starting with syntax explanation, create a "Eucalypt by Example" section as the primary way to learn the language. Each example would show:
1. A real-world problem statement
2. The input data (JSON/YAML)
3. The eucalypt code
4. The output
5. A brief explanation of the key concepts used

This mirrors "Rust by Example" and "Go by Example" which are enormously popular.

### 9.3 Self-Documenting Documentation

Since eucalypt has a metadata system and `doc-gen.eu`, take this further:
- Make the prelude reference **entirely auto-generated** from source
- Add `example:` metadata to prelude functions and render them in the docs
- Add `see-also:` cross-references
- Run the examples as tests to ensure they stay correct
- Build this into the CI pipeline so docs are always in sync with source

### 9.4 Interactive Error Catalogue

Create a searchable catalogue of error messages, each with:
- The exact error text
- What causes it
- A minimal reproduction
- How to fix it

This would be incredibly valuable for both humans and AI agents.

### 9.5 "Rosetta Stone" Comparisons

Create comparison pages showing how common operations are expressed in eucalypt vs. other tools:
- Eucalypt vs. jq
- Eucalypt vs. yq
- Eucalypt vs. Jsonnet
- Eucalypt vs. Python (for data processing)
- Eucalypt vs. Dhall

This helps users from other ecosystems translate their knowledge. It also helps AI agents understand what eucalypt is and is not.

### 9.6 Documentation as a Test Suite

Every code example in the documentation should be executable and verified:
- Extract code blocks during build
- Run them through `eu` with assertions
- Fail the build if any example is broken

This ensures documentation never drifts from reality. mdBook supports this pattern through its `mdbook test` command (for Rust), and a custom preprocessor could do the same for eucalypt.

### 9.7 Contextual AI Agent Docs

Create a purpose-built `/agent-reference.md` file that is specifically designed for AI coding agents. Unlike human documentation which can afford progressive disclosure, agent documentation should be:
- Dense and comprehensive (agents have large context windows)
- Example-heavy (agents learn by pattern matching)
- Explicit about syntax (agents cannot guess eucalypt syntax from other languages)
- Structured with consistent formatting (agents parse structure)

This file would essentially be a "cheat sheet on steroids" — the complete knowledge an agent needs to write eucalypt code competently.

---

## 10. Prioritised Roadmap

### Phase 1: Quick Wins (1-2 days)

1. **Fix stale content** — Update version references, fix UK English inconsistencies, update CLI flag references
2. **Document missing features** — Add sections for sets, random numbers, ZDT literals, deep find/query, streaming imports, base64/SHA-256 to `prelude.md`
3. **Create `llms.txt`** — Simple markdown index of all documentation
4. **Expand `AGENTS.md`** — Add language quick reference section

### Phase 2: Restructure (3-5 days)

5. **Switch to mdBook** — Create `book.toml`, `SUMMARY.md`, migrate content, set up GitHub Actions
6. **Reorganise into Guide/Reference/Cookbook** — Separate tutorial from reference material
7. **Create auto-generated prelude reference** — Integrate `doc-gen.eu` into the build pipeline
8. **Add custom `eu` syntax highlighting** — Create language definition for mdBook

### Phase 3: Enrich (5-10 days)

9. **Write "Eucalypt by Example"** — 15-20 progressive, practical examples
10. **Write Cookbook recipes** — 5-7 real-world task-oriented guides
11. **Create FAQ** — The 20 proposed questions above
12. **Create error message catalogue** — Map common errors to solutions
13. **Create syntax cheat sheet** — Single-page dense reference

### Phase 4: Innovate (ongoing)

14. **Build WASM playground** — Online code execution
15. **Implement documentation testing** — Extract and verify all code examples
16. **Create `llms-full.txt`** — Complete docs in single file, auto-generated
17. **Write Rosetta Stone comparisons** — vs. jq, yq, Jsonnet
18. **Create agent-optimised reference** — Purpose-built AI agent documentation

---

## Sources and References

### Documentation Tools
- [mdBook Documentation](https://rust-lang.github.io/mdBook/)
- [mdBook on GitHub](https://github.com/rust-lang/mdBook)
- [Starlight (Astro)](https://starlight.astro.build/)
- [Starlight vs. Docusaurus](https://blog.logrocket.com/starlight-vs-docusaurus-building-documentation/)
- [Docusaurus Alternatives](https://alternativeto.net/software/docusaurus/)
- [10 Open-Source Documentation Frameworks](https://dev.to/silviaodwyer/10-open-source-documentation-frameworks-to-check-out-331f)

### AI Agent Documentation
- [The /llms.txt file specification](https://llmstxt.org/)
- [AGENTS.md specification](https://agents.md/)
- [llms.txt on GitBook](https://www.gitbook.com/blog/what-is-llms-txt)
- [Simplifying docs for AI with /llms.txt](https://www.mintlify.com/blog/simplifying-docs-with-llms-txt)
- [How to Create an llms-full.txt File](https://llms-txt.io/blog/how-to-create-llms-full-txt)
- [Improve your AI code output with AGENTS.md](https://www.builder.io/blog/agents-md)
- [Coding Guidelines for Your AI Agents (JetBrains)](https://blog.jetbrains.com/idea/2025/05/coding-guidelines-for-your-ai-agents/)
- [Context Management for AI Agents (DigitalOcean)](https://docs.digitalocean.com/products/gradient-ai-platform/concepts/context-management/)

### Programming Language Documentation
- [The Rust Programming Language (Book)](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Gleam Documentation](https://gleam.run/documentation/)

### Existing Eucalypt Files Referenced
- `/Users/greg/dev/curvelogic/eucalypt/mkdocs.yml` — Current docs configuration
- `/Users/greg/dev/curvelogic/eucalypt/docs/requirements.txt` — Python dependencies
- `/Users/greg/dev/curvelogic/eucalypt/.github/workflows/docs.yaml` — Docs deployment
- `/Users/greg/dev/curvelogic/eucalypt/lib/prelude.eu` — Standard library (~1100 lines)
- `/Users/greg/dev/curvelogic/eucalypt/lib/doc-gen.eu` — Documentation generator
- `/Users/greg/dev/curvelogic/eucalypt/docs/index.md` — Landing page
- `/Users/greg/dev/curvelogic/eucalypt/docs/prelude.md` — Prelude reference
- `/Users/greg/dev/curvelogic/eucalypt/docs/syntax.md` — Syntax reference
- `/Users/greg/dev/curvelogic/eucalypt/docs/command-line.md` — CLI reference
- `/Users/greg/dev/curvelogic/eucalypt/docs/architecture.md` — Architecture documentation
- `/Users/greg/dev/curvelogic/eucalypt/CLAUDE.md` — Agent instructions (project)
- `/Users/greg/dev/curvelogic/eucalypt/AGENTS.md` — Agent workflow instructions
- `/Users/greg/dev/curvelogic/eucalypt/harness/test/` — 79 harness tests covering all language features
