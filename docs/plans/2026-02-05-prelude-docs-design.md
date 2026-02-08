# Prelude Docs Generator — Design

## Problem

The prelude has ~176 documented items but no way to render them as
user-facing documentation. Users can't easily discover available functions.

## Solution

A general-purpose documentation generator written in eucalypt that extracts
metadata from any eucalypt code and renders it as Markdown.

## Key Properties

- **Eucalypt program** — "eat your own dog food", demonstrates capabilities
- **General-purpose** — works on any eucalypt code, not just prelude
- **Markdown output** — viewable anywhere, convertible to HTML later
- **Single file** — one markdown file with sections for namespaces
- **Structured metadata** — recognizes `doc:`, `export:`, `example:`, `see-also:`
- **Omit undocumented** — only documented items appear in output

## Usage

```bash
# Document the prelude
eu lib/doc-gen.eu target=lib/prelude.eu -f text > prelude-reference.md

# Document a user library
eu lib/doc-gen.eu target=my-library.eu -f text > my-library-docs.md
```

## Metadata Format

**Recognized metadata keys**:

| Key | Purpose | Example |
|-----|---------|---------|
| `doc:` | Description | `` ` { doc: "Map function over list" } `` |
| `export: :suppress` | Hide from docs | `` ` { export: :suppress } `` |
| `example:` | Usage example | `` ` { example: "[1,2,3] map(_ * 2)" } `` |
| `see-also:` | Related functions | `` ` { see-also: [:filter, :fold] } `` |

**Shorthand**: `` ` "description" `` is equivalent to `` ` { doc: "description" } ``

**Nested blocks**: Blocks within blocks become sections. `cal.parse` appears
under a "cal" heading.

## Output Format

```markdown
# Library Reference

## cal

Calendar functions

### cal.parse

Parse ISO8601 date string

**Example:**
```eu
cal.parse("2024-01-15")
```

**See also:** [cal.format](#calformat)

---

## str

String functions

### str.upper

...
```

**Rendering rules**:

- Top-level documented items get H2 headings
- Nested items (e.g., `cal.parse`) get H3 under their parent's H2
- Doc string becomes description paragraph
- `example:` renders as fenced code block
- `see-also:` renders as links (anchor format)
- Items sorted alphabetically within sections

## Implementation

**Location**: `lib/doc-gen.eu`

**Core functions**:

```eu
` { doc: "Generate markdown documentation for a block" }
generate-docs(target): ...

` { doc: "Extract documented items from a block" }
extract-docs(b): b elements filter(has-doc?) map(extract-item)

` { doc: "Check if item has documentation (not suppressed)" }
has-doc?(kv): kv second meta has(:doc) and(not(is-suppressed?(kv)))

` { doc: "Check if item is suppressed" }
is-suppressed?(kv): kv second meta lookup-or(:export, :public) eq(:suppress)

` { doc: "Render item as markdown" }
render-item(name, m): ...
```

**Intrinsics used**:

- `meta(x)` — get metadata block for value
- `elements(b)` — get key-value pairs from block
- `lookup-or` — safe metadata access

**Dependencies**: Only prelude functions — no new intrinsics needed.

## Testing

- Harness test with fixture file containing various metadata patterns
- Verify output contains expected sections and formatting
- Test nested blocks render correctly
- Test suppressed items are omitted

## Out of Scope

- Multiple output files (future enhancement)
- HTML generation (use mdbook or similar)
- Automatic signature extraction (would need type inference)
- Interactive examples (would need eval capability)
