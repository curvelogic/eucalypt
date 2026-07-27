# Metadata

Metadata is a powerful mechanism for attaching auxiliary information to
any eucalypt expression. It is used for documentation, export control,
import declarations, operator definitions, and testing assertions.

## Attaching and Reading Metadata

## Metadata Basics

| Function | Description |
|----------|-------------|
| `with-meta` | Add metadata block `m` to expression `e` |
| `meta` | Retrieve expression metadata for `e`, combining every stacked metadata layer. Block layers merge, outer winning; a non-block layer is opaque and stands alone |
| `raw-meta` | Retrieve the outermost metadata layer of e alone, without recursing into inner layers and so without merging |
| `merge-meta(m, e)` | Merge block `m` into `e`'s metadata |

## Documentation Metadata

The backtick (`` ` ``) before a declaration attaches metadata. When the
value is a string, it sets the `doc` key:

```eu
` "Add two numbers together"
add(a, b): a + b
```

This is equivalent to:

```eu
` { doc: "Add two numbers together" }
add(a, b): a + b
```

For richer metadata, use a block:

```eu
` { doc: "Infix addition operator"
    precedence: :sum
    associates: :left }
(a + b): __ADD(a, b)
```

### Reading Metadata Back

A value can carry several stacked metadata layers — declaration
metadata over a `//` layer, or one `//` over another. `meta` combines
them all; `raw-meta` reports only the outermost layer.

```eu
` { doc: "documented" }
counter: 7 // { unit: :seconds }

combined: counter meta          # { unit: :seconds, doc: "documented" }
outermost: counter raw-meta     # { doc: "documented" }

RESULT: (combined.unit = :seconds) ∧ (outermost.doc = "documented")
```

Two block layers merge, with the outer (later applied) layer winning on
key conflicts. A layer that is **not** a block is *opaque*: it cannot be
merged, so the outermost layer stands as the answer on its own. This is
what the leading-symbol block shorthand produces — the tag is attached
as a bare symbol, not a block:

```eu
spec: { :only k: 1 }

tag: spec meta          # :only — the symbol itself, not a block
raw-tag: spec raw-meta  # :only

RESULT: (tag = :only) ∧ (raw-tag = :only)
```

Note that the built-in monadic tags (`:io`, `:for`, `:let`, `:random`,
`:state`) never reach the runtime this way — the desugarer consumes
them and rewrites the block into a bind chain, so the resulting value
has no metadata at all.

### Common Metadata Keys

| Key | Purpose |
|-----|---------|
| `doc` | Documentation string |
| `import` | Import specification |
| `target` | Export target name |
| `export` | Export control (`:suppress` to hide) |
| `precedence` | Operator precedence level |
| `associates` | Operator associativity (`:left`, `:right`) |
| `parse-embed` | Embedded representation format |
