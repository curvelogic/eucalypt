# Metadata

Metadata is a powerful mechanism for attaching auxiliary information to
any eucalypt expression. It is used for documentation, export control,
import declarations, operator definitions, and testing assertions.

## Attaching and Reading Metadata

## Metadata Basics

| Function | Description |
|----------|-------------|
| `with-meta` | Add metadata block `m` to expression `e` |
| `meta` | Retrieve expression metadata for e |
| `raw-meta` | Retrieve immediate metadata of e without recursing into inner layers |
| `merge-meta(m, e)` | Merge block `m` into `e`'s metadata |
| `validator(v)` | Find the validator for a value `v` in its metadata |
| `check(v)` | True if v is valid according to assert metadata |
| `checked(v)` | Panic if value doesn't satisfy its validator |

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
