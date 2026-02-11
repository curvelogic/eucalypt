# Metadata

Metadata is a powerful mechanism for attaching auxiliary information to
any eucalypt expression. It is used for documentation, export control,
import declarations, operator definitions, and testing assertions.

## Attaching and Reading Metadata

| Function | Description |
|----------|-------------|
| `with-meta(m, e)` | Add metadata block `m` to expression `e` |
| `e // m` | Operator form of `with-meta` |
| `meta(e)` | Retrieve metadata from expression |
| `raw-meta(e)` | Retrieve immediate metadata without recursing into inner layers |
| `merge-meta(m, e)` | Merge into existing metadata |
| `e //<< m` | Operator form of `merge-meta` |

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

## Assertions

| Operator | Description |
|----------|-------------|
| `e //= v` | Check if `e` equals `v`, return boolean |
| `e //=> v` | Assert `e` equals `v`, return `e` or panic |
| `e //=? f` | Assert `e` satisfies predicate `f` |
| `e //!? f` | Assert `e` does not satisfy `f` |
| `e //!` | Assert `e` is true |
| `e //!!` | Assert `e` is false |

### Assertion Helpers

| Function | Description |
|----------|-------------|
| `assertions.validator(v)` | Find the validator for value `v` in its metadata |
| `assertions.check(v)` | True if `v` is valid according to its `assert` metadata |
| `assertions.checked(v)` | Panic if value does not satisfy its validator, else return `v` |
