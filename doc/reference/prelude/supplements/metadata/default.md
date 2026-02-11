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
