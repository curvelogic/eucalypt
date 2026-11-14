# Export Formats

*Detailed export format documentation is under construction.*

Eucalypt can export to the following formats:

| Format | Flag | Notes |
|--------|------|-------|
| YAML | (default) | Default output format |
| JSON | `-j` or `-x json` | Compact JSON output |
| TOML | `-x toml` | TOML output |
| EDN | `-x edn` | EDN output |
| Text | `-x text` | Plain text output |
| HTML | `-x html` | HTML output |
| Eucalypt | `-x eu` | Eucalypt-syntax output; preserves symbols as `:name` |

The output format can also be inferred from the output file extension
when using `-o`:

```sh
eu input.eu -o output.json  # infers JSON format
eu input.eu -o output.toml  # infers TOML format
```

## Dicts

A [dict](../guide/dicts.md) renders in every format as a mapping of its
entries in key order, exactly as the block `dict.to-block` would give: a
symbol key renders as its name, a string key as its text, and a number key
as its rendered text (`1`, `2.5`). Values are evaluated and rendered as
block values are. Two cases are errors, since a mapping key in these formats
is a single string-like scalar and appears once:

- a list key (`[x, y]`), and
- two keys with the same text, such as `1` and `"1"`, or `"x"` and `:x`.

Render `d dict.to-list` instead to output such a dict as `[key, value]`
pairs.
