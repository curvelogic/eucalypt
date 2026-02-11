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

The output format can also be inferred from the output file extension
when using `-o`:

```sh
eu input.eu -o output.json  # infers JSON format
eu input.eu -o output.toml  # infers TOML format
```
