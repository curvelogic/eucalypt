# Imports and Modules

Eucalypt units can import other units and data files. This chapter
covers the import system and how to organise code across files.

## Basic imports

Use the `import` key in metadata to bring names from another file
into scope:

```eu,notest
{ import: "helpers.eu" }

result: helper-function(42)
```

The names defined in `helpers.eu` become available in the current
unit.

## Named imports

Give an import a name to access its contents under a namespace:

```eu,notest
{ import: "cfg=config.eu" }

host: cfg.host
port: cfg.port
```

This avoids name collisions when importing multiple files.

## Multiple imports

Import several files at once with a list:

```eu,notest
{ import: ["helpers.eu", "cfg=config.eu"] }

result: helper-function(cfg.value)
```

## Scoped imports

Imports in unit-level metadata are available throughout the file.
Imports in declaration metadata are scoped to that declaration:

```eu,notest
` { import: "math.eu" }
result: {
  area: pi * r * r
}
```

Names from `math.eu` are only visible inside `result`.

## Importing data files

You can import any format eucalypt supports. The format is inferred
from the file extension:

```eu,notest
{ import: "data=people.yaml" }

names: data map(.name)
```

Supported formats include YAML, JSON, TOML, CSV, EDN, XML, and
plain text.

## Format override

When the file extension does not match the content, specify the
format explicitly:

```eu,notest
{ import: "yaml@raw-data.txt" }
```

The format goes before the `@` sign, followed by the file path.

## Named data imports

Formats that produce a list (CSV, text, JSON Lines) require a name:

```eu,notest
{ import: "txns=transactions.csv" }

total: txns map(.amount) foldl(+, 0)
```

## Git imports

Import eucalypt directly from a git repository at a specific commit:

```eu,notest
{ import: { git: "https://github.com/user/repo"
            commit: "abc123def456..."
            import: "lib/helpers.eu" } }
```

All three keys (`git`, `commit`, `import`) are required. The `commit`
should be a full SHA for reproducibility.

## Streaming imports

For large files, use streaming formats that read data lazily:

```eu,notest
{ import: "events=jsonl-stream@events.jsonl" }

recent: events take(100)
```

Streaming formats:

| Format         | Description                                |
|----------------|--------------------------------------------|
| `jsonl-stream` | JSON Lines (one JSON object per line)      |
| `csv-stream`   | CSV with headers (rows become blocks)      |
| `text-stream`  | Plain text (lines become strings)          |

Streaming imports always require a name binding.

## Command-line inputs as imports

The same input syntax works on the command line:

```sh
eu -e 'data take(5)' data=people.csv
eu -e 'cfg.host' cfg=config.yaml
eu -e 'text count' text=text-stream@-
```

Imports in source files and command-line inputs share the same
mechanisms. See [The Command Line](command-line.md) for more on
command-line usage.

## YAML-specific features

When importing YAML files, eucalypt supports:

- **Anchors and aliases** -- `&name` defines, `*name` references
- **Merge keys** -- `<<: *base` merges mappings
- **Timestamps** -- unquoted dates are converted to ZDT values

See [YAML Embedding](yaml-embedding.md) for more on YAML integration.

## Practical example

A project with configuration layering:

```eu,notest
{ import: ["base=config/base.yaml", "env=config/prod.yaml"] }

config: base << env
db-url: "postgres://{config.db.host}:{config.db.port}/{config.db.name}"
```

Deep merge (`<<`) combines the base and environment configs, with the
environment overriding where they overlap.

## Next steps

- [Working with Data](working-with-data.md) -- end-to-end data
  processing pipelines
- [The Command Line](command-line.md) -- running eucalypt from the
  shell
