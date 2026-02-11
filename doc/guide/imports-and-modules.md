# Imports and Modules

In this chapter you will learn:

- How to import other eucalypt files and data files
- How to scope imports to specific declarations
- How to use named imports for namespacing
- How git imports work for external dependencies

## Basic Imports

Import another eucalypt file using the `import` key in declaration
metadata:

```eu
{ import: "helpers.eu" }

# Names from helpers.eu are now available
result: helper-function(42)
```

When the metadata is at unit level (the first item in the file), the
imported names are available throughout the entire file.

## Scoped Imports

Imports can be scoped to a specific declaration, limiting where the
imported names are visible:

```eu
` { import: "math.eu" }
calculations: {
  # Names from math.eu are available only within this block
  result: advanced-calculation(10)
}

# math.eu names are NOT available here
```

## Named Imports

Give an import a name to access its contents under a namespace:

```eu
{ import: "cfg=config.eu" }

host: cfg.host
port: cfg.port
```

This is especially useful when importing data files that might contain
names which clash with your own:

```eu
{ import: "prod=production.yaml" }

url: "https://{prod.host}:{prod.port}"
```

## Importing Multiple Files

Supply a list to import several files at once:

```eu
{ import: ["helpers.eu", "config.eu"] }

result: helper(config-value)
```

Named and unnamed imports can be mixed:

```eu
{ import: ["helpers.eu", "cfg=config.eu"] }

result: helper(cfg.port)
```

## Importing Data Files

Eucalypt can import files in any supported format. The format is
inferred from the file extension:

```eu
{ import: "data=records.yaml" }

first-record: data head
```

You can override the format when the extension is misleading:

```eu
{ import: "data=yaml@records.txt" }
```

### Formats That Return Lists

Some formats (CSV, JSON Lines, text) produce lists rather than blocks.
These **must** be given a name:

```eu
{ import: "rows=transactions.csv" }

total: rows map(_.amount num) foldl(+, 0)
```

## Nested Imports

Imports can be placed at any level of nesting:

```eu
deep: {
  nested: {
    ` { import: "local-config.eu" }
    config: {
      value: local-setting
    }
  }
}
```

## Git Imports

Import eucalypt code directly from a git repository. This is useful
for sharing libraries without manually managing local copies:

```eu
{ import: { git: "https://github.com/user/eu-lib"
            commit: "abc123def456"
            import: "lib/helpers.eu" } }

result: lib-function(42)
```

The `commit` field is mandatory and should be a full SHA. This ensures
the import is repeatable and cacheable.

Multiple git imports can be listed alongside simple imports:

```eu
{ import: [
  "local.eu",
  { git: "https://github.com/user/lib"
    commit: "abc123"
    import: "helpers.eu" }
] }
```

## Streaming Imports

For large files, streaming imports read data lazily:

```eu
{ import: "events=jsonl-stream@events.jsonl" }

recent: events take(100)
```

Available streaming formats:

| Format | Description |
|--------|-------------|
| `jsonl-stream` | JSON Lines (one object per line) |
| `csv-stream` | CSV with headers |
| `text-stream` | Plain text (one string per line) |

## Combining Imports with the Command Line

Imports in `.eu` files complement the command line input system. You
can use both together:

```sh
eu data.yaml transform.eu
```

Here `data.yaml` is a command-line input and `transform.eu` can also
have its own `{ import: ... }` declarations for helpers or
configuration.

See [The Command Line](command-line.md) for details on the input
system.

## Practical Example: Configuration Layering

```eu
# base.eu
defaults: {
  host: "0.0.0.0"
  port: 8080
  workers: 4
}
```

```eu
# deploy.eu
{ import: "base.eu" }

production: defaults << {
  workers: 16
  host: "prod.example.com"
}

staging: defaults << {
  host: "staging.example.com"
}
```

Running `eu deploy.eu` produces layered configuration with shared
defaults.

## Key Concepts

- Use `{ import: "file.eu" }` in metadata to import files
- **Named imports** (`"name=file"`) provide namespace isolation
- Imports can be **scoped** to individual declarations
- **Data files** (YAML, JSON, CSV, etc.) can be imported like code
- **Git imports** pull code directly from repositories at a specific
  commit
- **Streaming imports** (`jsonl-stream@`, `csv-stream@`,
  `text-stream@`) handle large files lazily
