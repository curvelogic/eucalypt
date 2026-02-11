# The Command Line

The `eu` command is the main interface to eucalypt. This chapter
covers its most useful options and patterns.

## Basic usage

Run a eucalypt file:

```sh
eu file.eu
```

Output defaults to YAML. Use `-j` for JSON or `-t` for TOML:

```sh
eu file.eu -j
eu file.eu -t
```

## Inline expressions

Use `-e` to evaluate an expression:

```sh
eu -e '{greeting: "hello"}'
eu -e '2 + 2'
```

Combine with input files to query data:

```sh
eu config.yaml -e 'db.host'
```

## Multiple inputs

When multiple inputs are given, they are merged. The final input
determines what is rendered:

```sh
eu base.yaml overrides.yaml
```

Names from earlier inputs are available in later ones:

```sh
eu data.yaml logic.eu
```

## Named inputs

Give an input a name to access its content under that name:

```sh
eu data=people.csv -e 'data map(.name)'
```

This is required for formats that produce lists (CSV, text, JSON
Lines).

## Format override

Override the inferred format with a `format@` prefix:

```sh
eu yaml@data.txt
eu json@-
```

The full input syntax is `[name=][format@]path`.

## Reading from stdin

Use `-` for stdin, or just pipe into `eu` with no arguments:

```sh
curl -s https://api.example.com/data | eu -j
echo '{"x": 1}' | eu -e 'x + 1'
```

## Passing arguments

Arguments after `--` are available via `io.args`:

```sh
eu script.eu -e 'result' -- arg1 arg2
```

In the eucalypt source:

```eu,notest
name: io.args head-or("default")
```

## Render targets

Target metadata controls which declarations are rendered:

```eu,notest
` :target
summary: len(data)

data: [1, 2, 3]
```

Select a target with `-t`:

```sh
eu file.eu -t summary
```

List available targets:

```sh
eu list-targets file.eu
```

A target named `main` is used by default when present.

## Text output

Use `-x text` or `-T` for plain text output:

```sh
eu -T -e '"hello, world"'
```

Text output renders strings without quotes.

## Collecting inputs

Aggregate many files into a single list with `--collect-as` / `-c`:

```sh
eu -c inputs *.yaml -e 'inputs map(.name)'
```

Add `--name-inputs` / `-N` for a block keyed by filename:

```sh
eu -c inputs -N *.yaml
```

## Random seed

For reproducible output involving random functions:

```sh
eu --seed 42 template.eu
```

## Suppressing the prelude

The standard prelude is loaded by default. Suppress it with `-Q`:

```sh
eu -Q file.eu
```

Warning: most basic functions (including `if`, `true`, `false`) come
from the prelude.

## Formatting source files

```sh
eu fmt file.eu              # print formatted to stdout
eu fmt --write file.eu      # format in place
eu fmt --check file.eu      # check formatting (exit 1 if not)
```

Options: `-w` for line width, `--indent` for indent size,
`--reformat` for full reformatting.

## Debugging

The `dump` subcommand shows internal representations:

```sh
eu dump ast file.eu         # syntax tree
eu dump desugared file.eu   # core expression
eu dump stg file.eu         # compiled STG
```

## LSP server

Start the language server for editor integration:

```sh
eu lsp
```

Provides syntax diagnostics and formatting.

## Subcommand reference

| Subcommand     | Description                        |
|----------------|------------------------------------|
| `run` (default)| Evaluate eucalypt code             |
| `test`         | Run embedded tests                 |
| `dump`         | Dump intermediate representations  |
| `fmt`          | Format source files                |
| `lsp`          | Start language server              |
| `list-targets` | List render targets                |
| `explain`      | Explain what would be executed     |
| `version`      | Show version information           |

## Quick reference

```sh
eu file.eu                  # run, output YAML
eu file.eu -j               # output JSON
eu -e 'expression'          # evaluate inline
eu a.yaml b.eu              # merge inputs
eu data=file.csv -e 'data'  # named input
eu -c all *.yaml            # collect inputs
eu --seed 42 file.eu        # reproducible random
eu fmt --write file.eu      # format in place
eu test file.eu             # run tests
```

## Next steps

- [YAML Embedding](yaml-embedding.md) -- integrating eucalypt with
  YAML
- [Testing](testing.md) -- writing and running tests
- [Advanced Topics](advanced-topics.md) -- metadata, sets, and more
