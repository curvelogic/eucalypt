# The Command Line

In this chapter you will learn:

- The `eu` command structure and subcommands
- How to specify inputs, outputs, and evaluands
- How to use targets, arguments, and environment variables
- How to use the formatter and other tools

## Command Structure

```sh
eu [GLOBAL_OPTIONS] [SUBCOMMAND] [SUBCOMMAND_OPTIONS] [FILES...]
```

When no subcommand is given, `run` is assumed:

```sh
eu file.eu          # same as: eu run file.eu
```

### Subcommands

| Command | Description |
|---------|-------------|
| `run` | Evaluate and render (default) |
| `test` | Run embedded tests |
| `dump` | Dump intermediate representations |
| `version` | Show version information |
| `explain` | Show what would be executed |
| `list-targets` | List export targets |
| `fmt` | Format source files |
| `lsp` | Start the Language Server Protocol server |

## Inputs

### File Inputs

Specify one or more files to process:

```sh
eu data.yaml transform.eu
```

Inputs are merged left to right. Names from earlier inputs are
available to later ones. The final input determines what is rendered.

### stdin

Use `-` to read from stdin, or simply pipe data when no files are
specified:

```sh
curl -s https://api.example.com/data | eu -e 'items count'
```

### Format Override

Override the assumed format with a `format@` prefix:

```sh
eu yaml@data.txt json@-
```

### Named Inputs

Prefix with `name=` to make the input available under a name:

```sh
eu config=settings.yaml app.eu
```

In `app.eu`, the YAML content is available as `config`:

```eu
port: config.port
```

### Collecting Inputs

Gather multiple files into a named collection:

```sh
eu -c data *.json -e 'data map(_.name)'
```

Add `-N` to key by filename:

```sh
eu -c data -N *.json
```

## Outputs

### Format

Output defaults to YAML. Common options:

```sh
eu file.eu            # YAML (default)
eu file.eu -j         # JSON (shortcut)
eu file.eu -x json    # JSON (explicit)
eu file.eu -x toml    # TOML
eu file.eu -x text    # Plain text
```

### Output File

Write to a file (format inferred from extension):

```sh
eu data.eu -o output.json
```

## Evaluands

The `-e` flag specifies an expression to evaluate:

```sh
eu -e '2 + 2'
```

```yaml
4
```

When combined with file inputs, the evaluand has access to all loaded
names:

```sh
eu data.yaml -e 'users filter(_.active) count'
```

Multiple `-e` flags are allowed; the last one determines the output.

### Quick Expressions

Use `-e` for quick data exploration:

```sh
# Inspect a value
eu config.yaml -e 'database'

# Count items
eu data.json -e 'items count'

# Extract and transform
eu data.json -e 'items map(_.name) reverse'
```

## Targets

Declarations annotated with `:target` metadata can be selected for
rendering:

```eu
# multi-output.eu
` { target: :summary }
summary: { count: items count }

` { target: :detail }
detail: items
```

List available targets:

```sh
eu list-targets multi-output.eu
```

Select a target:

```sh
eu -t summary multi-output.eu
```

A target named `main` is rendered by default. If no `:main` target
exists, the entire unit is the target.

## Passing Arguments

Arguments after `--` are available via `io.args`:

```sh
eu -e 'io.args' -- hello world
```

```yaml
- hello
- world
```

Use in scripts:

```eu
# greet.eu
name: io.args head-or("World")
greeting: "Hello, {name}!"
```

```sh
eu greet.eu -e greeting -- Alice
```

```yaml
Hello, Alice!
```

Arguments are strings. Use `num` to convert:

```eu
numbers: io.args map(num)
total: numbers foldl(+, 0)
```

## Environment Variables

Access environment variables through `io.env`:

```eu
home: io.env lookup-or(:HOME, "/tmp")
path: io.env lookup(:PATH)
```

## Random Seed

By default, random numbers use system entropy. Use `--seed` for
reproducible output:

```sh
eu --seed 42 template.eu
```

## The Formatter

Format eucalypt source files:

```sh
eu fmt file.eu              # print formatted to stdout
eu fmt --write file.eu      # format in place
eu fmt --check file.eu      # check (exit 1 if unformatted)
eu fmt --reformat file.eu   # full reformatting
```

Options:
- `-w, --width <N>` -- line width (default: 80)
- `--indent <N>` -- indent size (default: 2)

## Debugging

### Dumping Intermediate Representations

```sh
eu dump ast file.eu         # syntax tree
eu dump desugared file.eu   # core expression
eu dump stg file.eu         # compiled STG
```

### Explaining Execution

```sh
eu explain file.eu          # show what would be executed
```

### Statistics

```sh
eu -S file.eu               # print metrics to stderr
```

## Batch Mode

Use `-B` for repeatable builds (disables ergonomic mode and
`~/.eucalypt`):

```sh
eu -B file.eu
```

## Suppressing the Prelude

The standard prelude is loaded automatically. Suppress it with `-Q`:

```sh
eu -Q file.eu
```

> **Warning:** Without the prelude, even `true`, `false`, `if`, and
> basic operators are unavailable.

## Version Assertions

Ensure a minimum `eu` version in source files:

```eu
_ : eu.requires(">=0.3.0")
```

Check the current version:

```sh
eu version
```

## LSP Server

Start a Language Server Protocol server for editor integration:

```sh
eu lsp
```

Provides syntax error diagnostics and formatting support.

## Key Concepts

- `eu` defaults to `run` when no subcommand is given
- Inputs are merged left to right; the final input determines output
- Named inputs (`name=file`) provide namespace isolation
- `-e` evaluates an expression against loaded inputs
- `-t` selects a named target for rendering
- `--` passes arguments available via `io.args`
- `eu fmt` formats source files; `eu test` runs tests; `eu lsp`
  starts the language server
