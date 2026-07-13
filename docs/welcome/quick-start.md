# Quick Start

## Installation

### On macOS via Homebrew

If you use Homebrew, you can install using:

```sh
brew install curvelogic/homebrew-tap/eucalypt
```

### Linux / macOS (install script)

Alternatively, install the latest release binary directly:

```sh
curl -sSf https://raw.githubusercontent.com/curvelogic/eucalypt/master/install.sh | sh
```

This installs to `~/.local/bin`. Set `EUCALYPT_INSTALL_DIR` to
override the install location.

Otherwise binaries for macOS are available on the [releases
page](https://github.com/curvelogic/eucalypt/releases).

### On Linux

x86_64 and aarch64 binaries built in CI are available on the [releases
page](https://github.com/curvelogic/eucalypt/releases).

### On Windows

Sorry, haven't got there yet. But you could try installing from
source.

### From source

You will need a [Rust](https://rust-lang.org) installation and *cargo*.

Build and install should be as simple as:

```sh
cargo install --path .
```

## Testing your installation

```sh
eu --version
```

...prints the version, e.g.:

```text
eu - Eucalypt v0.12.1
```

...and...

```sh
eu --help
```

...shows command line help:

```text
A functional language for structured data

Usage: eu [OPTIONS] [FILES]... [COMMAND]

Commands:
  run           Evaluate eucalypt code (default)
  test          Run tests
  dump          Dump intermediate representations
  version       Show version information
  explain       Explain what would be executed
  list-targets  List targets defined in the source
  fmt           Format eucalypt source files
  lsp           Start the Language Server Protocol server
  check         Check type annotations in eucalypt source files
  doc           Extract documentation from eucalypt source files
  help          Print this message or the help of the given subcommand(s)

Arguments:
  [FILES]...  Files to process (used when no subcommand specified)

Options:
      --source-prelude  Force source-prelude pipeline even when pre-compiled blob is available
  -h, --help            Print help
  -V, --version         Print version
```

Most of the options familiar from older versions (`-L`, `-Q`, `-d`,
`-S`, `--statistics-file`, and many more) now live under the `run`
subcommand — see `eu run --help` or the [CLI
Reference](../reference/cli.md) for the full list.

Use `eu <command> --help` for detailed help on each subcommand.

## Your first program

Create a file called `hello.eu`:

```eu
greeting: "Hello, World!"
```

Run it:

```shell
eu hello.eu
```

Output:

```yaml
greeting: Hello, World!
```

Try JSON output:

```shell
eu hello.eu -j
```

```json
{"greeting": "Hello, World!"}
```

## Next steps

- Read the [lightning tour](index.md#a-lightning-tour) for a quick
  taste of what eucalypt can do
- Work through [The Eucalypt Guide](../guide/blocks-and-declarations.md)
  for a progressive tutorial
- Browse [Eucalypt by Example](by-example.md) for worked examples
