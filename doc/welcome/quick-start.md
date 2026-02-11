# Quick Start

## Installation

### On macOS via Homebrew

If you use Homebrew, you can install using:

```sh
brew install curvelogic/homebrew-tap/eucalypt
```

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

...prints the version:

```text
eu 0.3.0
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
  help          Print this message or the help of the given subcommand(s)

Arguments:
  [FILES]...  Files to process (used when no subcommand specified)

Options:
  -L, --lib-path <LIB_PATH>                Add directory to lib path
  -Q, --no-prelude                         Don't load the standard prelude
  -B, --batch                              Batch mode (no .eucalypt.d)
  -d, --debug                              Turn on debug features
  -S, --statistics                         Print metrics to stderr before exiting
      --statistics-file <STATISTICS_FILE>  Write statistics as JSON to a file
  -h, --help                               Print help
  -V, --version                            Print version
```

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
