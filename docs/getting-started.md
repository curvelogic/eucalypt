# Installation

The current implementation of eucalypt is available in the
[eucalypt project](https://github.com/curvelogic/eucalypt) and
can be installed as follows.

## On macOS via Homebrew

If you use homebrew, you can install using

```sh
brew install curvelogic/homebrew-tap/eucalypt
```

Otherwise binaries for macOS are available on the [releases
page](https://github.com/curvelogic/eucalypt/releases).

## On Linux

x86_64 binaries built in CI are available on the [releases
page](https://github.com/curvelogic/eucalypt/releases)

## On Windows

Sorry, haven't got there yet. But you could try installing from
source.

## From source

You will need a [rust](https://rust-lang.org) installation and *cargo*.

Build and install should be as simple as:

```sh
cargo install --path .
```

# Testing your installation

```sh
eu --version
```

...prints the version:

```shell
$ eu --version
eu 0.2.0
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
  help          Print this message or the help of the given subcommand(s)

Arguments:
  [FILES]...  Files to process (used when no subcommand specified)

Options:
  -L, --lib-path <LIB_PATH>  Add directory to lib path
  -Q, --no-prelude           Don't load the standard prelude
  -B, --batch                Batch mode (no .eucalypt.d)
  -d, --debug                Turn on debug features
  -S, --statistics           Print metrics to stderr before exiting
  -h, --help                 Print help
  -V, --version              Print version
```

Use `eu <command> --help` for detailed help on each subcommand.
