# Installation

The current implementation of eucalypt is available in the
[eucalypt project](https://github.com/curvelogic/eucalypt) and
can be installed as follows.

## On macOS via Homebrew

If you use homebrew, you can install using

```sh
brew install curvelogic/homebrew-tap/eucalypt
```

Otherwise binaries for macOS x86_64 are available on the [releases
page](https://github.com/curvelogic/eucalypt/releases).

No binaries are available for Apple Silicon yet.

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
eu - Eucalypt (Rust Impl: v0.2.0.0)
```

...and...

```sh
eu --help
```

...shows command line help:

```text
reu 0.2.0
Option to select the command actually run

USAGE:
	eu [FLAGS] [OPTIONS] [--] [inputs]...

FLAGS:
	-d, --debug             Turn on debug features
		--dump-cooked       Dump core expression once operator soup has been analysed for precedence
		--dump-desugared    Dump core expression as initially translated from syntax tree
		--dump-inlined      Dump core expression once inliner pass has run
		--dump-pruned       Dump core expression once dead ocde has been eliminated
		--dump-runtime      Dump code for runtime globals
		--dump-stg          Dump compiled STG syntax
	-n, --explain           Explain command (do not run)
	-h, --help              Prints help information
	-j                      Shortcut for `-x json``
	-l, --list-targets      List targets defined in the source
	-B, --batch             Batch mode (no .eucalypt.d)
	-Q, --no-prelude        Don't load the standard prelude
	-p, --parse             Parse only
		--quote-debug       When outputing AST or Core expressions, quote as debug print of structure
		--quote-embed       When outputing AST or Core expressions, quote-embed as eucalypt
	-S, --statistics        Print metrics to stderr before exiting
	-T, --test              Run file as test
	-v, --version           Explain command (do not run)

OPTIONS:
	-e, --evaluate <evaluate>          Expression to evaluate
	-x, --export-type <export-type>    Format to export output in (e.g. yaml, json, toml, text)
	-L, --lib-path <lib-path>...       Add directory to lib path
	-o <output>                        Output file to export to
	-t, --target <target>              Target to run (identified by target metadata in eucalypt source)

ARGS:
	<inputs>...    Source code / data inputs (in order)
```
