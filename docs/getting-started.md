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

You will need [stack](https://docs.haskellstack.org/en/stable/README/)
(which can be found in Homebrew as `haskell-stack`).

Build and install should be as simple as:

```sh
stack install
```

# Testing your installation

```sh
eu --version
```

...prints the version:

```shell
$ eu --version
eu - Eucalypt (Haskell Impl: v0.1.1.1610)
```

...and...

```sh
eu --help
```

...shows command line help:

```text
eu - command line interface to Eucalypt

Usage: eu [-B|--batch] ([-x|--exportType FORMAT] | [-j|--json])
		  [-t|--target TARGET] [-o|--output FILENAME] [-e|--evaluate EXPRESSION]
		  [-Q|--no-prelude] ([-n|--explain] | [-v|--version] |
		  [-l|--list-targets] | [--dump-desugared] | [--dump-eval-subbed] |
		  [--dump-cooked] | [--dump-core] | [--dump-stg] | [-H|--headless] |
		  [-p|--parse]) [INPUTS...] [-d|--debug] [-L|--lib-path ARG]
  Run eucalypt transformations

Available options:
  -B,--batch               Batch (i.e. non-ergonomic) mode
  -x,--exportType FORMAT   Format for export (e.g. yaml, json)
  -j,--json                JSON output (equivalent to -x json)
  -t,--target TARGET       Target name to export (must be defined in the source)
  -o,--output FILENAME     Output file or directory to export to
  -e,--evaluate EXPRESSION Expression to evaluate and render
  -Q,--no-prelude          Don't include standard prelude
  -n,--explain             Explain command line interpretation and exit
  -v,--version             Show version information and exit
  -l,--list-targets        List declared targets
  --dump-desugared         Dump core syntax after desugar and merge
  --dump-eval-subbed       Dump core syntax after evaluand substituted in
  --dump-cooked            Dump core syntax after operator fixities have been
						   resolved
  --dump-core              Dump final core syntax prior to evaluation
  --dump-stg               Dump STG syntax prior to evaluation
  -H,--headless            Run evaluation without a render (for timing)
  -p,--parse               Parse program text and output AST - do not evaluate
  -d,--debug               Switch on debugging features
  -L,--lib-path ARG        Add a directory at the front of library search path
  -h,--help                Show this help text
```
