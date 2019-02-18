# Installation

The current implementation of eucalypt is available in the
[eucalypt project](https://github.com/curvelogic/eucalypt) and
can be installed as follows.

## On macOS via Homebrew

If you use homebrew, you can install using

```sh
brew install curvelogic/homebrew-tap/eucalypt
```

## On Linux

x86_64 binaries built in CI are available on the [releases
page](https://github.com/curvelogic/eucalypt/releases)

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

...prints the version.

```sh
eu --help
```

...shows command line help.
