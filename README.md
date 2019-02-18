# eucalypt-hs

[![CircleCI](https://circleci.com/gh/curvelogic/eucalypt-hs.svg?style=svg&circle-token=97ae77777028be6a88a53b23b78d5c858a49ef33)](https://circleci.com/gh/curvelogic/eucalypt-hs)

[eucalypt](https://curvelogic.github.io/eucalypt/) is a tool and a
small language for generating, templating, rendering and processing
structured data formats like YAML, JSON and TOML.

# Getting started

See the [documentation](https://curvelogic.github.io/eucalypt/).

# Development

You need stack (`brew install haskell-stack`...)

For development, run something like:

```
stack install hlint
stack build --test --file-watch --fast --copy-bins --exec "hlint ."
```

To build and install the `eu` binary:

```
stack install
```
