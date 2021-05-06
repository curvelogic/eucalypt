# eucalypt

[eucalypt](https://curvelogic.github.io/eucalypt/) is a tool and a
small language for generating, templating, rendering and processing
structured data formats like YAML, JSON and TOML.

# Getting started

See the [documentation](https://curvelogic.github.io/eucalypt/).

# Development

## 0.2 Versions

Since v0.2 eucalypt has been implemented in Rust and builds with cargo.

```
cargo test
```

A local `eu` binary can be installed with:

```
cargo install --path .
```

## 0.1 Versions

Versions prior to v0.2 were implemented in Haskell. You need stack
(`brew install haskell-stack`...)

Then for development, run something like:

```
stack install hlint
stack build --test --file-watch --fast --copy-bins --exec "hlint ."
```

To build and install the `eu` binary:

```
stack install
```
