# eucalypt

[![eucalypt build](https://github.com/curvelogic/eucalypt/actions/workflows/build-rust.yaml/badge.svg)](https://github.com/curvelogic/eucalypt/actions/workflows/build-rust.yaml)

[Eucalypt](https://curvelogic.github.io/eucalypt/) is a tool and a
small functional language for generating, templating, rendering and
processing structured data formats like YAML, JSON and TOML.

## Quick Example

```eu
target-zones: ["a", "b", "c"] map("eu-west-1{}")
```

Run with `eu example.eu` to produce:

```yaml
target-zones:
  - eu-west-1a
  - eu-west-1b
  - eu-west-1c
```

## Features

- Concise native syntax for defining data, functions, and operators
- YAML embedding with `!eu` tags for in-place data manipulation
- Powerful block (object) manipulation facilities
- String interpolation and regular expressions
- Ergonomic command line interface with environment variable access
- Standard prelude of built-in functions

## Installation

### macOS (Homebrew)

```sh
brew install curvelogic/homebrew-tap/eucalypt
```

### From Source

```sh
cargo install --path .
```

See the [documentation](https://curvelogic.github.io/eucalypt/) for
other installation options.

## Documentation

Full documentation is available at https://curvelogic.github.io/eucalypt/

- [Getting Started](https://curvelogic.github.io/eucalypt/getting-started/)
- [Language Syntax](https://curvelogic.github.io/eucalypt/syntax/)
- [Command Line](https://curvelogic.github.io/eucalypt/command-line/)
- [Prelude Reference](docs/prelude.md)

## Development

Eucalypt is implemented in Rust (since v0.2) and builds with cargo:

```sh
cargo build
cargo test
```

Install a local `eu` binary:

```sh
cargo install --path .
```

## License

See [LICENSE](LICENSE) for details.
