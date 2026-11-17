# eucalypt

[![eucalypt build](https://github.com/curvelogic/eucalypt/actions/workflows/build-rust.yaml/badge.svg)](https://github.com/curvelogic/eucalypt/actions/workflows/build-rust.yaml)

[Eucalypt](https://eucalypt.curvelogic.info/) is a tool and a
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

### Linux / macOS (install script)

```sh
curl -sSf https://raw.githubusercontent.com/curvelogic/eucalypt/master/install.sh | sh
```

Installs the latest release binary to `~/.local/bin`. Set `EUCALYPT_INSTALL_DIR` to override.

### From Source

```sh
cargo install --path .
```

See the [documentation](https://eucalypt.curvelogic.info/docs/) for
other installation options.

## Documentation

Full documentation is available at https://eucalypt.curvelogic.info/docs/

- [Getting Started](https://eucalypt.curvelogic.info/docs/welcome/quick-start/)
- [Language Syntax](https://eucalypt.curvelogic.info/docs/reference/syntax/)
- [Command Line](https://eucalypt.curvelogic.info/docs/reference/cli/)
- [Prelude Reference](https://eucalypt.curvelogic.info/docs/reference/prelude/)

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

## Licence

Copyright (c) 2018-2026 Curve Logic.

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  http://www.apache.org/licenses/LICENSE-2.0)
- MIT License ([LICENSE-MIT](LICENSE-MIT) or
  http://opensource.org/licenses/MIT)

at your option. See [LICENSE](LICENSE) for a summary.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
licence, shall be dual licensed as above, without any additional terms or
conditions.
