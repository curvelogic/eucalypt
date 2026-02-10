# Add aarch64-linux release binary

**Bead**: eu-epe4
**Date**: 2026-02-09

## Summary

Add an ARM64 Linux build target to the CI/CD release pipeline so that
GitHub releases include an `eucalypt-aarch64-linux.tgz` asset. Also
correct the macOS tarball name to reflect that `macos-latest` is ARM.

## Build Pipeline Changes

Add a new `release-candidate-linux-arm` job to `build-rust.yaml`:

- Runs on `ubuntu-24.04-arm` (GitHub native ARM runner)
- Mirrors the existing `release-candidate-linux` job exactly:
  install stable Rust, build temporary `eu` for `build-meta.yaml`,
  `cargo test --release`, `cargo build --all --release`, strip binary,
  run full harness test suite against release binary
- Uploads the binary as the `eu_aarch64` artifact

Rename the macOS tarball from `eucalypt-x86_64-osx.tgz` to
`eucalypt-aarch64-osx.tgz` since `macos-latest` is Apple Silicon.

Update `prepare-release` job:

- Add dependency on `release-candidate-linux-arm`
- Download the `eu_aarch64` artifact
- Produce three tarballs:
  - `eucalypt-x86_64-linux.tgz`
  - `eucalypt-aarch64-linux.tgz`
  - `eucalypt-aarch64-osx.tgz`

## Release Assets

| Platform              | Tarball                        |
|-----------------------|--------------------------------|
| Linux x86_64          | `eucalypt-x86_64-linux.tgz`   |
| Linux aarch64 (ARM64) | `eucalypt-aarch64-linux.tgz` |
| macOS aarch64 (Apple Silicon) | `eucalypt-aarch64-osx.tgz` |

## Install Documentation

Document the three supported binary platforms. Add a note that Intel
Mac users should build from source:

```
cargo install --path .
```

or

```
cargo install --git https://github.com/curvelogic/eucalypt
```

The full install script (eu-1j9u) will handle platform detection
automatically but is a separate piece of work.
