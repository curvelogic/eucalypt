# Create curl-installable install script

**Bead**: eu-1j9u
**Date**: 2026-02-09

## Summary

Provide an `install.sh` at the repo root that can be curled and piped
to sh for single-line installation of the eucalypt binary.

## Usage

```bash
curl -sSf https://raw.githubusercontent.com/curvelogic/eucalypt/master/install.sh | sh
```

With options:

```bash
EUCALYPT_VERSION=0.4.0 EUCALYPT_INSTALL_DIR=/opt/bin \
  curl -sSf https://raw.githubusercontent.com/curvelogic/eucalypt/master/install.sh | sh
```

## Script Behaviour

1. **Detect platform** — `uname -s` and `uname -m` to determine one
   of three supported targets. Exit with a clear error for unsupported
   platforms (Intel Mac, Windows, etc.), directing users to build from
   source.

2. **Determine version** — Use `EUCALYPT_VERSION` env var if set,
   otherwise query the GitHub releases API for the latest release tag.

3. **Download tarball** — Fetch
   `eucalypt-{arch}-{os}.tgz` from the GitHub release assets.

4. **Verify integrity** — Download `SHA256SUMS` from the same release.
   Verify the tarball hash using `sha256sum` (Linux) or
   `shasum -a 256` (macOS). Abort with an error if verification fails.

5. **Install binary** — Extract the `eu` binary to the install
   directory. Default: `~/.local/bin`. Override:
   `EUCALYPT_INSTALL_DIR`. Create the directory if it doesn't exist.

6. **PATH check** — If the install directory is not on `$PATH`, print
   a warning with instructions to add it to the user's shell profile.

## Supported Platforms

| uname -s | uname -m | Tarball                        |
|----------|----------|--------------------------------|
| Linux    | x86_64   | `eucalypt-x86_64-linux.tgz`   |
| Linux    | aarch64  | `eucalypt-aarch64-linux.tgz`  |
| Darwin   | arm64    | `eucalypt-aarch64-osx.tgz`    |

Unsupported combinations print:

> eucalypt does not provide prebuilt binaries for {os}/{arch}.
> Build from source: cargo install --git https://github.com/curvelogic/eucalypt

## CI Changes

The `prepare-release` job in `build-rust.yaml` is updated to:

- Generate `SHA256SUMS` via `sha256sum *.tgz > SHA256SUMS`
- Attach `SHA256SUMS` as a release asset alongside the tarballs

## Environment Variables

| Variable               | Default         | Description                |
|------------------------|-----------------|----------------------------|
| `EUCALYPT_VERSION`     | latest release  | Pin a specific version     |
| `EUCALYPT_INSTALL_DIR` | `~/.local/bin`  | Override install location  |

## Dependencies

- Depends on eu-epe4 (aarch64-linux release binary) for the full set
  of platform assets to be available.
