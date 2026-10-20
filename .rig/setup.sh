#!/usr/bin/env bash
# .rig/setup.sh — eucalypt's toolchain for a cloud session.
#
# Run by curve-rig's SessionStart hook with the repository as working
# directory (see curve-rig's docs/design.md, "Repo contract"). bd, dolt and
# the beads store are rig's job, not this script's.
#
# Provides what CLAUDE.md's pre-commit checklist and CI's lint and test jobs
# need: a pinned Rust stable toolchain with rustfmt and clippy, and the
# crates in Cargo.lock. No prebuilt `eu` is installed: nothing in the
# checklist uses one, and CI builds its own (`cargo build --release`).
#
# Idempotent: every step checks before it downloads. Never writes to the
# working tree.
#
# Pins. Bump RUST_VERSION when Rust stable moves (CI runs
# dtolnay/rust-toolchain@stable), together with the channel manifest's
# digest, from https://static.rust-lang.org/dist/channel-rust-<v>.toml.sha256.
# The pinned digest checks the manifest as served before installing;
# rustup then fetches the manifest itself and checks every component
# against its SHA-256 entries. Versioned manifests are immutable, so the
# two copies are the same file.

set -euo pipefail

RUSTUP_VERSION="1.29.1"
RUSTUP_SHA256_linux_amd64="dda7234360b7f578ca8b0ddcb80145646fa61a67c1720a5abc7051b35c9fcb71"
RUSTUP_SHA256_linux_arm64="15f6e4ce9f583b929c996c91562bad6d4454f3281de858b02cdfdef615fac433"

RUST_VERSION="1.99.1"
RUST_MANIFEST_SHA256="fb152be80975c194aa5fc684b2aec3a6e14e2b855efe2faa4e5f8ae6e590f463"

RIG_HOME="${RIG_HOME:-$HOME/.local/share/curve-rig}"
# shellcheck source=/dev/null
. "$RIG_HOME/lib/common.sh"

rig_ensure_path
CARGO_BIN="${CARGO_HOME:-$HOME/.cargo}/bin"
case ":$PATH:" in *":$CARGO_BIN:"*) ;; *) PATH="$CARGO_BIN:$PATH" ;; esac
export PATH

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

install_rustup() {
    command -v rustup >/dev/null 2>&1 && return 0
    local platform triple digest
    platform="$(rig_platform)" || { rig_warn "unsupported platform for rustup"; return 1; }
    case "$platform" in
        linux_amd64) triple=x86_64-unknown-linux-gnu digest="$RUSTUP_SHA256_linux_amd64" ;;
        linux_arm64) triple=aarch64-unknown-linux-gnu digest="$RUSTUP_SHA256_linux_arm64" ;;
    esac
    rig_fetch "https://static.rust-lang.org/rustup/archive/$RUSTUP_VERSION/$triple/rustup-init" \
        "$work/rustup-init"
    rig_sha256_ok "$work/rustup-init" "$digest" ||
        { rig_warn "rustup-init $RUSTUP_VERSION failed its digest check"; return 1; }
    chmod +x "$work/rustup-init"
    "$work/rustup-init" -y --profile minimal --default-toolchain none
}

install_rust() {
    if rustup run "$RUST_VERSION" rustc --version >/dev/null 2>&1 &&
       rustup run "$RUST_VERSION" cargo fmt --version >/dev/null 2>&1 &&
       rustup run "$RUST_VERSION" cargo clippy --version >/dev/null 2>&1; then
        rustup default "$RUST_VERSION" >/dev/null
        return 0
    fi
    rig_fetch "https://static.rust-lang.org/dist/channel-rust-$RUST_VERSION.toml" \
        "$work/channel.toml"
    rig_sha256_ok "$work/channel.toml" "$RUST_MANIFEST_SHA256" ||
        { rig_warn "Rust $RUST_VERSION channel manifest failed its digest check"; return 1; }
    rustup toolchain install "$RUST_VERSION" --profile minimal \
        --component rustfmt --component clippy
    rustup default "$RUST_VERSION"
}

install_rustup
install_rust
rig_log "eucalypt: $(rig_first_line "$(rustc --version)")"

# Warm the crate cache so the first cargo command needs no download. Not
# fatal: cargo fetches on demand anyway.
cargo fetch --locked >/dev/null 2>&1 || rig_warn "cargo fetch failed; crates will download on first build"
