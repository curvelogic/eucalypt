#!/bin/sh
# install.sh - Install eucalypt binary from GitHub releases
#
# Usage:
#   curl -sSf https://raw.githubusercontent.com/curvelogic/eucalypt/master/install.sh | sh
#
# Environment variables:
#   EUCALYPT_VERSION     - Pin a specific version (default: latest release)
#   EUCALYPT_INSTALL_DIR - Override install location (default: ~/.local/bin)

set -eu

REPO="curvelogic/eucalypt"
DEFAULT_INSTALL_DIR="$HOME/.local/bin"

main() {
    install_dir="${EUCALYPT_INSTALL_DIR:-$DEFAULT_INSTALL_DIR}"

    detect_platform
    determine_version
    download_and_verify
    install_binary
    check_path
}

detect_platform() {
    os="$(uname -s)"
    arch="$(uname -m)"

    case "$os" in
        Linux)
            case "$arch" in
                x86_64)
                    tarball="eucalypt-x86_64-linux.tgz"
                    platform_dir="eucalypt-x86_64-linux"
                    ;;
                aarch64)
                    tarball="eucalypt-aarch64-linux.tgz"
                    platform_dir="eucalypt-aarch64-linux"
                    ;;
                *)
                    unsupported "$os" "$arch"
                    ;;
            esac
            sha_cmd="sha256sum"
            ;;
        Darwin)
            case "$arch" in
                arm64)
                    tarball="eucalypt-aarch64-osx.tgz"
                    platform_dir="eucalypt-aarch64-osx"
                    ;;
                *)
                    unsupported "$os" "$arch"
                    ;;
            esac
            sha_cmd="shasum -a 256"
            ;;
        *)
            unsupported "$os" "$arch"
            ;;
    esac

    printf "Detected platform: %s/%s\n" "$os" "$arch"
}

unsupported() {
    printf "eucalypt does not provide prebuilt binaries for %s/%s.\n" "$1" "$2"
    printf "Build from source: cargo install --git https://github.com/%s\n" "$REPO"
    exit 1
}

determine_version() {
    if [ -n "${EUCALYPT_VERSION:-}" ]; then
        version="$EUCALYPT_VERSION"
        printf "Using specified version: %s\n" "$version"
    else
        printf "Querying latest release...\n"
        version="$(curl -sSf "https://api.github.com/repos/$REPO/releases/latest" \
            | sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p')"
        if [ -z "$version" ]; then
            printf "Error: could not determine latest release version.\n" >&2
            exit 1
        fi
        printf "Latest version: %s\n" "$version"
    fi

    base_url="https://github.com/$REPO/releases/download/$version"
}

download_and_verify() {
    tmp_dir="$(mktemp -d)"
    trap 'rm -rf "$tmp_dir"' EXIT

    printf "Downloading %s...\n" "$tarball"
    curl -sSfL -o "$tmp_dir/$tarball" "$base_url/$tarball"

    printf "Downloading SHA256SUMS...\n"
    curl -sSfL -o "$tmp_dir/SHA256SUMS" "$base_url/SHA256SUMS"

    printf "Verifying integrity...\n"
    expected="$(grep "$tarball" "$tmp_dir/SHA256SUMS" | awk '{print $1}')"
    if [ -z "$expected" ]; then
        printf "Error: %s not found in SHA256SUMS.\n" "$tarball" >&2
        exit 1
    fi

    actual="$(cd "$tmp_dir" && $sha_cmd "$tarball" | awk '{print $1}')"
    if [ "$expected" != "$actual" ]; then
        printf "Error: SHA256 verification failed.\n" >&2
        printf "  Expected: %s\n" "$expected" >&2
        printf "  Actual:   %s\n" "$actual" >&2
        exit 1
    fi

    printf "Integrity verified.\n"

    tar -xzf "$tmp_dir/$tarball" -C "$tmp_dir"
}

install_binary() {
    mkdir -p "$install_dir"
    cp "$tmp_dir/$platform_dir/eu" "$install_dir/eu"
    chmod +x "$install_dir/eu"
    printf "Installed eu to %s/eu\n" "$install_dir"
}

check_path() {
    case ":${PATH}:" in
        *":${install_dir}:"*)
            printf "eu is ready to use.\n"
            ;;
        *)
            printf "\nWarning: %s is not on your PATH.\n" "$install_dir"
            printf "Add it to your shell profile:\n\n"
            printf "  export PATH=\"%s:\$PATH\"\n\n" "$install_dir"
            printf "Then restart your shell or run the export command above.\n"
            ;;
    esac
}

main
