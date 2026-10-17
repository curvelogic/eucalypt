#!/usr/bin/env bash
# Regenerate the Homebrew formula for eucalypt deterministically.
#
# Usage: generate-homebrew-formula.sh <version> <sha256sums-file>
#
#   <version>          release tag, e.g. 0.14.1 (eucalypt tags carry no
#                      leading "v")
#   <sha256sums-file>  the SHA256SUMS asset published on the GitHub release:
#                      `sha256sum` output, one "<hex>  <filename>" line per
#                      release asset
#
# Writes the complete Formula/eucalypt.rb to stdout. All-or-nothing: if any
# expected checksum is missing, duplicated or malformed the script fails
# without emitting anything, so a half-updated formula can never be produced.
#
# Used by .github/workflows/homebrew-bump.yml, and runnable locally against
# a downloaded SHA256SUMS to verify the output (it reproduces the tap's
# Formula/eucalypt.rb for a published release byte for byte):
#
#   curl -sSfLO https://github.com/curvelogic/eucalypt/releases/download/0.14.1/SHA256SUMS
#   scripts/generate-homebrew-formula.sh 0.14.1 SHA256SUMS > eucalypt.rb

set -euo pipefail

die() {
  echo "generate-homebrew-formula: $*" >&2
  exit 1
}

[ "$#" -eq 2 ] || die "usage: generate-homebrew-formula.sh <version> <sha256sums-file>"

version=$1
sums=$2

# Semver with an optional pre-release/build suffix; no leading "v".
printf '%s' "$version" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+([.-][A-Za-z0-9.-]+)?$' \
  || die "unexpected version format: '$version' (expected e.g. 0.14.1, no leading v)"
[ -f "$sums" ] || die "not a file: $sums"

# Read the sha256 for one asset from SHA256SUMS. Exactly one line must name
# the asset (an optional "*" before the name is sha256sum's binary-mode marker).
sha_for() {
  local asset=$1
  local matches
  matches=$(awk -v a="$asset" '$2 == a || $2 == "*" a { print $1 }' "$sums")
  [ -n "$matches" ] || die "missing checksum for $asset in $sums"
  [ "$(printf '%s\n' "$matches" | wc -l)" -eq 1 ] \
    || die "more than one checksum for $asset in $sums"
  printf '%s' "$matches" | grep -Eq '^[0-9a-f]{64}$' \
    || die "malformed sha256 for $asset in $sums: '$matches'"
  printf '%s' "$matches"
}

# Resolve (and validate) all three before emitting anything.
sha_macos_arm=$(sha_for eucalypt-aarch64-osx.tgz)
sha_linux_arm=$(sha_for eucalypt-aarch64-linux.tgz)
sha_linux_x86=$(sha_for eucalypt-x86_64-linux.tgz)

base="https://github.com/curvelogic/eucalypt/releases/download/${version}"

cat <<EOF
class Eucalypt < Formula
  desc "Tool for generating, templating and processing YAML, JSON, TOML"
  homepage "https://curvelogic.github.io/eucalypt/"
  version "${version}"

  on_macos do
    if Hardware::CPU.arm?
      url "${base}/eucalypt-aarch64-osx.tgz"
      sha256 "${sha_macos_arm}"
    end
  end

  on_linux do
    if Hardware::CPU.arm?
      url "${base}/eucalypt-aarch64-linux.tgz"
      sha256 "${sha_linux_arm}"
    elsif Hardware::CPU.intel?
      url "${base}/eucalypt-x86_64-linux.tgz"
      sha256 "${sha_linux_x86}"
    end
  end

  def install
    bin.install "eu"
  end

  test do
    assert_match "eu", shell_output("#{bin}/eu --version").strip
  end
end
EOF
