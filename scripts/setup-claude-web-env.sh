#!/usr/bin/env bash
# setup-claude-web-env.sh - Provision a Claude Code remote dev environment.
#
# This script exists solely to bootstrap Claude Code on the web (remote dev
# environment) sessions for this repo. Point the environment's startup/setup
# command at it. It is safe to re-run: every step is idempotent.
#
# Installs:
#   - eucalypt (eu)   the project binary          -> ~/.local/bin/eu
#   - beads (bd)      issue tracking CLI          -> ~/.local/bin/bd (symlink to go/bin)
#   - dolt            beads' dolt-native backend  -> /usr/local/bin/dolt (or ~/.local/bin)
#
# Environment variables (all optional):
#   EUCALYPT_VERSION   Pin the eu release        (default: 0.11.1)
#   DOLT_VERSION       Pin the dolt release, e.g. v1.58.6, or "latest"
#                                                (default: latest)
#   BEADS_SYNC         Set to 0 to skip `bd sync` (default: 1)
#
# Note on GitHub access: dolt is fetched from github.com/dolthub/dolt releases.
# In Claude Code on the web, github.com traffic is gated by the session's
# repository scope, so an interactive session limited to curvelogic/eucalypt will
# get a 403 for dolthub/dolt. The environment *setup* phase generally has broader
# access; if it does not, grant the environment access to dolthub/dolt.

set -euo pipefail

EUCALYPT_VERSION="${EUCALYPT_VERSION:-0.11.1}"
DOLT_VERSION="${DOLT_VERSION:-latest}"
BEADS_SYNC="${BEADS_SYNC:-1}"

LOCAL_BIN="$HOME/.local/bin"
mkdir -p "$LOCAL_BIN"

# Run from the repo root regardless of the caller's working directory, so the
# beads sync step finds .beads. The script lives in <repo>/scripts/.
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

log() { printf '\n=== %s ===\n' "$*"; }

# Ensure ~/.local/bin is on PATH for this process and for later shells.
ensure_path() {
    case ":$PATH:" in
        *":$LOCAL_BIN:"*) ;;
        *) export PATH="$LOCAL_BIN:$PATH" ;;
    esac
    local line='export PATH="$HOME/.local/bin:$PATH"'
    if [ -f "$HOME/.bashrc" ] && ! grep -qF "$line" "$HOME/.bashrc"; then
        printf '%s\n' "$line" >> "$HOME/.bashrc"
    fi
}

# ---------------------------------------------------------------------------
# beads
# ---------------------------------------------------------------------------
install_beads() {
    log "Installing beads (bd)"
    export PATH="$PATH:/root/go/bin"
    if command -v bd >/dev/null 2>&1; then
        echo "bd already present: $(command -v bd)"
    else
        curl -fsSL https://raw.githubusercontent.com/gastownhall/beads/main/scripts/install.sh | bash
    fi
    # Expose bd on ~/.local/bin regardless of how it was installed.
    if [ -x /root/go/bin/bd ] && [ ! -e "$LOCAL_BIN/bd" ]; then
        ln -sf /root/go/bin/bd "$LOCAL_BIN/bd"
    fi
    command -v bd >/dev/null 2>&1 && bd version || echo "WARNING: bd not on PATH"
}

# ---------------------------------------------------------------------------
# dolt (beads' dolt-native backend)
# ---------------------------------------------------------------------------
install_dolt() {
    log "Installing dolt ($DOLT_VERSION)"
    if command -v dolt >/dev/null 2>&1; then
        echo "dolt already present: $(command -v dolt) ($(dolt version | head -1))"
        return
    fi

    local os arch asset url tmp dest
    case "$(uname -s)" in
        Linux)  os=linux ;;
        Darwin) os=darwin ;;
        *) echo "Unsupported OS for dolt: $(uname -s)"; return 1 ;;
    esac
    case "$(uname -m)" in
        x86_64|amd64)  arch=amd64 ;;
        aarch64|arm64) arch=arm64 ;;
        *) echo "Unsupported arch for dolt: $(uname -m)"; return 1 ;;
    esac
    asset="dolt-${os}-${arch}.tar.gz"

    if [ "$DOLT_VERSION" = "latest" ]; then
        url="https://github.com/dolthub/dolt/releases/latest/download/${asset}"
    else
        url="https://github.com/dolthub/dolt/releases/download/${DOLT_VERSION}/${asset}"
    fi

    tmp="$(mktemp -d)"
    if ! curl -fsSL "$url" | tar -xz -C "$tmp"; then
        rm -rf "$tmp"
        echo "Failed to download/extract dolt from $url"
        return 1
    fi

    # Prefer /usr/local/bin (on PATH by default); fall back to ~/.local/bin.
    if [ -w /usr/local/bin ]; then
        dest=/usr/local/bin/dolt
    else
        dest="$LOCAL_BIN/dolt"
    fi
    install -m 0755 "$tmp/dolt-${os}-${arch}/bin/dolt" "$dest"
    rm -rf "$tmp"
    "$dest" version | head -1
}

# ---------------------------------------------------------------------------
# eucalypt
# ---------------------------------------------------------------------------
install_eucalypt() {
    log "Installing eucalypt (eu $EUCALYPT_VERSION)"
    if command -v eu >/dev/null 2>&1 && eu version 2>/dev/null | grep -qE "v${EUCALYPT_VERSION}([^0-9]|$)"; then
        echo "eu $EUCALYPT_VERSION already present"
    else
        EUCALYPT_VERSION="$EUCALYPT_VERSION" \
            curl -sSfL https://raw.githubusercontent.com/curvelogic/eucalypt/master/install.sh | sh
    fi
    command -v eu >/dev/null 2>&1 && eu version || echo "WARNING: eu not on PATH"
}

# ---------------------------------------------------------------------------
# beads database: materialise the dolt-native store so `bd list` works
# ---------------------------------------------------------------------------
sync_beads() {
    [ "$BEADS_SYNC" = "1" ] || return 0
    command -v bd >/dev/null 2>&1 || return 0
    command -v dolt >/dev/null 2>&1 || return 0
    [ -d .beads ] || return 0

    log "Syncing beads store"
    chmod 700 .beads 2>/dev/null || true
    # Non-fatal: git-ssh sync may be unavailable at provision time.
    bd sync || echo "WARNING: 'bd sync' failed (network/git access?); run it later."
}

main() {
    ensure_path
    install_eucalypt
    install_beads
    # dolt fetch can fail where github.com release access is not granted; keep
    # going so eu/bd are still usable and the failure is reported, not fatal.
    install_dolt || echo "WARNING: dolt install failed; beads' dolt backend will be unavailable."
    sync_beads
    log "Setup complete"
}

main "$@"
