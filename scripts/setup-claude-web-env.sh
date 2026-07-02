#!/usr/bin/env bash
# setup-claude-web-env.sh - Provision a Claude Code remote dev environment.
#
# This script exists solely to bootstrap Claude Code on the web (remote dev
# environment) sessions for this repo. It is safe to re-run: every step is
# idempotent.
#
# The environment's setup phase does NOT run in the repo checkout directory, so
# a repo-relative path (bash scripts/setup-claude-web-env.sh) fails with
# exit 127. Set the environment's setup command to fetch and pipe this script,
# which is working-directory-independent:
#
#   curl -fsSL https://raw.githubusercontent.com/curvelogic/eucalypt/master/scripts/setup-claude-web-env.sh | bash
#
# It is also safe to run as a file from anywhere: the repo (for the beads sync
# step) is located independently of the caller's working directory.
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
#   BEADS_SYNC         Set to 0 to skip `bd bootstrap` (default: 1)
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

log() { printf '\n=== %s ===\n' "$*"; }

# Locate the eucalypt checkout (the dir containing .beads) for the sync step.
# Works whether this script is run as a file or piped via `curl ... | bash`
# (in which case BASH_SOURCE is unset, hence the ${..:-} guard under `set -u`).
find_repo_root() {
    local src="${BASH_SOURCE[0]:-}"
    if [ -n "$src" ] && [ -f "$src" ]; then
        local d; d="$(cd "$(dirname "$src")/.." && pwd)"
        [ -d "$d/.beads" ] && { printf '%s\n' "$d"; return 0; }
    fi
    local cand
    for cand in /home/user/eucalypt "$HOME/eucalypt" "$PWD"; do
        [ -d "$cand/.beads" ] && { printf '%s\n' "$cand"; return 0; }
    done
    local hit
    hit="$(find /home /root /workspace -maxdepth 4 -type d -name .beads 2>/dev/null | head -1)"
    [ -n "$hit" ] && { dirname "$hit"; return 0; }
    return 1
}

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
        # The version must be exported so the piped `sh` (which runs install.sh)
        # inherits it; a prefix on `curl` would only set it for curl, leaving
        # install.sh to resolve "latest" via the GitHub API (which 403s here).
        export EUCALYPT_VERSION
        curl -sSfL https://raw.githubusercontent.com/curvelogic/eucalypt/master/install.sh | sh
    fi
    command -v eu >/dev/null 2>&1 && eu version || echo "WARNING: eu not on PATH"
}

# ---------------------------------------------------------------------------
# beads database: materialise the dolt-native store so `bd list` works
# ---------------------------------------------------------------------------
# The authoritative issue store is Dolt (refs/dolt/data on the remote); the
# dolt/ runtime store is gitignored, so a fresh checkout has no usable database
# until it is rebuilt. `bd bootstrap` is beads' purpose-built command for this.
# (The old `bd sync` this script used to call no longer exists in beads >= 1.0.)
#
# Caveat that this function works around: `bd bootstrap` is priority-ordered,
# and a configured `sync.remote` is the FIRST rule — it clones from that remote
# and short-circuits every fallback (git-origin dolt data, backup, JSONL). This
# repo pins `sync.remote: git+ssh://…`, but the Claude-web container has no ssh
# binary and dolt shells out to ssh directly (bypassing the proxy's git-ssh
# rewrite), so the clone dead-ends and leaves an empty store — every later `bd`
# call then reports "database … not found". The fallback below neutralises
# `sync.remote` and retries, so bd falls through to the git origin (the same
# refs/dolt/data, reached as https via the agent proxy).

# True when the beads store is materialised (`bd list` exits 0 only then).
_beads_store_ok() { ( cd "$1" && bd list >/dev/null 2>&1 ); }

# Retry bootstrap with `sync.remote` temporarily disabled so bd uses the git
# origin / JSONL instead of the unusable ssh remote. Always restores
# config.yaml (a git-tracked file) so the working tree — and the ssh remote
# relied on for local dev — is left untouched, even on error under `set -e`.
_bootstrap_without_ssh_remote() {
    # Separate `local` statements: a single `local a=… b="$a"` expands every
    # RHS before assigning, so `$repo` would be unbound under `set -u`.
    local repo="$1"
    local cfg="$repo/.beads/config.yaml"
    local bak
    [ -f "$cfg" ] && grep -q '^[[:space:]]*sync\.remote:' "$cfg" || return 1
    bak="$(mktemp)"
    cp "$cfg" "$bak"
    # shellcheck disable=SC2064
    trap "cp '$bak' '$cfg'; rm -f '$bak'; trap - RETURN" RETURN
    sed -i 's/^\([[:space:]]*\)sync\.remote:/\1# sync.remote:/' "$cfg"
    ( cd "$repo" && bd bootstrap --yes ) || true
}

bootstrap_beads() {
    [ "$BEADS_SYNC" = "1" ] || return 0
    command -v bd >/dev/null 2>&1 || return 0
    command -v dolt >/dev/null 2>&1 || return 0

    local repo
    if ! repo="$(find_repo_root)"; then
        echo "Skipping beads bootstrap: could not locate a .beads checkout."
        return 0
    fi

    log "Bootstrapping beads store ($repo)"
    chmod 700 "$repo/.beads" 2>/dev/null || true

    # Idempotency guard: skip once the store is already usable. `bd bootstrap`
    # is NOT self-idempotent when sync.remote is set — a second run re-attempts
    # the clone and errors with "database exists".
    if _beads_store_ok "$repo"; then
        echo "beads store already materialised; skipping bootstrap."
        return 0
    fi

    # First attempt: honour the configured sync.remote (ssh, for local dev).
    ( cd "$repo" && bd bootstrap --yes ) || true

    # Fallback for the no-ssh container: retry over the git origin / JSONL.
    if ! _beads_store_ok "$repo"; then
        echo "bootstrap via sync.remote failed; retrying over the git origin (no ssh)."
        _bootstrap_without_ssh_remote "$repo" || true
    fi

    if _beads_store_ok "$repo"; then
        echo "beads store materialised."
    else
        echo "WARNING: 'bd bootstrap' failed (network/git access?); run 'bd bootstrap' later."
    fi
}

# ---------------------------------------------------------------------------
# beads push path for web sessions
# ---------------------------------------------------------------------------
# Local dev pushes over the ssh `sync.remote` in config.yaml. A Claude-web
# container has no ssh binary, so that remote is a dead end for writes as well
# as reads. Point beads' Dolt remote at the proxy-brokered https endpoint
# instead — a stable URL (unlike the ephemeral git-origin http URL, whose port
# is per-session) that the agent proxy authenticates with the session's write
# token. Dolt-native sync reads/writes refs/dolt/data + refs/heads/
# __dolt_remote_info__ over this remote. The tracked ssh config.yaml is left
# untouched, so local dev is unaffected.
#
# Detection: absence of `ssh` marks the no-ssh container. On a dev machine ssh
# is present and this is a no-op, so the ssh remote stays authoritative.
configure_web_beads_remote() {
    [ "$BEADS_SYNC" = "1" ] || return 0
    command -v bd >/dev/null 2>&1 || return 0
    command -v ssh >/dev/null 2>&1 && return 0   # dev machine: keep ssh remote

    local repo
    repo="$(find_repo_root)" || return 0
    _beads_store_ok "$repo" || return 0          # only if the store came up

    local https="git+https://github.com/curvelogic/eucalypt.git"
    ( cd "$repo"
      bd dolt remote remove origin >/dev/null 2>&1 || true
      bd dolt remote add origin "$https" >/dev/null 2>&1 || true )
    echo "beads Dolt remote set to https for this container: $https"
    echo "  Persist web-session issues with: bd dolt commit -am '…' && bd dolt push"
}

main() {
    ensure_path
    install_eucalypt
    install_beads
    # dolt fetch can fail where github.com release access is not granted; keep
    # going so eu/bd are still usable and the failure is reported, not fatal.
    install_dolt || echo "WARNING: dolt install failed; beads' dolt backend will be unavailable."
    bootstrap_beads
    configure_web_beads_remote
    log "Setup complete"
}

main "$@"
