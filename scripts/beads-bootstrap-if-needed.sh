#!/usr/bin/env bash
# beads-bootstrap-if-needed.sh - Ensure the beads Dolt store is materialised.
#
# The authoritative issue store is Dolt (refs/dolt/data on the remote); the
# runtime dolt/ store under .beads is gitignored, so a fresh checkout has no
# usable database until it is rebuilt with `bd bootstrap`. This script does
# that rebuild, but only when it is actually needed, so it is safe to run on
# every session start and safe to re-run.
#
# It is deliberately READ-ONLY with respect to the remote: it makes `bd list`
# work. It never pushes. (Pushing dolt-native data needs a write to the
# refs/dolt/* namespace, which the Claude-web git proxy blocks with 403 - see
# .beads/config.yaml. That is a separate, unsolved problem.)
#
# Why a dedicated https remote for the no-ssh case:
#   - The tracked sync.remote is git+ssh://…, but the Claude-web container has
#     no ssh binary, so `bd bootstrap` over it dead-ends and (worse) can leave
#     an empty beads_eu shell that blocks a retry.
#   - The git origin's http URL carries a per-session proxy port that is not up
#     during the environment setup phase, so it is unreliable there.
#   - git+https://github.com/<repo>.git is stable, port-independent, and
#     reachable via the agent proxy in BOTH the setup phase and interactive
#     sessions. Reads over it work (verified); only writes are blocked.
#
# The tracked config.yaml (which pins the ssh remote for local dev) is always
# restored, so a dev machine is left untouched and keeps using ssh.

set -euo pipefail

# Stable, port-independent https remote reachable without ssh.
BEADS_HTTPS_REMOTE="${BEADS_HTTPS_REMOTE:-git+https://github.com/curvelogic/eucalypt.git}"

# Tools we need. Absent tools => nothing to do (exit 0 so we never wedge
# session start or the setup script under `set -e`).
command -v bd   >/dev/null 2>&1 || exit 0
command -v dolt >/dev/null 2>&1 || exit 0

# Locate the repo (the dir containing .beads). Prefer this script's location;
# fall back to well-known paths and the caller's cwd.
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
    return 1
}

REPO="$(find_repo_root)" || { echo "beads-bootstrap: no .beads checkout found; skipping."; exit 0; }

# True when the store is materialised (`bd list` exits 0 only then).
_store_ok() { ( cd "$REPO" && bd list >/dev/null 2>&1 ); }

# Idempotency guard: skip once the store is usable. `bd bootstrap` is NOT
# self-idempotent when a remote is configured - a second run re-attempts the
# clone and errors "database exists".
if _store_ok; then
    exit 0
fi

echo "beads-bootstrap: store not materialised; bootstrapping ($REPO)."
chmod 700 "$REPO/.beads" 2>/dev/null || true

# On a dev machine (ssh present) honour the configured ssh sync.remote.
# In the no-ssh container skip straight to https: attempting the ssh remote
# there only fails and can leave a partial beads_eu shell that blocks the
# https retry with "database exists".
if command -v ssh >/dev/null 2>&1; then
    ( cd "$REPO" && bd bootstrap --yes ) || true
fi

# https fallback: bootstrap with sync.remote temporarily repointed at the
# stable https remote. config.yaml is git-tracked, so restore it unconditionally
# (even on error under `set -e`) to leave the ssh remote in place for local dev.
if ! _store_ok; then
    cfg="$REPO/.beads/config.yaml"
    if [ -f "$cfg" ] && grep -q '^[[:space:]]*sync\.remote:' "$cfg"; then
        bak="$(mktemp)"
        cp "$cfg" "$bak"
        # shellcheck disable=SC2064
        trap "cp '$bak' '$cfg'; rm -f '$bak'; trap - EXIT" EXIT
        sed -i 's#^\([[:space:]]*\)sync\.remote:.*#\1sync.remote: "'"$BEADS_HTTPS_REMOTE"'"#' "$cfg"
        echo "beads-bootstrap: bootstrapping over $BEADS_HTTPS_REMOTE"
        ( cd "$REPO" && bd bootstrap --yes ) || true
        cp "$bak" "$cfg"; rm -f "$bak"; trap - EXIT
    fi
fi

if _store_ok; then
    echo "beads-bootstrap: store materialised."
else
    echo "beads-bootstrap: WARNING - bootstrap failed (network/git access?); run 'bd bootstrap' later." >&2
fi
