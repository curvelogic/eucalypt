#!/usr/bin/env bash
# Check that eucalypt-mode's tree-sitter queries compile against the current
# grammar in editors/tree-sitter-eucalypt, and that the mode fontifies a
# corpus at every font-lock level without a query error.
#
# Usage: scripts/check-emacs-mode.sh [--require] [FILE.eu...]
#
# FILEs default to examples/aoc25/*.eu. Requires Emacs 29+ built with
# tree-sitter and a C compiler (the grammar is compiled from the checked-in
# src/parser.c and src/scanner.c into a temporary directory). When either
# is missing the check is skipped with exit 0, or fails with exit 2 under
# --require (use that in CI, where the tools must be present).
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
require=0
if [[ "${1:-}" == "--require" ]]; then
  require=1
  shift
fi

skip() {
  echo "SKIP: $1" >&2
  if [[ $require -eq 1 ]]; then exit 2; fi
  exit 0
}

EMACS="${EMACS:-emacs}"
CC="${CC:-cc}"
command -v "$EMACS" >/dev/null 2>&1 || skip "emacs not found (set EMACS)"
command -v "$CC" >/dev/null 2>&1 || skip "C compiler not found (set CC)"
"$EMACS" --batch -Q --eval \
  '(kill-emacs (if (and (>= emacs-major-version 29) (treesit-available-p)) 0 1))' \
  >/dev/null 2>&1 || skip "$EMACS is not Emacs 29+ with tree-sitter support"

grammar="$root/editors/tree-sitter-eucalypt"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
"$CC" -shared -fPIC -O1 -I"$grammar/src" \
  "$grammar/src/parser.c" "$grammar/src/scanner.c" \
  -o "$tmp/libtree-sitter-eucalypt.so"

if [[ $# -eq 0 ]]; then
  set -- "$root"/examples/aoc25/*.eu
fi

"$EMACS" --batch -Q \
  -L "$root/editors/emacs" \
  --eval "(setq treesit-extra-load-path (list \"$tmp\"))" \
  -l "$root/editors/emacs/test/eucalypt-mode-font-lock-check.el" \
  -f eucalypt-check-run "$@"
