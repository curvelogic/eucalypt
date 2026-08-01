#!/usr/bin/env bash
#
# Run every prelude-mode-sensitive `cargo test` target under an embedded
# prelude blob (eu-1tkk.7.43).
#
# Why this exists
# ---------------
# `lib/prelude.blob` is gitignored, so a build without it silently falls back
# to the source prelude — and the two are not the same pipeline. Blob-mode
# prelude globals carry `Smid::global_slot(i)` stamps, the checker takes
# `run_type_checker_from_blob_core` rather than `run_type_checker`, and
# `tests/diagnostics/DIVERGENCE.md` is the checked-in inventory of fixtures
# that render a *different diagnostic* depending on which mode is in force.
# Blob mode is what a released binary does; it is the primary case.
#
# Which mode a given `cargo test` target ran under used to be an emergent
# property of which job happened to run it, listed by hand in the workflow.
# That is an allowlist by another name, and it had the usual hole: a target
# added to the tree ran blob-less only, and a `#[cfg(prelude_blob_ok)]` test
# inside it compiled to nothing at all and reported green. PR #1104 landed on
# exactly that hole — a blob-mode-specific fix whose new tests passed on the
# author's machine (fresh blob) and failed on ubuntu and windows, because
# `tests/harness_test.rs` ran in no blob-mode job anywhere.
#
# What this does instead
# ----------------------
# The target list is DERIVED FROM THE TREE, not maintained by hand. A target
# is prelude-mode-sensitive when either:
#
#   * it spawns the `eu` binary (`CARGO_BIN_EXE_eu`) — the binary is the only
#     caller of `driver::eval::maybe_load_prelude_blob` besides the in-process
#     tester, so spawning it makes the prelude mode a genuine input; or
#   * it is gated on `cfg(prelude_blob_ok)` — those assertions do not merely
#     go untested without a blob, they cease to exist.
#
# A new test target that spawns `eu` is therefore swept from its first CI run,
# with no workflow edit. `tests/gate_liveness.rs` models the same two markers
# and fails if they drift out of step with this script.
#
# Each target runs WHOLE, through `scripts/diagnostics-gate.sh`, which brings
# its ratchet: a known blob-mode failure is named here with its bead, is
# skipped in the whole-target run, and must STILL FAIL on its own — so fixing
# it turns this job red until the line is deleted. Going red on a fix is the
# mechanism working.
#
# Usage: blob-mode-tests.sh
# Requires: a fresh `lib/prelude.blob` (run `cargo xtask prelude-compile`).
set -uo pipefail

cd "$(dirname "$0")/.." || exit 1

# Known blob-mode failures, ratcheted. One entry per known-failing test:
#
#     "<target> <test>:<bead>"
#
# The named test is skipped in its target's whole-target run and then required
# to still fail on its own. Deleting an entry is the only correct response to
# one starting to pass. The list is EMPTY today: every mode-sensitive target
# passes whole under a blob on master.
#
# A plain array rather than `declare -A` so the script runs under the bash 3.2
# that ships with macOS.
KNOWN_FAILURES=()

# ── Discovery ────────────────────────────────────────────────────────────────
# Kept in step with `SWEEP_MARKERS` in tests/gate_liveness.rs.
targets=()
for src in tests/*.rs; do
  if grep -q -e 'CARGO_BIN_EXE_eu' -e 'cfg(prelude_blob_ok)' "$src"; then
    targets+=("$(basename "$src" .rs)")
  fi
done

# A broken glob or a moved directory must not quietly sweep nothing. The count
# is a floor, not the exact number, so adding a target does not require editing
# this line — only a collapse does.
if [ "${#targets[@]}" -lt 10 ]; then
  echo "::error::discovered only ${#targets[@]} mode-sensitive test target(s) — the scan is broken, and a broken scan sweeps nothing"
  exit 1
fi

echo "=== ${#targets[@]} prelude-mode-sensitive target(s) to run under the blob ==="
printf '  %s\n' "${targets[@]}"
echo

rc=0
failed=()
for target in "${targets[@]}"; do
  excl=()
  for entry in ${KNOWN_FAILURES[@]+"${KNOWN_FAILURES[@]}"}; do
    case "$entry" in
      "$target "*) excl+=("${entry#* }") ;;
    esac
  done
  if ! scripts/diagnostics-gate.sh "$target" "${excl[@]+"${excl[@]}"}"; then
    failed+=("$target")
    rc=1
  fi
  echo
done

if [ "$rc" -ne 0 ]; then
  echo "::error::blob-mode failures in: ${failed[*]}"
  echo "::error::These targets pass without a prelude blob and fail with one (or vice versa)."
  echo "::error::Reproduce locally with: cargo run --package xtask -- prelude-compile && cargo test --test <target>"
fi
exit $rc
