#!/usr/bin/env bash
#
# Run a diagnostics test target as a WHOLE target, with a ratcheted exclusion
# list instead of a hand-picked allowlist (eu-oxtcq mechanism 3).
#
# Why this exists
# ---------------
# The blob-mode CI job used to name the individual tests it wanted:
#
#     cargo test --test diagnostics_invariants corpus_satisfies_invariants -- --exact
#
# Two things are wrong with that. A test added to the target is excluded by
# omission, silently — the comment beside it said "widen as each lands" and
# nothing enforced it. And a filter that matches nothing exits 0:
#
#     $ cargo test --test diagnostics_invariants no_such_test -- --exact
#     test result: ok. 0 passed; 0 failed; 3 filtered out
#     $ echo $?
#     0
#
# so renaming a test silently removes it from CI while the job stays green.
#
# What this does instead
# ----------------------
# Runs the whole target with `--skip` for each known failure, then runs each
# known failure ON ITS OWN and requires it to STILL FAIL. That inverts the
# default: a new test is included automatically, and a known failure that gets
# fixed turns this job RED until it is deleted from the list below. Going red
# on a fix is the mechanism working, not a fault in it — the fix is to remove
# the line, never to loosen the check.
#
# Usage: diagnostics-gate.sh <target> [<test>:<bead> ...]
set -uo pipefail

target="${1:?usage: diagnostics-gate.sh <target> [<test>:<bead> ...]}"
shift

skips=()
for entry in "$@"; do
  skips+=(--skip "${entry%%:*}")
done

echo "=== ${target}: whole target, excluding ${#@} known failure(s) ==="
output=$(cargo test --test "$target" -- --include-ignored "${skips[@]+"${skips[@]}"}" 2>&1)
status=$?
echo "$output"
if [ "$status" -ne 0 ]; then
  echo "::error::${target} failed under this configuration"
  exit 1
fi

# A target that contributes zero tests reports "ok. 0 passed" and exit 0.
# Under a blob every target here must run something; see tests/gate_liveness.rs
# for the general form of this check.
if ! echo "$output" | grep -qE 'test result: ok\. [1-9][0-9]* passed'; then
  echo "::error::${target} ran no tests — a cfg-gated file compiling to zero tests reports green (eu-oxtcq mechanism 1)"
  exit 1
fi

rc=0
for entry in "$@"; do
  name="${entry%%:*}"
  bead="${entry##*:}"
  echo "=== ratchet: ${target}::${name} must still fail (${bead}) ==="
  if cargo test --test "$target" "$name" -- --exact --include-ignored >/dev/null 2>&1; then
    echo "::error::${target}::${name} now PASSES (or no longer exists) under this configuration."
    echo "::error::Remove it from the exclusion list in .github/workflows/build-rust.yaml so the gate guards it — ${bead}."
    rc=1
  else
    echo "still failing, as recorded (${bead})"
  fi
done
exit $rc
