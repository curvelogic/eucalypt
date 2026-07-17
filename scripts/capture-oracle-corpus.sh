#!/usr/bin/env bash
# Prototype capture script for eu-2sa6.14 (Phase-4 oracle-replacement plan).
#
# For each standard (non-IO, non-error) harness test source, evaluates it
# under both engines (default bytecode, and EU_HEAPSYN=1), confirms
# byte-identical stdout/stderr/exit code, and — only if identical — freezes
# the bytecode stdout as a golden snapshot.
#
# This is a throwaway prototype proving the mechanism and producing real
# size/mismatch numbers for the design doc; the production version is
# `cargo xtask oracle-corpus-capture` per the design doc's follow-up phase.
#
# Usage: scripts/capture-oracle-corpus.sh <eu-binary> <out-dir>
set -euo pipefail

EU_BIN="${1:?usage: capture-oracle-corpus.sh <eu-binary> <out-dir>}"
OUT_DIR="${2:?usage: capture-oracle-corpus.sh <eu-binary> <out-dir>}"
LIB_PATH="tests/harness"
HEAP_LIMIT=2048

mkdir -p "$OUT_DIR"

total=0
captured=0
mismatched=0
skipped_io=0
> "$OUT_DIR/MANIFEST.tsv"
> "$OUT_DIR/MISMATCHES.txt"

# Files that need --allow-io are not special-cased: without the flag, IO
# intrinsics refuse deterministically on both engines, so they still capture
# cleanly (as an "IO disabled" refusal). Genuine IO differential coverage is
# tests/bytecode_io_differential_test.rs, out of scope for this prototype.
for f in tests/harness/*.eu; do
    name=$(basename "$f" .eu)
    total=$((total+1))

    set +e
    bc_out=$(timeout 60 "$EU_BIN" --heap-limit-mib "$HEAP_LIMIT" --lib-path "$LIB_PATH" "$f" 2>/tmp/bc_err.$$)
    bc_code=$?
    set -e
    bc_err=$(cat /tmp/bc_err.$$); rm -f /tmp/bc_err.$$

    set +e
    hs_out=$(EU_HEAPSYN=1 timeout 60 "$EU_BIN" --heap-limit-mib "$HEAP_LIMIT" --lib-path "$LIB_PATH" "$f" 2>/tmp/hs_err.$$)
    hs_code=$?
    set -e
    hs_err=$(cat /tmp/hs_err.$$); rm -f /tmp/hs_err.$$

    if [ "$bc_out" = "$hs_out" ] && [ "$bc_err" = "$hs_err" ] && [ "$bc_code" = "$hs_code" ]; then
        printf '%s' "$bc_out" > "$OUT_DIR/$name.stdout"
        printf '%s' "$bc_err" > "$OUT_DIR/$name.stderr"
        echo "$bc_code" > "$OUT_DIR/$name.exit"
        echo -e "$name\t$bc_code\t$(printf '%s' "$bc_out" | wc -c)" >> "$OUT_DIR/MANIFEST.tsv"
        captured=$((captured+1))
    else
        echo "MISMATCH: $name (bc_exit=$bc_code hs_exit=$hs_code)" >> "$OUT_DIR/MISMATCHES.txt"
        mismatched=$((mismatched+1))
    fi
done

echo "total=$total captured=$captured mismatched=$mismatched"
