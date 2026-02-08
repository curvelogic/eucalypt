#!/usr/bin/env bash
#
# gc-bench.sh — GC performance benchmarking script
#
# Usage:
#   scripts/gc-bench.sh baseline [--runs N] [--heap-limit MIB]
#   scripts/gc-bench.sh compare  [--runs N] [--heap-limit MIB] [--threshold PCT]
#
# In baseline mode, runs all GC stress benchmarks and saves results
# to gc-bench-baseline.json.
#
# In compare mode, runs the same benchmarks and compares against
# the saved baseline, flagging regressions above the threshold.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Defaults
RUNS=5
HEAP_LIMIT=""
THRESHOLD=5
BASELINE_FILE="$PROJECT_DIR/gc-bench-baseline.json"
EU_BIN="${EU_BIN:-$PROJECT_DIR/target/release/eu}"

# Benchmark programs: file and target name
BENCHMARKS=(
    "harness/test/bench/007_short_lived.eu:bench-short-lived"
    "harness/test/bench/008_long_lived_graph.eu:bench-long-lived-graph"
    "harness/test/bench/009_fragmentation.eu:bench-fragmentation"
)

usage() {
    echo "Usage: $0 {baseline|compare} [OPTIONS]"
    echo
    echo "Options:"
    echo "  --runs N         Number of runs per benchmark (default: $RUNS)"
    echo "  --heap-limit MIB Heap limit in MiB (default: no limit)"
    echo "  --threshold PCT  Regression threshold percentage (default: $THRESHOLD)"
    echo
    echo "Environment:"
    echo "  EU_BIN            Path to eu binary (default: target/release/eu)"
    exit 1
}

# Parse arguments
MODE="${1:-}"
shift || true

while [[ $# -gt 0 ]]; do
    case "$1" in
        --runs) RUNS="$2"; shift 2 ;;
        --heap-limit) HEAP_LIMIT="$2"; shift 2 ;;
        --threshold) THRESHOLD="$2"; shift 2 ;;
        *) usage ;;
    esac
done

if [[ -z "$MODE" ]] || [[ "$MODE" != "baseline" && "$MODE" != "compare" ]]; then
    usage
fi

# Check eu binary exists
if [[ ! -x "$EU_BIN" ]]; then
    echo "Error: eu binary not found at $EU_BIN"
    echo "Build with: cargo build --release"
    exit 1
fi

# Extract a numeric field from a JSON stats file using sed (no jq dependency)
json_field() {
    local file="$1"
    local field="$2"
    # Extract the value after "field": from JSON
    sed -n "s/.*\"${field}\": *\([0-9.e+-]*\).*/\1/p" "$file" | head -1
}

# Compute median of a sorted array of numbers (using awk)
median() {
    local -a values=("$@")
    local n=${#values[@]}
    if (( n == 0 )); then
        echo "0"
        return
    fi
    # Sort values
    local -a sorted
    IFS=$'\n' sorted=($(printf '%s\n' "${values[@]}" | sort -g))
    unset IFS
    local mid=$(( n / 2 ))
    if (( n % 2 == 1 )); then
        echo "${sorted[$mid]}"
    else
        # Average of two middle values
        echo "${sorted[$mid-1]}" "${sorted[$mid]}" | awk '{printf "%.9f", ($1 + $2) / 2}'
    fi
}

# Run all benchmarks and collect median statistics
run_benchmarks() {
    local output_file="$1"
    local tmpdir
    tmpdir=$(mktemp -d)
    trap "rm -rf $tmpdir" RETURN

    echo "{"
    local first=true

    for bench_spec in "${BENCHMARKS[@]}"; do
        local file="${bench_spec%%:*}"
        local target="${bench_spec##*:}"
        local bench_path="$PROJECT_DIR/$file"

        if [[ ! -f "$bench_path" ]]; then
            echo "Warning: benchmark file $bench_path not found, skipping" >&2
            continue
        fi

        if [[ "$first" != "true" ]]; then
            echo ","
        fi
        first=false

        echo "Benchmark: $target" >&2

        # Collect metrics across runs
        local -a ticks_arr=()
        local -a allocs_arr=()
        local -a collections_arr=()
        local -a mark_time_arr=()
        local -a sweep_time_arr=()
        local -a peak_blocks_arr=()

        for (( run=1; run<=RUNS; run++ )); do
            local stats_file="$tmpdir/${target}_run${run}.json"
            echo -n "    Run $run/$RUNS..." >&2

            local -a eu_args=(--statistics-file "$stats_file")
            if [[ -n "$HEAP_LIMIT" ]]; then
                eu_args+=(--heap-limit-mib "$HEAP_LIMIT")
            fi
            eu_args+=(-t "$target" "$bench_path")

            "$EU_BIN" "${eu_args[@]}" > /dev/null 2>&1 || {
                    echo " FAILED" >&2
                    continue
                }
            echo " done" >&2

            ticks_arr+=("$(json_field "$stats_file" "machine_ticks")")
            allocs_arr+=("$(json_field "$stats_file" "machine_allocs")")
            collections_arr+=("$(json_field "$stats_file" "collections_count")")
            mark_time_arr+=("$(json_field "$stats_file" "total_mark_time_secs")")
            sweep_time_arr+=("$(json_field "$stats_file" "total_sweep_time_secs")")
            peak_blocks_arr+=("$(json_field "$stats_file" "peak_heap_blocks")")
        done

        # Compute medians
        local med_ticks med_allocs med_collections med_mark med_sweep med_peak
        med_ticks=$(median "${ticks_arr[@]}")
        med_allocs=$(median "${allocs_arr[@]}")
        med_collections=$(median "${collections_arr[@]}")
        med_mark=$(median "${mark_time_arr[@]}")
        med_sweep=$(median "${sweep_time_arr[@]}")
        med_peak=$(median "${peak_blocks_arr[@]}")

        printf '  "%s": {\n' "$target"
        printf '    "machine_ticks": %s,\n' "$med_ticks"
        printf '    "machine_allocs": %s,\n' "$med_allocs"
        printf '    "collections_count": %s,\n' "$med_collections"
        printf '    "total_mark_time_secs": %s,\n' "$med_mark"
        printf '    "total_sweep_time_secs": %s,\n' "$med_sweep"
        printf '    "peak_heap_blocks": %s\n' "$med_peak"
        printf '  }'
    done

    echo
    echo "}"
}

# Compare two values and report percentage change
pct_change() {
    local baseline="$1"
    local current="$2"
    if awk "BEGIN { exit !($baseline == 0) }"; then
        echo "0.0"
    else
        awk "BEGIN { printf \"%.1f\", (($current - $baseline) / $baseline) * 100 }"
    fi
}

case "$MODE" in
    baseline)
        echo "=== GC Benchmark: Baseline ===" >&2
        echo "Runs: $RUNS${HEAP_LIMIT:+, Heap limit: ${HEAP_LIMIT} MiB}" >&2
        echo >&2

        run_benchmarks "$BASELINE_FILE" > "$BASELINE_FILE"

        echo >&2
        echo "Baseline saved to $BASELINE_FILE" >&2
        ;;

    compare)
        if [[ ! -f "$BASELINE_FILE" ]]; then
            echo "Error: baseline file $BASELINE_FILE not found."
            echo "Run '$0 baseline' first."
            exit 1
        fi

        echo "=== GC Benchmark: Compare ===" >&2
        echo "Runs: $RUNS${HEAP_LIMIT:+, Heap limit: ${HEAP_LIMIT} MiB}, Threshold: ${THRESHOLD}%" >&2
        echo >&2

        COMPARE_FILE=$(mktemp)
        trap "rm -f $COMPARE_FILE" EXIT

        run_benchmarks "$COMPARE_FILE" > "$COMPARE_FILE"

        echo >&2
        echo "=== Results ===" >&2
        echo >&2

        REGRESSIONS=0
        METRICS=("machine_ticks" "machine_allocs" "collections_count" "total_mark_time_secs" "total_sweep_time_secs" "peak_heap_blocks")

        for bench_spec in "${BENCHMARKS[@]}"; do
            target="${bench_spec##*:}"
            echo "--- $target ---"

            for metric in "${METRICS[@]}"; do
                base_val=$(sed -n "/\"$target\"/,/}/s/.*\"$metric\": *\([0-9.e+-]*\).*/\1/p" "$BASELINE_FILE" | head -1)
                cur_val=$(sed -n "/\"$target\"/,/}/s/.*\"$metric\": *\([0-9.e+-]*\).*/\1/p" "$COMPARE_FILE" | head -1)

                if [[ -z "$base_val" ]] || [[ -z "$cur_val" ]]; then
                    continue
                fi

                change=$(pct_change "$base_val" "$cur_val")
                flag=""
                if awk "BEGIN { exit !($change > $THRESHOLD) }"; then
                    flag=" ** REGRESSION **"
                    REGRESSIONS=$((REGRESSIONS + 1))
                fi
                printf "  %-30s baseline=%-12s current=%-12s change=%s%%%s\n" \
                    "$metric" "$base_val" "$cur_val" "$change" "$flag"
            done
            echo
        done

        if (( REGRESSIONS > 0 )); then
            echo "FAIL: $REGRESSIONS regression(s) detected (>${THRESHOLD}% increase)"
            exit 1
        else
            echo "PASS: No regressions detected"
        fi
        ;;
esac
