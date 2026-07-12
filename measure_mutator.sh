#!/bin/bash
# Interleaved three-mode VM-Mutator measurement (eu-2sa6.9 pre-decode spike).
# Uses the -S "VM Mutator" figure (eval-only, excludes parse/compile/render) —
# the precise metric the 07-12 spike used for sub-200ms cases. Modes interleaved
# as triples each round: bc-predecode, bc-normal, hs.
set -u
EU=/tmp/eu-predecode/target/release/eu
ROUNDS=${ROUNDS:-11}
HEAP="--heap-limit-mib 12288"

CASES=(
  "drop_cons|.|tests/harness/bench/005_drop_cons.eu|bench-drop-cons"
  "naive_fib|.|tests/harness/bench/001_naive_fib.eu|bench-naive-fib"
  "short_lived|.|tests/harness/bench/007_short_lived.eu|bench-short-lived"
  "aoc25_day03_p2|examples/aoc25|day03.eu|part-2"
)

median() { printf '%s\n' "$@" | sort -n | awk '{a[NR]=$1} END{if(NR%2){print a[(NR+1)/2]}else{printf "%.6f\n",(a[NR/2]+a[NR/2+1])/2}}'; }
minmax() { printf '%s\n' "$@" | sort -n | awk 'NR==1{min=$1}{max=$1}END{printf "%.6f/%.6f",min,max}'; }

# extract the VM-section Mutator seconds
mutator() { # env dir file target
  ( cd "$2" && env $1 timeout 600 $EU $HEAP -S "$3" -t "$4" 2>&1 ) \
    | awk '/Mutator/{v=$3; gsub(/s$/,"",v); print v; exit}'
}

echo "load at start: $(uptime | sed 's/.*load aver[a-z]*: //')"
echo "rounds: $ROUNDS   metric: VM Mutator (s), eval-only"
echo
for spec in "${CASES[@]}"; do
  IFS='|' read -r label dir file tgt <<< "$spec"
  declare -a PD=() BC=() HS=()
  for r in $(seq 1 $ROUNDS); do
    PD+=( "$(mutator EU_PREDECODE=1 "$dir" "$file" "$tgt")" )
    BC+=( "$(mutator '' "$dir" "$file" "$tgt")" )
    HS+=( "$(mutator EU_HEAPSYN=1 "$dir" "$file" "$tgt")" )
  done
  pd=$(median "${PD[@]}"); bc=$(median "${BC[@]}"); hs=$(median "${HS[@]}")
  echo "== $label =="
  echo "  bc-predecode: med=${pd}s  min/max=$(minmax "${PD[@]}")"
  echo "  bc-normal   : med=${bc}s  min/max=$(minmax "${BC[@]}")"
  echo "  hs          : med=${hs}s  min/max=$(minmax "${HS[@]}")"
  awk "BEGIN{printf \"  RATIO  bc-predecode/hs = %.3f   bc-normal/hs = %.3f   predecode/normal = %.3f\n\", $pd/$hs, $bc/$hs, $pd/$bc}"
  echo
done
echo "load at end: $(uptime | sed 's/.*load aver[a-z]*: //')"
