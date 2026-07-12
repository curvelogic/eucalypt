#!/bin/bash
# Interleaved three-mode measurement for the pre-decode spike (eu-2sa6.9).
# Modes per case, interleaved as triples each round: bc-predecode, bc-normal, hs.
# Wall time via /usr/bin/time -p (real), timeout 600, --heap-limit-mib 12288.
set -u
EU=/tmp/eu-predecode/target/release/eu
ROUNDS=${ROUNDS:-11}
HEAP="--heap-limit-mib 12288"

# case spec: label|dir|file|target
CASES=(
  "drop_cons|.|tests/harness/bench/005_drop_cons.eu|bench-drop-cons"
  "naive_fib|.|tests/harness/bench/001_naive_fib.eu|bench-naive-fib"
  "short_lived|.|tests/harness/bench/007_short_lived.eu|bench-short-lived"
  "aoc25_day03_p2|examples/aoc25|day03.eu|part-2"
)

median() { printf '%s\n' "$@" | sort -n | awk '{a[NR]=$1} END{if(NR%2){print a[(NR+1)/2]}else{printf "%.4f\n",(a[NR/2]+a[NR/2+1])/2}}'; }
spread() { printf '%s\n' "$@" | sort -n | awk 'NR==1{min=$1} {max=$1} END{printf "%.4f/%.4f",min,max}'; }

runwall() { # env-prefix dir file target
  local envp="$1" dir="$2" file="$3" tgt="$4"
  ( cd "$dir" && env $envp /usr/bin/time -p timeout 600 $EU $HEAP "$file" -t "$tgt" >/dev/null 2>/tmp/pd_time.txt )
  grep '^real' /tmp/pd_time.txt | awk '{print $2}'
}

echo "load at start: $(uptime | sed 's/.*load averages*: //')"
echo "rounds: $ROUNDS"
echo

for spec in "${CASES[@]}"; do
  IFS='|' read -r label dir file tgt <<< "$spec"
  declare -a PD=() BC=() HS=()
  for r in $(seq 1 $ROUNDS); do
    PD+=( "$(runwall EU_PREDECODE=1 "$dir" "$file" "$tgt")" )
    BC+=( "$(runwall '' "$dir" "$file" "$tgt")" )
    HS+=( "$(runwall EU_HEAPSYN=1 "$dir" "$file" "$tgt")" )
  done
  pd_med=$(median "${PD[@]}"); bc_med=$(median "${BC[@]}"); hs_med=$(median "${HS[@]}")
  pd_sp=$(spread "${PD[@]}"); bc_sp=$(spread "${BC[@]}"); hs_sp=$(spread "${HS[@]}")
  # ticks (deterministic; one capture each)
  pd_tk=$(cd "$dir" && EU_PREDECODE=1 timeout 600 $EU $HEAP -S "$file" -t "$tgt" 2>&1 | grep Ticks | grep -oE '[0-9,]+')
  bc_tk=$(cd "$dir" && timeout 600 $EU $HEAP -S "$file" -t "$tgt" 2>&1 | grep Ticks | grep -oE '[0-9,]+')
  hs_tk=$(cd "$dir" && EU_HEAPSYN=1 timeout 600 $EU $HEAP -S "$file" -t "$tgt" 2>&1 | grep Ticks | grep -oE '[0-9,]+')
  ratio_pd=$(awk "BEGIN{printf \"%.3f\", $pd_med/$hs_med}")
  ratio_bc=$(awk "BEGIN{printf \"%.3f\", $bc_med/$hs_med}")
  echo "== $label =="
  echo "  bc-predecode: med=$pd_med  spread(min/max)=$pd_sp  ticks=$pd_tk"
  echo "  bc-normal   : med=$bc_med  spread(min/max)=$bc_sp  ticks=$bc_tk"
  echo "  hs          : med=$hs_med  spread(min/max)=$hs_sp  ticks=$hs_tk"
  echo "  RATIO bc-predecode/hs = $ratio_pd    bc-normal/hs = $ratio_bc"
  echo
done
echo "load at end: $(uptime | sed 's/.*load averages*: //')"
