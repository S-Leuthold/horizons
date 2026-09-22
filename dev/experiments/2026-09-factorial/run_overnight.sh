#!/usr/bin/env bash
## The overnight chain, 2026-09-21: 12 (memory premise) -> 13 (thinning
## competition) -> 14 (bias correction), each into its own results dir with
## its own log, under the memory watchdog. Waits first for 11-metric-followup
## to exit. A failure in one script does not stop the next.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_overnight.sh [--workers=N] > /dev/null 2>&1 &

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/overnight-logs
mkdir -p "$LOGS"
stamp() { date '+%Y-%m-%d %H:%M:%S'; }

while pgrep -f "Rscript.*11-metric-followup" >/dev/null; do
  echo "$(stamp) waiting for 11-metric-followup.R" >> "$LOGS/chain.log"
  sleep 120
done

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

run_one() {
  local script=$1; shift
  local name=$(basename "$script" .R)
  echo "$(stamp) === START $name | args: $* ===" >> "$LOGS/chain.log"
  /usr/bin/time -v Rscript "$script" "$@" >> "$LOGS/$name.log" 2>&1
  local rc=$?
  echo "$(stamp) === END $name (exit $rc) ===" >> "$LOGS/chain.log"
  grep -E "Maximum resident set size|Elapsed \(wall clock\)" "$LOGS/$name.log" | tail -2 >> "$LOGS/chain.log"
  df -h / | tail -1 >> "$LOGS/chain.log"
}

## 12 is sequential and single-core by design (it measures one process's peak
## RSS), so it runs beside 13 rather than ahead of it; 13 and 14 share the
## workers and run one after the other.
run_one "$E2/12-memory-premise.R" &
PID12=$!
run_one "$E2/13-thinning.R" "$@"
run_one "$E2/14-bias-correction.R" "$@"
wait $PID12
echo "$(stamp) === CHAIN DONE ===" >> "$LOGS/chain.log"
