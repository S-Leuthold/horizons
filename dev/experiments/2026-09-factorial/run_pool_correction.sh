#!/usr/bin/env bash
## Launch 15-pool-correction.R with the memory watchdog, under /usr/bin/time -v.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_pool_correction.sh [--workers=N] [--k=100,400] [--dry] > /dev/null 2>&1 &

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/pool-correction/logs
mkdir -p "$LOGS"
stamp() { date '+%Y-%m-%d %H:%M:%S'; }

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

echo "$(stamp) === START 15-pool-correction | args: $* ===" >> "$LOGS/chain.log"
/usr/bin/time -v Rscript "$E2/15-pool-correction.R" "$@" >> "$LOGS/15-pool-correction.log" 2>&1
rc=$?
echo "$(stamp) === END 15-pool-correction (exit $rc) ===" >> "$LOGS/chain.log"
grep -E "Maximum resident set size|Elapsed \(wall clock\)" "$LOGS/15-pool-correction.log" | tail -2 >> "$LOGS/chain.log"
exit $rc
