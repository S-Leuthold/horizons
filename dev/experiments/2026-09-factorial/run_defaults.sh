#!/usr/bin/env bash
## Launch 16-defaults.R with the memory watchdog, under /usr/bin/time -v.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_defaults.sh [--workers=N] [--batches=a,b] [--dry] > /dev/null 2>&1 &

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/defaults/logs
mkdir -p "$LOGS"
stamp() { date '+%Y-%m-%d %H:%M:%S'; }

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

echo "$(stamp) === START 16-defaults | args: $* ===" >> "$LOGS/chain.log"
/usr/bin/time -v Rscript "$E2/16-defaults.R" "$@" >> "$LOGS/16-defaults.log" 2>&1
rc=$?
echo "$(stamp) === END 16-defaults (exit $rc) ===" >> "$LOGS/chain.log"
grep -E "Maximum resident set size|Elapsed \(wall clock\)" "$LOGS/16-defaults.log" | tail -2 >> "$LOGS/chain.log"
exit $rc
