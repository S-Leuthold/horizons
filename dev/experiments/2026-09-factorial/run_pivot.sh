#!/usr/bin/env bash
## Launch the P-versus-M pivot with the memory watchdog.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_pivot.sh [clay oc ph] [--workers=N] > /dev/null 2>&1 &
##
## Logs: dev/experiments/2026-09-factorial/results/pivot/logs/
##   pivot.log    - the R script's output
##   watchdog.log - MemAvailable every 10 s; kills everything below 15 GB

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/pivot/logs
mkdir -p "$LOGS"

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

stamp() { date '+%Y-%m-%d %H:%M:%S'; }
echo "$(stamp) === START pivot | args: $* ===" >> "$LOGS/pivot.log"
Rscript "$E2/03-pivot-pm.R" "$@" >> "$LOGS/pivot.log" 2>&1
rc=$?
echo "$(stamp) === END pivot (exit $rc) ===" >> "$LOGS/pivot.log"
exit $rc
