#!/usr/bin/env bash
## Launch the locality curve with the memory watchdog.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_locality.sh [clay oc ph] [--workers=N] > /dev/null 2>&1 &
##
## Logs: dev/experiments/2026-09-factorial/results/locality-curve/logs/

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/locality-curve/logs
mkdir -p "$LOGS"

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

stamp() { date '+%Y-%m-%d %H:%M:%S'; }
echo "$(stamp) === START locality-curve | args: $* ===" >> "$LOGS/locality-curve.log"
Rscript "$E2/04-locality-curve.R" "$@" >> "$LOGS/locality-curve.log" 2>&1
rc=$?
echo "$(stamp) === END locality-curve (exit $rc) ===" >> "$LOGS/locality-curve.log"
exit $rc
