#!/usr/bin/env bash
## Launch the clay learning curve with the memory watchdog.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_learning_curve.sh [--workers=N] > /dev/null 2>&1 &
##
## Logs: dev/experiments/2026-09-factorial/results/learning-curve/logs/
##   learning-curve.log - the R script's output
##   watchdog.log       - MemAvailable every 10 s; kills everything below 15 GB

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/learning-curve/logs
mkdir -p "$LOGS"

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

stamp() { date '+%Y-%m-%d %H:%M:%S'; }
echo "$(stamp) === START learning-curve | args: $* ===" >> "$LOGS/learning-curve.log"
Rscript "$E2/01-learning-curve.R" "$@" >> "$LOGS/learning-curve.log" 2>&1
rc=$?
echo "$(stamp) === END learning-curve (exit $rc) ===" >> "$LOGS/learning-curve.log"
exit $rc
