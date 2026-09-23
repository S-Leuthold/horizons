#!/usr/bin/env bash
## Launch the metric-accuracy experiment (10) with the memory watchdog.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_metric.sh [--workers=N] [--blocks=iowa,moys] [--check] [--dry] > /dev/null 2>&1 &
##
## Logs: dev/experiments/2026-09-factorial/results/metric-accuracy/logs/
##   metric.log    - the R script's output
##   watchdog.log  - MemAvailable every 10 s; kills everything below 15 GB

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/metric-accuracy/logs
mkdir -p "$LOGS"

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

stamp() { date '+%Y-%m-%d %H:%M:%S'; }
echo "$(stamp) === START metric | args: $* ===" >> "$LOGS/metric.log"
Rscript "$E2/10-metric-accuracy.R" "$@" >> "$LOGS/metric.log" 2>&1
rc=$?
echo "$(stamp) === END metric (exit $rc) ===" >> "$LOGS/metric.log"
exit $rc
