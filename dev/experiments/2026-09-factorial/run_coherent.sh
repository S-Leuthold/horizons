#!/usr/bin/env bash
## Launch the coherent-batch experiment (05) with the memory watchdog.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_coherent.sh [--workers=N] [--arms=G,B,C,M] [--batches=moys,kssl] [--k=100,200,400,800] [--dry] > /dev/null 2>&1 &
##
## Logs: dev/experiments/2026-09-factorial/results/coherent-batch/logs/
##   coherent.log  - the R script's output
##   watchdog.log  - MemAvailable every 10 s; kills everything below 15 GB

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/coherent-batch/logs
mkdir -p "$LOGS"

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

stamp() { date '+%Y-%m-%d %H:%M:%S'; }
echo "$(stamp) === START coherent | args: $* ===" >> "$LOGS/coherent.log"
Rscript "$E2/05-coherent-batch.R" "$@" >> "$LOGS/coherent.log" 2>&1
rc=$?
echo "$(stamp) === END coherent (exit $rc) ===" >> "$LOGS/coherent.log"
exit $rc
