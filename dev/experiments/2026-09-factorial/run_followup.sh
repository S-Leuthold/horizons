#!/usr/bin/env bash
## Launch the metric follow-up (11) with the memory watchdog, waiting first
## for a running 10-metric-accuracy.R to exit so the two never share the box.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_followup.sh [--workers=N] [--dry] > /dev/null 2>&1 &

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/metric-followup/logs
mkdir -p "$LOGS"
stamp() { date '+%Y-%m-%d %H:%M:%S'; }

while pgrep -f "Rscript.*10-metric-accuracy" >/dev/null; do
  echo "$(stamp) waiting for 10-metric-accuracy.R to finish" >> "$LOGS/followup.log"
  sleep 120
done

pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

echo "$(stamp) === START followup | args: $* ===" >> "$LOGS/followup.log"
Rscript "$E2/11-metric-followup.R" "$@" >> "$LOGS/followup.log" 2>&1
rc=$?
echo "$(stamp) === END followup (exit $rc) ===" >> "$LOGS/followup.log"
exit $rc
