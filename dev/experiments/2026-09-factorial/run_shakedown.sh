#!/usr/bin/env bash
## Launch the 2 cm-1 parallelism shakedown with the memory watchdog and an
## RSS sampler, all three properties sequentially.
##
## Usage (from package/):
##   nohup bash dev/experiments/2026-09-factorial/run_shakedown.sh [clay oc ph] > /dev/null 2>&1 &
##
## Logs: dev/experiments/2026-09-factorial/results/shakedown/logs/
##   shakedown.log  - the R script's output
##   watchdog.log   - MemAvailable every 10 s; kills everything below 15 GB
##   rss.log        - every 10 s: n workers, total worker RSS (MB), max single
##                    worker RSS (MB), main Rscript RSS (MB)

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E1=dev/experiments/2026-09-local-strategy
E2=dev/experiments/2026-09-factorial
LOGS=$PWD/$E2/results/shakedown/logs
mkdir -p "$LOGS"

PROPS="${*:-clay oc ph}"

## Match the harness watchdog specifically: a stale scratch watchdog with a
## similar name must not stop this one from starting.
pgrep -f "$E1/watchdog.sh" >/dev/null || nohup bash "$E1/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

## RSS sampler: exits on its own once the shakedown Rscript is gone.
(
  while pgrep -f '[-]-file=dev/experiments/2026-09-factorial/00-shakedown.R' >/dev/null; do
    main=$(ps -o rss= -p "$(pgrep -f '[-]-file=dev/experiments/2026-09-factorial/00-shakedown.R' | head -1)" 2>/dev/null | awk '{printf "%d", $1/1024}')
    ps -o rss= -C R 2>/dev/null | awk -v main="${main:-0}" -v ts="$(date -Iseconds)" \
      'BEGIN{n=0;s=0;m=0} {n++; s+=$1; if($1>m)m=$1} END{printf "%s workers=%d rss_total_mb=%d rss_max_mb=%d main_mb=%d\n", ts, n, s/1024, m/1024, main}' >> "$LOGS/rss.log"
    sleep 10
  done
) &

stamp() { date '+%Y-%m-%d %H:%M:%S'; }
echo "$(stamp) === START shakedown | props: $PROPS ===" >> "$LOGS/shakedown.log"
Rscript "$E2/00-shakedown.R" $PROPS >> "$LOGS/shakedown.log" 2>&1
rc=$?
echo "$(stamp) === END shakedown (exit $rc) ===" >> "$LOGS/shakedown.log"
exit $rc
