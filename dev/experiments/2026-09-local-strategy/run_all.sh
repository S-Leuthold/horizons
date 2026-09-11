#!/usr/bin/env bash
## Full local-strategy experiment, run sequentially with the memory watchdog.
##
## Order: A(all properties) -> B+E(all) -> C(all) -> D(all) -> collect.
## Sequential by design: after the 2026-09-11 memory incident nothing large
## runs concurrently on this box. Every (property x strategy) checkpoints, so
## a kill or crash resumes by re-running this script.
##
## Usage (from package/):  nohup bash dev/experiments/2026-09-local-strategy/run_all.sh > .../run_all.log 2>&1 &

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E=dev/experiments/2026-09-local-strategy
LOGS=$PWD/$E/results/logs
mkdir -p "$LOGS"

export R_PARALLELLY_MAXWORKERS_LOCALHOST=Inf
PROPS="clay oc ph"
W=8

pgrep -f 'watchdog.sh' >/dev/null || nohup bash "$E/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

stamp() { date '+%Y-%m-%d %H:%M:%S'; }
run() {   # run <label> <script> <args...>
  local label=$1; shift
  echo "$(stamp) === START $label ==="
  Rscript "$@" > "$LOGS/$label.log" 2>&1
  local rc=$?
  echo "$(stamp) === END   $label (exit $rc) ==="
  if [ $rc -ne 0 ]; then
    echo "$(stamp) !!! $label failed — see $LOGS/$label.log; continuing to the next stage"
    tail -20 "$LOGS/$label.log"
  fi
}

echo "$(stamp) experiment start | props: $PROPS | workers: $W"

for p in $PROPS; do run "A-$p" "$E/03-A-global.R"       "$p" "--workers=$W"; done
for p in $PROPS; do run "B-$p" "$E/04-B-gmm-local.R"    "$p" "--workers=$W"; done
for p in $PROPS; do run "C-$p" "$E/05-C-gmm-perconfig.R" "$p" "--workers=$W"; done
for p in $PROPS; do run "D-$p" "$E/06-D-mbl.R"          "$p"; done

run "collect" "$E/07-collect.R"

echo "$(stamp) experiment done"
echo "--- verdict ---"
cat "$PWD/$E/results/verdict.md" 2>/dev/null
