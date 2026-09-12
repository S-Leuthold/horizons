#!/usr/bin/env bash
## Resume the local-strategy experiment after the 2026-09-11 overnight run.
##
## That run completed A-clay, A-ph and D for all three properties. It lost:
##   A-oc  — watchdog kill at 18:21 (MemAvailable fell to 12 GB with 8 workers
##           on oc's 24,903 training rows; oc is the largest property)
##   B     — mclust::Mclust() cannot find mclustBIC unless mclust is attached
##   C, E  — skipped, they depend on B
##   collect — two bugs in 07-collect.R (vectorised switch, tibble self-reference)
## All four are fixed. Checkpoints make completed cells skip automatically, so
## this re-runs only what is missing.
##
## Usage (from package/): nohup bash dev/experiments/2026-09-local-strategy/run_resume.sh > .../run_resume.log 2>&1 &

set -u
cd "$(dirname "$0")/../../.." || exit 1          # package/
E=dev/experiments/2026-09-local-strategy
LOGS=$PWD/$E/results/logs
mkdir -p "$LOGS"

export R_PARALLELLY_MAXWORKERS_LOCALHOST=Inf

pgrep -f 'watchdog.sh' >/dev/null || nohup bash "$E/watchdog.sh" 15 "$LOGS/watchdog.log" >/dev/null 2>&1 &

stamp() { date '+%Y-%m-%d %H:%M:%S'; }
run() {
  local label=$1; shift
  echo "$(stamp) === START $label ==="
  Rscript "$@" > "$LOGS/$label.log" 2>&1
  local rc=$?
  echo "$(stamp) === END   $label (exit $rc) ==="
  if [ $rc -ne 0 ]; then
    echo "$(stamp) !!! $label failed — see $LOGS/$label.log"
    tail -15 "$LOGS/$label.log"
  fi
}

echo "$(stamp) resume start"

## oc is the largest property (24,903 train rows after the internal split) and
## is what tripped the 15 GB floor at 8 workers. 5 workers keeps the same
## per-worker footprint well inside the margin; it only changes wall-clock.
run "A-oc" "$E/03-A-global.R" oc --workers=5

for p in clay ph oc; do run "B-$p" "$E/04-B-gmm-local.R"     "$p" --workers=6; done
for p in clay ph oc; do run "C-$p" "$E/05-C-gmm-perconfig.R" "$p" --workers=6; done

run "collect" "$E/07-collect.R"

echo "$(stamp) resume done"
echo "--- verdict ---"
cat "$PWD/$E/results/verdict.md" 2>/dev/null
