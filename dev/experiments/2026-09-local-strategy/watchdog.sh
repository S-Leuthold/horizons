#!/usr/bin/env bash
## Memory watchdog for the local-strategy experiment.
##
## Every 10 s: log MemAvailable, load, and the number of experiment R
## processes. If MemAvailable drops below MIN_GB, kill every experiment
## Rscript and every multisession worker, log it, and exit 1. Exits 0 on its
## own once no experiment process has been seen for 2 minutes.
##
## Usage: nohup bash watchdog.sh [MIN_GB=15] [LOG=results/logs/watchdog.log] &

MIN_GB=${1:-15}
LOG=${2:-/dev/stdout}
idle=0

while true; do
  avail=$(awk '/MemAvailable/{printf "%d", $2/1024/1024}' /proc/meminfo)
  nproc_exp=$(pgrep -fc '[w]orkRSOCK|[-]-file=dev/experiments|[R]script dev/experiments')
  load=$(cut -d' ' -f1 /proc/loadavg)

  if [ "$avail" -lt "$MIN_GB" ]; then
    echo "$(date -Iseconds) WATCHDOG: MemAvailable ${avail} GB < ${MIN_GB} GB - killing experiment (${nproc_exp} procs)" >> "$LOG"
    for p in $(pgrep -f '[-]-file=dev/experiments|[R]script dev/experiments'); do kill -9 "$p" 2>/dev/null; done
    for p in $(pgrep -f '[w]orkRSOCK'); do kill -9 "$p" 2>/dev/null; done
    sleep 2
    ## callr workers (future.callr) do not match the patterns above and survive
    ## their parent's death as orphans holding their full footprint (2026-09-16:
    ## ten of them kept 53 GB after the kill). An orphan has PPID 1; a live R
    ## console or Steve's R kernel does not, so this only reaps the leftovers.
    for p in $(ps -o pid=,ppid=,args= -C R | awk '$2 == 1 && /--slave --no-save --no-restore/ {print $1}'); do kill -9 "$p" 2>/dev/null; done
    echo "$(date -Iseconds) WATCHDOG: killed; MemAvailable now $(awk '/MemAvailable/{printf "%d", $2/1024/1024}' /proc/meminfo) GB" >> "$LOG"
    exit 1
  fi

  echo "$(date -Iseconds) mem_avail_gb=${avail} load=${load} rprocs=${nproc_exp}" >> "$LOG"

  if [ "$nproc_exp" -eq 0 ]; then
    idle=$((idle + 1))
    if [ "$idle" -ge 12 ]; then
      echo "$(date -Iseconds) WATCHDOG: no experiment processes for 2 min - exiting" >> "$LOG"
      exit 0
    fi
  else
    idle=0
  fi
  sleep 10
done
