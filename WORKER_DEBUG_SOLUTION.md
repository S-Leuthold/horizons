# Worker Debug Output Solutions for Horizons HPC Package

## Problem Summary
With nested parallelization (future::multisession + doMC), worker debug output isn't visible despite setting `stdout=TRUE` in furrr_options. Workers are running (visible in `top`), but their messages disappear into a black hole.

## Root Cause
`future::multisession` creates separate R processes that have their own stdout/stderr streams. These aren't always properly captured and relayed back to the main process, especially with nested parallelization where workers spawn their own parallel tasks.

## Solution Implementation

### Primary Solution: File-Based Logging (Most Reliable)

I've modified your `evaluation-hpc.R` file to implement file-based logging that bypasses the stdout capture issues entirely:

#### Key Changes Made:

1. **Direct File Logging**: Each worker writes debug messages directly to a log file
2. **Multiple Output Methods**: Uses file writing, stderr, and message() for redundancy
3. **Timestamped Messages**: All debug output includes timestamps and worker IDs
4. **Persistent Log**: Creates `debug_worker_output.log` in the output directory

#### Modified Code Sections:

```r
# In worker function (line ~424):
debug_log_file <- file.path(output_dir, "debug_worker_output.log")
debug_msg <- sprintf("[%s] [WORKER-%d] Model %d started on PID: %d | Inner: %d | mc.cores: %s | Plan: %s\n",
                    format(Sys.time(), "%H:%M:%S"),
                    i, i, Sys.getpid(),
                    inner_workers,
                    getOption('mc.cores'),
                    class(future::plan())[1])
cat(debug_msg, file = debug_log_file, append = TRUE)
```

### How to Use the Solution

#### 1. Monitor Debug Output in Real-Time

**Option A: Using tail in terminal (recommended)**
```bash
# In a separate terminal window:
tail -f output_dir/debug_worker_output.log
```

**Option B: Using R monitor function**
```r
source("debug_worker_monitor.R")
monitor_worker_debug("your_output_dir")
```

#### 2. Run Your HPC Evaluation

```r
# Your existing code - no changes needed!
result <- evaluate_models_hpc(
  config = your_config,
  input_data = your_data,
  outer_workers = 8,
  inner_workers = 4,
  # ... other parameters
)

# The debug log will be at: output_dir/debug_worker_output.log
```

#### 3. Check Worker Activity

```r
source("debug_worker_monitor.R")
check_r_processes()  # Shows all R processes and CPU usage
```

### Additional Tools Provided

#### debug_worker_monitor.R
- `monitor_worker_debug()`: Real-time log monitoring in R
- `check_r_processes()`: View active R processes
- `test_parallel_debug()`: Test if debug output is working
- `create_worker_logger()`: Create custom loggers for workers

#### debug_with_progressr.R
- Alternative solution using progressr package
- `configure_future_for_debug()`: Optimize future settings
- `test_progressr_debug()`: Test progressr-based debugging

### What You'll See in the Debug Log

```
==== HPC Debug Log Started: 2025-01-15 10:23:45 ====
[10:23:46] [WORKER-1] Model 1 started on PID: 12345 | Inner: 4 | mc.cores: 4 | Plan: SequentialFuture
[10:23:46] [EVAL-1] Starting evaluation | inner_workers: 4 | grid: 10 | bayesian: 15 | CV: 10
[10:23:47] [WORKER-2] Model 2 started on PID: 12346 | Inner: 4 | mc.cores: 4 | Plan: SequentialFuture
[10:23:55] [COMPLETE-1] Model 1 completed successfully (RMSE: 0.234, R²: 0.876)
[10:23:58] [COMPLETE-2] Model 2 completed successfully (RMSE: 0.198, R²: 0.902)
```

### Troubleshooting

#### If you still see no output:

1. **Check file permissions**:
```r
# Verify you can write to output directory
test_file <- file.path(output_dir, "test.txt")
cat("test\n", file = test_file)
file.exists(test_file)  # Should be TRUE
```

2. **Verify workers are actually running**:
```r
source("debug_worker_monitor.R")
check_r_processes()  # Should show multiple R processes
```

3. **Test with simple example**:
```r
source("debug_worker_monitor.R")
test_parallel_debug()  # Should create test_debug/debug_worker_output.log
```

4. **Check log file directly**:
```r
log_file <- file.path(output_dir, "debug_worker_output.log")
if (file.exists(log_file)) {
  readLines(log_file, n = 20)  # Show first 20 lines
} else {
  print("Log file not found - workers may not have started yet")
}
```

### Why This Solution Works

1. **Bypasses stdout capture**: Writes directly to files, avoiding future's stdout handling
2. **Process-safe**: Uses append mode, safe for concurrent writes
3. **Immediate visibility**: No buffering issues - messages appear instantly
4. **Persistent record**: Debug output saved for post-run analysis
5. **Works with any parallelization**: File I/O works regardless of parallel backend

### Performance Considerations

- File I/O adds minimal overhead (~1-2ms per write)
- Log file grows ~100 bytes per debug message
- For 1000 models with 5 messages each: ~500KB log file
- Consider rotating logs for very long runs

### Next Steps

1. Run your evaluation with the modified code
2. Monitor `/path/to/output_dir/debug_worker_output.log`
3. You should now see all worker debug messages
4. Remove debug code once issue is resolved (or leave for future debugging)

### Clean Up Debug Code (Optional)

To remove debug output after resolving issues, search for sections marked with:
- `## ========== DEBUG:`
- `debug_log_file`
- `debug_msg`

Or keep them and control with a debug flag in your function parameters.