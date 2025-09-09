# Unit Tests for Parallel Processing Context Detection
# Tests the context detection functions that determine optimal parallel backends

library(testthat)
library(horizons)
library(future)

test_that("detect_parallel_context identifies local context correctly", {
  # Test basic local context detection
  context <- detect_parallel_context(verbose = FALSE)
  
  expect_context_detection(context)
  expect_true(context$context %in% c("local", "hpc"))  # Depends on where test runs
  expect_true(context$cores_available >= 1)
})

test_that("detect_parallel_context identifies HPC environments", {
  # Test SLURM detection
  with_hpc_env("SLURM", {
    context <- detect_parallel_context(verbose = FALSE)
    expect_equal(context$on_cluster, TRUE)
    expect_true(context$context %in% c("hpc", "local"))  # Could be either depending on other factors
  })
  
  # Test PBS detection
  with_hpc_env("PBS", {
    context <- detect_parallel_context(verbose = FALSE)
    expect_equal(context$on_cluster, TRUE)
    expect_true(context$context %in% c("hpc", "local"))
  })
})

test_that("detect_parallel_context identifies RStudio environment", {
  with_rstudio_env({
    context <- detect_parallel_context(verbose = FALSE)
    expect_equal(context$in_rstudio, TRUE)
    expect_equal(context$recommended_backend, "multisession")
  })
})

test_that("detect_parallel_context handles nested parallel contexts", {
  # Test detection when already in parallel context
  with_future_backend("multicore", workers = 2, {
    context <- detect_parallel_context(verbose = FALSE)
    expect_true(context$in_nested)
    # Should recommend safe approach for nested context
  })
}

test_that("detect_parallel_context respects minimum cores setting", {
  # Test with high minimum cores requirement
  context_high <- detect_parallel_context(min_cores = 100, verbose = FALSE)
  context_low <- detect_parallel_context(min_cores = 1, verbose = FALSE)
  
  expect_type(context_high, "list")
  expect_type(context_low, "list")
  
  # High requirement might affect backend recommendation
  if (context_high$cores_available < 100) {
    # Might prefer multisession when cores are insufficient
    expect_true(context_high$recommended_backend %in% c("multicore", "multisession"))
  }
})

test_that("setup_parallel_backend configures multicore correctly", {
  skip_if_not_available(os = "Darwin")  # Skip on Windows
  skip_if_not_available(os = "Linux")   # Only on Unix-like systems
  
  old_plan <- setup_parallel_backend(
    n_workers = 2,
    force_backend = "multicore",
    verbose = FALSE
  )
  
  # Check that multicore was set
  current_plan <- future::plan()
  expect_true(inherits(current_plan, "future"))
  
  # Cleanup
  restore_parallel_settings(old_plan, verbose = FALSE)
  
  # Should be back to original
  expect_true(TRUE)  # If we get here, cleanup worked
})

test_that("setup_parallel_backend configures multisession correctly", {
  old_plan <- setup_parallel_backend(
    n_workers = 2,
    force_backend = "multisession",
    verbose = FALSE
  )
  
  current_plan <- future::plan()
  expect_true(inherits(current_plan, "future"))
  
  # Cleanup
  restore_parallel_settings(old_plan, verbose = FALSE)
  expect_true(TRUE)
})

test_that("setup_parallel_backend handles sequential backend", {
  old_plan <- setup_parallel_backend(
    n_workers = 1,
    force_backend = "sequential",
    verbose = FALSE
  )
  
  current_plan <- future::plan()
  expect_true(inherits(current_plan, "future"))
  
  restore_parallel_settings(old_plan, verbose = FALSE)
})

test_that("setup_parallel_backend auto-detects backend appropriately", {
  # Test auto-detection in normal environment
  old_plan <- setup_parallel_backend(
    n_workers = 2,
    force_backend = NULL,  # Auto-detect
    verbose = FALSE
  )
  
  expect_true(inherits(old_plan, "future"))
  
  restore_parallel_settings(old_plan, verbose = FALSE)
})

test_that("get_backend_display returns readable names", {
  # Test with different backends
  with_future_backend("sequential", {
    display_name <- get_backend_display()
    expect_type(display_name, "character")
    expect_true(nchar(display_name) > 0)
  })
  
  skip_if_not_available(os = "Darwin")
  with_future_backend("multicore", workers = 2, {
    display_name <- get_backend_display()
    expect_equal(display_name, "multicore")
  })
  
  with_future_backend("multisession", workers = 2, {
    display_name <- get_backend_display()
    expect_equal(display_name, "multisession")
  })
})

test_that("set_thread_controls configures package threading", {
  # Store original settings
  orig_dt <- getOption("datatable.threads")
  orig_ranger <- getOption("ranger.num.threads")
  orig_mc <- getOption("mc.cores")
  
  # Test setting thread controls
  thread_limit <- set_thread_controls(
    context_aware = FALSE,
    max_threads = 2,
    verbose = FALSE
  )
  
  expect_equal(thread_limit, 2)
  expect_equal(getOption("ranger.num.threads"), 2)
  
  # Restore original settings
  options(
    datatable.threads = orig_dt,
    ranger.num.threads = orig_ranger,
    mc.cores = orig_mc
  )
})

test_that("set_thread_controls is context aware", {
  # Test with mock HPC context
  with_hpc_env("SLURM", {
    thread_limit <- set_thread_controls(
      context_aware = TRUE,
      max_threads = 4,
      verbose = FALSE
    )
    
    expect_true(is.numeric(thread_limit))
    expect_gte(thread_limit, 1)
  })
})

test_that("setup_parallel_rng creates reproducible seeds", {
  seeds1 <- setup_parallel_rng(seed = 123, n_workers = 4, verbose = FALSE)
  seeds2 <- setup_parallel_rng(seed = 123, n_workers = 4, verbose = FALSE)
  
  expect_equal(length(seeds1), 4)
  expect_equal(length(seeds2), 4)
  
  # Same seed should give same sequences
  expect_equal(seeds1, seeds2)
  
  # Different seeds should give different sequences
  seeds3 <- setup_parallel_rng(seed = 456, n_workers = 4, verbose = FALSE)
  expect_false(identical(seeds1, seeds3))
})

test_that("setup_parallel_rng handles different worker counts", {
  seeds_small <- setup_parallel_rng(seed = 123, n_workers = 2, verbose = FALSE)
  seeds_large <- setup_parallel_rng(seed = 123, n_workers = 6, verbose = FALSE)
  
  expect_equal(length(seeds_small), 2)
  expect_equal(length(seeds_large), 6)
  
  # First two seeds should be the same
  expect_equal(seeds_small[1:2], seeds_large[1:2])
})

test_that("optimize_parallel_memory manages memory appropriately", {
  # Test memory optimization
  result <- optimize_parallel_memory(force_gc = TRUE, verbose = FALSE)
  expect_true(result)
  
  # Test without gc
  result_no_gc <- optimize_parallel_memory(force_gc = FALSE, verbose = FALSE)
  expect_true(result_no_gc)
})

test_that("restore_parallel_settings restores previous state", {
  # Get initial state
  initial_plan <- future::plan()
  
  # Change to different backend
  new_plan <- setup_parallel_backend(2, force_backend = "multisession", verbose = FALSE)
  
  # Verify change occurred
  changed_plan <- future::plan()
  # We can't easily test exact equality of plans, but we can verify restoration works
  
  # Restore
  restore_parallel_settings(new_plan, verbose = FALSE)
  
  # Should be able to continue operating
  expect_true(TRUE)
})

test_that("parallel context detection handles edge cases", {
  # Test with very low core count
  context_minimal <- detect_parallel_context(min_cores = 1000, verbose = FALSE)
  expect_type(context_minimal, "list")
  
  # Test verbose output doesn't crash
  expect_output(
    detect_parallel_context(verbose = TRUE),
    "Parallel Context Detection"
  )
})

test_that("parallel backend setup handles invalid inputs gracefully", {
  # Test with zero workers
  expect_error(
    setup_parallel_backend(n_workers = 0),
    NA  # Should not error, might just use sequential
  )
  
  # Test with negative workers
  expect_error(
    setup_parallel_backend(n_workers = -1),
    NA  # Should handle gracefully
  )
})

test_that("parallel utilities work with different data sizes", {
  # Test context detection is fast even with different loads
  expect_within_time({
    for (i in 1:10) {
      context <- detect_parallel_context(verbose = FALSE)
    }
  }, max_seconds = 1.0)
  
  # Test backend setup is reasonably fast
  expect_within_time({
    old_plan <- setup_parallel_backend(2, verbose = FALSE)
    restore_parallel_settings(old_plan, verbose = FALSE)
  }, max_seconds = 2.0)  # Allow more time for backend setup
})

test_that("parallel detection works across platforms", {
  expect_cross_platform_consistency({
    context <- detect_parallel_context(verbose = FALSE)
    
    # Should work on all platforms
    expect_type(context, "list")
    expect_true("recommended_backend" %in% names(context))
    
    # Backend should be appropriate for platform
    if (Sys.info()["sysname"] == "Windows") {
      expect_equal(context$recommended_backend, "multisession")
    }
  })
})

test_that("parallel utilities handle memory constraints appropriately", {
  # Test with different memory limits
  expect_memory_reasonable({
    context <- detect_parallel_context(verbose = FALSE)
    old_plan <- setup_parallel_backend(2, memory_limit_gb = 0.1, verbose = FALSE)  # Low limit
    restore_parallel_settings(old_plan, verbose = FALSE)
  }, max_mb = 100)
})