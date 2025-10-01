#!/usr/bin/env Rscript

## ---------------------------------------------------------------------------
## Diagnose Parallel Ensemble Issue
## ---------------------------------------------------------------------------

library(tidyverse)
library(horizons)
library(stacks)
library(rules)
library(future)
library(furrr)

cli::cli_h1("Diagnosing Parallel Ensemble Namespace Issue")

## ---------------------------------------------------------------------------
## Test 1: Check what packages are in namespace
## ---------------------------------------------------------------------------

cli::cli_h2("Test 1: Current Session Namespace")

cli::cli_text("Loaded namespaces:")
loadedNamespaces() %>%
  intersect(c("rules", "Cubist", "parsnip", "stacks", "furrr", "future")) %>%
  paste(collapse = ", ") %>%
  cli::cli_alert_info(.)

## ---------------------------------------------------------------------------
## Test 2: Check if workers inherit packages
## ---------------------------------------------------------------------------

cli::cli_h2("Test 2: Worker Package Access")

plan(multisession, workers = 2)

cli::cli_text("Testing if worker can access rules...")

test_result <- tryCatch({

  future({
    "rules" %in% loadedNamespaces()
  }) %>% value()

}, error = function(e) e$message)

cli::cli_alert_info("Worker has rules loaded: {test_result}")

## ---------------------------------------------------------------------------
## Test 3: Test actual workflow fitting in worker
## ---------------------------------------------------------------------------

cli::cli_h2("Test 3: Workflow Fitting in Worker")

# Create a simple cubist workflow
spec <- parsnip::cubist_rules(committees = 5) %>%
  parsnip::set_engine("Cubist")

wf <- workflows::workflow() %>%
  workflows::add_model(spec) %>%
  workflows::add_formula(mpg ~ .)

cli::cli_text("Testing workflow fit in worker...")

fit_result <- tryCatch({

  future({
    parsnip::fit(wf, data = mtcars)
  }) %>% value()

  "SUCCESS"

}, error = function(e) {
  paste0("FAILED: ", e$message)
})

cli::cli_alert_info("Result: {fit_result}")

## ---------------------------------------------------------------------------
## Test 4: Check furrr behavior
## ---------------------------------------------------------------------------

cli::cli_h2("Test 4: furrr::future_map with Workflows")

workflows_list <- list(wf1 = wf, wf2 = wf)

cli::cli_text("Testing furrr::future_map with workflows...")

furrr_result <- tryCatch({

  furrr::future_map(workflows_list, ~ {
    parsnip::fit(.x, data = mtcars)
  }, .options = furrr::furrr_options(seed = TRUE))

  "SUCCESS"

}, error = function(e) {
  paste0("FAILED: ", e$message)
})

cli::cli_alert_info("Result: {furrr_result}")

## ---------------------------------------------------------------------------
## Test 5: Check if old finalization files work
## ---------------------------------------------------------------------------

cli::cli_h2("Test 5: Compare Old vs New Finalization Format")

old_finalized_path <- "/Users/samleuthold/Desktop/_brain/1_Current_Projects/AI-CLIMATE/results/current/finalized_models/POM/finalized"

if (dir.exists(old_finalized_path)) {

  old_files <- list.files(old_finalized_path, pattern = "finalized_models.*\\.qs", full.names = TRUE)

  if (length(old_files) > 0) {

    # Load most recent old file
    old_files %>%
      sort(decreasing = TRUE) %>%
      .[1] -> most_recent

    cli::cli_text("Checking: {basename(most_recent)}")

    old_finalized <- qs::qread(most_recent)

    cli::cli_text("Columns in old finalization:")
    cli::cli_ul(names(old_finalized))

    has_metrics <- "metrics" %in% names(old_finalized)
    has_cv_metrics <- "cv_metrics" %in% names(old_finalized)

    cli::cli_alert_info("Has 'metrics': {has_metrics}")
    cli::cli_alert_info("Has 'cv_metrics': {has_cv_metrics}")

  }
}

## ---------------------------------------------------------------------------
## Test 6: Check globals configuration
## ---------------------------------------------------------------------------

cli::cli_h2("Test 6: Future Globals Configuration")

cli::cli_text("future.globals.maxSize: {getOption('future.globals.maxSize')}")
cli::cli_text("Current plan: {class(plan())[1]}")

## ---------------------------------------------------------------------------
## Recommendations
## ---------------------------------------------------------------------------

cli::cli_h1("Recommendations")

if (fit_result == "SUCCESS") {

  cli::cli_alert_success("Workers CAN fit workflows - the issue may be intermittent or context-dependent")
  cli::cli_text("Possible causes:")
  cli::cli_ul(c(
    "Package loading order differences",
    "Session state differences",
    "Caching or environment issues"
  ))

} else {

  cli::cli_alert_danger("Workers CANNOT fit workflows - this is a consistent namespace issue")
  cli::cli_text("Solutions:")
  cli::cli_ul(c(
    "Option 1: Force sequential for fit_members (safest)",
    "Option 2: Use globals argument to export packages to workers",
    "Option 3: Set future.packages option to auto-load in workers"
  ))

}

## Clean up
plan(sequential)