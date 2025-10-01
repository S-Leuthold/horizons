#!/usr/bin/env Rscript

## Test solutions for parallel namespace issue

library(horizons)
library(rules)
library(future)
library(furrr)

spec <- parsnip::cubist_rules() %>% parsnip::set_engine("Cubist")
wf <- workflows::workflow() %>% workflows::add_model(spec) %>% workflows::add_formula(mpg ~ .)

## ---------------------------------------------------------------------------
## Solution 1: Force sequential (what I implemented)
## ---------------------------------------------------------------------------

cli::cli_h1("Solution 1: Force Sequential for fit_members")

plan(multisession, workers = 2)
old_plan <- plan()
plan(sequential)

result1 <- tryCatch({
  future({ parsnip::fit(wf, data = mtcars) }) %>% value()
  "SUCCESS"
}, error = function(e) "FAILED")

plan(old_plan)  # Restore
plan(sequential)  # Clean up

cli::cli_alert_info("Result: {result1}")

## ---------------------------------------------------------------------------
## Solution 2: Use future.packages option
## ---------------------------------------------------------------------------

cli::cli_h1("Solution 2: Auto-load Packages in Workers")

plan(multisession, workers = 2)

# Tell future to load these packages in workers
options(future.packages = c("parsnip", "workflows", "rules", "Cubist"))

result2 <- tryCatch({
  future({ parsnip::fit(wf, data = mtcars) }, packages = c("parsnip", "workflows", "rules", "Cubist")) %>% value()
  "SUCCESS"
}, error = function(e) paste0("FAILED: ", substr(e$message, 1, 50)))

plan(sequential)
options(future.packages = NULL)

cli::cli_alert_info("Result: {result2}")

## ---------------------------------------------------------------------------
## Solution 3: Use furrr with explicit packages
## ---------------------------------------------------------------------------

cli::cli_h1("Solution 3: furrr with Explicit Packages")

plan(multisession, workers = 2)

wf_list <- list(wf1 = wf, wf2 = wf)

result3 <- tryCatch({
  furrr::future_map(
    wf_list,
    ~ parsnip::fit(.x, data = mtcars),
    .options = furrr::furrr_options(
      seed = TRUE,
      packages = c("parsnip", "workflows", "rules", "Cubist")
    )
  )
  "SUCCESS"
}, error = function(e) paste0("FAILED: ", substr(e$message, 1, 50)))

plan(sequential)

cli::cli_alert_info("Result: {result3}")

## ---------------------------------------------------------------------------
## Conclusion
## ---------------------------------------------------------------------------

cli::cli_h1("Summary")

results <- c("Solution 1 (force sequential)" = result1,
             "Solution 2 (future.packages)" = result2,
             "Solution 3 (furrr packages)" = result3)

for (i in seq_along(results)) {
  if (results[i] == "SUCCESS") {
    cli::cli_alert_success("{names(results)[i]}: {results[i]}")
  } else {
    cli::cli_alert_danger("{names(results)[i]}: {results[i]}")
  }
}