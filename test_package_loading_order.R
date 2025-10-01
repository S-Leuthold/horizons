#!/usr/bin/env Rscript

## Test if package loading order affects parallel behavior

library(future)
library(furrr)

cli::cli_h1("Test 1: Load packages BEFORE setting parallel plan")

# Load everything first
library(horizons)
library(rules)
library(Cubist)

# Then set parallel
plan(multisession, workers = 2)

spec <- parsnip::cubist_rules() %>% parsnip::set_engine("Cubist")
wf <- workflows::workflow() %>% workflows::add_model(spec) %>% workflows::add_formula(mpg ~ .)

result1 <- tryCatch({
  future({ parsnip::fit(wf, data = mtcars) }) %>% value()
  "SUCCESS"
}, error = function(e) paste0("FAILED: ", substr(e$message, 1, 50)))

cli::cli_alert_info("Result: {result1}")

plan(sequential)

## ---------------------------------------------------------------------------

cli::cli_h1("Test 2: Set parallel BEFORE loading packages")

# Restart R session simulation - detach packages
try(detach("package:Cubist", unload = TRUE), silent = TRUE)
try(detach("package:rules", unload = TRUE), silent = TRUE)

# Set parallel first
plan(multisession, workers = 2)

# Then load packages
library(rules)
library(Cubist)

result2 <- tryCatch({
  future({ parsnip::fit(wf, data = mtcars) }) %>% value()
  "SUCCESS"
}, error = function(e) paste0("FAILED: ", substr(e$message, 1, 50)))

cli::cli_alert_info("Result: {result2}")

plan(sequential)

cli::cli_h1("Conclusion")
if (result1 == "SUCCESS" || result2 == "SUCCESS") {
  cli::cli_alert_success("Order matters! One approach works.")
} else {
  cli::cli_alert_danger("Neither approach works - need different solution")
}