#!/usr/bin/env Rscript

# Line-by-line diagnostic for closure error

library(cli)
library(horizons)
library(dplyr)
library(tibble)

cli::cli_h1("Closure Error Diagnostic - Line by Line")

# Step 1: Create a simple config like the one causing issues
cli::cli_h2("Step 1: Create Config")

config <- tibble::tibble(
  model = "plsr",
  transformation = "log",
  preprocessing = "snv",
  feature_selection = "boruta",
  covariates = list(c("clay", "sand"))
)

cli::cli_text("Config created:")
print(config)

# Check each field
cli::cli_h2("Step 2: Check Each Field Type")

for (col in names(config)) {
  val <- config[[col]][[1]]
  cli::cli_text("{col}: class = {class(val)}, is.function = {is.function(val)}")
  
  # Try as.character on each
  tryCatch({
    char_val <- as.character(val)
    cli::cli_alert_success("  as.character({col}) = '{char_val}'")
  }, error = function(e) {
    cli::cli_alert_danger("  as.character({col}) FAILED: {e$message}")
  })
}

# Step 3: Test the extract_safe_config_value function directly
cli::cli_h2("Step 3: Test Safe Extraction Function")

# We need to simulate what happens in evaluate_configuration
# Let's create the function here to test it
extract_safe_config_value <- function(value, field_name, config_id = "test") {
  
  cli::cli_text("Testing {field_name}:")
  cli::cli_text("  Input class: {class(value)}")
  cli::cli_text("  is.function: {is.function(value)}")
  cli::cli_text("  is.list: {is.list(value)}")
  
  # Check for closure/function contamination first
  if (is.function(value) || "closure" %in% class(value)) {
    cli::cli_alert_warning("  CLOSURE DETECTED in {field_name}!")
    return("ERROR_CLOSURE")
  }
  
  # Handle list columns (extract first element if needed)
  if (is.list(value) && length(value) == 1) {
    cli::cli_text("  Extracting from list...")
    value <- value[[1]]
    cli::cli_text("  After extraction: class = {class(value)}")
  }
  
  # Final safety check after extraction
  if (is.function(value) || "closure" %in% class(value)) {
    cli::cli_alert_warning("  CLOSURE FOUND after list extraction in {field_name}!")
    return("ERROR_CLOSURE")
  }
  
  # Safe character conversion with error handling
  result <- tryCatch({
    as.character(value)
  }, error = function(e) {
    cli::cli_alert_danger("  as.character() failed: {e$message}")
    if (grepl("closure", e$message, ignore.case = TRUE)) {
      return("ERROR_CLOSURE")
    }
    stop(e)
  })
  
  cli::cli_alert_success("  Result: '{result}'")
  return(result)
}

# Test each field
for (col in names(config)) {
  cli::cli_rule()
  val <- extract_safe_config_value(config[[col]][[1]], col)
}

# Step 4: Test in a parallel context (this is where issues often appear)
cli::cli_h2("Step 4: Test in Parallel Context")

library(future)
library(future.apply)

# Test with multisession (what we're using on HPC)
plan(multisession, workers = 2)

cli::cli_text("Testing with multisession backend...")

# Try to process the config in parallel
results <- future_lapply(1:2, function(i) {
  # Inside the worker, check the config
  config_row <- config[1, ]
  
  # Check each field
  field_checks <- list()
  for (col in names(config_row)) {
    val <- config_row[[col]][[1]]
    field_checks[[col]] <- list(
      class = class(val),
      is_function = is.function(val),
      as_char_success = tryCatch({
        as.character(val)
        TRUE
      }, error = function(e) {
        FALSE
      })
    )
  }
  
  return(field_checks)
})

# Display results from parallel workers
cli::cli_text("Results from parallel workers:")
for (i in seq_along(results)) {
  cli::cli_text("Worker {i}:")
  for (field in names(results[[i]])) {
    check <- results[[i]][[field]]
    cli::cli_text("  {field}: class={check$class}, is_function={check$is_function}, converts={check$as_char_success}")
  }
}

plan(sequential)  # Reset

# Step 5: Test with actual data structures used in the package
cli::cli_h2("Step 5: Test with Package Data Structures")

# Create a minimal dataset
set.seed(123)
test_data <- data.frame(
  sample_id = 1:30,
  Response = rnorm(30),
  feat1 = rnorm(30),
  feat2 = rnorm(30),
  feat3 = rnorm(30)
)

# Try creating configs with create_configs (if available)
if (exists("create_configs", where = "package:horizons")) {
  cli::cli_text("Testing create_configs function...")
  
  configs <- tryCatch({
    create_configs(
      models = "plsr",
      transformations = "log",
      preprocessings = "snv",
      feature_selections = "boruta"
    )
  }, error = function(e) {
    cli::cli_alert_danger("create_configs failed: {e$message}")
    NULL
  })
  
  if (!is.null(configs)) {
    cli::cli_text("Configs created successfully")
    # Check first row
    for (col in names(configs)) {
      val <- configs[[col]][[1]]
      cli::cli_text("{col}: class = {class(val)}")
    }
  }
}

cli::cli_rule()
cli::cli_alert_success("Diagnostic complete")
cli::cli_text("")
cli::cli_text("Summary:")
cli::cli_text("If closures appear only in parallel context, it's a scoping issue.")
cli::cli_text("If closures appear in direct testing, it's a data creation issue.")
cli::cli_text("Check which field(s) fail as.character() conversion.")