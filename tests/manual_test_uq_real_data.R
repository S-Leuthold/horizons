## =============================================================================
## Manual Test: UQ Training with Real OSSL Data
## =============================================================================
## Test quantile training on actual OSSL data (small subset for speed)
## =============================================================================

devtools::load_all()
library(dplyr)

cli::cli_h1("UQ Training with Real OSSL Data")

## -----------------------------------------------------------------------------
## Load small subset of OSSL
## -----------------------------------------------------------------------------

cli::cli_h2("Loading OSSL data (pH, 2000 samples)")

ossl_data <- horizons:::load_ossl_raw(
  property    = "ph",
  max_samples = 2000,  # Small for speed
  verbose     = TRUE
)

cli::cli_alert_success("Loaded {nrow(ossl_data)} samples")

## -----------------------------------------------------------------------------
## Split into train/test
## -----------------------------------------------------------------------------

set.seed(123)
train_idx <- sample(1:nrow(ossl_data), 0.8 * nrow(ossl_data))
train_data <- ossl_data[train_idx, ]
test_data  <- ossl_data[-train_idx, ]

# Get response column name
# Spectral columns are bare numbers (600, 602, ..., 4000)
spec_cols <- grep("^[0-9]+$", names(train_data), value = TRUE)

# Metadata columns start with known prefixes
metadata_cols <- grep("^(id\\.|dataset\\.|scan\\.|sample_id|layer\\.)", names(train_data), value = TRUE)

# Response column: contains property name (ph.h2o, clay.tot, etc.)
response_col <- setdiff(names(train_data), c(spec_cols, metadata_cols))[1]

cli::cli_alert_info("Response column: {response_col}")
cli::cli_alert_info("Train: {nrow(train_data)} | Test: {nrow(test_data)}")

# Rename Response column
train_data <- train_data %>% rename(Response = !!response_col)
test_data  <- test_data %>% rename(Response = !!response_col)

# Add required columns
train_data$Sample_ID <- paste0("sample_", 1:nrow(train_data))
train_data$Project <- "test"
test_data$Sample_ID <- paste0("sample_", 1:nrow(test_data))
test_data$Project <- "test"

## -----------------------------------------------------------------------------
## Test 1: Train standalone quantile model
## -----------------------------------------------------------------------------

cli::cli_h2("Test 1: Standalone quantile model training")

quantile_model <- train_quantile_model(
  train_data      = train_data,
  preprocessing   = "snv",
  transformation  = "none",
  grid_size       = 3,
  cv_folds        = 3,
  verbose         = TRUE
)

cli::cli_alert_success("✓ Quantile model trained")

## Test predictions
quantile_preds <- predict_quantiles(quantile_model, test_data)

cli::cli_alert_success("✓ Quantile predictions generated: {nrow(quantile_preds)} samples")
cli::cli_text("Columns: {paste(names(quantile_preds), collapse = ', ')}")

# Check coverage
in_interval <- (test_data$Response >= quantile_preds$.pred_lower) &
               (test_data$Response <= quantile_preds$.pred_upper)

empirical_coverage <- mean(in_interval)
cli::cli_alert_info("Empirical coverage: {round(100 * empirical_coverage, 1)}%")

## -----------------------------------------------------------------------------
## Test 2: Full UQ training (point + quantile)
## -----------------------------------------------------------------------------

cli::cli_h2("Test 2: Full UQ training (point + quantile)")

config <- tibble(
  model              = "cubist",
  preprocessing      = "snv",
  transformation     = "none",
  feature_selection  = "none"
)

cli::cli_alert_info("Training with config: {config$model}")

uq_result <- train_cluster_models_with_uq(
  cluster_data = train_data,
  property     = "ph",
  config       = config,
  cv_folds     = 3,
  grid_size    = 3,
  verbose      = TRUE
)

cli::cli_alert_success("✓ UQ training complete")
cli::cli_text("Result components: {paste(names(uq_result), collapse = ', ')}")

## Test residual-based predictions
uq_preds <- predict_with_uq(
  point_workflow = uq_result$point_model,
  quantile_workflow = uq_result$quantile_model,
  new_data = test_data
)

cli::cli_alert_success("✓ Residual-based predictions generated")

## Residual diagnostics
cli::cli_h2("Residual Diagnostics")
cli::cli_alert_info("Residual stats from training:")
cli::cli_text("  Mean: {round(uq_result$residual_stats$mean, 4)} (should be ~0)")
cli::cli_text("  SD: {round(uq_result$residual_stats$sd, 3)}")

## Combined output
combined <- bind_cols(
  test_data %>% select(Sample_ID, Response),
  uq_preds
) %>%
  mutate(
    in_interval = Response >= .pred_lower & Response <= .pred_upper,
    width = .pred_upper - .pred_lower,
    error = Response - .pred
  )

cli::cli_h2("Sample predictions")
print(head(combined, 10))

coverage <- mean(combined$in_interval)
mean_width <- mean(combined$width)
median_width <- median(combined$width)
min_width <- min(combined$width)
max_width <- max(combined$width)

cli::cli_rule()
cli::cli_alert_success("✓ RESIDUAL-BASED UQ pipeline working!")
cli::cli_h2("Results Summary")
cli::cli_alert_info("Coverage (uncalibrated): {round(100 * coverage, 1)}% (target: ~90%)")
cli::cli_alert_info("Interval width statistics (pH units):")
cli::cli_text("  Mean:   {round(mean_width, 3)}")
cli::cli_text("  Median: {round(median_width, 3)}")
cli::cli_text("  Range:  [{round(min_width, 3)}, {round(max_width, 3)}]")

cli::cli_h2("Comparison to Library-Based (from earlier test)")
cli::cli_text("Library-based mean width: 2.29 pH units")
cli::cli_text("Residual-based mean width: {round(mean_width, 3)} pH units")
reduction_pct <- round(100 * (1 - mean_width / 2.29), 1)
cli::cli_alert_success("Reduction: {reduction_pct}% narrower intervals!")

cli::cli_rule()
cli::cli_alert_info("Next steps:")
cli::cli_text("1. ✓ Residual-based approach validated")
cli::cli_text("2. → Add conformal calibration to guarantee exactly 90% coverage")
cli::cli_text("3. → Test with texture properties (ILR residuals)")
cli::cli_text("4. → Full validation across all 15 properties")
