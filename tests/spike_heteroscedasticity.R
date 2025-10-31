## ---------------------------------------------------------------------------
## Research Spike: Heteroscedasticity Validation (Quick Version)
## ---------------------------------------------------------------------------
## Goal: Test if prediction errors vary with magnitude (justifies quantile models)
## Time: ~30-45 min runtime
## Properties: clay, ph, oc (diverse representatives)
## Output: Plots + statistical tests saved to tests/spike_results/
## ---------------------------------------------------------------------------
devtools::load_all()
library(horizons)
library(dplyr)
library(ggplot2)
library(lmtest)

## ---------------------------------------------------------------------------
## Setup
## ---------------------------------------------------------------------------

cli::cli_h1("Heteroscedasticity Validation Spike")

# Create output directory
output_dir <- here::here("tests", "spike_results")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Test properties (3 diverse NON-TEXTURE types)
# Avoid clay/sand/silt since they require ILR transformation
test_properties <- c("ph", "oc", "total_carbon")

results_list <- list()

## ---------------------------------------------------------------------------
## Test Each Property
## ---------------------------------------------------------------------------

for (prop in test_properties) {

  cli::cli_h2("Testing: {prop}")

  # Load OSSL data
  cli::cli_alert_info("Loading OSSL data...")
  ossl_data <- horizons:::load_ossl_raw(
    property = prop,
    verbose  = TRUE
  )

  cli::cli_alert_success("Loaded {nrow(ossl_data)} samples")

  # Simple 80/20 split
  set.seed(123)
  train_idx <- sample(1:nrow(ossl_data), 0.8 * nrow(ossl_data))
  train_data <- ossl_data[train_idx, ]
  test_data  <- ossl_data[-train_idx, ]

  # Get spectral columns (bare numbers: 600, 602, etc.)
  spec_cols <- grep("^[0-9]+$", names(train_data), value = TRUE)

  # Response column: find the property column (contains "usda" or property name)
  # Exclude: id columns, dataset, scan metadata, depth
  all_cols <- names(train_data)
  metadata_cols <- grep("^(id\\.|dataset\\.|scan\\.|sample_id|layer\\.)", all_cols, value = TRUE)
  response_col <- setdiff(all_cols, c(spec_cols, metadata_cols))[1]

  cli::cli_alert_info("Response column: {response_col}")
  cli::cli_alert_info("Spectral columns: {length(spec_cols)} wavenumbers")
  cli::cli_alert_info("Training baseline model (ranger, no tuning)...")

  # Train simple baseline model
  spec <- parsnip::rand_forest(trees = 500) %>%
    parsnip::set_engine("ranger", num.threads = 4) %>%
    parsnip::set_mode("regression")

  # Backtick numeric column names and response column for formula
  spec_cols_backticked <- paste0("`", spec_cols, "`")
  response_col_backticked <- paste0("`", response_col, "`")

  formula_str <- paste(response_col_backticked, "~",
                      paste(spec_cols_backticked, collapse = " + "))

  model <- spec %>%
    parsnip::fit(as.formula(formula_str), data = train_data)

  cli::cli_alert_success("Model trained")

  # Predict on test set
  cli::cli_alert_info("Generating predictions...")
  preds <- predict(model, test_data)$.pred
  actual <- test_data[[response_col]]
  residuals <- actual - preds

  # Breusch-Pagan test for heteroscedasticity
  cli::cli_alert_info("Running Breusch-Pagan test...")
  bp_test <- lmtest::bptest(residuals ~ preds)

  # Store results
  results_list[[prop]] <- tibble::tibble(
    property       = prop,
    n_train        = nrow(train_data),
    n_test         = length(actual),
    rmse           = sqrt(mean(residuals^2)),
    r2             = cor(actual, preds)^2,
    bp_statistic   = as.numeric(bp_test$statistic),
    bp_pvalue      = bp_test$p.value,
    heteroscedastic = bp_test$p.value < 0.05,
    conclusion     = ifelse(bp_test$p.value < 0.05,
                           "CONFIRMED",
                           "Not detected")
  )

  cli::cli_alert_info("Breusch-Pagan p-value: {round(bp_test$p.value, 5)}")

  if (bp_test$p.value < 0.05) {
    cli::cli_alert_success("✓ Heteroscedasticity CONFIRMED (p < 0.05)")
  } else {
    cli::cli_alert_warning("✗ No significant heteroscedasticity (p ≥ 0.05)")
  }

  # Create residual plot
  cli::cli_alert_info("Creating diagnostic plot...")

  plot_data <- data.frame(
    predicted = preds,
    residual  = residuals,
    abs_residual = abs(residuals)
  )

  p <- ggplot(plot_data, aes(x = predicted, y = residual)) +
    geom_point(alpha = 0.3, size = 1) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    geom_smooth(method = "loess", color = "blue", linewidth = 1.2, se = TRUE) +
    geom_smooth(aes(y = abs_residual),
                method = "loess",
                color = "orange",
                linetype = "dashed",
                linewidth = 1) +
    labs(
      title = paste("Residuals vs Predicted:", toupper(prop)),
      subtitle = sprintf(
        "Breusch-Pagan p = %.4f | R² = %.3f | RMSE = %.3f | n = %d",
        bp_test$p.value,
        cor(actual, preds)^2,
        sqrt(mean(residuals^2)),
        length(residuals)
      ),
      x = paste("Predicted", prop),
      y = "Residual",
      caption = "Blue: mean residual | Orange: absolute residual (variance proxy)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10)
    )

  # Save plot
  plot_file <- file.path(output_dir, paste0("heteroscedasticity_", prop, ".png"))
  ggsave(plot_file, plot = p, width = 10, height = 6, dpi = 300)
  cli::cli_alert_success("Plot saved: {basename(plot_file)}")

  # Cleanup
  rm(model, train_data, test_data, ossl_data)
  gc(verbose = FALSE)

  cli::cli_rule()
}

## ---------------------------------------------------------------------------
## Summary Results
## ---------------------------------------------------------------------------

cli::cli_h1("Summary Results")

results_df <- dplyr::bind_rows(results_list)

# Print table
print(results_df, width = Inf)

# Save results
results_file <- file.path(output_dir, "heteroscedasticity_results.csv")
readr::write_csv(results_df, results_file)
cli::cli_alert_success("Results saved: {basename(results_file)}")

## ---------------------------------------------------------------------------
## Conclusion
## ---------------------------------------------------------------------------

n_hetero <- sum(results_df$heteroscedastic)
n_total  <- nrow(results_df)

cli::cli_rule()
cli::cli_h2("CONCLUSION")

cli::cli_alert_info("{n_hetero}/{n_total} properties show significant heteroscedasticity (p < 0.05)")

if (n_hetero >= 2) {
  cli::cli_alert_success("✓ DECISION: Heteroscedasticity confirmed")
  cli::cli_text("  → Quantile models will provide adaptive interval widths")
  cli::cli_text("  → Narrower intervals at low values, wider at high values")
  cli::cli_text("  → Proceed with belt-and-suspenders approach (quantile + conformal)")
} else if (n_hetero == 1) {
  cli::cli_alert_warning("⚠ DECISION: Mixed evidence")
  cli::cli_text("  → Proceed with belt-and-suspenders approach for robustness")
  cli::cli_text("  → Quantile models provide safety even if gains are modest")
} else {
  cli::cli_alert_warning("⚠ DECISION: Weak evidence, but proceed anyway")
  cli::cli_text("  → Belt-and-suspenders provides robustness regardless")
  cli::cli_text("  → Quantile models won't hurt, and conformal ensures coverage")
}

cli::cli_rule()
cli::cli_alert_info("Review plots in: {output_dir}")
cli::cli_alert_info("Results table: {basename(results_file)}")
cli::cli_rule()
