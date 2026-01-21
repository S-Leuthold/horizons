rm(list=ls())

setwd("./Desktop/_brain/1_Current_Projects/horizons/worktrees/feature-library-predictions/")
## -----------------------------------------------------------------------------
devtools::document()
devtools::install(build_vignettes = FALSE, upgrade = "never", quick = TRUE)
devtools::load_all()

## -----------------------------------------------------------------------------
## Load OSSL data
## -----------------------------------------------------------------------------

ossl_data <- load_ossl_raw(
  property = "ph",
  max_samples = 1000
)

## -----------------------------------------------------------------------------
## 80/20 split for train/test
## -----------------------------------------------------------------------------

set.seed(123)
split_idx <- sample(1:nrow(ossl_data), size = 0.8 * nrow(ossl_data))

train_data <- ossl_data[split_idx, ] %>%
  mutate(cluster_id = 1) %>%
  rename(Response = ph.h2o_usda.a268_index,
         Sample_ID = sample_id) %>%
  mutate(Project = "ossl")

test_data <- ossl_data[-split_idx, ] %>%
  mutate(cluster_id = 1) %>%
  rename(Response = ph.h2o_usda.a268_index,
         Sample_ID = sample_id) %>%
  mutate(Project = "ossl")

cat("Train:", nrow(train_data), "| Test:", nrow(test_data), "\n\n")

## -----------------------------------------------------------------------------
## Get optimal config for pH
## -----------------------------------------------------------------------------

ph_config <- OPTIMAL_CONFIGS_V1 %>%
  filter(property == "ph") %>%
  slice(1)

## -----------------------------------------------------------------------------
## Train with CV+ conformal
## -----------------------------------------------------------------------------

cat("Training with CV+ conformal...\n")

models <- train_cluster_models_with_uq(
  cluster_data = train_data,
  property     = "ph",
  config       = ph_config,
  cv_folds     = 10,
  grid_size    = 5,
  verbose      = TRUE
)


## -----------------------------------------------------------------------------
## Predict on test set
## -----------------------------------------------------------------------------

cat("Predicting on test set...\n\n")

test_preds <- predict_with_uq(
  point_workflow    = models$point_model,
  quantile_workflow = models$quantile_model,
  new_data          = test_data,
  c_alpha           = models$c_alpha,
  repair_crossings  = TRUE
)

## -----------------------------------------------------------------------------
## Evaluate coverage
## -----------------------------------------------------------------------------

test_preds$truth <- test_data$Response
test_preds$covered <- (test_preds$truth >= test_preds$.pred_lower &
                       test_preds$truth <= test_preds$.pred_upper)
test_preds$width <- test_preds$.pred_upper - test_preds$.pred_lower

coverage   <- mean(test_preds$covered) * 100
mean_width <- mean(test_preds$width)
point_rmse <- sqrt(mean((test_preds$truth - test_preds$.pred)^2))

## -----------------------------------------------------------------------------
## Results
## -----------------------------------------------------------------------------

cat("=" %>% strrep(70), "\n")
cat("RESULTS\n")
cat("=" %>% strrep(70), "\n\n")

cat("Point Model:\n")
cat("  R²:   ", round(models$point_metrics$rsq, 3), "\n")
cat("  RMSE: ", round(point_rmse, 3), "\n")
cat("  RPD:  ", round(models$point_metrics$rpd, 2), "\n\n")

cat("Conformal Calibration:\n")
cat("  Base coverage:      ", round(89.4, 1), "% (from CV)\n")
cat("  c_alpha:            ", round(models$c_alpha, 4), "pH\n\n")

cat("Test Set Performance (n=", nrow(test_data), "):\n", sep="")
cat("  Coverage:           ", round(coverage, 1), "% (target: 90%)\n", sep="")
cat("  Mean interval width:", round(mean_width, 2), "pH\n")
cat("  Width range:        ", round(min(test_preds$width), 2), "-",
    round(max(test_preds$width), 2), "pH\n\n")

if (coverage >= 88 && coverage <= 92) {
  cat("✓ Coverage in target range (88-92%)\n")
} else if (coverage < 88) {
  cat("⚠ Coverage below target\n")
} else {
  cat("⚠ Coverage above target\n")
}

cat("\nFirst 10 predictions:\n")
test_preds %>%
  slice(1:10) %>%
  mutate(status = ifelse(covered, "+", "-"),
         display = paste0(status, " ", round(truth, 2), " in [",
                         round(.pred_lower, 2), ", ", round(.pred_upper, 2), "]")) %>%
  pull(display) %>%
  walk(~cat("  ", .x, "\n"))
