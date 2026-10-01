## ===========================================================================
## horizons — end-to-end pipeline example
## ===========================================================================
##
## A fully self-contained, runnable walk-through of the v1 pipeline:
##
##   spectra() -> standardize() -> add_response() -> configure()
##             -> validate()    -> evaluate()     -> fit() -> predict()
##
## It uses SYNTHETIC spectra (generated below), so it runs anywhere with no
## external data files. The point is a live integration test you can run by
## hand to confirm the whole chain works after a change — distinct from the
## unit tests, which exercise pieces in isolation.
##
## Run with:  Rscript inst/examples/end-to-end-pipeline.R
##        or, during development:  devtools::load_all(); source(this file)
##
## Keep it fast: one light model (rf/ranger), a small grid, modest n.
## ===========================================================================

## Load the package. Prefer load_all() when run from inside the source tree
## (so the example exercises the working-tree code, not a stale install);
## fall back to the installed package otherwise.
if (file.exists("DESCRIPTION") &&
    any(grepl("^Package: horizons", readLines("DESCRIPTION")))) {

  devtools::load_all(quiet = TRUE)

} else {

  library(horizons)

}

set.seed(42)

## ---------------------------------------------------------------------------
## 0. Generate synthetic MIR-like spectra + a non-negative response
## ---------------------------------------------------------------------------
## Realistic-ish shape: smooth absorbance curves over a wavenumber axis, with
## a few "bands" whose depth drives the response (so models have real signal
## to find). This stands in for OPUS/CSV spectra; the pipeline does not care
## that they are synthetic.

n_samples <- 240
wavenumbers <- seq(4000, 600, by = -20)        # 171 bands, decreasing (MIR convention)
n_wn        <- length(wavenumbers)

## Smooth baseline + a couple of Gaussian absorption bands per sample.
make_spectrum <- function(band_depths) {

  baseline <- 0.2 + 0.0001 * (4000 - wavenumbers)
  band1    <- band_depths[1] * exp(-((wavenumbers - 1600)^2) / (2 * 80^2))
  band2    <- band_depths[2] * exp(-((wavenumbers - 1030)^2) / (2 * 60^2))
  baseline + band1 + band2 + rnorm(n_wn, sd = 0.005)

}

## Per-sample band depths; band1 depth drives the response.
depths <- cbind(runif(n_samples, 0.1, 0.6), runif(n_samples, 0.1, 0.4))

spec_mat <- t(vapply(seq_len(n_samples), function(i) make_spectrum(depths[i, ]),
                     numeric(n_wn)))
colnames(spec_mat) <- paste0("wn_", wavenumbers)

spectra_df <- tibble::as_tibble(spec_mat)
spectra_df$sample_id <- sprintf("SOIL_%03d", seq_len(n_samples))

## Response: non-negative SOC-like value driven by band1 depth + noise.
response_df <- tibble::tibble(
  sample_id = spectra_df$sample_id,
  SOC       = 5 + 25 * depths[, 1] + rnorm(n_samples, sd = 1.0)
)

cat("Synthetic data:", n_samples, "samples x", n_wn, "wavenumbers\n")
cat("SOC range:", round(range(response_df$SOC), 1), "\n\n")

## Hold out 10 samples as "unknowns" to predict at the end — these never
## enter the modelling pipeline.
unknown_idx   <- sample(n_samples, 10)
unknown_specs <- spectra_df[unknown_idx, ]
train_specs   <- spectra_df[-unknown_idx, ]
train_resp    <- response_df[!response_df$sample_id %in% unknown_specs$sample_id, ]

## ---------------------------------------------------------------------------
## 1. spectra() — load into a horizons_data object
## ---------------------------------------------------------------------------

hz <- spectra(train_specs, id_col = "sample_id")

## ---------------------------------------------------------------------------
## 2. standardize() — object-level preprocessing (resample / trim)
## ---------------------------------------------------------------------------
## Defaults resample to 2 cm^-1 and trim to 600-4000. Our synthetic axis is
## already in range; this normalises it to the canonical grid.

hz <- standardize(hz)

## ---------------------------------------------------------------------------
## 3. add_response() — join the outcome (custom-data mode)
## ---------------------------------------------------------------------------

hz <- add_response(hz, source = train_resp, variable = "SOC")

## ---------------------------------------------------------------------------
## 4. configure() — define the model search space
## ---------------------------------------------------------------------------
## Minimal for speed: one model, one preprocessing, no feature selection.

hz <- configure(
  hz,
  models            = "rf",
  preprocessing     = "snv",
  feature_selection = "none",
  transformation    = "none"
)

## ---------------------------------------------------------------------------
## 5. validate() — pre-flight checks before expensive compute
## ---------------------------------------------------------------------------

hz <- validate(hz)

## ---------------------------------------------------------------------------
## 6. evaluate() — broad screening across configs (here just the one)
## ---------------------------------------------------------------------------

hz <- evaluate(hz)

## ---------------------------------------------------------------------------
## 7. fit() — re-tune the top config(s) and train UQ
## ---------------------------------------------------------------------------
## n_best = 1 since we only configured one model. compute_uq trains the
## conformal interval machinery (needs enough calibration samples; ~230
## training rows is comfortably above the minimum).

hz <- fit(hz, n_best = 1L, compute_uq = TRUE)

cat("\nFitted object class:", paste(class(hz), collapse = " -> "), "\n")
cat("Models stored:", length(hz$models$workflows), "\n")
cat("UQ computed:", !is.null(hz$models$uq), "\n\n")

## ---------------------------------------------------------------------------
## 8. predict() — point predictions + conformal intervals on the unknowns
## ---------------------------------------------------------------------------
## The unknowns must be on the SAME wavenumber axis the model trained on.
## standardize() them through the same path so the schema gate passes.

unknown_hz <- spectra(unknown_specs, id_col = "sample_id") |>
  standardize()

preds <- predict(hz, unknown_hz, level = 0.90)

cat("=== Predictions on 10 held-out unknowns (90% intervals) ===\n")
print(preds)

## Sanity checks a human can eyeball:
cat("\nAll intervals ordered (.pred_lower <= .pred <= .pred_upper)? ",
    all(preds$.pred_lower <= preds$.pred & preds$.pred <= preds$.pred_upper), "\n")
cat("All predictions non-negative? ", all(preds$.pred >= 0), "\n")
cat("Mean interval width: ", round(mean(preds$.interval_width), 2), " SOC units\n")

## If you want to see the actual held-out truth alongside (we know it here
## because the data is synthetic):
truth <- response_df$SOC[unknown_idx]
covered <- truth >= preds$.pred_lower & truth <= preds$.pred_upper
cat("Empirical coverage on these 10 unknowns: ",
    round(mean(covered) * 100), "% (nominal 90%)\n")

cat("\nPipeline ran end to end. ✓\n")

