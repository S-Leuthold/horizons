## -----------------------------------------------------------------------------
## ad.R: Applicability domain (AD) in model feature space
## -----------------------------------------------------------------------------
##
## AD assesses whether a new sample falls within the training distribution, so
## predict() can flag or abstain on out-of-domain (OOD) inputs. The metric is
## the squared Mahalanobis distance in the model's own feature space (the baked
## predictor matrix a fitted workflow sees), using the Schäfer-Strimmer
## shrinkage covariance (corpcor), which is well-defined when p > n (wide
## spectral features).
##
## The OOD threshold is calibrated on a HELD-OUT calibration set disjoint from
## the rows used to estimate the centroid + covariance, with the conformal
## ceil(level * (n + 1)) order-statistic correction. Thresholding on the
## training rows' own distances (distances to a centroid fit to them) is
## optimistically small and systematically over-flags fresh in-distribution
## samples as OOD; the held-out calibration removes that self-reference.
## -----------------------------------------------------------------------------


#' Compute applicability-domain center and covariance (training phase)
#'
#' @description
#' Estimates the AD centroid (feature means) and a shrinkage covariance from a
#' training feature matrix. These define the Mahalanobis geometry; the
#' OOD/quartile thresholds are calibrated separately on held-out data by
#' [compute_ad_thresholds()].
#'
#' @details
#' The covariance is `corpcor::cov.shrink()`, the Schäfer-Strimmer shrinkage
#' estimator: the empirical variances are shrunk toward their median and the
#' correlations toward the identity, each with an intensity estimated
#' analytically from the data.
#'
#' It is the estimator for fewer samples than features, the regime full spectra
#' produce, where the sample covariance is singular. The shrinkage estimate is
#' positive definite and well conditioned whatever the ratio, so its inverse is
#' stable, and the fewer the rows, the more the correlations are shrunk, so the
#' distance moves toward one that scales each feature by its shrunk variance. The
#' out-of-domain cutoff is calibrated on held-out rows, not taken from a
#' distribution. So p > n is the case the estimator is built for, not a
#' degraded one, and it is not warned about.
#'
#' Because the distance is computed with a *shrinkage* inverse covariance, the
#' squared Mahalanobis distance is NOT chi-squared distributed; do not apply a
#' `qchisq()` cutoff to it. Thresholds are calibrated empirically instead
#' ([compute_ad_thresholds()]).
#'
#' The estimator is not robust to training contamination: `colMeans` and the
#' sample-covariance basis are pulled by aberrant training rows, which shrinks
#' those rows' own distances. AD assumes a vetted training set.
#'
#' @param feature_matrix Numeric matrix (n x p) of baked predictors, with
#'   column names. Rows are samples, columns the model's features. No NA/NaN/Inf
#'   and at least `min_ad_rows` rows: [fit_ad()], its one caller, checks both.
#'
#' @return List with `centroid` (numeric length p, named), `cov_matrix`
#'   (p x p shrinkage covariance).
#'
#' @seealso [compute_ad_thresholds()], [calculate_ad_distance()].
#'
#' @noRd
compute_ad_metadata <- function(feature_matrix) {

  ## The mold's predictors, which hardhat builds: without names the centroid
  ## is unnamed and calculate_ad_distance()'s name check is skipped, silently.
  ## A matrix that is not numeric fails loudly in the covariance below.

  if (is.null(colnames(feature_matrix))) {

    abort_horizons("{.arg feature_matrix} must have column names", kind = "internal")

  }

  ## Centroid + shrinkage covariance --------------------------------------------

  centroid   <- colMeans(feature_matrix)
  cov_matrix <- corpcor::cov.shrink(feature_matrix, verbose = FALSE)

  list(
    centroid   = centroid,
    cov_matrix = cov_matrix
  )

}


#' Calibrate AD distance thresholds on held-out data (training phase)
#'
#' @description
#' Computes the quartile marks and out-of-domain (OOD) cutoff for AD binning
#' from a HELD-OUT calibration matrix — samples disjoint from those used to
#' estimate `ad_metadata`. The OOD cutoff uses the conformal
#' `ceil(level * (n_calib + 1))` order statistic so fresh in-distribution
#' samples are flagged at approximately the nominal `1 - level` rate.
#'
#' @details
#' Thresholding on the training rows' own distances is self-referential and
#' under-covers — the centroid + covariance were fit to those rows, so their
#' distances are optimistically small and the resulting cutoff over-flags fresh
#' samples as OOD. Calibrating on held-out `calib_matrix` removes that bias.
#'
#' Coverage is approximate, not exact: the held-out calibration corrects the
#' quantile self-reference, but the centroid + covariance estimates themselves
#' carry finite-sample bias that leaves the OOD rate modestly above nominal at
#' small n. Report AD as held-out-calibrated, not as an exact guarantee.
#'
#' @param calib_matrix Numeric matrix (m x p) of baked predictors for the
#'   held-out calibration samples. Same columns, in the same order, as the
#'   matrix passed to [compute_ad_metadata()].
#' @param ad_metadata List from [compute_ad_metadata()] (`centroid`,
#'   `cov_matrix`).
#' @param level Numeric in (0, 1). OOD coverage level. Default
#'   `DEFAULT_AD_LEVEL` (0.99).
#'
#' @return Numeric vector of length 4 (named FALSE): the Q1, Q2, Q3 quantile
#'   marks and the OOD cutoff, on the squared-distance scale.
#'
#' @seealso [compute_ad_metadata()], [assign_ad_bin()].
#'
#' @noRd
compute_ad_thresholds <- function(calib_matrix,
                                  ad_metadata,
                                  level = DEFAULT_AD_LEVEL) {

  calib_distances <- calculate_ad_distance(calib_matrix, ad_metadata)

  n_calib <- length(calib_distances)

  ## Quartile marks (reporting) -------------------------------------------------

  quartiles <- stats::quantile(
    calib_distances,
    probs = c(0.25, 0.50, 0.75),
    names = FALSE
  )

  ## OOD cutoff: conformal ceil(level * (n + 1)) order statistic -----------------
  ## Matches the finite-sample discipline of the UQ path's compute_c_alpha().

  k <- min(ceiling(level * (n_calib + 1)), n_calib)

  ood_cutoff <- sort(calib_distances)[k]

  c(quartiles, ood_cutoff)

}


#' Calculate applicability-domain distance (prediction phase)
#'
#' @description
#' Squared Mahalanobis distance of new samples to the training centroid under
#' the stored covariance. Larger distances lie further from the training
#' domain.
#'
#' @details
#' Returns the *squared* Mahalanobis distance (`stats::mahalanobis()`'s native
#' output); the thresholds from [compute_ad_thresholds()] are on the same scale,
#' so binning is consistent. Do not take a square root or compare to a
#' chi-squared quantile.
#'
#' New samples must be preprocessed with the same recipe as training so they
#' land in the identical feature space. Column names and count are checked, but
#' name-matching alone does not guarantee an identical transform — the caller
#' must bake `new_data` through the fitted workflow's prepped recipe.
#'
#' @param new_matrix Numeric matrix (m x p) of baked predictors for new
#'   samples. Same column names and order as the training matrix.
#' @param ad_metadata List from [compute_ad_metadata()] (`centroid`,
#'   `cov_matrix`).
#'
#' @return Numeric vector (length m) of non-negative squared distances.
#'
#' @seealso [compute_ad_metadata()], [assign_ad_bin()].
#'
#' @noRd
calculate_ad_distance <- function(new_matrix, ad_metadata) {

  ### A matrix that is not numeric fails loudly in mahalanobis().

  ### Both callers build ad_metadata with both fields, and predict()'s door
  ### validator checks each saved bundle's shape; a feature count that does
  ### not match fails loudly in mahalanobis().

  new_names   <- colnames(new_matrix)
  train_names <- names(ad_metadata$centroid)

  if (!is.null(new_names) && !is.null(train_names) &&
      !identical(new_names, train_names)) {

    ### Same count, other order would bind distances to the wrong features
    abort_horizons("Feature column names must match training data", kind = "internal")

  }

  ## Squared Mahalanobis distance -----------------------------------------------

  stats::mahalanobis(
    x      = new_matrix,
    center = ad_metadata$centroid,
    cov    = ad_metadata$cov_matrix
  )

}


#' The applicability-domain bins, in order
#'
#' @keywords internal
#' @noRd
AD_BINS <- c("Q1", "Q2", "Q3", "Q4", "OOD")


#' Assign applicability-domain bins
#'
#' @description
#' Categorizes squared distances into quartile bins (`Q1`-`Q4`) plus
#' out-of-domain (`OOD`), using the thresholds from [compute_ad_thresholds()].
#'
#' @details
#' The bins are quartiles of the held-out calibration distances plus an OOD tail
#' above the `level` cutoff — they are calibration-distance quartiles, a
#' reporting convenience, not an absolute risk scale. A sample exactly at a
#' threshold falls in the lower bin (right-closed intervals).
#'
#' @param distances Numeric vector of squared distances from
#'   [calculate_ad_distance()]. Non-negative.
#' @param thresholds Numeric vector of length 4: `[Q1, Q2, Q3, OOD_cutoff]` from
#'   [compute_ad_thresholds()].
#'
#' @return Factor (length `length(distances)`) with levels `Q1`, `Q2`, `Q3`,
#'   `Q4`, `OOD`.
#'
#' @seealso [compute_ad_thresholds()], [calculate_ad_distance()].
#'
#' @noRd
assign_ad_bin <- function(distances, thresholds) {

  ## A negative squared distance falls outside cut()'s breaks and bins to NA,
  ## silently, and an NA flag is never abstained on; cut() itself refuses a
  ## non-numeric.

  if (any(distances < 0, na.rm = TRUE)) {

    abort_horizons("{.arg distances} must be non-negative", kind = "internal")

  }

  ### The thresholds come from a saved AD bundle, whose shape predict()'s door
  ### validator checks.

  cut(
    distances,
    breaks         = c(0, thresholds, Inf),
    labels         = AD_BINS,
    right          = TRUE,
    include.lowest = TRUE
  )

}


#' Fit the applicability-domain bundle for one config (training phase)
#'
#' @description
#' Assembles a config's AD bundle from its fitted workflow: centroid + shrinkage
#' covariance on the model's own training feature space, and OOD/quartile
#' thresholds calibrated on a held-out set. Called once per config by
#' [fit_single_config()] when `compute_ad = TRUE`, alongside [fit_uq()] and
#' sharing its calibration partition. It runs as a unit of its own
#' (`run_unit()`), so a failure, or data it cannot estimate from, costs the
#' member its AD and is named in a `skipped` warning, not the fit.
#'
#' @details
#' The training feature matrix is `extract_mold(fitted_workflow)$predictors` —
#' the baked predictors the model actually sees (D1) — matching what [fit_uq()]
#' uses, so AD and UQ live in the identical feature space. The calibration
#' features are baked from `calib_data` through the same prepped recipe (D4), so
#' the held-out distances are commensurate with the training ones. Both must run
#' before the workflow is butchered (the mold is stripped by `butcher()`).
#'
#' Aborts with class `horizons_error_run`, saying why, when `calib_data` is
#' `NULL` or under `min_calib_rows` rows, when the training feature matrix has
#' fewer than `min_ad_rows` rows, or when the training or calibration
#' features hold a value that is not finite (NA, NaN or Inf); and with the
#' step's label (`ad_covariance`, `ad_calibration_bake`, `ad_thresholds`)
#' when one of those steps errors. Fewer training rows than features is the
#' regime the covariance is built for (see [compute_ad_metadata()]) and is not
#' warned about.
#'
#' @param fitted_workflow A trained `workflows::workflow` (pre-butcher).
#' @param calib_data Data frame for threshold calibration — the same held-out
#'   `calib_Fit` split UQ uses.
#' @param level Numeric in (0, 1). OOD coverage level. Default `DEFAULT_AD_LEVEL`.
#' @param min_calib_rows Integer. The fewest calibration rows the thresholds
#'   are computed from; `control_horizons(min_calib_rows = )`. Default
#'   `N_CALIB_MIN`.
#' @param min_ad_rows Integer. The fewest training rows the covariance is
#'   estimated from; `control_horizons(min_ad_rows = )`. Default
#'   `N_AD_TRAIN_MIN`.
#'
#' @return List `list(centroid, cov_matrix, ad_thresholds, n_calib)`.
#'
#' @seealso [compute_ad_metadata()], [compute_ad_thresholds()], [fit_uq()].
#'
#' @keywords internal
#' @noRd
fit_ad <- function(fitted_workflow,
                   calib_data,
                   level          = DEFAULT_AD_LEVEL,
                   min_calib_rows = N_CALIB_MIN,
                   min_ad_rows    = N_AD_TRAIN_MIN) {

  ## Guard: insufficient calibration data ----------------------------------------

  check_calibration_rows(calib_data, min_calib_rows)

  ## Training feature matrix — the baked predictors the model sees --------------

  train_matrix <- as.matrix(mold_predictors(fitted_workflow))

  if (nrow(train_matrix) < min_ad_rows) {

    abort_horizons(
      "The model was fit on {nrow(train_matrix)} row{?s}, fewer than the {min_ad_rows} the covariance needs ({.code min_ad_rows}).",
      kind = "run"
    )

  }

  check_finite_features(train_matrix, "training")

  ## Centroid + shrinkage covariance --------------------------------------------

  ad_metadata <- unit_step("ad_covariance", compute_ad_metadata(train_matrix))

  ## Calibration features — baked through the SAME prepped recipe ---------------

  calib_matrix <- unit_step("ad_calibration_bake", {
    prepped_recipe <- workflows::extract_recipe(fitted_workflow, estimated = TRUE)
    as.matrix(recipes::bake(prepped_recipe, new_data = calib_data, recipes::all_predictors()))
  })

  check_finite_features(calib_matrix, "calibration")

  ## Held-out conformal thresholds (D6) -----------------------------------------

  ad_thresholds <- unit_step("ad_thresholds",
                             compute_ad_thresholds(calib_matrix, ad_metadata, level = level))

  list(
    centroid      = ad_metadata$centroid,
    cov_matrix    = ad_metadata$cov_matrix,
    ad_thresholds = ad_thresholds,
    n_calib       = nrow(calib_matrix)
  )

}


#' Refuse features the applicability domain cannot be estimated from
#'
#' @param x Numeric matrix of baked features.
#' @param what Character(1). Which features, `"training"` or `"calibration"`.
#' @param call The frame the failure reports.
#' @return `NULL`, invisibly; aborts with class `horizons_error_run` when a
#'   value is NA, NaN or Inf, since the centroid, the covariance and every
#'   distance would carry it.
#' @keywords internal
#' @noRd
check_finite_features <- function(x, what, call = rlang::caller_env()) {

  bad <- !is.finite(x)

  if (!any(bad)) return(invisible(NULL))

  n_rows <- sum(rowSums(bad) > 0)

  abort_horizons(
    "{n_rows} of the {nrow(x)} {what} rows {cli::qty(n_rows)}ha{?s/ve} a feature that is not finite (NA, NaN or Inf).",
    kind = "run", call = call
  )

}


#' Compute AD distance + flag for new samples (prediction phase)
#'
#' @description
#' Each sample's squared Mahalanobis distance to the training centroid, and
#' its AD bin, from the features `predict()` baked through the configuration's
#' recipe (the same recipe the model was fit with, so new samples land in the
#' identical feature space the centroid and covariance were estimated in,
#' D4). Run by `predict_one_config()` as a unit of its own: a failure raises,
#' labelled `ad_distance`, and the configuration's AD columns are `NA`, with
#' one `skipped` warning. Rows that baked to non-finite values never reach
#' here: `predict()` returns them as `NA`, flagged.
#'
#' Column-name alignment is re-checked inside [calculate_ad_distance()], a
#' tripwire that propagates.
#'
#' @param new_matrix Numeric matrix of the baked predictors, every value
#'   finite.
#' @param ad_bundle The config's AD bundle from `models$ad[[config_id]]`
#'   (`centroid`, `cov_matrix`, `ad_thresholds`).
#'
#' @return Tibble with `.ad_distance` (numeric, squared) and `.ad_flag`
#'   (factor Q1-Q4/OOD), one row per row of `new_matrix`.
#'
#' @seealso [fit_ad()], [calculate_ad_distance()], [assign_ad_bin()].
#'
#' @keywords internal
#' @noRd
predict_ad <- function(new_matrix, ad_bundle) {

  unit_step("ad_distance", {

    distances <- calculate_ad_distance(
      new_matrix,
      list(centroid = ad_bundle$centroid, cov_matrix = ad_bundle$cov_matrix)
    )

    tibble::tibble(
      .ad_distance = distances,
      .ad_flag     = assign_ad_bin(distances, ad_bundle$ad_thresholds)
    )

  })

}
