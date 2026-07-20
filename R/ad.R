## -----------------------------------------------------------------------------
## ad.R: Applicability domain (AD) in model feature space
## -----------------------------------------------------------------------------
##
## AD assesses whether a new sample falls within the training distribution, so
## predict() can flag or abstain on out-of-domain (OOD) inputs. The metric is
## the squared Mahalanobis distance in the model's own feature space (the baked
## predictor matrix a fitted workflow sees), using a Ledoit-Wolf shrinkage
## covariance so it is well-defined when p > n (wide spectral features).
##
## The OOD threshold is calibrated on a HELD-OUT calibration set disjoint from
## the rows used to estimate the centroid + covariance, with the conformal
## ceil(level * (n + 1)) order-statistic correction. Thresholding on the
## training rows' own distances (distances to a centroid fit to them) is
## optimistically small and systematically over-flags fresh in-distribution
## samples as OOD; the held-out calibration removes that self-reference.
##
## Design: dev/specs/v1-refactor/ad-design.md.
## -----------------------------------------------------------------------------


#' Compute applicability-domain center and covariance (training phase)
#'
#' @description
#' Estimates the AD centroid (feature means) and a Ledoit-Wolf shrinkage
#' covariance from a training feature matrix. These define the Mahalanobis
#' geometry; the OOD/quartile thresholds are calibrated separately on held-out
#' data by [compute_ad_thresholds()].
#'
#' @details
#' The covariance uses `corpcor::cov.shrink()` (Ledoit-Wolf shrinkage toward a
#' scaled identity), which stays positive-definite and invertible when p > n —
#' the regime wide spectral feature matrices produce, where the sample
#' covariance is singular.
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
#'   column names. Rows are samples, columns the model's features. No NA/NaN/Inf.
#'
#' @return List with `centroid` (numeric length p, named), `cov_matrix`
#'   (p x p shrinkage covariance). Aborts on invalid input.
#'
#' @seealso [compute_ad_thresholds()], [calculate_ad_distance()].
#'
#' @noRd
compute_ad_metadata <- function(feature_matrix) {

  ## Validate inputs ------------------------------------------------------------

  if (!is.matrix(feature_matrix) || !is.numeric(feature_matrix)) {

    cli::cli_abort("{.arg feature_matrix} must be a numeric matrix")

  }

  if (is.null(colnames(feature_matrix))) {

    cli::cli_abort("{.arg feature_matrix} must have column names")

  }

  if (anyNA(feature_matrix) || any(!is.finite(feature_matrix))) {

    cli::cli_abort("{.arg feature_matrix} must not contain NA, NaN, or Inf")

  }

  n_samples  <- nrow(feature_matrix)
  n_features <- ncol(feature_matrix)

  if (n_samples < N_AD_TRAIN_MIN) {

    cli::cli_abort(
      "{.arg feature_matrix} needs at least {N_AD_TRAIN_MIN} samples (has {n_samples})"
    )

  }

  ## Warn when the covariance cannot be meaningfully estimated -------------------
  ## Below n ~ p the shrinkage target dominates; the AD is still well-defined
  ## but its geometry is close to a scaled identity. Warn, do not block.

  if (n_samples < n_features) {

    cli::cli_warn(c(
      "AD covariance estimated with fewer samples than features ({n_samples} < {n_features}).",
      "i" = "The shrinkage target dominates; AD distances may be unreliable."
    ))

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

  ## Validate inputs ------------------------------------------------------------

  if (!is.matrix(new_matrix) || !is.numeric(new_matrix)) {

    cli::cli_abort("{.arg new_matrix} must be a numeric matrix")

  }

  if (!is.list(ad_metadata) ||
      !all(c("centroid", "cov_matrix") %in% names(ad_metadata))) {

    cli::cli_abort("{.arg ad_metadata} must be a list with {.field centroid} and {.field cov_matrix}")

  }

  ## Feature alignment ----------------------------------------------------------

  n_features_new   <- ncol(new_matrix)
  n_features_train <- length(ad_metadata$centroid)

  if (n_features_new != n_features_train) {

    cli::cli_abort(
      "Number of features must match: new has {n_features_new}, training has {n_features_train}"
    )

  }

  new_names   <- colnames(new_matrix)
  train_names <- names(ad_metadata$centroid)

  if (!is.null(new_names) && !is.null(train_names) &&
      !identical(new_names, train_names)) {

    cli::cli_abort("Feature column names must match training data")

  }

  ## Squared Mahalanobis distance -----------------------------------------------

  stats::mahalanobis(
    x      = new_matrix,
    center = ad_metadata$centroid,
    cov    = ad_metadata$cov_matrix
  )

}


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

  ## Validate inputs ------------------------------------------------------------

  if (!is.numeric(distances)) {

    cli::cli_abort("{.arg distances} must be numeric")

  }

  if (any(distances < 0, na.rm = TRUE)) {

    cli::cli_abort("{.arg distances} must be non-negative")

  }

  if (!is.numeric(thresholds) || length(thresholds) != 4) {

    cli::cli_abort("{.arg thresholds} must be a numeric vector of length 4")

  }

  cut(
    distances,
    breaks         = c(0, thresholds, Inf),
    labels         = c("Q1", "Q2", "Q3", "Q4", "OOD"),
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
#' sharing its calibration partition. Returns `NULL` on any failure or
#' insufficient data, so one config's AD failure never aborts the fit.
#'
#' @details
#' The training feature matrix is `extract_mold(fitted_workflow)$predictors` —
#' the baked predictors the model actually sees (D1) — matching what [fit_uq()]
#' uses, so AD and UQ live in the identical feature space. The calibration
#' features are baked from `calib_data` through the same prepped recipe (D4), so
#' the held-out distances are commensurate with the training ones. Both must run
#' before the workflow is butchered (the mold is stripped by `butcher()`).
#'
#' @param fitted_workflow A trained `workflows::workflow` (pre-butcher).
#' @param calib_data Data frame for threshold calibration — the same held-out
#'   `calib_Fit` split UQ uses. `NULL` or under `N_CALIB_MIN` rows returns
#'   `NULL`.
#' @param level Numeric in (0, 1). OOD coverage level. Default `DEFAULT_AD_LEVEL`.
#'
#' @return List `list(centroid, cov_matrix, ad_thresholds, n_calib)`, or `NULL`.
#'
#' @seealso [compute_ad_metadata()], [compute_ad_thresholds()], [fit_uq()].
#'
#' @keywords internal
#' @noRd
fit_ad <- function(fitted_workflow,
                   calib_data,
                   level = DEFAULT_AD_LEVEL) {

  ## Guard: NULL or insufficient calibration data -------------------------------

  if (is.null(calib_data) || nrow(calib_data) < N_CALIB_MIN) {

    return(NULL)

  }

  ## Training feature matrix — the baked predictors the model sees --------------

  train_matrix <- as.matrix(workflows::extract_mold(fitted_workflow)$predictors)

  if (nrow(train_matrix) < N_AD_TRAIN_MIN || anyNA(train_matrix)) {

    return(NULL)

  }

  ## Centroid + shrinkage covariance (safely — degenerate matrices return NULL) -

  md_safe <- safely_execute(
    suppressWarnings(compute_ad_metadata(train_matrix)),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(md_safe$error)) {

    return(NULL)

  }

  ad_metadata <- md_safe$result

  ## Calibration features — baked through the SAME prepped recipe ---------------

  prepped_recipe <- workflows::extract_recipe(fitted_workflow, estimated = TRUE)

  calib_safe <- safely_execute(
    as.matrix(recipes::bake(
      prepped_recipe,
      new_data = calib_data,
      recipes::all_predictors()
    )),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(calib_safe$error) || anyNA(calib_safe$result)) {

    return(NULL)

  }

  calib_matrix <- calib_safe$result

  ## Held-out conformal thresholds (D6) -----------------------------------------

  thr_safe <- safely_execute(
    compute_ad_thresholds(calib_matrix, ad_metadata, level = level),
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(thr_safe$error)) {

    return(NULL)

  }

  list(
    centroid      = ad_metadata$centroid,
    cov_matrix    = ad_metadata$cov_matrix,
    ad_thresholds = thr_safe$result,
    n_calib       = nrow(calib_matrix)
  )

}


#' Compute AD distance + flag for new samples (prediction phase)
#'
#' @description
#' Bakes `new_spectra` through the fitted workflow's recipe to reach the
#' model's feature space, then computes each sample's squared Mahalanobis
#' distance to the training centroid and its AD bin. Returns the two predict-time
#' AD columns, or `NULL` on any failure (so predict degrades to no-AD rather
#' than erroring).
#'
#' @details
#' The recipe is re-extracted from the (butchered) stored `workflow` rather than
#' cached in the AD bundle — the same recipe the model was fit with, so new
#' samples land in the identical feature space the centroid/covariance were
#' estimated in (D4). Column-name alignment is re-checked inside
#' [calculate_ad_distance()].
#'
#' @param workflow The config's stored (butchered) fitted workflow.
#' @param ad_bundle The config's AD bundle from `models$ad[[config_id]]`
#'   (`centroid`, `cov_matrix`, `ad_thresholds`).
#' @param new_spectra Tibble from `resolve_new_data()` (sample_id + predictors).
#'
#' @return Tibble with `.ad_distance` (numeric, squared) and `.ad_flag`
#'   (factor Q1-Q4/OOD), one row per sample; or `NULL` on failure.
#'
#' @seealso [fit_ad()], [calculate_ad_distance()], [assign_ad_bin()].
#'
#' @keywords internal
#' @noRd
predict_ad <- function(workflow, ad_bundle, new_spectra) {

  if (is.null(ad_bundle)) {

    return(NULL)

  }

  ## Bake new_data through the SAME recipe the model was fit with ---------------

  bake_safe <- safely_execute(
    {
      prepped_recipe <- workflows::extract_recipe(workflow, estimated = TRUE)
      as.matrix(recipes::bake(
        prepped_recipe,
        new_data = new_spectra,
        recipes::all_predictors()
      ))
    },
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(bake_safe$error) || is.null(bake_safe$result) ||
      anyNA(bake_safe$result)) {

    return(NULL)

  }

  ## Distance + bin (safely — a covariance/alignment mismatch returns NULL) -----

  ad_safe <- safely_execute(
    {
      metadata  <- list(centroid   = ad_bundle$centroid,
                        cov_matrix = ad_bundle$cov_matrix)
      distances <- calculate_ad_distance(bake_safe$result, metadata)
      flags     <- assign_ad_bin(distances, ad_bundle$ad_thresholds)
      tibble::tibble(.ad_distance = distances, .ad_flag = flags)
    },
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(ad_safe$error)) {

    return(NULL)

  }

  ad_safe$result

}
