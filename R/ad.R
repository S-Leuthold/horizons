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
#'   column names. Rows are samples, columns the model's features. No NA/NaN/Inf
#'   and at least `min_ad_rows` rows: [fit_ad()], its one caller, checks both.
#'
#' @return List with `centroid` (numeric length p, named), `cov_matrix`
#'   (p x p shrinkage covariance). Warns when there are fewer samples than
#'   features.
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

  n_samples  <- nrow(feature_matrix)
  n_features <- ncol(feature_matrix)

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
#' when one of those steps errors. The covariance's warning for fewer samples
#' than features is recorded on the member's row.
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

  ## Centroid + shrinkage covariance. Its warning for fewer samples than
  ## features is recorded on the member's row, not muffled.

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
#' Bakes `new_spectra` through the fitted workflow's recipe to reach the
#' model's feature space, then computes each sample's squared Mahalanobis
#' distance to the training centroid and its AD bin. Returns the two predict-time
#' AD columns, or `NULL` with a warning on any failure (so predict degrades to
#' no-AD rather than erroring).
#'
#' @details
#' The recipe is re-extracted from the (butchered) stored `workflow` rather than
#' cached in the AD bundle — the same recipe the model was fit with, so new
#' samples land in the identical feature space the centroid/covariance were
#' estimated in (D4). Column-name alignment is re-checked inside
#' [calculate_ad_distance()].
#'
#' Two failure modes are kept distinct, because they mean different things. A
#' bake that *aborts* is a bug or a schema problem — no sample gets an AD
#' column, and the function warns rather than degrading quietly. A bake that
#' *succeeds* but returns NA for some rows is the documented
#' `step_transform_spectra()` path for a malformed spectrum: those rows alone
#' get `NA` distance and flag, and every other row is scored normally. The
#' earlier whole-matrix `anyNA()` check conflated the two, so one bad spectrum
#' in a batch silently removed AD (and therefore abstention) from the entire
#' batch.
#'
#' @param workflow The config's stored (butchered) fitted workflow.
#' @param ad_bundle The config's AD bundle from `models$ad[[config_id]]`
#'   (`centroid`, `cov_matrix`, `ad_thresholds`).
#' @param new_spectra Tibble from `resolve_new_data()` (sample_id + predictors).
#' @param config_id Character(1) or `NULL`. The config the bundle belongs to,
#'   named in every warning. `NULL` (default) omits it.
#'
#' @return Tibble with `.ad_distance` (numeric, squared) and `.ad_flag`
#'   (factor Q1-Q4/OOD), one row per sample — `NA` in both columns for rows
#'   that baked to NA; or `NULL` when the bake aborted or the distance could
#'   not be computed at all, each with a warning of class
#'   `horizons_ad_warning`. A `NULL` bundle returns `NULL` without a warning.
#'
#' @seealso [fit_ad()], [calculate_ad_distance()], [assign_ad_bin()].
#'
#' @keywords internal
#' @noRd
predict_ad <- function(workflow, ad_bundle, new_spectra, config_id = NULL) {

  if (is.null(ad_bundle)) {

    return(NULL)

  }

  ## Every warning below names the config when it is known, so a multi-config
  ## predict says which model lost its AD. The id enters as a value.
  ad_label <- if (is.null(config_id)) {
    "Applicability domain"
  } else {
    "Applicability domain for config {.val {config_id}}"
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

  if (!is.null(bake_safe$error) || is.null(bake_safe$result)) {

    ## Interpolated as a value, not as part of the template — an error message
    ## carrying braces would otherwise be re-evaluated by cli.
    bake_msg <- if (is.null(bake_safe$error)) {
      "The recipe returned nothing."
    } else {
      condition_summary(bake_safe$error)
    }

    cli::cli_warn(c(
      "!" = paste0(ad_label, " could not be computed: baking {.arg new_data} through the fitted recipe failed."),
      "x" = "{bake_msg}",
      "i" = "This is a bug or a schema mismatch between {.arg new_data} and the training axis, not a property of one sample.",
      "i" = "No AD columns are returned, so out-of-domain abstention cannot be applied to this batch."
    ), class = "horizons_ad_warning")

    return(NULL)

  }

  new_matrix <- bake_safe$result

  ## Per-row NA: degrade those rows, score the rest ----------------------------
  ## A row that baked to NA (step_transform_spectra's documented failure path
  ## for a malformed spectrum) has no position in feature space, so it gets no
  ## distance. mahalanobis() would propagate that NA across the whole call, so
  ## the bad rows are held out of the computation and re-inserted as NA.

  bad_rows <- !stats::complete.cases(new_matrix)

  if (all(bad_rows)) {

    cli::cli_warn(c(
      "!" = paste0(ad_label, " is unavailable for all {nrow(new_matrix)} sample{?s}: every spectrum baked to NA."),
      "i" = "See the {.fn step_transform_spectra} warning above for the cause."
    ), class = "horizons_ad_warning")

    return(NULL)

  }

  if (any(bad_rows)) {

    cli::cli_warn(c(
      "!" = paste0(ad_label, " is NA for {sum(bad_rows)} of {nrow(new_matrix)} sample{?s} whose spectra baked to NA."),
      "i" = "The remaining samples are scored normally; the NA rows are neither binned nor abstained on."
    ), class = "horizons_ad_warning")

  }

  ## Distance + bin (safely — a covariance/alignment mismatch returns NULL) -----

  ad_safe <- safely_execute(
    {
      metadata  <- list(centroid   = ad_bundle$centroid,
                        cov_matrix = ad_bundle$cov_matrix)

      distances            <- rep(NA_real_, nrow(new_matrix))
      distances[!bad_rows] <- calculate_ad_distance(
        new_matrix[!bad_rows, , drop = FALSE],
        metadata
      )

      flags <- assign_ad_bin(distances, ad_bundle$ad_thresholds)

      tibble::tibble(.ad_distance = distances, .ad_flag = flags)
    },
    log_error          = FALSE,
    capture_conditions = TRUE
  )

  if (!is.null(ad_safe$error)) {

    ## Warned rather than dropped silently: the AD columns vanishing with no
    ## signal is the failure this replaces. Interpolated as a value, as the
    ## bake warning above is.
    ad_msg <- condition_summary(ad_safe$error)

    cli::cli_warn(c(
      "!" = paste0(ad_label, " could not be computed: the distance to the training centroid failed."),
      "x" = "{ad_msg}",
      "i" = "Point predictions are returned without {.field .ad_distance} and {.field .ad_flag}, so out-of-domain abstention cannot be applied to this batch."
    ), class = "horizons_ad_warning")

    return(NULL)

  }

  ad_safe$result

}
