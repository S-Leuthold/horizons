#' Spectral Preprocessing Recipe Step
#'
#' @description
#' Custom recipes step that applies row-wise spectral preprocessing using
#' prospectr methods (SNV, Savitzky-Golay derivatives, combinations).
#' Designed for wide-format spectral data where columns are wavenumbers.
#'
#' Supported preprocessing methods:
#' - `"raw"` — trim edge artifacts only (no transformation)
#' - `"sg"` — Savitzky-Golay smoothing (0th derivative) at polynomial order
#'   1, which is a moving average: a boxcar `window_size` points wide
#' - `"snv"` — Standard Normal Variate + edge trimming
#' - `"deriv1"` — Savitzky-Golay 1st derivative
#' - `"deriv2"` — Savitzky-Golay 2nd derivative
#' - `"snv_deriv1"` — SNV then 1st derivative
#' - `"snv_deriv2"` — SNV then 2nd derivative
#'
#' @param recipe A `recipes::recipe()` object.
#' @param ... Column selectors for spectral columns.
#' @param preprocessing Character. One of the methods listed above, checked
#'   when the step is added.
#' @param window_size Odd integer, at least 5. Savitzky-Golay window size, in
#'   grid points; it also sets the edge trimmed for `"raw"` and `"snv"`.
#'   Default 9. Checked here, so a direct caller cannot build the step with a
#'   window some method cannot run.
#' @param masked Character or `NULL`. The names of the selected columns that
#'   are masked: transformed with the rest of the axis, so a window beside
#'   them reads real neighbours, and then dropped. `NULL` (the default)
#'   masks nothing.
#' @param role Character. Role for output columns. Default "predictor".
#' @param trained Logical. Internal recipes flag.
#' @param skip Logical. Skip during bake()? Default FALSE.
#' @param id Character. Unique step identifier.
#'
#' @details
#' A spectrum the step cannot use, one with a value that is not finite or
#' with no spread (every value equal), is returned as `NA` in every output
#' column, whatever the method: SNV would otherwise turn a flat spectrum
#' into zeros, and a single missing value would leave a partly `NA` row.
#' The rule is the one [standardize()] flags by. The rows are named, by the
#' column with the `id` role when the recipe has one and by position
#' otherwise, in one `horizons_warning_flagged`, which carries the field
#' `step`.
#'
#' **Masked columns.** The method runs over every selected column, masked
#' ones included, so a Savitzky-Golay window beside a masked band reads the
#' real values there rather than jumping the band. SNV's mean and spread are
#' taken over the unmasked columns alone and applied to every column. The
#' masked columns are then dropped: the output holds the unmasked columns
#' the window's edge trim leaves, named `spec1`, `spec2`, ... in axis order,
#' so nothing after the step sees a masked column. A missing value in a
#' masked column makes the spectrum unusable, since a window reads it; its
#' spread is judged over the unmasked columns. With nothing masked the step
#' is what it was before the argument existed.
#'
#' When you `tidy()` this step, a tibble is returned with columns `terms`,
#' `preprocessing`, `window_size`, `masked` and `id`: before `prep()`,
#' `terms` holds the selectors and `masked` is `NA`; after, the columns the
#' step transforms, and whether each is masked.
#'
#' @return Updated recipe with spectral preprocessing step.
#' @keywords internal
#' @export
step_transform_spectra <- function(recipe,
                                   ...,
                                   preprocessing,
                                   window_size = 9,
                                   masked      = NULL,
                                   role        = "predictor",
                                   trained     = FALSE,
                                   skip        = FALSE,
                                   id          = recipes::rand_id("transform_spectra")) {

  terms <- rlang::enquos(...)

  if (!rlang::is_string(preprocessing) || !preprocessing %in% VALID_PREPROCESSING) {

    abort_horizons(c(
      "{.arg preprocessing} must be one of {.val {VALID_PREPROCESSING}}.",
      "x" = "Got {.val {preprocessing}}."
    ), kind = "input")

  }

  ## An even window has no centre point, and a window under 5 cannot fit the
  ## cubic deriv2 needs. Either would otherwise build, prep, and fail at bake
  ## inside prospectr (or, for raw and snv, trim a fractional edge).
  if (!is_valid_sg_window(window_size)) {

    abort_horizons(c(
      "{.arg window_size} must be an odd whole number of at least {SG_WINDOW_MIN}.",
      "x" = "Got {.code {paste(deparse(window_size), collapse = ' ')}}.",
      "i" = "The window is centred on a point, so its width is odd, and the second-derivative methods fit a cubic, which needs at least {SG_WINDOW_MIN} points."
    ), kind = "input")

  }

  if (!is.null(masked) && !is.character(masked)) {

    abort_horizons(c(
      "{.arg masked} must be the names of the masked columns, or {.code NULL}.",
      "x" = "Got {.obj_type_friendly {masked}}."
    ), kind = "input")

  }

  recipes::add_step(
    recipe,
    step_transform_spectra_new(
      terms         = terms,
      columns       = NULL,
      preprocessing = preprocessing,
      window_size   = window_size,
      masked        = masked,
      role          = role,
      trained       = trained,
      skip          = skip,
      id            = id
    )
  )

}

## ---------------------------------------------------------------------------
## Constructor
## ---------------------------------------------------------------------------

## Follows recipes' own contract: `terms` holds the selector quosures and
## survives prep, so a trained recipe can be re-prepped (`fresh = TRUE`);
## `columns` holds the names prep resolved them to, and is what bake reads.
## `masked` holds the names of the masked columns among them, which prep
## keeps in `columns`' order: with `columns` and the window it fixes the
## columns bake returns, whose names are `trained_columns`. A step trained
## before `masked` existed has none, which is no mask.

#' @keywords internal
#' @noRd
step_transform_spectra_new <- function(terms,
                                       columns,
                                       preprocessing,
                                       window_size,
                                       role,
                                       trained,
                                       skip,
                                       id,
                                       trained_columns = NULL,
                                       id_column       = NULL,
                                       masked          = NULL) {

  out <- list(
    terms           = terms,
    columns         = columns,
    preprocessing   = preprocessing,
    window_size     = window_size,
    masked          = masked,
    role            = role,
    trained         = trained,
    skip            = skip,
    id              = id,
    trained_columns = trained_columns,
    id_column       = id_column
  )

  class(out) <- c("step_transform_spectra", "step")
  out

}

## ---------------------------------------------------------------------------
## prep method
## ---------------------------------------------------------------------------

#' @export
prep.step_transform_spectra <- function(x, training, info = NULL, ...) {

  col_names   <- recipes::recipes_eval_select(x$terms, training, info)
  non_numeric <- col_names[!vapply(training[, col_names], is.numeric, logical(1))]

  if (length(non_numeric) > 0) {

    abort_horizons(c(
      "All spectral columns must be numeric.",
      "x" = "Non-numeric: {.field {non_numeric}}."
    ), kind = "input")

  }

  ## Compute output column count after SG trimming --------------------------

  input_len    <- length(col_names)
  half_window  <- (x$window_size - 1) / 2
  out_len      <- input_len - 2 * half_window

  ## A window wider than the spectrum trims everything away, and one exactly
  ## as wide is refused by prospectr::savitzkyGolay() (it needs w < ncol) at
  ## bake time, inside tune, where the message is lost. Downstream the first
  ## surfaces as a `names0()` of a non-positive length and the second as a
  ## failed grid search, neither of which names the cause, so check both here
  ## with the numbers in the message.

  if (input_len <= x$window_size) {

    abort_horizons(c(
      "{.fn step_transform_spectra}'s window is at least as wide as the spectrum.",
      "x" = "{input_len} spectral column{?s} with {.code window_size = {x$window_size}}.",
      "i" = "Savitzky-Golay drops {half_window} column{?s} from each end and needs the window narrower than the spectrum, so {.arg window_size} must be smaller than the number of spectral columns."
    ), kind = "input")

  }

  ## The masked columns, and the columns bake() returns ---------------------
  ## Every selected column is transformed; of those the edge trim leaves,
  ## the masked ones are dropped. Nothing masked is the step as it was, its
  ## width and names included.

  masked <- x$masked %||% character(0)
  stray  <- setdiff(masked, col_names)

  if (length(stray) > 0) {

    abort_horizons(c(
      "{.fn step_transform_spectra} was given {length(stray)} masked column{?s} it does not select.",
      "x" = "Not selected: {.val {utils::head(stray, 5)}}{if (length(stray) > 5) ', ...' else ''}.",
      "i" = "{.arg masked} names columns among the spectra the step transforms."
    ), kind = "input")

  }

  is_masked <- col_names %in% masked

  if (any(is_masked)) {

    window_kept <- seq.int(1 + half_window, input_len - half_window)
    kept        <- window_kept[!is_masked[window_kept]]

    if (length(kept) == 0) {

      abort_horizons(c(
        "{.fn step_transform_spectra} would return no column: every column the window's edge trim leaves is masked.",
        "x" = "{sum(is_masked)} of {input_len} spectral column{?s} {?is/are} masked, with {.code window_size = {x$window_size}}.",
        "i" = "Keep more of the spectrum outside the masked bands."
      ), kind = "input")

    }

    out_len <- length(kept)

  }

  new_colnames <- recipes::names0(out_len, prefix = "spec")

  ## The generated names must not collide with a column this step passes
  ## through (a meta column literally named `spec01`, say). bake() binds the two
  ## blocks side by side, so a collision is repaired positionally: at training
  ## a spectral band is silently displaced from everything downstream, and at
  ## predict time the bind dies inside vctrs with no mention of the column at
  ## fault. Named here, at prep, where the recipe is still fixable.

  check_transform_name_collision(new_colnames, setdiff(names(training), col_names))

  ## The column with the id role names the spectra bake() cannot use; a
  ## recipe without one names them by position
  id_column <- if (!is.null(info)) info$variable[info$role %in% "id"][1] else NA_character_

  step_transform_spectra_new(
    terms           = x$terms,
    columns         = col_names,
    preprocessing   = x$preprocessing,
    window_size     = x$window_size,
    role            = x$role,
    trained         = TRUE,
    skip            = x$skip,
    id              = x$id,
    trained_columns = new_colnames,
    id_column       = if (is.na(id_column)) NULL else id_column,
    masked          = if (length(masked)) col_names[is_masked] else x$masked
  )

}

## ---------------------------------------------------------------------------
## bake method
## ---------------------------------------------------------------------------

#' @export
bake.step_transform_spectra <- function(object, new_data, ...) {

  check_step_new_data(object, new_data, columns = object$columns)

  spectral_matrix <- as.matrix(new_data[, object$columns])

  ## The masked columns prep() kept, by position among those it transforms;
  ## NULL with none, which is the step as it was before masking existed
  is_masked <- if (length(object$masked)) object$columns %in% object$masked

  ## The whole matrix goes through prospectr in one call. Every supported
  ## method is row-wise (SNV normalizes a spectrum by its own mean and SD; the
  ## Savitzky-Golay filter convolves along the wavenumber axis within one
  ## spectrum), so a matrix call gives the same numbers as the old
  ## one-row-at-a-time loop without the per-row list and rbind copies that
  ## dominated the prep transient at library scale (2026-09-16).

  ### The method was checked when the step was added (#199), so a failure
  ### here is prospectr's on this matrix, which every row shares.
  transformed_matrix <- rlang::try_fetch(
    transform_spectra_matrix(spectral_matrix,
                             preprocessing = object$preprocessing,
                             window_size   = object$window_size,
                             masked        = is_masked),
    error = function(e) {
      abort_horizons(c(
        "{.fn step_transform_spectra} could not transform its spectra.",
        "*" = "Method: {.val {object$preprocessing}}.",
        "*" = "Spectral matrix: {nrow(spectral_matrix)} x {ncol(spectral_matrix)}.",
        "*" = "Window size: {object$window_size}."
      ), kind = "run", parent = e)
    }
  )

  ## A spectrum it cannot use is NA, whatever the method -------------------
  ## The rule standardize() flags by (unusable_spectra()): prospectr raises no
  ## error on such a spectrum, but SNV turns a flat one into zeros and a
  ## Savitzky-Golay window leaves NA only around a missing value, so the rows
  ## are set NA here and named, not left to whatever the method made of them.
  ## Without this a malformed sample returns NA predictions, or injects NA
  ## rows into the model matrix, with no signal either way. See #52.

  unusable <- unusable_spectra(spectral_matrix, masked = is_masked)
  bad      <- !is.na(unusable)

  if (any(bad)) {

    transformed_matrix[bad, ] <- NA_real_

    id_col <- object$id_column
    named  <- if (!is.null(id_col) && id_col %in% names(new_data)) {
      as.character(new_data[[id_col]])
    } else {
      paste("row", seq_len(nrow(new_data)))
    }

    n          <- sum(bad)
    non_finite <- named[bad & unusable %in% "non-finite"]
    flat       <- named[bad & unusable %in% "no spread"]
    n_nf       <- length(non_finite)
    n_flat     <- length(flat)

    warn_in_unit("flagged", c(
      "{n} spectr{?um/a} could not be used by {.fn step_transform_spectra} and {?is/are} returned as {.code NA}.",
      "i" = if (n_nf > 0) "{n_nf} {?has/have} a value that is not finite: {shown_ids(non_finite)}.",
      "i" = if (n_flat > 0) "{n_flat} {?has/have} no spread (every value equal): {shown_ids(flat)}."
    ), step = "step_transform_spectra")

  }

  metadata <- new_data[, !names(new_data) %in% object$columns, drop = FALSE]

  ## Re-checked at bake as well as prep: `new_data` can carry a column the
  ## training table did not, and default name repair would resolve the clash
  ## positionally instead of reporting it. `.name_repair = "check_unique"`
  ## makes the bind itself refuse to repair silently; the check above is what
  ## names the offending column.

  check_transform_name_collision(object$trained_columns, names(metadata))

  dplyr::bind_cols(
    metadata,
    tibble::as_tibble(transformed_matrix, .name_repair = ~ object$trained_columns),
    .name_repair = "check_unique"
  )

}


## ---------------------------------------------------------------------------
## Name-collision check (shared by prep and bake)
## ---------------------------------------------------------------------------

#' Abort when a generated spectral name collides with a pass-through column
#'
#' @param generated Character vector of the step's generated column names
#'   (`spec1`, `spec01`, ... from `recipes::names0()`).
#' @param passthrough Character vector of the column names the step carries
#'   through untransformed (everything that is not a spectral column).
#' @param call The frame the refusal reports: the step's `prep()` or
#'   `bake()`.
#'
#' @return Invisibly `TRUE`; aborts naming the colliding columns.
#' @keywords internal
#' @noRd
check_transform_name_collision <- function(generated, passthrough, call = rlang::caller_env()) {

  collisions <- intersect(generated, passthrough)

  if (length(collisions) > 0) {

    abort_horizons(c(
      "{.fn step_transform_spectra} would generate {length(collisions)} column name{?s} that {?is/are} already in the data.",
      "x" = "Colliding: {.val {collisions}}",
      "i" = "The step names its output {.code spec1}, {.code spec2}, ... , so a non-spectral column using that pattern (an id or a meta column) cannot be carried through alongside it.",
      "i" = "{cli::qty(length(collisions))}Rename the offending column{?s} before building the recipe."
    ), kind = "input", call = call)

  }

  invisible(TRUE)

}

## ---------------------------------------------------------------------------
## Window rule (shared by the step constructor and configure())
## ---------------------------------------------------------------------------

#' Is this a Savitzky-Golay window every preprocessing method can run?
#'
#' @description
#' Odd, because the window is centred on a point, and at least
#' `SG_WINDOW_MIN`, because `deriv2` and `snv_deriv2` fit a cubic and
#' `prospectr::savitzkyGolay()` needs the window wider than the polynomial
#' order. `configure()` applies one window to every method in a grid, so the
#' rule is the strictest method's. Whether the window fits the spectrum is a
#' separate check, made where the spectrum is known (`prep()`, and
#' `evaluate()`'s preflight).
#'
#' @param w The window to check.
#'
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
is_valid_sg_window <- function(w) {

  is.numeric(w) && length(w) == 1 && is.finite(w) && w == round(w) &&
    w >= SG_WINDOW_MIN && w %% 2 == 1

}

## ---------------------------------------------------------------------------
## tidy method
## ---------------------------------------------------------------------------

#' What step_transform_spectra() transforms, and how
#'
#' @description
#' Follows recipes' convention: before `prep()`, `terms` holds the
#' selectors as text; after, the columns the step transforms (#109), and
#' `masked` whether each is masked (transformed, then dropped); before
#' `prep()` that is not known, so `NA`.
#'
#' @param x A `step_transform_spectra` step.
#' @param ... Not used.
#' @return Tibble with `terms`, `preprocessing`, `window_size`, `masked` and
#'   `id`, one row per term.
#' @importFrom recipes tidy
#' @keywords internal
#' @noRd
#' @export
tidy.step_transform_spectra <- function(x, ...) {

  trained <- isTRUE(x$trained)

  terms <- if (trained) {
    x$columns
  } else {
    recipes::sel2char(x$terms)
  }

  n <- length(terms)

  tibble::tibble(
    terms         = as.character(terms),
    preprocessing = rep(x$preprocessing, n),
    window_size   = rep(x$window_size, n),
    masked        = if (trained) terms %in% x$masked else rep(NA, n),
    id            = rep(x$id, n)
  )

}

## ---------------------------------------------------------------------------
## print method
## ---------------------------------------------------------------------------

#' @export
print.step_transform_spectra <- function(x, width = max(20, options()$width - 30), ...) {

  n_masked <- length(x$masked)

  cat("Spectral transformation step using", x$preprocessing,
      if (n_masked > 0) paste0("(", n_masked, " masked columns dropped after it)"), "\n")
  invisible(x)

}

## ---------------------------------------------------------------------------
## Matrix-level spectral processing
## ---------------------------------------------------------------------------

#' Transform a Spectral Matrix
#'
#' @description
#' Applies one of the supported preprocessing methods to a whole spectral
#' matrix (rows are spectra, columns are wavenumbers in monotonic order) in a
#' single prospectr call. This is the implementation behind
#' `bake.step_transform_spectra()`, and the unit a caller can use to apply the
#' same preprocessing outside a recipe.
#'
#' Every method is row-wise, so the result for a given spectrum does not
#' depend on which other rows are present. Output width is
#' `ncol(X) - (window_size - 1)` for every method: `raw` and `snv` drop
#' `(window_size - 1) / 2` columns from each edge explicitly, the
#' Savitzky-Golay methods lose the same edge inside `prospectr::savitzkyGolay()`.
#'
#' With `masked`, the method still runs over every column, so a window beside
#' a masked band reads its real values; SNV's mean and spread come from the
#' unmasked columns (`snv_unmasked()`); and the masked columns are dropped
#' from the result, after the edge trim. With nothing masked the result is
#' the one without the argument, to the bit.
#'
#' @param X Numeric matrix (or something `as.matrix()` turns into one). One
#'   spectrum per row.
#' @param preprocessing Character. One of the methods documented in
#'   [step_transform_spectra()], which checks it.
#' @param window_size Odd integer. Savitzky-Golay window size. Default 9.
#' @param masked Logical, one per column of `X`, or `NULL` (the default)
#'   for none: the masked columns.
#'
#' @return A numeric matrix with `nrow(X)` rows and
#'   `ncol(X) - (window_size - 1)` columns, less the masked columns among
#'   them, without dimnames. Rows whose input
#'   is entirely non-finite come back entirely `NA`; no error is raised for
#'   them, and a flat row comes back as whatever the method makes of it, so
#'   the step's `bake()` applies [standardize()]'s rule to the rows itself.
#' @keywords internal
transform_spectra_matrix <- function(X, preprocessing, window_size = 9, masked = NULL) {

  X <- as.matrix(X)
  if (!is.double(X)) storage.mode(X) <- "double"

  half_window <- (window_size - 1) / 2
  keep        <- seq.int(1 + half_window, ncol(X) - half_window)

  ### The columns returned: the edge trim's, less the masked ones
  masking  <- !is.null(masked) && any(masked)
  returned <- if (masking) !masked[keep] else rep(TRUE, length(keep))

  ## prospectr's convolution has nothing to do on an empty matrix; return the
  ## right shape rather than letting it error.
  if (nrow(X) == 0L) {
    return(matrix(numeric(0), nrow = 0L, ncol = sum(returned)))
  }

  sg  <- function(M, m, p) prospectr::savitzkyGolay(M, m = m, p = p, w = window_size)
  snv <- if (masking) {
    function(M) snv_unmasked(M, masked)
  } else {
    function(M) prospectr::standardNormalVariate(M)
  }

  out <- switch(as.character(preprocessing),

    "raw"        = X[, keep, drop = FALSE],
    "sg"         = sg(X, m = 0, p = 1),
    "snv"        = snv(X)[, keep, drop = FALSE],
    "deriv1"     = sg(X, m = 1, p = 1),
    "deriv2"     = sg(X, m = 2, p = 3),
    "snv_deriv1" = sg(snv(X), m = 1, p = 1),
    "snv_deriv2" = sg(snv(X), m = 2, p = 3),

    ### step_transform_spectra() checks the method when it is added (#199)
    abort_horizons("Unknown preprocessing method {.val {preprocessing}}.", kind = "internal")

  )

  out <- as.matrix(out)
  dimnames(out) <- NULL

  ## Dropped after the method, so it ran over their values
  if (masking) out <- out[, returned, drop = FALSE]

  out

}

#' Standard normal variate with its statistics from the unmasked columns
#'
#' @description
#' Each spectrum is centred on its mean and scaled by its standard deviation
#' (divisor `n - 1`), both taken over the unmasked columns alone, and the
#' result is applied to every column, so a derivative after it has the
#' masked columns too. This is prospectr's own arithmetic
#' (`prospectr::standardNormalVariate()`, 0.2.11) with the statistics read
#' from a subset: a missing value is left out of both, and a spread of zero
#' or one that cannot be computed is set to `Inf`, which returns zeros.
#' With nothing masked it equals prospectr's result to the bit, and with a
#' mask its unmasked columns equal prospectr's on the matrix without the
#' masked columns; a test holds both, so a change in prospectr's arithmetic
#' fails the suite rather than moving results quietly. The statistics are
#' taken from the unmasked bands so a constituent that varies only inside
#' the masked bands (water, at the water bands) does not set the scale of
#' the rest.
#'
#' @param M Numeric matrix, one spectrum per row.
#' @param masked Logical, one per column of `M`.
#' @return `M`, centred and scaled row by row, with its dimnames.
#' @keywords internal
#' @noRd
snv_unmasked <- function(M, masked) {

  M <- as.matrix(M)
  U <- M[, !masked, drop = FALSE]

  mn    <- rowMeans(U, na.rm = TRUE)
  U     <- U - mn
  n_obs <- rowSums(!is.na(U))
  sds   <- sqrt(rowSums(U^2, na.rm = TRUE) / (n_obs - 1))

  zero_var <- sds == 0 | is.na(sds)
  if (any(zero_var)) sds[zero_var] <- Inf

  (M - mn) / sds

}

## ---------------------------------------------------------------------------
## The training rows' spectra, transformed once
## ---------------------------------------------------------------------------

#' Transform the spectra of the rows a configuration's folds are cut from
#'
#' @description
#' The frame `step_transform_spectra()` bakes from `data` as [build_recipe()]
#' adds it: a recipe of `build_recipe()`'s roles and the transform step alone
#' (its `recipe_roles()` and `add_spectral_step()`), prepped on `data` and
#' baked with `new_data = NULL`. It is the step's own output, not a copy of
#' its rules: the non-spectral columns first, then the spectra under the
#' names the step's `prep()` gives them (`recipes::names0()`), as doubles,
#' with a spectrum the step cannot use set to `NA`. Every method is row-wise
#' ([transform_spectra_matrix()]), so a fold's rows of this frame are what
#' the step makes of that fold alone, and each fold's recipe is built over
#' it without the step (`build_recipe(transformed = TRUE)`).
#'
#' The step's id is drawn from the stream (`recipes::rand_id()`), so the
#' caller's stream is left as found. The step's warning about a spectrum it
#' cannot use is muffled ([quiet_selection_warnings()]): this is not the
#' recipe the configuration returns, and the rows a verb models hold no such
#' spectrum, since [validate_horizons_data()] refuses one in a row not
#' flagged and [modelling_rows()] leaves flagged rows out.
#'
#' @param data Data frame. The rows the configuration's folds were cut from.
#' @param config_row Single-row tibble; its `preprocessing` is read.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param sg_window Odd integer. The Savitzky-Golay window, in grid points.
#' @return Tibble with `nrow(data)` rows: `data`'s non-spectral columns, then
#'   the transformed spectra.
#' @keywords internal
#' @noRd
hoist_spectra <- function(data, config_row, role_map, sg_window = DEFAULT_SG_WINDOW) {

  local_rng_state()

  rec <- add_spectral_step(recipe_roles(data, role_map), role_map,
                           config_row$preprocessing, sg_window)

  quiet_selection_warnings(
    recipes::bake(recipes::prep(rec, training = data), new_data = NULL)
  )

}

## ---------------------------------------------------------------------------
## required_pkgs
## ---------------------------------------------------------------------------

#' Packages a worker needs to run step_transform_spectra()
#'
#' tune asks a workflow's steps which packages to load on each worker before
#' it preps and bakes the recipe there. Without this method a worker that has
#' not loaded horizons cannot find the step's prep() and bake() methods.
#'
#' @param x A `step_transform_spectra` step.
#' @param ... Not used.
#' @return Character: the packages the step calls, and horizons.
#' @importFrom recipes required_pkgs
#' @keywords internal
#' @noRd
#' @export
required_pkgs.step_transform_spectra <- function(x, ...) {

  c("prospectr", "horizons")

}
