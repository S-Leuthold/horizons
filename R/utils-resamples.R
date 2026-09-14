## ---------------------------------------------------------------------------
## Resample transfer helpers
## ---------------------------------------------------------------------------
##
## `rsample` objects share their underlying data frame by reference: an
## `initial_split` and the five `vfold_cv` splits derived from it all point at
## one table, so in memory they cost one copy. R's serializer does not
## deduplicate data frames, though, so each reference becomes a full copy the
## moment the objects are sent to a parallel worker.
##
## Measured on the KSSL clay training set at 2 cm-1 (17,788 x 1,701), the pair
## evaluate() exported was 418 MB resident but 1,158.7 MB serialized against
## 231.7 MB of unique data — a 5x tax paid once per future. Sending the data
## once with integer indices and rebuilding worker-side pays it once.


#' Extract Resample Indices for Worker Transfer
#'
#' @description
#' Reduces an `rsplit` and its `rset` to the integers needed to rebuild them,
#' plus the metadata `rsample` stores as attributes. The result is a few hundred
#' kilobytes regardless of how wide the spectra are.
#'
#' `split_args` is carried because `tune_grid_loop()` reads it via
#' `rsample::.get_split_args()` and passes it to
#' `rsample::internal_calibration_split()`. `manual_rset()` does not reproduce
#' those attributes on its own, so they are transported explicitly and restored
#' by `rebuild_resamples()`.
#'
#' @param split An `rsplit` (typically from `rsample::initial_split()`).
#' @param cv_folds An `rset` (typically from `rsample::vfold_cv()`) built from
#'   `rsample::training(split)`.
#'
#' @return A list with `train_idx`, `n_rows`, `fold_idx`, `fold_ids`,
#'   `fold_class`, and `split_args`. Carries no data.
#' @keywords internal
#' @noRd

resample_indices <- function(split, cv_folds) {

  ## -------------------------------------------------------------------------
  ## Guard the two assumptions the reconstruction rests on
  ## -------------------------------------------------------------------------
  ## 1. Complement semantics. rebuild_resamples() derives the assessment set as
  ##    setdiff(all rows, analysis rows), which is only correct when a split's
  ##    out_id is NA. initial_split() and vfold_cv() both store NA, but a
  ##    gap-carrying rset (sliding_window(), rolling_origin()) stores explicit
  ##    indices, and silently re-partitioning one of those would put rows the
  ##    model trained on into the assessment set.
  ## 2. A single id column. manual_rset() takes one `ids` vector, so a repeated
  ##    rset's id2 would be dropped and v * repeats resamples would collapse
  ##    onto v identifiers.

  all_splits <- c(list(split), cv_folds$splits)

  if (!all(vapply(all_splits, function(s) all(is.na(s$out_id)), logical(1)))) {

    rlang::abort(paste0(
      "`resample_indices()` supports only resamples whose assessment set is ",
      "the complement of its analysis set (`out_id` is NA). Got a split with ",
      "explicit `out_id`, which this transport would silently re-partition."
    ))

  }

  id_cols <- grep("^id", names(cv_folds), value = TRUE)

  if (length(id_cols) != 1L) {

    rlang::abort(paste0(
      "`resample_indices()` supports only single-id resamples. Got ",
      length(id_cols), " id columns (", paste(id_cols, collapse = ", "),
      "), which indicates repeats > 1; the extra identifiers would be lost."
    ))

  }

  list(
    train_idx   = split$in_id,
    n_rows      = nrow(split$data),
    split_class = class(split),
    fold_idx    = lapply(cv_folds$splits, function(s) s$in_id),
    fold_ids    = cv_folds$id,
    fold_class  = class(cv_folds),
    fold_split_class = class(cv_folds$splits[[1]]),
    split_args  = rsample::.get_split_args(cv_folds)
  )

}


#' Rebuild a Split and Resample Set From Indices
#'
#' @description
#' Inverse of `resample_indices()`. Reconstructs the `rsplit` and `rset` from
#' one copy of the data plus the transported indices, then restores the original
#' class and attributes so `rsample::.get_split_args()` reports what it did
#' before the round trip.
#'
#' Fold membership is reproduced by construction, not by replaying the RNG, so
#' the result does not depend on stratified sampling behaving identically in the
#' worker.
#'
#' @param data The analysis data frame — the same table `split` was built from.
#' @param idx The list returned by `resample_indices()`.
#'
#' @return A list with `split` (an `rsplit`) and `cv_folds` (an `rset`),
#'   equivalent to the originals.
#' @keywords internal
#' @noRd

rebuild_resamples <- function(data, idx) {

  ## Fail at the boundary rather than deep inside tune, where an out-of-range
  ## index surfaces as "Grid search failed" for every config, from a worker.
  if (nrow(data) != idx$n_rows) {

    rlang::abort(paste0(
      "`rebuild_resamples()` got ", nrow(data), " rows but the indices were ",
      "computed against ", idx$n_rows, ". The data and indices must come from ",
      "the same split."
    ))

  }

  stopifnot(
    max(idx$train_idx) <= idx$n_rows,
    max(unlist(idx$fold_idx)) <= length(idx$train_idx)
  )

  ## `class` is passed so the rebuilt splits keep their subclass. Without it
  ## make_splits() returns a bare rsplit, and rsample::internal_calibration_split()
  ## — the very function split_args is transported for — dispatches on the split
  ## subclass with a hard-aborting default method.
  split <- rsample::make_splits(
    x     = list(analysis   = idx$train_idx,
                 assessment = setdiff(seq_len(idx$n_rows), idx$train_idx)),
    data  = data,
    class = setdiff(idx$split_class, "rsplit")
  )

  train_data <- rsample::training(split)
  n_train    <- nrow(train_data)

  fold_splits <- lapply(idx$fold_idx, function(i) {

    rsample::make_splits(
      x     = list(analysis   = i,
                   assessment = setdiff(seq_len(n_train), i)),
      data  = train_data,
      class = setdiff(idx$fold_split_class, "rsplit")
    )

  })

  cv_folds <- rsample::manual_rset(fold_splits, ids = idx$fold_ids)

  ## Restore what manual_rset() does not carry. .get_split_args() resolves the
  ## creating function from class(x)[[1]] and reads its formals off the object's
  ## attributes, so both have to be put back.
  class(cv_folds) <- idx$fold_class

  for (nm in names(idx$split_args)) {

    attr(cv_folds, nm) <- idx$split_args[[nm]]

  }

  list(split = split, cv_folds = cv_folds)

}
