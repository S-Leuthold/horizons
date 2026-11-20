## ===========================================================================
## Each fold's recipe prepped once, and the folds a configuration assembles
## ===========================================================================
##
## tune preps a workflow's recipe on every fold for every tune call, so a
## configuration's grid and each of its Bayesian iterations prep the folds
## again, and a selection step (CARS, Boruta) selects afresh for each
## candidate on a stream tune draws. Instead each fold's recipe is prepped
## once, under a seed of its own, and tune searches a workflow of the model
## alone over folds that hold the baked rows. The final fit keeps the whole
## workflow on the raw rows, so the stored workflow and predict() do not
## change.
##
## What a fold's prep decides depends on the preprocessing, the feature
## selection and, for a selection that reads the outcome, the response
## transformation, and not on the model. So the prep is a unit of its own,
## named by what it is (fold_prep_scope()), and its value is an entry
## (prep_fold_entry()): the predictors it kept, the columns it made or
## changed, the warnings it raised and its records. The run builds each
## distinct entry once, before any configuration runs; a configuration
## called on its own builds its own with the same code. Each configuration
## assembles its folds from its entries, its training rows with their
## spectra transformed once, and its own outcome (assemble_folds()). A
## configuration with no feature selection trains nothing on a fold, so it
## has no entry: its folds are its transformed rows and its outcome.

#' The rows and outcome a configuration's folds are assembled over
#'
#' @description
#' The rows the folds were cut from with their spectra transformed once
#' ([hoist_spectra()]), and the configuration's outcome on them
#' ([fold_response()]): the value each row takes in a fold's analysis rows,
#' and the predictors the transformed rows hold before any selection. Both
#' draw step ids from the stream (`recipes::rand_id()`), so the caller's
#' stream is left as found: the whole recipe, built before this, keeps the
#' ids it would have had. Each is the first part of the folds' preparation,
#' so a failure in either is named as theirs.
#'
#' @param config_row Single-row tibble from `config$configs`.
#' @param data Data frame. The rows the folds were cut from.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param sg_window Odd integer. The Savitzky-Golay window, in grid points.
#' @return List with `data` (the transformed frame), `outcome` (the outcome
#'   at every row of `data`, as a fold's analysis rows hold it) and
#'   `predictors` (the predictor columns of `data`, in order).
#' @keywords internal
#' @noRd
fold_prep_inputs <- function(config_row, data, role_map, sg_window) {

  local_rng_state()

  hoisted  <- unit_step("fold_prep", hoist_spectra(data, config_row, role_map,
                                                   sg_window = sg_window))
  response <- unit_step("fold_prep", fold_response(config_row, hoisted, role_map))

  list(data = hoisted, outcome = response$outcome, predictors = response$predictors)

}

#' The outcome a configuration's folds train on, and the predictors before
#' any selection
#'
#' @description
#' The configuration's response transformation alone ([add_response_step()])
#' over the transformed rows, prepped on them and baked with
#' `new_data = NULL`. The transformations are row-wise and estimate nothing,
#' so a row's value here is the value a fold's prep gives it on the fold's
#' analysis rows, whichever rows are prepped with it. With no feature
#' selection this is the whole fold recipe, so its predictors are a fold's.
#'
#' @param config_row Single-row tibble; its `transformation` is read.
#' @param hoisted The transformed rows ([hoist_spectra()]).
#' @param role_map Tibble with `variable` and `role` columns.
#' @return List with `outcome` (the outcome column, baked) and `predictors`
#'   (the prepped recipe's predictor columns, in baked order).
#' @keywords internal
#' @noRd
fold_response <- function(config_row, hoisted, role_map) {

  local_rng_state()

  outcome_col <- role_map$variable[role_map$role == "outcome"]

  rec     <- add_response_step(recipe_roles(hoisted, role_map), config_row$transformation)
  prepped <- recipes::prep(rec, training = hoisted)
  baked   <- recipes::bake(prepped, new_data = NULL)

  list(outcome = baked[[outcome_col]], predictors = prepped_predictors(prepped, baked))

}

#' The unit a configuration's fold preps are named under
#'
#' @description
#' Named by what is prepped rather than by the configuration: the verb, then
#' `"prep"`, the preprocessing, the feature selection, and, for a selection
#' that reads the outcome (`OUTCOME_SELECTIONS`), the response
#' transformation. Two configurations that prep the same recipe on the same
#' rows draw the same seeds, so a selection step chooses the same bands on
#' each fold for both. A fold's prep is the fold's id under it, and the
#' prep that bounds the tuning parameters is `"training"` under it.
#'
#' The name seeds the prep; it does not identify the prepped fold. Under no
#' selection or PCA two configurations that differ only in the
#' transformation share the name while their folds' outcome columns differ,
#' so an entry holds the predictors only, and each configuration places its
#' own outcome ([assemble_folds()]).
#'
#' @param verb Character(1). `"evaluate"` or `"fit"`.
#' @param config_row Single-row tibble from `config$configs`.
#' @return Character: the name's parts.
#' @keywords internal
#' @noRd
fold_prep_scope <- function(verb, config_row) {

  selection <- tolower(as.character(config_row$feature_selection))

  c(verb, "prep", tolower(as.character(config_row$preprocessing)), selection,
    if (selection %in% OUTCOME_SELECTIONS) tolower(as.character(config_row$transformation)))

}

#' The workflow tune searches over prepped folds: the model alone
#'
#' @description
#' The outcome and every other column of a prepped fold's frame
#' ([assemble_folds()]) as the predictors, and the model. No recipe step is
#' tuned ([build_recipe()] has no `tune()`), so its parameter set is the
#' configuration's workflow's, which the grid and the bounds are read from.
#'
#' @param model_spec A parsnip model specification.
#' @param outcome_col Character(1). The outcome column.
#' @return An untrained `workflow`.
#' @keywords internal
#' @noRd
fold_workflow <- function(model_spec, outcome_col) {

  ## Forced here, as tuning_metric_set() forces what its metrics close over:
  ## the variables' selectors keep this frame, and tune serializes the
  ## workflow to a worker with every task, so an unforced promise would carry
  ## the caller's frame, its folds and training rows, along with it.
  force(outcome_col)

  workflows::workflow() |>
    workflows::add_variables(outcomes = dplyr::all_of(outcome_col), predictors = dplyr::everything()) |>
    workflows::add_model(model_spec)

}

## ---------------------------------------------------------------------------
## A fold prep's unit: what it is, and its key
## ---------------------------------------------------------------------------

#' What a configuration's fold preps are, apart from the configuration
#'
#' @description
#' The preprocessing, the feature selection, and the response transformation
#' where the selection reads the outcome (`OUTCOME_SELECTIONS`), `"none"`
#' otherwise: the recipe a fold is prepped with is built from these alone
#' ([fold_unit_recipe()]), so one PCA prep serves every transformation.
#'
#' @param config_row Single-row tibble from `config$configs`.
#' @return Single-row tibble with `preprocessing`, `feature_selection` and
#'   `transformation`.
#' @keywords internal
#' @noRd
fold_unit_row <- function(config_row) {

  selection <- as.character(config_row$feature_selection)

  tibble::tibble(
    preprocessing     = as.character(config_row$preprocessing),
    feature_selection = selection,
    transformation    = if (tolower(selection) %in% OUTCOME_SELECTIONS) {
      as.character(config_row$transformation)
    } else {
      "none"
    }
  )

}

#' The recipe a fold prep's unit preps
#'
#' @description
#' [build_recipe()] over the transformed rows (`transformed = TRUE`), from
#' the unit's row ([fold_unit_row()]), drawing its step ids without moving
#' the caller's stream.
#'
#' @param unit_row Single-row tibble from [fold_unit_row()].
#' @param hoisted The transformed rows ([hoist_spectra()]).
#' @param role_map Tibble with `variable` and `role` columns.
#' @param sg_window,pca_threshold Passed to [build_recipe()].
#' @return An untrained `recipe`.
#' @keywords internal
#' @noRd
fold_unit_recipe <- function(unit_row, hoisted, role_map, sg_window, pca_threshold) {

  local_rng_state()

  build_recipe(unit_row, hoisted, role_map,
               sg_window     = sg_window,
               pca_threshold = pca_threshold,
               transformed   = TRUE)

}

#' Whether a model's parameters are bounded by the predictors a recipe keeps
#'
#' @description
#' Whether it tunes one of `BOUNDED_PARAMETERS` (`MODEL_SPECS`' `tuned`), so
#' its configuration's recipe is prepped on the whole training part for the
#' bound, as well as on each fold. The single-config runners read the same
#' from the configuration's parameter set.
#'
#' @param model Character(1). A name in `MODEL_SPECS`.
#' @return Logical(1).
#' @keywords internal
#' @noRd
bounded_model <- function(model) {

  any(BOUNDED_PARAMETERS %in% MODEL_SPECS[[model]]$tuned)

}

#' The record a fold prep's entry is keyed on
#'
#' @description
#' Everything the prep's recipe and rows are built from: the verb, the
#' preprocessing and the window, the selection and `pca_threshold` (both
#' settings enter every key, since every recipe is built from both), the
#' transformation where the selection reads the outcome ([fold_unit_row()]),
#' the fold and its analysis rows (their sample ids, in order), the training
#' data ([eval_data_fingerprint()]'s fields, or `NULL` for a store that dies
#' with its call), and the seed with the rule that turns it into the prep's
#' draws (`SEED_RULE`) and the scoring schema. Two preps with one key are
#' one prep; an entry is read only under its own key ([read_fold_entry()]).
#'
#' @param verb Character(1). `"evaluate"` or `"fit"`.
#' @param unit_row Single-row tibble from [fold_unit_row()].
#' @param fold Character(1). The fold's id, or `"training"` for the whole
#'   training part.
#' @param ids Character. The sample ids of the rows the prep is trained on,
#'   in order.
#' @param data Named list or `NULL`. The training data's fingerprint fields.
#' @param seed The run's root.
#' @param sg_window,pca_threshold The object's recipe settings.
#' @return Named list.
#' @keywords internal
#' @noRd
fold_prep_key <- function(verb, unit_row, fold, ids, data, seed, sg_window, pca_threshold) {

  normalize_eval_settings(list(
    verb              = verb,
    preprocessing     = tolower(unit_row$preprocessing),
    sg_window         = sg_window,
    feature_selection = tolower(unit_row$feature_selection),
    pca_threshold     = pca_threshold,
    transformation    = tolower(unit_row$transformation),
    fold              = fold,
    rows              = digest::digest(as.character(ids), algo = "xxhash64"),
    data              = data,
    seed              = seed,
    seed_rule         = SEED_RULE,
    scoring_schema    = SCORING_SCHEMA
  ))

}

#' The file a fold prep's entry is kept in
#'
#' @description
#' `fold-<hash>.rds`, the hash digest's of the entry's key under its
#' defaults, the package's one hashing rule ([unit_seed()]): a name no other
#' prep draws, and the same in every process of a run.
#'
#' @param key A record from [fold_prep_key()].
#' @return Character(1).
#' @keywords internal
#' @noRd
fold_entry_file <- function(key) {

  paste0("fold-", digest::digest(key), ".rds")

}

#' The sample ids a configuration's folds index
#'
#' @param data The rows the folds were cut from.
#' @param role_map Tibble with `variable` and `role` columns.
#' @return Character, one per row: the id column's values, or the row
#'   positions when the rows carry no id column.
#' @keywords internal
#' @noRd
fold_row_ids <- function(data, role_map) {

  id_col <- id_column(role_map)

  if (id_col %in% names(data)) as.character(data[[id_col]]) else as.character(seq_len(nrow(data)))

}

#' The fold preps one configuration reads
#'
#' @description
#' One unit per fold of `folds`, and one on the whole training part
#' (`"training"`) when `bounded`, each with its key ([fold_prep_key()]), its
#' file ([fold_entry_file()]), a label for its messages and its cost
#' (`FOLD_PREP_COST_SECONDS`). A configuration with no feature selection has
#' none.
#'
#' @param verb Character(1). `"evaluate"` or `"fit"`.
#' @param config_row Single-row tibble from `config$configs`.
#' @param folds The `rset` the configuration is tuned over.
#' @param role_map Tibble with `variable` and `role` columns.
#' @param data,seed,sg_window,pca_threshold Passed to [fold_prep_key()].
#' @param bounded Logical(1). Whether the configuration's model is bounded
#'   ([bounded_model()]).
#' @return List with `training` (a unit, or `NULL`) and `folds` (a list of
#'   units in fold order, or `NULL`). Each unit is a list: `verb`, `row`
#'   ([fold_unit_row()]), `fold`, `fold_i` (its position in `folds`, `0` for
#'   the training part), `key`, `name`, `label` and `cost`.
#' @keywords internal
#' @noRd
config_fold_units <- function(verb, config_row, folds, role_map, data, seed, sg_window,
                              pca_threshold, bounded) {

  unit_row <- fold_unit_row(config_row)

  if (identical(tolower(unit_row$feature_selection), "none")) return(list(training = NULL, folds = NULL))

  ids  <- fold_row_ids(folds$splits[[1]]$data, role_map)
  cost <- FOLD_PREP_COST_SECONDS[[tolower(unit_row$feature_selection)]]

  unit_of <- function(fold, fold_i, trained_on) {
    key <- fold_prep_key(verb, unit_row, fold, trained_on, data, seed, sg_window, pca_threshold)
    list(verb   = verb,
         row    = unit_row,
         fold   = fold,
         fold_i = as.integer(fold_i),
         key    = key,
         name   = fold_entry_file(key),
         label  = paste(c(fold_prep_scope(verb, unit_row)[-(1:2)], fold), collapse = "/"),
         cost   = cost)
  }

  list(
    training = if (isTRUE(bounded)) unit_of("training", 0L, ids),
    folds    = lapply(seq_len(nrow(folds)), function(i) {
      unit_of(folds$id[[i]], i, ids[folds$splits[[i]]$in_id])
    })
  )

}

#' A fold's analysis and assessment rows
#'
#' @param split An `rsplit`.
#' @return List with `in_id` and `out_id`, positions in the split's data.
#' @keywords internal
#' @noRd
fold_rows <- function(split) {

  list(in_id = split$in_id, out_id = rsample::complement(split))

}

## ---------------------------------------------------------------------------
## One fold prep, as a unit
## ---------------------------------------------------------------------------

#' Prep one fold's recipe, as a unit of its own, and keep what it decided
#'
#' @description
#' The unit's recipe ([fold_unit_recipe()]) is prepped under the unit's seed
#' ([with_unit_seed()], [fold_prep_scope()] then the fold's id). On a fold
#' it is prepped on the fold's analysis rows of `hoisted`, its analysis rows
#' baked with `new_data = NULL` and its assessment rows with `new_data`, and
#' each predictor the prep kept is placed at the training rows' positions
#' from the two bakes: where the result is identical to `hoisted`'s column
#' of the same name (a band a selection kept) the entry names it, so the
#' configuration shares that column rather than copying it, and a column
#' the prep made or changed (a PCA score) is kept whole. On the whole
#' training part (`rows = NULL`) it is prepped once and baked, and the
#' entry names the predictors the bound is read from.
#'
#' The selection steps' warnings are muffled ([quiet_selection_warnings()]):
#' a configuration's come from its final fit's recipe alone. On a fold any
#' other package's warning is kept with the fold it came from
#' ([capture_fold_warnings()]). The rest of what the prep raises is recorded
#' by its unit ([run_unit()]): a horizons warning or note, another package's
#' warning on the training part, or the failure, which ends the unit with
#' the step's own class. Every configuration that reads the entry records
#' them as its own ([replay_unit_records()]).
#'
#' @param unit A unit from [config_fold_units()].
#' @param hoisted The transformed rows the folds were cut from
#'   ([hoist_spectra()]).
#' @param rows `NULL` for the training part, or the fold's rows
#'   ([fold_rows()]).
#' @param role_map Tibble with `variable` and `role` columns.
#' @param seed The run's root.
#' @param sg_window,pca_threshold The object's recipe settings.
#' @param debug,keep_traces Passed to [run_unit()].
#' @param recipe The recipe to prep, or `NULL` for the unit's own.
#' @return The entry: a list with `key` (the unit's), `failed`, `records`
#'   (the unit's, shaped like [new_unit_records()]'s), `kept` (the
#'   predictors, in baked order), `made` (named list of the columns the prep
#'   made or changed, each at every row of `hoisted`), `n_predictors`,
#'   `arrivals` (another package's warnings on the fold, with the fold) and
#'   `runtime_secs`.
#' @keywords internal
#' @noRd
prep_fold_entry <- function(unit, hoisted, rows, role_map, seed, sg_window, pca_threshold,
                            debug = FALSE, keep_traces = FALSE, recipe = NULL) {

  start_time <- Sys.time()
  scope      <- fold_prep_scope(unit$verb, unit$row)

  out <- run_unit({

    recipe <- recipe %||% unit_step("recipe", fold_unit_recipe(unit$row, hoisted, role_map,
                                                               sg_window, pca_threshold))

    if (is.null(rows)) {

      ## The training part, for the bound: not the model returned, so the
      ## selection steps' warnings are not taken from it
      prepped <- unit_step("prep", with_unit_seed(seed, c(scope, "training"),
                                                  quiet_selection_warnings(recipes::prep(recipe))))
      baked   <- unit_step("prep", recipes::bake(prepped, new_data = NULL))

      list(kept = prepped_predictors(prepped, baked), made = list(), arrivals = no_arrivals())

    } else {

      unit_step("fold_prep", prep_one_fold(recipe, hoisted, rows, role_map, seed, scope, unit$fold))

    }

  }, unit = "fold preparation", id = unit$label, debug = debug, keep_traces = keep_traces)

  value <- out$value

  list(
    key          = unit$key,
    failed       = out$failed,
    records      = out$records,
    kept         = value$kept %||% character(0),
    made         = value$made %||% list(),
    n_predictors = length(value$kept),
    arrivals     = value$arrivals %||% no_arrivals(),
    runtime_secs = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  )

}

#' One fold's prep, its two bakes, and what it kept
#'
#' @inheritParams prep_fold_entry
#' @param recipe The untrained recipe.
#' @param scope Character. The unit's name before the fold's id.
#' @param fold Character(1). The fold's id.
#' @return List with `kept`, `made` and `arrivals`, as [prep_fold_entry()]
#'   gives them.
#' @keywords internal
#' @noRd
prep_one_fold <- function(recipe, hoisted, rows, role_map, seed, scope, fold) {

  id_col <- id_column(role_map)
  ids    <- if (id_col %in% names(hoisted)) hoisted[[id_col]]
  n      <- nrow(hoisted)
  in_id  <- rows$in_id
  out_id <- rows$out_id

  caught <- capture_fold_warnings(with_unit_seed(seed, c(scope, fold), quiet_selection_warnings({
    prepped <- recipes::prep(recipe, training = hoisted[in_id, , drop = FALSE])
    list(prepped    = prepped,
         analysis   = recipes::bake(prepped, new_data = NULL),
         assessment = recipes::bake(prepped, new_data = hoisted[out_id, , drop = FALSE]))
  })), fold)

  an <- caught$value$analysis
  as <- caught$value$assessment

  ## Placed by position, so the rows must be the fold's, in its order
  check_rows_aligned(paste0("The baked analysis rows of fold ", fold), "the fold's analysis rows",
                     n = nrow(an), n_expected = length(in_id),
                     ids = an[[id_col]], expected_ids = ids[in_id])
  check_rows_aligned(paste0("The baked assessment rows of fold ", fold), "the fold's assessment rows",
                     n = nrow(as), n_expected = length(out_id),
                     ids = as[[id_col]], expected_ids = ids[out_id])

  ## Each predictor placed from the two bakes; one identical to hoisted's
  ## column of the same name is named only, for the configuration to share
  kept  <- prepped_predictors(caught$value$prepped, an)
  blank <- rep(NA_integer_, n)
  made  <- list()

  for (col in kept) {

    v         <- an[[col]][blank]
    v[in_id]  <- an[[col]]
    v[out_id] <- as[[col]]

    if (!(col %in% names(hoisted) && identical(v, hoisted[[col]]))) made[[col]] <- v

  }

  list(kept = kept, made = made, arrivals = caught$arrivals)

}

#' No warnings from a fold's prep
#'
#' @return Tibble shaped like [capture_fold_warnings()]'s `arrivals`, with no
#'   rows.
#' @keywords internal
#' @noRd
no_arrivals <- function() {

  tibble::tibble(cause = character(0), where = character(0), fold = character(0))

}

## ---------------------------------------------------------------------------
## The configuration's folds, from its entries
## ---------------------------------------------------------------------------

#' Where a configuration's fold preps come from
#'
#' @description
#' The entries the run built for it (`fold_entries`), or, for a
#' configuration called on its own, entries it builds as it reads them, with
#' the stage's own code ([prep_fold_entry()]), so a direct call gives a
#' run's numbers. Each is built only once it is read, so a prep that fails
#' leaves the folds after it unprepped, as a run's configuration reads it.
#'
#' @param fold_entries `NULL`, or a list with `training` (an entry or
#'   `NULL`) and `folds` (a list of entries in fold order, or `NULL`).
#' @param units The configuration's units ([config_fold_units()]).
#' @param build A function of a unit and its rows ([fold_rows()], `NULL` for
#'   the training part) that builds its entry.
#' @param folds The `rset` the configuration is tuned over.
#' @return A function of `"training"` or a fold's position, giving that
#'   entry, or `NULL` when the configuration has no such prep.
#' @keywords internal
#' @noRd
fold_entry_reader <- function(fold_entries, units, build, folds) {

  function(which) {

    training <- identical(which, "training")
    unit     <- if (training) units$training else units$folds[[which]]

    if (is.null(unit)) return(NULL)

    if (is.null(fold_entries)) {
      return(build(unit, if (!training) fold_rows(folds$splits[[which]])))
    }

    entry <- if (training) fold_entries$training else fold_entries$folds[[which]]

    if (is.null(entry)) {
      abort_horizons("No fold preparation was handed in for {.val {unit$label}}.", kind = "internal")
    }

    entry

  }

}

#' The predictors a configuration's tuning parameters are bounded by
#'
#' @description
#' Those the whole training part's prep kept, read from its entry, whose
#' records the configuration then holds as its own (a failure ends it
#' here), or, with no feature selection, the transformed rows' predictors.
#'
#' @param entry The training part's entry, or `NULL`.
#' @param inputs [fold_prep_inputs()]'s value.
#' @return Character.
#' @keywords internal
#' @noRd
fold_bound <- function(entry, inputs) {

  if (is.null(entry)) return(inputs$predictors)

  replay_unit_records(entry$records)

  entry$kept

}

#' A parameter set bounded by the predictors a recipe keeps
#'
#' @description
#' [dials::finalize()] reads only the width of the frame it is given for
#' `mtry` and `num_comp`, so it is given a frame of no rows of those
#' predictors; `num_comp` is capped as well ([cap_pls_components()]).
#'
#' @param param_set A `parameters` object.
#' @param predictors Character. The predictors the recipe keeps.
#' @param folds The `rset` the configuration is tuned over.
#' @return The finalized `parameters` object.
#' @keywords internal
#' @noRd
finalize_bound <- function(param_set, predictors, folds) {

  frame     <- tibble::new_tibble(stats::setNames(rep(list(double(0)), length(predictors)), predictors),
                                  nrow = 0L)
  finalized <- dials::finalize(param_set, frame)

  cap_pls_components(finalized, length(predictors), min_analysis_rows(folds))

}

#' Assemble the folds tune searches over, from each fold's prep
#'
#' @description
#' Each fold becomes an `rsplit` over a frame of `nrow(inputs$data)` rows
#' that keeps the training rows' positions, so a prediction's `.row` names
#' the row it was made for, as it does when tune preps. The frame holds the
#' outcome and the fold's predictors and nothing else, so a workflow of the
#' model alone ([fold_workflow()]) takes exactly them as predictors. Every
#' column is placed from the fold's bakes, and where the result is
#' identical to the transformed rows' column of the same name (a spectral
#' band a selection kept, or the outcome under no transformation), that
#' column is used, so it is shared rather than copied. The outcome is the
#' configuration's: its transformation on the analysis rows
#' (`inputs$outcome`), as measured on the assessment rows, so tune scores
#' each fold's predictions on the scale [tuning_metric_set()]
#' back-transforms to.
#'
#' Each fold's entry is read in fold order, and what its prep recorded is
#' recorded by the configuration's unit as its own
#' ([replay_unit_records()]): a prep that failed ends the configuration
#' there, with the step's own class. Another package's warning in a fold's
#' prep is given to the row with the folds that raised it.
#'
#' @param entry_at A function of a fold's position giving its entry, or
#'   `NULL` with no feature selection ([fold_entry_reader()]).
#' @param folds An `rset` of one id column, cut from `inputs$data`'s rows.
#' @param inputs [fold_prep_inputs()]'s value.
#' @param role_map Tibble with `variable` and `role` columns.
#' @return List with `folds` (an `rset` with `folds`' ids) and `warnings`
#'   (records shaped like `new_warning_log()`'s, scope `"cv"`, each with the
#'   folds that raised it).
#' @keywords internal
#' @noRd
assemble_folds <- function(entry_at, folds, inputs, role_map) {

  outcome_col <- role_map$variable[role_map$role == "outcome"]
  hoisted     <- inputs$data
  n           <- nrow(hoisted)
  split_class <- setdiff(class(folds$splits[[1]]), "rsplit")

  arrivals <- vector("list", nrow(folds))

  splits <- lapply(seq_len(nrow(folds)), function(i) {

    entry <- entry_at(i)

    if (!is.null(entry)) {
      replay_unit_records(entry$records)
      arrivals[[i]] <<- entry$arrivals
    }

    rows  <- fold_rows(folds$splits[[i]])
    kept  <- if (is.null(entry)) inputs$predictors else entry$kept
    blank <- rep(NA_integer_, n)

    ### The outcome placed as a fold's two bakes place it: transformed on the
    ### analysis rows, as measured on the assessment rows
    outcome              <- inputs$outcome[blank]
    outcome[rows$in_id]  <- inputs$outcome[rows$in_id]
    outcome[rows$out_id] <- hoisted[[outcome_col]][rows$out_id]

    if (identical(outcome, hoisted[[outcome_col]])) outcome <- hoisted[[outcome_col]]

    columns <- c(stats::setNames(list(outcome), outcome_col),
                 lapply(stats::setNames(kept, kept), function(col) entry$made[[col]] %||% hoisted[[col]]))

    rsample::make_splits(
      x     = list(analysis = rows$in_id, assessment = rows$out_id),
      data  = tibble::new_tibble(columns, nrow = n),
      class = split_class
    )

  })

  list(folds    = rsample::manual_rset(splits, ids = folds$id),
       warnings = fold_warning_records(arrivals, nrow(folds)))

}
