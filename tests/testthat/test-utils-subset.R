# tests/testthat/test-utils-subset.R
# Tests for subset_rows(), set_analysis() (issue #43) and add_columns() (#135)


## =============================================================================
## subset_rows() — Row selection
## =============================================================================

test_that("subset_rows() keeps the named ids in the order given", {

  ## Arrange
  fx   <- make_select_fixture(n_pool = 40)
  keep <- c("P010", "P003", "P025")

  ## Act
  out <- subset_rows(fx$pool, keep)

  ## Assert
  expect_s3_class(out, "horizons_data")
  expect_identical(out$data$analysis$sample_id, keep)
  expect_identical(out$data$n_rows, 3L)

})


test_that("subset_rows() accepts a logical keep of length n_rows", {

  fx   <- make_select_fixture(n_pool = 40)
  keep <- fx$pool$data$analysis$family == 1L

  out <- subset_rows(fx$pool, keep)

  expect_identical(out$data$n_rows, sum(keep))
  expect_true(all(out$data$analysis$family == 1L))

})


test_that("subset_rows() leaves role_map and column counts untouched", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, c("P001", "P002"))

  expect_identical(out$data$role_map,     fx$pool$data$role_map)
  expect_identical(out$data$n_predictors, fx$pool$data$n_predictors)
  expect_identical(out$data$n_responses,  fx$pool$data$n_responses)
  expect_identical(names(out$data$analysis), names(fx$pool$data$analysis))

})


test_that("subset_rows() output passes structural validation", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, c("P005", "P006", "P007"))

  expect_no_error(validate_horizons_data(out))

})


test_that("subset_rows() writes no provenance of its own", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, c("P001", "P002"))

  expect_identical(out$provenance, fx$pool$provenance)

})


## =============================================================================
## subset_rows() — Input validation
## =============================================================================

test_that("subset_rows() rejects non-horizons_data input", {

  expect_error(subset_rows(data.frame(sample_id = "a"), "a"),
               class = "horizons_input_error")

})


test_that("subset_rows() aborts on unknown ids and names them", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(subset_rows(fx$pool, c("P001", "nope", "also_nope")),
               regexp = "nope", class = "horizons_input_error")

})


test_that("subset_rows() aborts on duplicated ids in keep", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(subset_rows(fx$pool, c("P001", "P001")),
               class = "horizons_input_error")

})


test_that("subset_rows() aborts on a logical keep of the wrong length", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(subset_rows(fx$pool, c(TRUE, FALSE)),
               class = "horizons_input_error")

})


test_that("subset_rows() aborts when keep selects no rows", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(subset_rows(fx$pool, character(0)),
               class = "horizons_input_error")

})


## =============================================================================
## set_analysis() — Replace the analysis table and recompute counts
## =============================================================================

test_that("set_analysis() recomputes every count from the role_map", {

  fx <- make_select_fixture(n_pool = 40)

  ## Drop the family column and the oc response
  analysis <- fx$pool$data$analysis
  analysis <- analysis[1:5, setdiff(names(analysis), c("family", "oc"))]
  role_map <- fx$pool$data$role_map
  role_map <- role_map[!role_map$variable %in% c("family", "oc"), ]

  out <- set_analysis(fx$pool, analysis, role_map)

  expect_identical(out$data$n_rows,       5L)
  expect_identical(out$data$n_responses,  1L)
  expect_identical(out$data$n_covariates, 0L)
  expect_identical(out$data$n_predictors, sum(role_map$role == "predictor"))
  expect_no_error(validate_horizons_data(out))

})


test_that("set_analysis() keeps the existing role_map when none is given", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- set_analysis(fx$pool, fx$pool$data$analysis[1:3, ])

  expect_identical(out$data$role_map, fx$pool$data$role_map)
  expect_identical(out$data$n_rows, 3L)

})


test_that("set_analysis() aborts when analysis columns and role_map disagree", {

  fx       <- make_select_fixture(n_pool = 40)
  analysis <- fx$pool$data$analysis
  analysis$extra <- 1

  expect_error(set_analysis(fx$pool, analysis),
               class = "horizons_input_error")

})


## =============================================================================
## Promoted objects — both primitives refuse
## =============================================================================

test_that("subset_rows() aborts on an object carrying evaluation results", {

  ## Arrange
  fx <- make_select_fixture(n_pool = 40)
  ev <- fx$pool
  ev$evaluation$results <- tibble::tibble(config_id = "cfg_a", status = "success")

  ## Act & Assert
  expect_error(subset_rows(ev, c("P001", "P002")),
               regexp = "evaluation results",
               class  = "horizons_input_error")

})


test_that("subset_rows() aborts on an object carrying fitted models", {

  fx <- make_select_fixture(n_pool = 40)
  ft <- fx$pool
  ft$models$workflows <- list(cfg_a = "a fitted workflow")

  expect_error(subset_rows(ft, c("P001", "P002")),
               regexp = "fitted models",
               class  = "horizons_input_error")

})


test_that("subset_rows() aborts on an object carrying an ensemble", {

  fx <- make_select_fixture(n_pool = 40)
  en <- fx$pool
  en$ensemble$method <- "weighted"

  expect_error(subset_rows(en, c("P001", "P002")),
               regexp = "ensemble",
               class  = "horizons_input_error")

})


test_that("subset_rows() aborts on a stored split", {

  ## The slots the verbs actually write: evaluate() fills evaluation$split,
  ## fit() fills models$split.

  fx <- make_select_fixture(n_pool = 40)

  ev <- fx$pool
  ev$evaluation$split <- "an rsplit"

  expect_error(subset_rows(ev, c("P001", "P002")),
               regexp = "evaluation split",
               class  = "horizons_input_error")

  sp <- fx$pool
  sp$models$split <- "an rsplit"

  expect_error(subset_rows(sp, c("P001", "P002")),
               regexp = "model split",
               class  = "horizons_input_error")

})


test_that("promoted_state() reads models$workflows, not a stray row_index (#131)", {

  fx <- make_select_fixture(n_pool = 40)

  ## A row_index left on an otherwise plain object marks nothing
  ri <- fx$pool
  ri$models$row_index <- tibble::tibble(.row = 1L, sample_id = "P001")

  expect_identical(promoted_state(ri), character())
  expect_no_error(subset_rows(ri, c("P001", "P002")))

  ## Fitted workflows do
  wf <- fx$pool
  wf$models$workflows <- list(cfg_a = "a fitted workflow")

  expect_identical(promoted_state(wf), "fitted models")
  expect_error(subset_rows(wf, c("P001", "P002")),
               regexp = "fitted models",
               class  = "horizons_input_error")

})


test_that("subset_rows() refuses an object whose class claims a promotion", {

  ## Arrange — the class alone, with every slot this function knows about
  ## left empty. The claim is enough.
  fx <- make_select_fixture(n_pool = 40)
  ev <- fx$pool
  class(ev) <- c("horizons_eval", "horizons_data", "list")

  ## Act & Assert
  expect_error(subset_rows(ev, c("P001", "P002")),
               regexp = "horizons_eval",
               class  = "horizons_input_error")

})


test_that("subset_rows() names every promoted state it found", {

  fx <- make_select_fixture(n_pool = 40)
  pr <- fx$pool
  pr$evaluation$results <- tibble::tibble(config_id = "cfg_a")
  pr$models$workflows   <- list(cfg_a = "a fitted workflow")

  err <- expect_error(subset_rows(pr, "P001"), class = "horizons_input_error")

  expect_match(conditionMessage(err), "evaluation results")
  expect_match(conditionMessage(err), "fitted models")

})


test_that("set_analysis() aborts on a promoted object", {

  fx <- make_select_fixture(n_pool = 40)
  ft <- fx$pool
  ft$models$workflows <- list(cfg_a = "a fitted workflow")

  expect_error(set_analysis(ft, ft$data$analysis[1:3, ]),
               class = "horizons_input_error")

})


test_that("a freshly loaded object is not treated as promoted", {

  ## Arrange — the constructor's defaults must not read as promotion
  fx <- make_select_fixture(n_pool = 40)

  ## Act & Assert
  expect_identical(promoted_state(fx$pool), character())
  expect_identical(promoted_state(new_horizons_data()), character())
  expect_no_error(subset_rows(fx$pool, c("P001", "P002")))

})


## =============================================================================
## subset_rows() — Maintaining the selection record
## =============================================================================

test_that("subset_rows() filters selection membership to surviving rows", {

  ## Arrange
  obj  <- make_selected_object(n_rows = 12L)
  keep <- obj$data$analysis$sample_id[1:8]

  ## Act
  out <- subset_rows(obj, keep)

  ## Assert
  expect_setequal(unique(out$selection$membership$pool_id), keep)
  expect_true(all(out$selection$membership$pool_id %in% out$data$analysis$sample_id))

})


test_that("subset_rows() refilters each group and recounts n_rows", {

  obj  <- make_selected_object(n_rows = 12L)
  keep <- obj$data$analysis$sample_id[1:8]

  out <- subset_rows(obj, keep)

  ## Every group's ids survive, and the count matches what is left of them
  for (i in seq_len(nrow(out$selection$groups))) {

    ids <- out$selection$groups$pool_ids[[i]]

    expect_true(all(ids %in% keep))
    expect_identical(out$selection$groups$n_rows[i], length(ids))

  }

  expect_identical(sum(out$selection$groups$n_rows), 8L)

})


test_that("subset_rows() recounts pool_sizes$drawn per property", {

  obj  <- make_selected_object(n_rows = 12L)
  keep <- obj$data$analysis$sample_id[1:5]

  out <- subset_rows(obj, keep)

  expect_identical(out$selection$pool_sizes$drawn, c(5L, 5L))

  ## available describes the source pool and does not move
  expect_identical(out$selection$pool_sizes$available,
                   obj$selection$pool_sizes$available)

})


test_that("subset_rows() leaves the draw's own record alone", {

  obj  <- make_selected_object(n_rows = 12L)
  keep <- obj$data$analysis$sample_id[1:6]

  out <- subset_rows(obj, keep)

  expect_identical(out$selection$exclusions,       obj$selection$exclusions)
  expect_identical(out$selection$resemblance,      obj$selection$resemblance)
  expect_identical(out$selection$target_distances, obj$selection$target_distances)
  expect_identical(out$selection$settings,         obj$selection$settings)
  expect_identical(out$selection$clustering,       obj$selection$clustering)
  expect_identical(out$selection$pool,             obj$selection$pool)

})


test_that("subset_rows() records rows_removed and accumulates it", {

  obj <- make_selected_object(n_rows = 12L)

  out <- subset_rows(obj, obj$data$analysis$sample_id[1:9])
  expect_identical(out$selection$rows_removed, 3L)

  out <- subset_rows(out, out$data$analysis$sample_id[1:5])
  expect_identical(out$selection$rows_removed, 7L)

})


test_that("subset_rows() carries the per-row selection meta columns unchanged", {

  ## Arrange
  obj  <- make_selected_object(n_rows = 12L)
  keep <- obj$data$analysis$sample_id[c(2, 5, 9)]

  ## Act
  out <- subset_rows(obj, keep)

  ## Assert — per-row facts, not aggregates, so they survive as they were
  expect_identical(out$data$analysis$.min_distance,
                   obj$data$analysis$.min_distance[c(2, 5, 9)])
  expect_identical(out$data$analysis$.group,
                   obj$data$analysis$.group[c(2, 5, 9)])
  expect_identical(out$data$analysis$.drawn_by,
                   obj$data$analysis$.drawn_by[c(2, 5, 9)])

})


test_that("subset_rows() leaves a recomputed record that still validates", {

  obj <- make_selected_object(n_rows = 12L)
  out <- subset_rows(obj, obj$data$analysis$sample_id[1:7])

  expect_no_error(validate_horizons_data(out))

})


test_that("subset_rows() recounts drawn for a global-scope record", {

  ## Arrange — scope = "global" leaves membership empty by construction, so
  ## the survivors are the draw
  obj <- make_selected_object(n_rows = 12L)

  obj$selection$settings$scope <- "global"
  obj$selection$membership     <- obj$selection$membership[0, , drop = FALSE]

  ## Act
  out <- subset_rows(obj, obj$data$analysis$sample_id[1:4])

  ## Assert
  expect_identical(out$selection$pool_sizes$drawn, c(4L, 4L))
  expect_identical(nrow(out$selection$membership), 0L)

})


test_that("subset_rows() leaves an object without a record alone", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- subset_rows(fx$pool, c("P001", "P002"))

  expect_null(out$selection)

})


test_that("a batch record whose rows have all left is not read as global", {

  ## Arrange — an empty membership is also what a batch draw looks like once
  ## its rows are gone, so scope has to come from settings
  obj <- make_selected_object(n_rows = 12L)
  keep <- obj$data$analysis$sample_id[1:3]

  obj$selection$membership <- obj$selection$membership[0, , drop = FALSE]

  ## Act
  out <- subset_rows(obj, keep)

  ## Assert — nothing is drawn, rather than all three rows
  expect_identical(out$selection$settings$scope, "batch")
  expect_identical(out$selection$pool_sizes$drawn, c(0L, 0L))

})


test_that("subset_selection() keeps membership rows marked retained = FALSE", {

  ## Arrange — the union subtraction removed this neighbour, and the record
  ## keeps it on purpose; it was never a row of the analysis table
  obj <- make_selected_object(n_rows = 12L)
  ids <- obj$data$analysis$sample_id

  dropped <- obj$selection$membership[1, , drop = FALSE]
  dropped$pool_id  <- "P999"
  dropped$retained <- FALSE

  obj$selection$membership <- dplyr::bind_rows(obj$selection$membership, dropped)

  ## Act
  out <- subset_rows(obj, ids[1:6])

  ## Assert
  expect_true("P999" %in% out$selection$membership$pool_id)
  expect_false(any(out$selection$membership$retained &
                   !out$selection$membership$pool_id %in% ids[1:6]))

  ## drawn counts retained rows only, so the exempt row does not inflate it
  expect_identical(out$selection$pool_sizes$drawn, c(6L, 6L))

})


test_that("subset_selection() refuses a membership without retained (#130)", {

  ## Arrange — without the column there is no telling the rows that must
  ## survive from the neighbours the union subtraction already dropped, so
  ## the record is refused rather than read as all retained
  obj <- make_selected_object(n_rows = 12L)
  ids <- obj$data$analysis$sample_id

  obj$selection$membership$retained <- NULL

  ## Act & Assert
  expect_error(subset_selection(obj$selection, keep_ids = ids[1:6], n_removed = 6L),
               regexp = "retained",
               class  = "horizons_validation_error")

  expect_error(set_analysis(obj, obj$data$analysis[1:6, , drop = FALSE]),
               regexp = "retained",
               class  = "horizons_validation_error")

})


## =============================================================================
## set_analysis() — Maintaining the selection record
## =============================================================================

test_that("set_analysis() recomputes the record when the row set shrinks", {

  ## Arrange — collapsing rows outside subset_rows(), the way average() does
  obj  <- make_selected_object(n_rows = 12L)
  keep <- obj$data$analysis$sample_id[1:5]

  ## Act
  out <- set_analysis(obj, obj$data$analysis[1:5, , drop = FALSE])

  ## Assert
  expect_setequal(unique(out$selection$membership$pool_id), keep)
  expect_identical(out$selection$pool_sizes$drawn, c(5L, 5L))
  expect_identical(out$selection$rows_removed, 7L)
  expect_no_error(validate_horizons_data(out))

})


test_that("set_analysis() leaves the record alone when the row set is unchanged", {

  obj <- make_selected_object(n_rows = 12L)

  out <- set_analysis(obj, obj$data$analysis)

  expect_identical(out$selection, obj$selection)

})


test_that("set_analysis() refuses to carry a record onto renamed rows", {

  ## Arrange — new ids are not a subset of the drawn ids, so the record
  ## cannot be refiltered; it can only be wrong
  obj      <- make_selected_object(n_rows = 12L)
  analysis <- obj$data$analysis
  analysis$sample_id <- paste0("NEW_", seq_len(nrow(analysis)))

  ## Act & Assert
  expect_error(set_analysis(obj, analysis),
               regexp = "selection record",
               class  = "horizons_input_error")

})


## =============================================================================
## add_columns() — New columns, rows unchanged
## =============================================================================

test_that("add_columns() appends columns with their roles and recounts", {

  ## Arrange
  fx  <- make_select_fixture(n_pool = 40)
  new <- tibble::tibble(site = rep("A", 40), ph = seq_len(40) / 10)

  ## Act
  out <- add_columns(fx$pool, new, role = "response")

  ## Assert — appended at the end, one response row each, counts recomputed
  n_old <- ncol(fx$pool$data$analysis)

  expect_identical(names(out$data$analysis)[n_old + 1:2], c("site", "ph"))
  expect_identical(out$data$role_map$role[out$data$role_map$variable %in% c("site", "ph")],
                   c("response", "response"))
  expect_identical(out$data$n_responses, fx$pool$data$n_responses + 2L)
  expect_identical(out$data$n_rows, fx$pool$data$n_rows)
  expect_identical(out$data$analysis$sample_id, fx$pool$data$analysis$sample_id)
  expect_no_error(validate_horizons_data(out))

})


test_that("add_columns() places the columns after a named column", {

  fx  <- make_select_fixture(n_pool = 40)
  out <- add_columns(fx$pool, tibble::tibble(site = rep("A", 40)),
                     role = "meta", after = "sample_id")

  expect_identical(names(out$data$analysis)[1:2], c("sample_id", "site"))
  expect_identical(out$data$role_map$role[out$data$role_map$variable == "site"], "meta")

})


test_that("add_columns() runs on a promoted object and leaves its state alone", {

  ## Arrange — new columns move no rows, so nothing a promotion earned is
  ## stranded
  fx <- make_select_fixture(n_pool = 40)
  ft <- fx$pool
  ft$models$workflows <- list(cfg_a = "a fitted workflow")
  class(ft) <- c("horizons_fit", "horizons_eval", "horizons_data", "list")

  ## Act
  out <- add_columns(ft, tibble::tibble(site = rep("A", 40)), role = "meta")

  ## Assert
  expect_true("site" %in% names(out$data$analysis))
  expect_identical(class(out), class(ft))
  expect_identical(out$models, ft$models)

})


test_that("add_columns() checks a sample_id key against the table and drops it", {

  fx  <- make_select_fixture(n_pool = 40)
  ids <- fx$pool$data$analysis$sample_id

  ## In order: accepted, and the key is not added as a column
  out <- add_columns(fx$pool, tibble::tibble(sample_id = ids, site = "A"), role = "meta")

  expect_identical(sum(names(out$data$analysis) == "sample_id"), 1L)
  expect_identical(out$data$analysis$sample_id, ids)

  ## Out of order: refused, so values never attach to the wrong samples
  expect_error(add_columns(fx$pool, tibble::tibble(sample_id = rev(ids), site = "A"),
                           role = "meta"),
               regexp = "row order",
               class  = "horizons_input_error")

})


test_that("add_columns() refuses columns with the wrong row count", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(add_columns(fx$pool, tibble::tibble(site = rep("A", 39)), role = "meta"),
               regexp = "one row per row",
               class  = "horizons_input_error")

})


test_that("add_columns() refuses a column name already present", {

  fx <- make_select_fixture(n_pool = 40)

  expect_error(add_columns(fx$pool, tibble::tibble(family = rep(9L, 40)), role = "meta"),
               regexp = "already in the analysis table",
               class  = "horizons_input_error")

})


test_that("add_columns() refuses roles that modelling reads", {

  fx  <- make_select_fixture(n_pool = 40)
  new <- tibble::tibble(extra = rep(1, 40))

  for (role in c("predictor", "outcome", "id", "covariate", "nonsense")) {

    expect_error(add_columns(fx$pool, new, role = role),
                 class = "horizons_input_error",
                 info  = role)

  }

})
