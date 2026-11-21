## =========================================================================
## Each fold's recipe prepped once: the unit, its key, and the folds
## =========================================================================
##
## A fold's prep is a unit of its own (prep_fold_entry()), keyed by what it
## is, and a configuration assembles its folds from the units' entries, its
## transformed training rows (the hoist) and its own outcome
## (assemble_folds()). Every column is placed from the fold's bakes, and a
## column identical to the transformed rows' column of the same name (a band
## a selection kept) is that column itself, shared. These hold the assembled
## folds to the configuration's own fold recipe baked whole, to sharing what
## a fold left as it was, and to holding nothing a model could take as a
## predictor besides; and the units to one per thing prepped.

## Helper: training rows with spectra, an id, a meta column and a sibling
## response; three folds; one configuration's fold preps, as entries, and
## its folds assembled from them. A `recipe` given is the one each prep
## preps, in place of its unit's own (fold_unit_recipe() replaced).
prep_fold_case <- function(selection, transformation = "none", n = 60, n_wn = 30,
                           recipe = NULL) {

  set.seed(8)

  wn <- paste0("wn_", seq(4000, by = -4, length.out = n_wn))
  x  <- matrix(stats::rnorm(n * n_wn), n, dimnames = list(NULL, wn))

  train           <- tibble::as_tibble(x)
  train$SOC       <- 5 + 1.5 * x[, 5] + x[, 12] + stats::rnorm(n, sd = 0.5)
  train$sample_id <- sprintf("S%03d", seq_len(n))
  train$batch     <- rep(c("a", "b", "c"), length.out = n)
  train$clay      <- stats::runif(n)

  role_map <- tibble::tibble(
    variable = c(wn, "SOC", "sample_id", "batch", "clay"),
    role     = c(rep("predictor", n_wn), "outcome", "id", "meta", "response")
  )

  folds  <- rsample::vfold_cv(train, v = 3)
  config <- tibble::tibble(config_id = "cfg", model = "elastic_net", transformation = transformation,
                           preprocessing = "snv", feature_selection = selection)

  inputs <- fold_prep_inputs(config, train, role_map, DEFAULT_SG_WINDOW)
  units  <- config_fold_units("evaluate", config, folds, role_map, data = NULL, seed = 8L,
                              sg_window = DEFAULT_SG_WINDOW, pca_threshold = DEFAULT_PCA_THRESHOLD,
                              bounded = TRUE)

  if (!is.null(recipe)) testthat::local_mocked_bindings(fold_unit_recipe = function(...) recipe)

  entry_of <- function(unit, rows) {
    prep_fold_entry(unit, inputs$data, rows, role_map, 8L, DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD)
  }

  entries  <- lapply(seq_along(units$folds), function(i) entry_of(units$folds[[i]], fold_rows(folds$splits[[i]])))
  training <- if (!is.null(units$training)) entry_of(units$training, NULL)

  list(train = train, role_map = role_map, folds = folds, config = config, inputs = inputs,
       units = units, entries = entries, training = training,
       prepped = assemble_folds(function(i) if (length(entries) > 0) entries[[i]], folds, inputs, role_map))

}

## A configuration row with some fields set (transform() would read a
## field's new value from the row itself before the caller's frame)
config_as <- function(config, ...) {
  fields <- list(...)
  for (field in names(fields)) config[[field]] <- fields[[field]]
  config
}

## The case's folds, each fold's frame against the configuration's own fold
## recipe (or `recipe`) prepped on the fold under the unit's seed and baked
## whole: S1a's fold, which the assembled one must equal value for value
expect_folds_baked_whole <- function(case, label, recipe = NULL) {

  recipe <- recipe %||% build_recipe(case$config, case$inputs$data, case$role_map,
                                     transformed = TRUE)
  scope  <- fold_prep_scope("evaluate", case$config)

  for (i in seq_len(nrow(case$folds))) {

    in_id  <- case$folds$splits[[i]]$in_id
    out_id <- rsample::complement(case$folds$splits[[i]])
    split  <- case$prepped$folds$splits[[i]]
    cols   <- names(split$data)

    p  <- with_unit_seed(8L, c(scope, case$folds$id[[i]]), quiet_selection_warnings(
      recipes::prep(recipe, training = case$inputs$data[in_id, , drop = FALSE])
    ))
    an <- recipes::bake(p, new_data = NULL)
    as <- recipes::bake(p, new_data = case$inputs$data[out_id, , drop = FALSE])

    expect_identical(cols, c("SOC", prepped_predictors(p, an)),
                     label = paste(label, case$folds$id[[i]], "columns"))
    expect_identical(as.data.frame(rsample::analysis(split)), as.data.frame(an[, cols]),
                     label = paste(label, case$folds$id[[i]], "analysis"))
    expect_identical(as.data.frame(rsample::assessment(split)), as.data.frame(as[, cols]),
                     label = paste(label, case$folds$id[[i]], "assessment"))

  }

}

describe("assemble_folds()", {

  it("assembles every fold as the configuration's fold recipe baked whole, for no selection, PCA and correlation", {

    for (selection in c("none", "pca", "correlation")) {

      expect_folds_baked_whole(prep_fold_case(selection), selection)

    }

    ## Under a transformed response too: the analysis rows' outcome
    ## transformed, the assessment rows' as measured
    logged <- prep_fold_case("correlation", transformation = "log")
    expect_folds_baked_whole(logged, "correlation, log")

    for (i in seq_len(nrow(logged$folds))) {
      out_id <- rsample::complement(logged$folds$splits[[i]])
      expect_identical(rsample::assessment(logged$prepped$folds$splits[[i]])$SOC, logged$train$SOC[out_id])
    }

  })

  it("assembles a log-response configuration's PCA folds, from preps with no response step, as its own recipe baked whole", {

    ## PCA reads the spectra alone, so its unit's recipe has no response
    ## step and one prep serves every transformation; each configuration
    ## places its own outcome
    logged <- prep_fold_case("pca", transformation = "log")
    plain  <- prep_fold_case("pca", transformation = "none")

    expect_folds_baked_whole(logged, "pca, log")
    expect_identical(lapply(logged$entries, `[`, c("key", "kept", "made")),
                     lapply(plain$entries, `[`, c("key", "kept", "made")))

    for (i in seq_len(nrow(logged$folds))) {
      in_id <- logged$folds$splits[[i]]$in_id
      expect_identical(rsample::analysis(logged$prepped$folds$splits[[i]])$SOC, log(logged$train$SOC[in_id] + 1))
      expect_identical(rsample::analysis(plain$prepped$folds$splits[[i]])$SOC, plain$train$SOC[in_id])
    }

  })

  it("assembles every fold as the fold baked whole, for Boruta and CARS, and for CARS under log", {

    skip_unless_slow_tier()
    skip_if_not_installed("Boruta")

    cases <- list(boruta = prep_fold_case("boruta"), cars = prep_fold_case("cars"))

    for (selection in names(cases)) {

      expect_folds_baked_whole(cases[[selection]], selection)

    }

    ## CARS selects on the outcome its prep is given. On this outcome it
    ## keeps other bands under log than under none, shown first, so the log
    ## configuration's folds, held to its own recipe, hold its prep to the
    ## transformed outcome and to the seed named by it
    logged <- prep_fold_case("cars", transformation = "log")

    expect_false(identical(lapply(logged$entries, `[[`, "kept"), lapply(cases$cars$entries, `[[`, "kept")))
    expect_folds_baked_whole(logged, "cars, log")

  })

  it("assembles a fold as the fold baked whole when a step rewrites a band in place", {

    ## A step that changes a band under its own name: the fold's frame holds
    ## the fold's bake of it, not the transformed rows' column of that name
    base   <- prep_fold_case("none")
    recipe <- build_recipe(base$config, base$inputs$data, base$role_map, transformed = TRUE) |>
      recipes::step_mutate(spec01 = spec01 * 2)
    case   <- prep_fold_case("correlation", recipe = recipe)

    expect_folds_baked_whole(case, "spec01 doubled", recipe = recipe)
    expect_true(all(vapply(case$entries, function(e) "spec01" %in% names(e$made), logical(1))))

  })

  it("shares each column a fold left as it was with the transformed rows, rather than copying it", {

    ## Ten folds of a library-sized frame hold one copy of its bands, not ten
    addresses <- function(frame, cols) vapply(cols, function(col) rlang::obj_address(frame[[col]]), character(1))

    for (selection in c("none", "correlation")) {

      case <- prep_fold_case(selection)

      for (i in seq_len(nrow(case$folds))) {

        frame <- case$prepped$folds$splits[[i]]$data
        kept  <- intersect(names(frame), names(case$inputs$data))

        expect_gt(length(kept), 1L)
        expect_identical(addresses(frame, kept), addresses(case$inputs$data, kept),
                         label = paste(selection, case$folds$id[[i]]))

      }

    }

  })

  it("holds the outcome and the prepped predictors only, at the training rows' positions, with the folds' ids", {

    case <- prep_fold_case("none")
    spec <- grep("^spec[0-9]+$", names(case$inputs$data), value = TRUE)

    expect_identical(case$prepped$folds$id, case$folds$id)

    for (i in seq_len(nrow(case$folds))) {

      split <- case$prepped$folds$splits[[i]]

      ## No id, meta or held response for a model to take as a predictor
      expect_identical(names(split$data), c("SOC", spec))
      expect_identical(nrow(split$data), nrow(case$train))
      expect_identical(split$in_id, case$folds$splits[[i]]$in_id)
      expect_identical(as.integer(split, data = "assessment"), rsample::complement(case$folds$splits[[i]]))

    }

  })

  it("records another package's warning with the folds that raised it", {

    ## Two indicator columns, each constant on one fold's analysis rows only:
    ## step_corr() warns of the correlation it cannot compute on those two
    ## folds, and changes no column
    base   <- prep_fold_case("none")
    ids_of <- function(i) base$train$sample_id[rsample::complement(base$folds$splits[[i]])]
    recipe <- build_recipe(base$config, base$inputs$data, base$role_map, transformed = TRUE) |>
      recipes::step_mutate(z1 = as.numeric(sample_id %in% !!ids_of(1)),
                           z2 = as.numeric(sample_id %in% !!ids_of(2))) |>
      recipes::step_corr(recipes::all_predictors())

    out <- prep_fold_case("correlation", recipe = recipe)$prepped

    excluded <- grepl("The correlation matrix has missing values", out$warnings$cause, fixed = TRUE)

    expect_identical(sum(excluded), 1L)
    expect_identical(out$warnings$scope[excluded], "cv")
    expect_identical(out$warnings$folds[excluded][[1]], base$folds$id[1:2])
    expect_identical(out$warnings$n[excluded], 2L)
    expect_identical(out$warnings$n_folds[excluded], 3L)
    expect_match(render_warning_log(out$warnings), "The correlation matrix has missing values.*2 of 3 folds",
                 all = FALSE)

  })

  it("ends the unit reading the entries at the first fold whose prep failed, with that prep's record", {

    ## The second fold's prep fails: the configuration's unit records the
    ## prep's failure as the prep recorded it, step, class and words, and
    ## reads no fold after it
    case   <- prep_fold_case("correlation")
    boom   <- recipes::recipe(SOC ~ ., data = case$inputs$data) |>
      recipes::step_mutate(boom = stop("boom on a fold"))
    failed <- testthat::with_mocked_bindings(
      prep_fold_entry(case$units$folds[[2]], case$inputs$data, fold_rows(case$folds$splits[[2]]),
                      case$role_map, 8L, DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD),
      fold_unit_recipe = function(...) boom
    )
    read   <- integer(0)

    unit <- run_unit(
      unit_step("fold_prep", assemble_folds(function(i) {
        read <<- c(read, i)
        if (i == 2L) failed else case$entries[[i]]
      }, case$folds, case$inputs, case$role_map)),
      unit = "configuration", id = "cfg"
    )

    expect_true(failed$failed)
    expect_true(unit$failed)
    expect_identical(read, 1:2)
    expect_identical(unit_failure_message(unit$records, "Evaluation"),
                     unit_failure_message(failed$records, "Evaluation"))
    expect_match(unit_failure_message(unit$records, "Evaluation"), "^Fold preparation failed: boom on a fold")

    got <- unit$records[unit$records$type == "failure", ]
    expect_identical(got[c("unit", "id")], tibble::tibble(unit = "configuration", id = "cfg"))
    expect_identical(got[setdiff(names(got), c("unit", "id"))],
                     failed$records[failed$records$type == "failure", setdiff(names(got), c("unit", "id"))])

  })

})

describe("prep_fold_entry()", {

  it("names the bands a selection kept and keeps whole only the columns the prep made", {

    corr <- prep_fold_case("correlation")
    pca  <- prep_fold_case("pca")

    for (entry in corr$entries) {
      expect_false(entry$failed)
      expect_length(entry$made, 0L)
      expect_true(all(entry$kept %in% names(corr$inputs$data)))
    }

    for (entry in pca$entries) {
      expect_identical(names(entry$made), entry$kept)
      expect_true(all(startsWith(entry$kept, "PC")))
      expect_true(all(lengths(entry$made) == nrow(pca$train)))
    }

  })

  it("counts the predictors each prep kept: the assembled fold's, and on the training part the bound finalize() is given", {

    ## N17: the count P1 reads before its dispatch, the stage's record, is
    ## the assembled folds' predictor columns, and the training part's is
    ## the bound
    case   <- prep_fold_case("correlation")
    record <- build_fold_stage(c(list(case$units$training), case$units$folds),
                               open_fold_store(NULL, results_so_far(list())), case$train, case$folds,
                               case$role_map, 8L, DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD,
                               debug = FALSE, keep_traces = FALSE, collector = log_open("evaluate"),
                               output_dir = NULL, results = results_so_far(list()))

    expect_identical(record$fold, c("training", case$folds$id))

    for (i in seq_along(case$entries)) {
      expect_identical(record$n_predictors[i + 1L], ncol(case$prepped$folds$splits[[i]]$data) - 1L)
    }

    recipe  <- build_recipe(case$config, case$inputs$data, case$role_map, transformed = TRUE)
    prepped <- with_unit_seed(8L, c(fold_prep_scope("evaluate", case$config), "training"),
                              quiet_selection_warnings(recipes::prep(recipe)))
    baked   <- recipes::bake(prepped, new_data = NULL)

    expect_identical(case$training$kept, prepped_predictors(prepped, baked))
    expect_identical(record$n_predictors[1], length(prepped_predictors(prepped, baked)))
    expect_identical(fold_bound(case$training, case$inputs), case$training$kept)

  })

  it("contains a step's failure, recording it with the step's class, on a fold and on the training part", {

    case   <- prep_fold_case("none")
    recipe <- recipes::recipe(SOC ~ ., data = case$inputs$data) |>
      recipes::step_mutate(boom = stop("boom from step"))
    units  <- config_fold_units("evaluate", config_as(case$config, feature_selection = "pca"), case$folds,
                                case$role_map, NULL, 8L, DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD, bounded = TRUE)

    ## Each prep given `recipe` in place of its unit's own
    prep_with <- function(recipe, unit, rows) {
      testthat::with_mocked_bindings(
        prep_fold_entry(unit, case$inputs$data, rows, case$role_map, 8L, DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD),
        fold_unit_recipe = function(...) recipe
      )
    }

    on_fold     <- prep_with(recipe, units$folds[[1]], fold_rows(case$folds$splits[[1]]))
    on_training <- prep_with(recipe, units$training, NULL)

    expect_true(on_fold$failed)
    expect_identical(unit_failure_message(on_fold$records, "x"), "Fold preparation failed: boom from step (in `step_mutate()`)")
    expect_identical(unit_failure_message(on_training$records, "x"), "Recipe preparation failed: boom from step (in `step_mutate()`)")
    expect_identical(on_fold$records$class[on_fold$records$type == "failure"], "simpleError")
    expect_identical(on_fold$kept, character(0))

    ## A kind no unit contains propagates
    tripwire <- recipes::recipe(SOC ~ ., data = case$inputs$data) |>
      recipes::step_mutate(boom = horizons:::abort_horizons("Rows misaligned.", kind = "internal"))

    err <- expect_error(prep_with(tripwire, units$folds[[1]], fold_rows(case$folds$splits[[1]])),
                        "Rows misaligned.", fixed = TRUE)
    expect_true(rlang::cnd_inherits(err, "horizons_error_internal"))

  })

})

describe("fold_prep_inputs()", {

  it("builds the transformed rows and the outcome without moving the caller's stream", {

    ## The whole recipe, built before them, draws its step ids from the
    ## stream (recipes::rand_id()), so the recipe stored is the one it was.
    ## A log response, whose step draws an id.
    case   <- prep_fold_case("none")
    config <- tibble::tibble(config_id = "cfg", model = "elastic_net", transformation = "log",
                             preprocessing = "snv", feature_selection = "pca")

    withr::local_seed(3)
    before <- .Random.seed

    fold_prep_inputs(config, case$train, case$role_map, DEFAULT_SG_WINDOW)

    expect_identical(.Random.seed, before)

  })

  it("gives each row the outcome a fold's prep gives it on that fold's analysis rows", {

    ## The transformations are row-wise and estimate nothing, so one bake on
    ## the training part is each fold's bake at its rows
    for (transformation in setdiff(VALID_TRANSFORMATIONS, "none")) {

      case   <- prep_fold_case("none", transformation = transformation)
      recipe <- build_recipe(case$config, case$inputs$data, case$role_map, transformed = TRUE)

      for (i in seq_len(nrow(case$folds))) {
        in_id <- case$folds$splits[[i]]$in_id
        own   <- recipes::bake(recipes::prep(recipe, training = case$inputs$data[in_id, , drop = FALSE]),
                               new_data = NULL)
        expect_identical(case$inputs$outcome[in_id], own$SOC, label = paste(transformation, i))
      }

    }

  })

})

describe("config_fold_units()", {

  units_of <- function(config, folds, role_map, bounded = TRUE, data = NULL, seed = 8L) {
    config_fold_units("evaluate", config, folds, role_map, data, seed, DEFAULT_SG_WINDOW,
                      DEFAULT_PCA_THRESHOLD, bounded = bounded)
  }

  it("lists no prep for no selection, whose bound is the transformed rows' predictor count", {

    ## N3: a configuration with no feature selection trains nothing on a
    ## fold, so it has no unit, and its bound is read from its rows
    case <- prep_fold_case("none")

    expect_identical(units_of(case$config, case$folds, case$role_map), list(training = NULL, folds = NULL))
    expect_identical(fold_bound(NULL, case$inputs), grep("^spec[0-9]+$", names(case$inputs$data), value = TRUE))

    recipe  <- build_recipe(case$config, case$inputs$data, case$role_map, transformed = TRUE)
    prepped <- recipes::prep(recipe)
    expect_identical(fold_bound(NULL, case$inputs), prepped_predictors(prepped, recipes::bake(prepped, new_data = NULL)))

  })

  it("keys a selection that reads the outcome by the transformation, and PCA by none", {

    ## N2: two configurations that differ only in the transformation share
    ## PCA's preps and not correlation's
    case <- prep_fold_case("none")
    of   <- function(selection, transformation) {
      vapply(units_of(config_as(case$config, feature_selection = selection, transformation = transformation),
                      case$folds, case$role_map)$folds, `[[`, character(1), "name")
    }

    expect_identical(of("pca", "none"), of("pca", "log"))
    expect_false(any(of("correlation", "none") %in% of("correlation", "log")))
    expect_false(any(of("cars", "none") %in% of("cars", "sqrt")))

    ## And by the fold: one name per fold, the training part's another
    named <- units_of(config_as(case$config, feature_selection = "pca"), case$folds, case$role_map)
    expect_identical(anyDuplicated(c(named$training$name, vapply(named$folds, `[[`, character(1), "name"))), 0L)

  })

  it("lists the training part's prep only for a bounded model", {

    case <- prep_fold_case("none")
    pca  <- config_as(case$config, feature_selection = "pca")

    expect_null(units_of(pca, case$folds, case$role_map, bounded = FALSE)$training)
    expect_identical(units_of(pca, case$folds, case$role_map)$training$fold, "training")
    expect_length(units_of(pca, case$folds, case$role_map)$folds, nrow(case$folds))

  })

  it("keys each prep on its rows, the data, the settings, the seed and the rules", {

    case  <- prep_fold_case("none")
    pca   <- config_as(case$config, feature_selection = "pca")
    first <- function(folds = case$folds, verb = "evaluate", data = NULL, seed = 8L,
                      sg_window = DEFAULT_SG_WINDOW, pca_threshold = DEFAULT_PCA_THRESHOLD) {
      config_fold_units(verb, pca, folds, case$role_map, data, seed, sg_window, pca_threshold,
                        bounded = TRUE)$folds[[1]]
    }
    plain <- first()$name

    expect_identical(first()$name, plain)

    ## Each thing a prep is built from makes another prep: the verb, the
    ## data, the seed, and the recipe settings a resume can change while the
    ## data and the seed stay as they were
    changed <- list(verb          = first(verb = "fit")$name,
                    data          = first(data = list(predictors = "x"))$name,
                    seed          = first(seed = 9L)$name,
                    sg_window     = first(sg_window = DEFAULT_SG_WINDOW + 2L)$name,
                    pca_threshold = first(pca_threshold = DEFAULT_PCA_THRESHOLD / 2)$name)

    for (what in names(changed)) expect_false(identical(changed[[what]], plain), label = what)

    ## The rows: the fold's analysis rows in another order
    other_rows <- case$folds
    other_rows$splits[[1]]$in_id <- rev(other_rows$splits[[1]]$in_id)
    expect_false(identical(first(other_rows)$name, plain))

    ## And every position an entry fills: two of the first fold's
    ## assessment rows swapped in the training part leave its analysis rows
    ## as they were, shown first, while a PCA entry's scores for them would
    ## sit at each other's rows
    out     <- rsample::complement(case$folds$splits[[1]])[1:2]
    swapped <- case$folds
    rows    <- swapped$splits[[1]]$data
    rows[out, ] <- rows[rev(out), ]
    for (i in seq_len(nrow(swapped))) swapped$splits[[i]]$data <- rows

    analysis_ids <- function(folds) fold_row_ids(folds$splits[[1]]$data, case$role_map)[folds$splits[[1]]$in_id]
    expect_identical(analysis_ids(swapped), analysis_ids(case$folds))
    expect_false(identical(first(swapped)$name, plain))

    ## The rules a prep's draws and its scoring follow, as they are now
    key <- first()$key
    expect_identical(key$seed_rule, as.double(SEED_RULE))
    expect_identical(key$scoring_schema, as.double(SCORING_SCHEMA))
    expect_identical(fold_entry_file(key), plain)

  })

})

describe("fold_unit_recipe()", {

  it("builds a prep's recipe as the configuration's own for a selection that reads the outcome, and with no response step for PCA", {

    ## A selection that reads the outcome selects on it as the configuration
    ## transforms it, so its prep's recipe is the configuration's fold
    ## recipe, response step and all; PCA reads the spectra alone, so its
    ## prep's has no response step and serves every transformation. Ranks
    ## are the same under every transformation, so correlation keeps the
    ## same bands under each, and only the recipe shows the step is there.
    case    <- prep_fold_case("none")
    classes <- function(rec) vapply(rec$steps, function(s) class(s)[1], character(1))
    unit_of <- function(cfg) fold_unit_recipe(fold_unit_row(cfg), case$inputs$data, case$role_map,
                                              DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD)
    own_of  <- function(cfg) build_recipe(cfg, case$inputs$data, case$role_map, transformed = TRUE)

    ## The configuration's own recipe carries its response step
    expect_true("step_log" %in% classes(own_of(config_as(case$config, feature_selection = "correlation",
                                                         transformation = "log"))))
    expect_true("step_sqrt" %in% classes(own_of(config_as(case$config, feature_selection = "cars",
                                                          transformation = "sqrt"))))

    for (transformation in VALID_TRANSFORMATIONS) {

      for (selection in OUTCOME_SELECTIONS) {
        cfg <- config_as(case$config, feature_selection = selection, transformation = transformation)
        expect_identical(classes(unit_of(cfg)), classes(own_of(cfg)), label = paste(selection, transformation))
      }

      cfg <- config_as(case$config, feature_selection = "pca", transformation = transformation)
      expect_identical(classes(unit_of(cfg)), classes(own_of(config_as(cfg, transformation = "none"))),
                       label = paste("pca", transformation))

    }

  })

})

describe("fold_prep_units()", {

  it("lists each prep the configurations read once, longest first, a prep's training part before its folds", {

    case    <- prep_fold_case("none")
    configs <- tibble::tibble(
      config_id         = paste0("cfg_", 1:5),
      model             = c("elastic_net", "plsr", "elastic_net", "rf", "elastic_net"),
      transformation    = c("none", "none", "log", "none", "none"),
      preprocessing     = "snv",
      feature_selection = c("correlation", "correlation", "pca", "none", "cars")
    )

    units <- fold_prep_units(configs, case$folds, case$role_map, "evaluate", NULL, 8L,
                             DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD)
    sel   <- vapply(units, function(u) u$row$feature_selection, character(1))
    fold  <- vapply(units, `[[`, character(1), "fold")

    ## CARS's three folds, then PCA's, then correlation's training part (plsr
    ## bounds num_comp) and its folds; none for rf with no selection
    expect_identical(sel, rep(c("cars", "pca", "correlation"), c(3L, 3L, 4L)))
    expect_identical(fold, c(rep(case$folds$id, 2L), "training", case$folds$id))
    expect_identical(anyDuplicated(fold_unit_files(units)), 0L)

    ## The cost table covers every selection that has a prep, and only those
    expect_setequal(names(FOLD_PREP_COST_SECONDS), setdiff(VALID_FEATURE_SELECTION, "none"))

  })

})

describe("build_fold_stage()", {

  ## The case's three PCA preps through the stage, with the store in
  ## `dir`'s tmp/
  stage_of <- function(case, dir) {
    build_fold_stage(case$units$folds, open_fold_store(dir, results_so_far(list())), case$train, case$folds,
                     case$role_map, 8L, DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD,
                     debug = FALSE, keep_traces = FALSE, collector = log_open("evaluate"),
                     output_dir = dir, results = results_so_far(list()))
  }

  it("holds no entry once it is written, or read and found usable, keeping only what its record needs", {

    ## Each entry the stage is given, built or read, carries a tag whose
    ## finalizer says when nothing holds the entry any more. Before each is
    ## given, the entries given before it that something still holds are
    ## counted: with the store on disk, none.
    case <- prep_fold_case("pca")
    dir  <- withr::local_tempdir()

    given <- character(0)
    freed <- character(0)
    held  <- integer(0)

    tag_entry <- function(entry, name) {
      gc()
      held  <<- c(held, length(setdiff(given, freed)))
      tag   <- new.env()
      reg.finalizer(tag, function(e) freed <<- c(freed, name))
      given <<- c(given, name)
      entry$tag <- tag
      entry
    }

    real_prep <- prep_fold_entry
    built <- testthat::with_mocked_bindings(
      stage_of(case, dir),
      prep_fold_entry = function(unit, ...) tag_entry(real_prep(unit, ...), paste("built", unit$name))
    )

    expect_identical(built$reused, rep(FALSE, 3L))
    expect_identical(held, rep(0L, 3L))

    ## The resumed run reads each, and uses all three
    real_read <- read_fold_entry
    held      <- integer(0)
    reread <- testthat::with_mocked_bindings(
      stage_of(case, dir),
      read_fold_entry = function(path, key) {
        got       <- real_read(path, key)
        got$entry <- tag_entry(got$entry, paste("read", basename(path)))
        got
      }
    )

    expect_identical(reread$reused, rep(TRUE, 3L))
    expect_identical(held, rep(0L, 3L))
    expect_identical(reread$n_predictors, built$n_predictors)

  })

  it("says on the tree which preps in the run's store it could not use, and prepares them again", {

    case <- prep_fold_case("pca")
    dir  <- withr::local_tempdir()
    tmp  <- file.path(dir, "tmp")

    expect_length(fold_stage_lines(stage_of(case, dir)), 1L)

    ## The first prep's file copied over the second's
    names <- vapply(case$units$folds, `[[`, character(1), "name")
    file.copy(file.path(tmp, names[1]), file.path(tmp, names[2]), overwrite = TRUE)

    record <- stage_of(case, dir)
    lines  <- cli::ansi_strip(fold_stage_lines(record))

    expect_identical(record$reused, c(TRUE, FALSE, TRUE))
    expect_length(lines, 2L)
    expect_identical(lines[2], paste0("Could not use ", file.path("tmp", names[2]),
                                      ": holds another fold preparation's record (prepared again)"))

  })

})

describe("read_fold_entry()", {

  it("reads an entry only under its own key, and says why a file is not one", {

    case <- prep_fold_case("pca")
    dir  <- withr::local_tempdir()
    unit <- case$units$folds[[1]]
    path <- file.path(dir, unit$name)

    saveRDS(case$entries[[1]], path)
    expect_identical(read_fold_entry(path, unit$key), list(entry = case$entries[[1]], error = NULL))

    ## Another prep's entry under this one's name: a copied file
    saveRDS(case$entries[[2]], path)
    expect_identical(read_fold_entry(path, unit$key)$error, "holds another fold preparation's record")

    writeLines("not an entry", path)
    expect_null(read_fold_entry(path, unit$key)$entry)
    expect_match(read_fold_entry(path, unit$key)$error, "unknown input format|magic number|cannot", perl = TRUE)

    saveRDS(list(a = 1), path)
    expect_identical(read_fold_entry(path, unit$key)$error, "not a fold preparation")

  })

})


describe("finalize_bound()", {

  it("bounds mtry and num_comp from the predictors' names alone, as from the baked frame", {

    ## N20: dials::finalize() reads only the frame's width for both
    case    <- prep_fold_case("correlation")
    recipe  <- build_recipe(case$config, case$inputs$data, case$role_map, transformed = TRUE)
    prepped <- with_unit_seed(8L, "x", quiet_selection_warnings(recipes::prep(recipe)))
    baked   <- recipes::bake(prepped, new_data = NULL)
    kept    <- prepped_predictors(prepped, baked)

    for (model in c("rf", "plsr")) {

      pset <- workflows::extract_parameter_set_dials(
        workflows::workflow() |> workflows::add_recipe(recipe) |> workflows::add_model(define_model_spec(model))
      )

      from_frame <- cap_by_data(dials::finalize(pset, baked[, kept, drop = FALSE]), length(kept),
                                min_analysis_rows(case$folds))

      expect_identical(finalize_bound(pset, kept, case$folds), from_frame, label = model)

    }

  })

})

describe("config_fold_entries()", {

  it("reads a configuration's preps from the run's directory, and stops as systemic, naming the file and why, when one the run wrote cannot be read", {

    ## The session's side of the one reader a configs-axis daemon uses too
    ## (its side: test-evaluate-parallel.R)
    case  <- prep_fold_case("pca")
    dir   <- withr::local_tempdir()
    store <- list(dir = normalizePath(dir, winslash = "/"), shown = dir, memory = NULL)
    read  <- function() {
      config_fold_entries(store, case$config, case$folds, case$role_map, "evaluate", NULL, 8L,
                          DEFAULT_SG_WINDOW, DEFAULT_PCA_THRESHOLD, results = results_so_far(list()))
    }

    for (i in seq_along(case$units$folds)) saveRDS(case$entries[[i]], file.path(dir, case$units$folds[[i]]$name))

    expect_identical(read(), list(training = NULL, folds = case$entries))

    ## The second fold's file gone, as when another run starts into the
    ## same output_dir
    unlink(file.path(dir, case$units$folds[[2]]$name))

    err <- expect_error(read(), class = "horizons_error_systemic")
    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))

    expect_match(msg, "could not read a fold preparation it wrote to", fixed = TRUE)
    expect_match(msg, paste0(case$units$folds[[2]]$name, ": "), fixed = TRUE)
    expect_identical(nrow(err$results), 0L)

  })

})

describe("fold_prep_scope()", {

  it("names a fold's prep by the response transformation only for a selection that reads the outcome", {

    ## Two configurations that differ only in the transformation must draw
    ## other seeds for a selection that selects on the outcome, and the same
    ## for one that does not
    reads_outcome <- c(none = FALSE, pca = FALSE, correlation = TRUE, boruta = TRUE, cars = TRUE)

    expect_setequal(names(reads_outcome), VALID_FEATURE_SELECTION)

    for (verb in c("evaluate", "fit")) {

      for (selection in names(reads_outcome)) {

        config <- tibble::tibble(config_id = "cfg", model = "rf", transformation = "log",
                                 preprocessing = "snv", feature_selection = selection)

        expect_identical(fold_prep_scope(verb, config),
                         c(verb, "prep", "snv", selection, if (reads_outcome[[selection]]) "log"),
                         label = paste(verb, selection))

      }

    }

  })

})

describe("fold_workflow()", {

  it("has the parameter set of the configuration's recipe workflow, since no recipe step is tuned", {

    ## tune searches the model alone over prepped folds, while the grid and
    ## the bounds are read from the workflow with the whole recipe, so the
    ## two must tune the same parameters: no recipe step is tuned, under any
    ## selection or transformation
    withr::local_seed(1)
    wn <- paste0("wn_", seq(4000, by = -4, length.out = 20))
    d  <- tibble::as_tibble(matrix(stats::rnorm(30 * 20), 30, dimnames = list(NULL, wn)))
    d$SOC       <- stats::runif(30, 1, 5)
    d$sample_id <- sprintf("S%02d", 1:30)
    role_map <- tibble::tibble(variable = c(wn, "SOC", "sample_id"),
                               role     = c(rep("predictor", 20), "outcome", "id"))

    config_of <- function(selection, transformation) {
      tibble::tibble(model = "rf", transformation = transformation, preprocessing = "snv",
                     feature_selection = selection)
    }

    for (selection in VALID_FEATURE_SELECTION) {

      for (transformation in VALID_TRANSFORMATIONS) {

        expect_identical(nrow(tune::tune_args(build_recipe(config_of(selection, transformation), d, role_map))),
                         0L, label = paste(selection, transformation))

      }

    }

    ## So the model alone tunes what the whole workflow tunes: rf's mtry,
    ## whose bounds are read from the recipe, among them
    spec  <- define_model_spec("rf")
    whole <- workflows::workflow() |>
      workflows::add_recipe(build_recipe(config_of("pca", "log"), d, role_map)) |>
      workflows::add_model(spec)

    expect_identical(workflows::extract_parameter_set_dials(fold_workflow(spec, "SOC")),
                     workflows::extract_parameter_set_dials(whole))

  })

  it("carries nothing of its caller's frame, so a tune task stays small", {

    ## tune serializes the workflow to a worker with every task. Its
    ## variables' selectors keep fold_workflow()'s frame, so an outcome name
    ## left unforced there would carry the caller's frame with it: here
    ## 40 MB, in the verbs the folds and the training rows.
    in_a_heavy_frame <- function() {
      heavy       <- numeric(5e6)
      outcome_col <- "SOC"
      fold_workflow(define_model_spec("rf"), outcome_col)
    }

    expect_lt(length(serialize(in_a_heavy_frame(), NULL)), 1e6)

  })

})
