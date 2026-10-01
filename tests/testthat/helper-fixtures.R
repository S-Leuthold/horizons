# Test Data Fixtures and Generators
# Helper functions to create consistent test data across the test suite


## ---------------------------------------------------------------------------
## Evaluate Test Object
## ---------------------------------------------------------------------------

#' Build a minimal horizons_data object ready for evaluate()
make_eval_object <- function(n = 40, n_wn = 10, n_configs = 2,
                             covariates = NULL,
                             add_validation = TRUE) {

  set.seed(42)

  ## Spectral data
  wn_names <- paste0("wn_", seq(4000, by = -2, length.out = n_wn))
  spec_mat <- matrix(rnorm(n * n_wn), nrow = n)
  colnames(spec_mat) <- wn_names

  df <- tibble::as_tibble(spec_mat)
  df$sample_id <- paste0("S", sprintf("%03d", seq_len(n)))

  ## Outcome with weak signal
  df$SOC <- 2 + rowMeans(spec_mat[, 1:min(3, n_wn)]) * 0.5 + rnorm(n, sd = 0.5)

  ## Role map
  roles <- tibble::tibble(
    variable = c("sample_id", wn_names, "SOC"),
    role     = c("id", rep("predictor", n_wn), "outcome")
  )

  ## Optionally add covariates
  if (!is.null(covariates)) {

    for (cov in covariates) {
      df[[cov]] <- runif(n, 0, 100)
      roles <- rbind(roles, tibble::tibble(variable = cov, role = "covariate"))
    }

  }

  ## Build configs
  models <- rep(c("rf", "cubist"), length.out = n_configs)
  configs <- tibble::tibble(
    config_id         = paste0("cfg_", sprintf("%03d", seq_len(n_configs))),
    model             = models,
    transformation    = "none",
    preprocessing     = "raw",
    feature_selection = "none",
    covariates        = NA_character_
  )

  ## Build horizons_data-like structure
  contract <- new_horizons_data()

  obj <- list(

    data = list(
      analysis     = df,
      role_map     = roles,
      n_rows       = nrow(df),
      n_predictors = n_wn,
      n_covariates = 0L,
      ## SOC carries role "outcome" above, not "response" — those are
      ## distinct roles (n_responses counts role == "response", the sibling
      ## responses add_response()/select_training() can carry alongside the
      ## one outcome being modeled). evaluate()'s entry-stage
      ## validate_horizons_data() call (#24) is the first thing to actually
      ## check this stored count against the role_map.
      n_responses  = 0L
    ),

    provenance = list(
      spectra_source = "test",
      spectra_type   = "mir",
      schema_version = 1L
    ),

    config = list(
      configs   = configs,
      n_configs = n_configs,
      tuning    = list(
        cv_folds      = 3L,
        grid_size     = 2L,
        bayesian_iter = 0L
      )
    ),

    validation = list(
      passed    = if (add_validation) TRUE else NULL,
      checks    = NULL,
      timestamp = if (add_validation) Sys.time() else NULL,
      outliers  = list(
        spectral_ids   = NULL,
        response_ids   = NULL,
        removed_ids    = NULL,
        removal_detail = NULL,
        removed        = FALSE
      )
    ),

    ## Downstream slots in the constructor's shape
    evaluation = contract$evaluation,
    models     = contract$models,
    ensemble   = contract$ensemble,
    artifacts  = list(cache_dir = NULL)

  )

  class(obj) <- c("horizons_data", "list")
  obj

}

## ---------------------------------------------------------------------------
## An object validated before #77
## ---------------------------------------------------------------------------

#' Remove rows the way validate(remove_outliers = "response") did before #77
#'
#' Versions before #77 removed response outliers from the object, on fences
#' over the whole table, and recorded them in removal_detail with reason
#' "response" (or "both" when the row was a spectral outlier too), the
#' outcome whose fences flagged them and the response threshold. They wrote
#' no response_trim request. This builds that record shape on an unpromoted
#' object.
legacy_label_removal <- function(hd, ids, reason = "response", outcome = "SOC",
                                 response_threshold = 1.5) {

  x      <- subset_rows(hd, keep = !hd$data$analysis$sample_id %in% ids)
  reason <- rep_len(reason, length(ids))

  x$validation$outliers["removed_ids"]    <- list(ids)
  x$validation$outliers["removal_detail"] <- list(tibble::tibble(
    sample_id          = ids,
    reason             = reason,
    outcome            = outcome,
    spectral_threshold = ifelse(reason == "both", 0.975, NA_real_),
    response_threshold = response_threshold
  ))
  x$validation$outliers["removed"]        <- list(TRUE)
  x$validation$outliers$response_trim     <- NULL   # NULL removes the key

  x

}
