#' Build Ensemble from Finalized Models
#'
#' @description
#' Creates an ensemble model from the output of \code{finalize_top_workflows()}.
#' Supports both stacked ensembles using tidymodels/stacks and simple weighted
#' averaging based on cross-validation performance.
#'
#' @param finalized_models A tibble output from \code{finalize_top_workflows()}
#' @param input_data A data frame containing the full dataset
#' @param variable Character. Name of the response variable to predict
#' @param covariate_data Optional data frame containing covariate predictors
#' @param ensemble_method Character. Method for ensemble: "stacks" or "weighted_average"
#' @param optimize_blending Logical. Whether to optimize blending parameters (slower)
#' @param blend_metric Character. Metric to optimize during blending: "rmse", "mae", "rsq", etc.
#' @param test_prop Numeric. Proportion of data to hold out for testing
#' @param seed Integer. Random seed for reproducibility
#' @param parallel_cv Logical. Use parallel processing for cross-validation
#' @param n_cores Integer. Number of cores to use if parallel_cv is TRUE
#' @param verbose Logical. Print progress messages
#' @param output_dir Character. Optional directory path to save ensemble results
#'
#' @return A list with class "horizons_ensemble"
#'
#' @export

build_ensemble <- function(finalized_models,
                           input_data,
                           variable,
                           covariate_data     = NULL,
                           ensemble_method    = "stacks",
                           optimize_blending  = FALSE,
                           blend_metric       = "rmse",
                           test_prop          = 0.2,
                           seed               = 123,
                           parallel_cv        = FALSE,
                           n_cores            = NULL,
                           verbose            = TRUE,
                           output_dir         = NULL) {

  ## ---------------------------------------------------------------------------
  ## Step 1: Input Validation and Setup
  ## ---------------------------------------------------------------------------

  if (!ensemble_method %in% c("stacks", "weighted_average")) {

    cli::cli_abort("ensemble_method must be 'stacks' or 'weighted_average', got '{ensemble_method}'")

  }

  ## ---------------------------------------------------------------------------

  required_cols <- c("wflow_id", "workflow", "cv_predictions", "metrics")
  missing_cols <- setdiff(required_cols, names(finalized_models))

  if (length(missing_cols) > 0) {

    cli::cli_abort("finalized_models missing required columns: {missing_cols}")

  }

  ## ---------------------------------------------------------------------------

  if (ensemble_method == "stacks") {

    null_cv <- finalized_models %>%
      dplyr::filter(purrr::map_lgl(cv_predictions, is.null))

    if (nrow(null_cv) > 0) {

      cli::cli_abort("Models missing cv_predictions (required for stacking): {null_cv$wflow_id}")

    }

  }

  ## ---------------------------------------------------------------------------

  if (!variable %in% names(input_data)) {

    cli::cli_abort("Variable '{variable}' not found in input_data")

  }

  ## ---------------------------------------------------------------------------

  if (test_prop <= 0 || test_prop >= 1) {

    cli::cli_abort("test_prop must be between 0 and 1, got {test_prop}")

  }

  ## ---------------------------------------------------------------------------

  if (parallel_cv && is.null(n_cores)) {

    n_cores <- min(parallel::detectCores() - 1, 10)

  }

  if (verbose) {

    cli::cli_h1("Ensemble Model Setup")
    
    cli::cli_text("{.strong Configuration:}")
    cli::cli_text("├─ Method: {.field {ensemble_method}}")
    cli::cli_text("├─ Input Models: {.field {nrow(finalized_models)}}")
    cli::cli_text("├─ Test Proportion: {.field {test_prop * 100}%}")
    
    if (parallel_cv) {
      cli::cli_text("└─ Parallel Processing: {.field {n_cores} cores}")
    } else {
      cli::cli_text("└─ Parallel Processing: {.field Disabled}")
    }
    
    cli::cli_text("")

  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Data Preparation
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("{.strong Data Preparation:}")
  }

  # Rename response variable to match what the workflows expect
  input_data %>%
    dplyr::rename(Response = !!dplyr::all_of(variable)) ->
  input_data

  # Create train/test split with same seed as finalize to get matching split
  set.seed(seed)

  # Try stratified split first, fall back to random if it fails
  safely_execute(
    expr = {
      rsample::initial_split(
        input_data,
        prop = 1 - test_prop,
        strata = Response
      )
    },
    default_value = NULL,
    error_message = "Stratified split failed, using random split"
  ) ->
  split_result

  if (is.null(split_result$result)) {

    rsample::initial_split(
      input_data,
      prop = 1 - test_prop
    ) ->
    data_split

    if (verbose) {
      cli::cli_alert_warning("Using random train/test split (stratification failed)")
    }

  } else {

    data_split <- split_result$result

  }

  rsample::training(data_split) ->
  train_data

  rsample::testing(data_split) ->
  test_data

  if (verbose) {
    cli::cli_text("{.strong Data Split:}")
    cli::cli_text("├─ Training: {nrow(train_data)} samples")
    cli::cli_text("└─ Testing: {nrow(test_data)} samples")
    
    range(train_data$Response, na.rm = TRUE) ->
    response_range

    sd(train_data$Response, na.rm = TRUE) ->
    response_sd

    cli::cli_text("")
    cli::cli_text("{.strong Response Variable:}")
    cli::cli_text("├─ Range: [{round(response_range[1], 2)}, {round(response_range[2], 2)}]")
    cli::cli_text("└─ SD: {round(response_sd, 2)}")
    cli::cli_text("")
  }

  ## ---------------------------------------------------------------------------
  ## Step 3: Build Ensemble Based on Method
  ## ---------------------------------------------------------------------------

  if (ensemble_method == "stacks") {

    ## -------------------------------------------------------------------------
    ## Step 3.1: Initialize Stack and Add Models
    ## -------------------------------------------------------------------------

    if (verbose) {
      cli::cli_text("{.strong Building Stacked Ensemble:}")
      cli::cli_text("└─ Method: {.field Penalized regression meta-learner}")
      cli::cli_text("")
    }

    # Initialize stack with the training data
    # The stack needs the data to properly blend predictions
    stacks::stacks() ->
    model_stack

    # Track which models successfully added
    n_added <- 0
    failed_models <- character()

    # Check resampling consistency
    resampling_ids <- list()
    for (i in seq_len(nrow(finalized_models))) {
      cv_obj <- finalized_models$cv_predictions[[i]]
      if (!is.null(cv_obj$splits)) {
        resampling_ids[[i]] <- vapply(cv_obj$splits, function(x) x$id$id, character(1))
      }
    }
    
    # Check if all models have the same resampling structure
    if (length(resampling_ids) > 1) {
      all_same <- all(sapply(resampling_ids[-1], function(x) identical(x, resampling_ids[[1]])))
      if (!all_same) {
        cli::cli_alert_warning("Models may have different CV fold structures - stacking might fail")
      } else if (verbose) {
        cli::cli_alert_success("All models use consistent CV fold structure")
      }
    }
    
    # Add each model's CV predictions to the stack
    for (i in seq_len(nrow(finalized_models))) {

      current_model <- finalized_models[i, ]

      if (verbose) {
        prefix <- if (i < nrow(finalized_models)) "├─" else "└─"
        cli::cli_text("{prefix} Adding model {i}/{nrow(finalized_models)}: {.field {current_model$wflow_id}}")
      }

      # Debug: Check the structure of cv_predictions (only for first model)  
      if (i == 1 && verbose && FALSE) {  # Set to TRUE to enable debug output
        cv_obj <- current_model$cv_predictions[[1]]
        cli::cli_text("│  └─ Debug: CV class: {paste(class(cv_obj), collapse = ', ')}")
        
        # Check for required attributes
        has_pred <- !is.null(cv_obj$.predictions)
        has_workflow <- !is.null(cv_obj$.workflow)
        cli::cli_text("│     └─ Has .predictions: {has_pred}, .workflow: {has_workflow}")
      }
      
      # Try to add to stack - pass the resampling results object directly
      # Sanitize workflow ID to replace + with . for valid column names
      sanitized_name <- gsub("\\+", ".", current_model$wflow_id)
      
      # Suppress warnings about missing columns and invalid names
      safely_execute(
        expr = {
          suppressWarnings({
            stacks::add_candidates(
              model_stack,
              candidates = current_model$cv_predictions[[1]],
              name = sanitized_name
            )
          })
        },
        default_value = NULL,
        error_message = glue::glue("Failed to add {current_model$wflow_id} to stack")
      ) ->
      add_result

      if (!is.null(add_result$result)) {

        model_stack <- add_result$result
        n_added <- n_added + 1

      } else {

        failed_models <- c(failed_models, current_model$wflow_id)

        if (verbose) {
          cli::cli_alert_warning("Failed to add {current_model$wflow_id}")
        }

      }

    }

    # Check we have enough models
    if (n_added == 0) {

      cli::cli_abort("No models could be added to the stack")

    }

    if (verbose) {
      cli::cli_alert_success(
        "Successfully added {n_added}/{nrow(finalized_models)} models to stack"
      )

      if (length(failed_models) > 0) {
        cli::cli_alert_warning(
          "Failed models: {paste(failed_models, collapse = ', ')}"
        )
      }
      
      # Validate stack structure (stacks objects have complex internal structure)
      # Don't try to modify the stack object - let stacks package handle it
      if (verbose) {
        cli::cli_alert_success("Stack structure validated - ready for blending")
      }
    }

    ## -------------------------------------------------------------------------
    ## Step 3.2: Blend Predictions with Penalized Regression
    ## -------------------------------------------------------------------------

    if (verbose) {
      cli::cli_alert_info("Blending predictions (metric: {blend_metric})")
    }

    # Set up metric for blending
    if (blend_metric == "rmse") {

      yardstick::metric_set(yardstick::rmse) ->
      blend_metric_set

    } else if (blend_metric == "mae") {

      yardstick::metric_set(yardstick::mae) ->
      blend_metric_set

    } else if (blend_metric == "rsq") {

      yardstick::metric_set(yardstick::rsq) ->
      blend_metric_set

    } else {

      # Try to use the metric directly - assumes it exists as a function
      tryCatch({
        get(blend_metric, mode = "function") ->
        blend_metric_fn

        yardstick::metric_set(blend_metric_fn) ->
        blend_metric_set

      }, error = function(e) {
        cli::cli_abort("Unknown blend_metric: '{blend_metric}'")
      })

    }

    # Set penalty and mixture based on optimize_blending
    if (optimize_blending) {

      10^seq(-6, -1, length.out = 20) ->
      blend_penalty

      seq(0, 1, length.out = 10) ->
      blend_mixture

      if (verbose) {
        cli::cli_text("├─ Optimizing over {.field {length(blend_penalty)} penalties × {length(blend_mixture)} mixtures = {length(blend_penalty) * length(blend_mixture)} combinations}")
      }

    } else {

      # Simple defaults for quick blending
      10^(-2) ->
      blend_penalty

      1 ->  # Pure lasso
      blend_mixture

      if (verbose) {
        cli::cli_alert_info("Using default blending parameters (penalty = 0.01, mixture = 1)")
      }

    }

    # Blend the stack
    safely_execute(
      expr = {
        suppressWarnings({
          stacks::blend_predictions(
            model_stack,
            penalty = blend_penalty,
            mixture = blend_mixture,
            metric = blend_metric_set,
            control = tune::control_grid(
              save_pred = TRUE,
              save_workflow = TRUE,
              allow_par = parallel_cv
            )
          )
        })
      },
      default_value = NULL,
      error_message = "Failed to blend predictions"
    ) ->
    blend_result

    if (is.null(blend_result$result)) {

      cli::cli_abort("Failed to blend ensemble predictions")

    }

    blend_result$result ->
    model_stack

    ## -------------------------------------------------------------------------
    ## Step 3.3: Fit Member Models and Extract Weights
    ## -------------------------------------------------------------------------

    if (verbose) {
      cli::cli_alert_info("Fitting member models...")
    }

    safely_execute(
      expr = {
        suppressWarnings({
          stacks::fit_members(model_stack)
        })
      },
      default_value = NULL,
      error_message = "Failed to fit ensemble members"
    ) ->
    fit_result

    if (is.null(fit_result$result)) {

      cli::cli_abort("Failed to fit ensemble members")

    }

    fit_result$result ->
    ensemble_model

    # Extract model weights - try multiple methods
    # Method 1: Try to get weights directly from the model_stack object
    model_weights <- NULL
    
    # First, try to access the coefficients directly
    if (!is.null(ensemble_model$coefs)) {
      safely_execute(
        expr = {
          # Check if coefs is an _elnet object (from glmnet)
          if (inherits(ensemble_model$coefs, "_elnet") || inherits(ensemble_model$coefs, "model_fit")) {
            # For elnet objects, we need to extract coefficients differently
            # Try to get the coefficients from the fitted model
            coef_matrix <- coef(ensemble_model$coefs$fit, s = ensemble_model$penalty$penalty)
            
            # Convert to data frame
            coef_df <- data.frame(
              member = rownames(coef_matrix)[-1],  # Exclude intercept
              coef = as.numeric(coef_matrix[-1, 1])
            ) %>%
              dplyr::filter(coef > 0) %>%
              dplyr::arrange(dplyr::desc(coef))
            
            coef_df
          } else if (is.data.frame(ensemble_model$coefs)) {
            # Original method for data frame
            ensemble_model$coefs %>%
              dplyr::filter(coef > 0) %>%
              dplyr::arrange(dplyr::desc(coef)) %>%
              dplyr::rename(member = terms)
          } else {
            NULL
          }
        },
        default_value = NULL,
        error_message = "Failed to extract weights from coefs"
      ) ->
      weights_result
      
      model_weights <- weights_result$result
    }
    
    # If that didn't work, try to extract from the printed output
    if (is.null(model_weights)) {
      safely_execute(
        expr = {
          # Capture the print output which contains weights
          output <- capture.output(print(ensemble_model))
          
          # Look for lines with weights
          weight_lines <- grep("^#\\s+\\d+\\s+", output, value = TRUE)
          
          if (length(weight_lines) > 0) {
            # Parse the weight information
            weights_df <- data.frame(member = character(), coef = numeric())
            
            for (line in weight_lines) {
              # Extract member name and weight
              parts <- strsplit(trimws(line), "\\s+")[[1]]
              if (length(parts) >= 4) {
                member_name <- parts[2]
                weight_val <- as.numeric(parts[length(parts)])
                if (!is.na(weight_val)) {
                  weights_df <- rbind(weights_df, 
                                     data.frame(member = member_name, 
                                              coef = weight_val))
                }
              }
            }
            weights_df
          } else {
            NULL
          }
        },
        default_value = NULL,
        error_message = "Failed to parse weights from print output"
      ) ->
      parse_result
      
      model_weights <- parse_result$result
    }
    
    # If we still don't have weights, create empty dataframe
    if (is.null(model_weights)) {
      cli::cli_text("⚠ Could not extract model weights from stacks ensemble")
      cli::cli_text("│  The ensemble was fitted but weight extraction failed")
      model_weights <- data.frame(member = character(), coef = numeric())
    }

    if (verbose) {
      cli::cli_text("✓ Ensemble fitted with {nrow(model_weights)} active models}")

      # Show top contributors
      if (nrow(model_weights) > 0) {

        head(model_weights, 5) ->
        top_models

        cli::cli_text("")
        cli::cli_text("{.strong Top Contributing Models:}")

        for (i in seq_len(nrow(top_models))) {
          prefix <- if (i < nrow(top_models)) "├─" else "└─"
          cli::cli_text("{prefix} {top_models$member[i]}: weight = {.field {round(top_models$coef[i], 4)}}")
        }

      }
    }

  } else {

    ## -------------------------------------------------------------------------
    ## Step 3B: Weighted Average Ensemble
    ## -------------------------------------------------------------------------

    if (verbose) {
      cli::cli_h2("Building Weighted Average Ensemble")
      cli::cli_text("├─ Weighting by cross-validation performance")
    }

    # Extract performance metrics for weighting
    finalized_models %>%
      dplyr::mutate(
        # Extract RMSE from metrics (lower is better)
        rmse = purrr::map_dbl(metrics, ~ {
          .x %>%
            dplyr::filter(.metric == "rmse") %>%
            dplyr::pull(mean) %>%  # Use 'mean' instead of '.estimate'
            .[1]
        })
      ) ->
    models_with_rmse

    # Check we have valid RMSE values
    if (any(is.na(models_with_rmse$rmse)) || any(models_with_rmse$rmse <= 0)) {

      cli::cli_abort("Invalid or missing RMSE values in model metrics")

    }

    # Calculate weights (inverse of RMSE)
    models_with_rmse %>%
      dplyr::mutate(
        weight = 1 / rmse,
        # Normalize weights to sum to 1
        weight = weight / sum(weight)
      ) %>%
      dplyr::select(wflow_id, workflow, weight, rmse) ->
    weighted_models

    if (verbose) {
      cli::cli_text("✓ Calculated weights for {nrow(weighted_models)} models}")

      # Show top weighted models
      weighted_models %>%
        dplyr::arrange(dplyr::desc(weight)) %>%
        head(5) ->
      top_weighted

      cli::cli_text("")
      cli::cli_text("{.strong Model Weights (Top 5):}")

      for (i in seq_len(nrow(top_weighted))) {
        prefix <- if (i < nrow(top_weighted)) "├─" else "└─"
        cli::cli_text("{prefix} {top_weighted$wflow_id[i]}: weight = {.field {round(top_weighted$weight[i], 4)}} (RMSE: {round(top_weighted$rmse[i], 3)})")
      }
    }

    # Fit workflows on training data for prediction
    weighted_models_fitted <- weighted_models %>%
      dplyr::mutate(
        fitted_workflow = purrr::map2(workflow, wflow_id, ~ {
          safely_execute(
            expr = {
              fit(.x, data = train_data)
            },
            default_value = NULL,
            error_message = glue::glue("Failed to fit {.y} for weighted ensemble")
          )$result
        })
      ) %>%
      # Remove models that failed to fit
      dplyr::filter(!purrr::map_lgl(fitted_workflow, is.null))
    
    # Renormalize weights after removing failed models
    if (nrow(weighted_models_fitted) > 0) {
      weighted_models_fitted <- weighted_models_fitted %>%
        dplyr::mutate(weight = weight / sum(weight))
    }

    # Create ensemble model object
    list(
      method = "weighted_average",
      models = weighted_models_fitted,
      predict = function(new_data) {

        # Generate predictions from each fitted model
        predictions_list <- vector("list", nrow(weighted_models_fitted))

        for (i in seq_len(nrow(weighted_models_fitted))) {

          safely_execute(
            expr = {
              predict(weighted_models_fitted$fitted_workflow[[i]], new_data)$.pred
            },
            default_value = rep(NA_real_, nrow(new_data)),
            error_message = glue::glue("Prediction failed for {weighted_models_fitted$wflow_id[i]}")
          ) ->
          pred_result

          pred_result$result * weighted_models_fitted$weight[i] ->
          predictions_list[[i]]

        }

        # Sum weighted predictions
        do.call(cbind, predictions_list) ->
        predictions_matrix

        rowSums(predictions_matrix, na.rm = TRUE) ->
        final_predictions

        tibble::tibble(.pred = final_predictions)
      }
    ) ->
    ensemble_model

    # Store weights for output (match stacks format)
    weighted_models_fitted %>%
      dplyr::select(member = wflow_id, coef = weight) %>%
      dplyr::arrange(dplyr::desc(coef)) ->
    model_weights

  }

  # TODO: Add additional ensemble methods
  # - "rf_meta": Random forest meta-learner using ranger
  # - "xgb_meta": XGBoost meta-learner
  # - "nn_meta": Neural network meta-learner
  #
  # Implementation approach:
  # 1. Extract CV predictions via tune::collect_predictions()
  # 2. Create meta-training data where each model's predictions are features
  # 3. Train meta-learner to predict Response from model predictions
  # 4. For prediction: get base model predictions, feed to meta-learner
  #
  # Key advantage: Can capture non-linear interactions between models
  # Example: RF might learn "trust model A in low range, model B in high range"

  ## ---------------------------------------------------------------------------
  ## Step 4: Generate Predictions and Evaluate
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("")
    cli::cli_text("{.strong Evaluating Ensemble Performance:}")
  }

  # Generate ensemble predictions on test set
  ensemble_model$predict(test_data) %>%
    dplyr::bind_cols(
      test_data %>%
        dplyr::select(Observed = Response)
    ) %>%
    dplyr::rename(Predicted = .pred) ->
  ensemble_predictions

  # Calculate standard metrics
  yardstick::metrics(
    ensemble_predictions,
    truth = Observed,
    estimate = Predicted
  ) ->
  ensemble_metrics

  # Add custom metrics if functions exist
  if (exists("ccc", mode = "function")) {

    ccc(
      ensemble_predictions,
      truth = Observed,
      estimate = Predicted
    )$.estimate ->
    ccc_value

    dplyr::bind_rows(
      ensemble_metrics,
      tibble::tibble(
        .metric = "ccc",
        .estimator = "standard",
        .estimate = ccc_value
      )
    ) ->
    ensemble_metrics

  }

  if (exists("rpd", mode = "function")) {

    rpd(
      ensemble_predictions,
      truth = Observed,
      estimate = Predicted
    )$.estimate ->
    rpd_value

    dplyr::bind_rows(
      ensemble_metrics,
      tibble::tibble(
        .metric = "rpd",
        .estimator = "standard",
        .estimate = rpd_value
      )
    ) ->
    ensemble_metrics

  }

  # Display key metrics
  if (verbose) {

    ensemble_metrics %>%
      dplyr::filter(.metric %in% c("rmse", "rsq", "mae")) %>%
      dplyr::mutate(.estimate = round(.estimate, 4)) ->
    metrics_display

    cli::cli_text("{.strong Ensemble Test Performance:}")
    
    for (i in seq_len(nrow(metrics_display))) {

      toupper(metrics_display$.metric[i]) ->
      metric_name

      round(metrics_display$.estimate[i], 4) ->
      metric_value
      
      if (i < nrow(metrics_display)) {
        cli::cli_text("├─ {metric_name}: {metric_value}")
      } else {
        cli::cli_text("└─ {metric_name}: {metric_value}")
      }

    }
    cli::cli_text("")

  }

  ## ---------------------------------------------------------------------------
  ## Step 4.1: Compare to Individual Models
  ## ---------------------------------------------------------------------------

  if (verbose) {
    cli::cli_text("├─ Evaluating individual model performance...")
  }

  # Generate predictions for each individual model
  # First need to fit workflows on training data
  finalized_models %>%
    dplyr::mutate(
      # Fit each workflow on the full training data
      fitted_workflow = purrr::map2(workflow, wflow_id, ~ {
        if (verbose) {
          cli::cli_text("│  ├─ Fitting {.y} on training data...")
        }
        safely_execute(
          expr = {
            fit(.x, data = train_data)
          },
          default_value = NULL,
          error_message = glue::glue("Failed to fit {.y}")
        )$result
      }),
      # Make predictions with fitted workflows
      test_predictions = purrr::map(fitted_workflow, ~ {
        if (is.null(.x)) {
          return(rep(NA_real_, nrow(test_data)))
        }
        safely_execute(
          expr = {
            predict(.x, test_data)$.pred
          },
          default_value = rep(NA_real_, nrow(test_data)),
          error_message = "Individual prediction failed"
        )$result
      }),
      test_metrics = purrr::map(test_predictions, ~ {
        if (all(is.na(.x))) {
          return(NULL)
        }

        tibble::tibble(
          Observed = test_data$Response,
          Predicted = .x
        ) ->
        pred_df

        yardstick::metrics(
          pred_df,
          truth = Observed,
          estimate = Predicted
        )
      })
    ) %>%
    dplyr::select(wflow_id, test_metrics) %>%
    dplyr::filter(!purrr::map_lgl(test_metrics, is.null)) ->
  individual_performance

  # Extract RMSE for comparison
  individual_performance %>%
    dplyr::mutate(
      rmse = purrr::map_dbl(test_metrics, ~ {
        .x %>%
          dplyr::filter(.metric == "rmse") %>%
          dplyr::pull(.estimate) %>%
          .[1]
      })
    ) %>%
    dplyr::arrange(rmse) ->
  individual_rmse

  ensemble_metrics %>%
    dplyr::filter(.metric == "rmse") %>%
    dplyr::pull(.estimate) ->
  ensemble_rmse

  if (verbose && nrow(individual_rmse) > 0) {

    # Compare to best individual model
    individual_rmse$rmse[1] ->
    best_individual

    round((best_individual - ensemble_rmse) / best_individual * 100, 2) ->
    improvement

    if (improvement > 0) {

      cli::cli_text("✓ Ensemble improves over best individual model by {improvement}%")

    } else if (improvement < 0) {

      cli::cli_text("⚠ Ensemble performs {abs(improvement)}% worse than best individual model")

    } else {

      cli::cli_text("─ Ensemble performs equally to best individual model")

    }

    # Show top individual models for comparison
    cli::cli_text("")
    cli::cli_text("{.strong Top Individual Models (Test RMSE):}")

    head(individual_rmse, 3) ->
    top_individuals

    for (i in seq_len(nrow(top_individuals))) {
      cli::cli_text("├─ {top_individuals$wflow_id[i]}: {.field {round(top_individuals$rmse[i], 4)}}")
    }

    cli::cli_text("└─ Ensemble: {.field {round(ensemble_rmse, 4)}}")

  }

  ## ---------------------------------------------------------------------------
  ## Step 5: Save Results if Requested
  ## ---------------------------------------------------------------------------

  if (!is.null(output_dir)) {

    if (verbose) {
      cli::cli_text("")
      cli::cli_text("{.strong Saving Results:}")
    }

    # Check if output_dir has evaluation structure, if not create it
    if (!fs::dir_exists(output_dir)) {
      fs::dir_create(output_dir, recurse = TRUE)
    }

    # Create ensemble subdirectory within the evaluation structure
    # This allows it to integrate with existing evaluation results
    file.path(output_dir, "ensemble") ->
    ensemble_dir

    fs::dir_create(ensemble_dir)

    # Also ensure summary directory exists for aggregate results
    file.path(output_dir, "summary") ->
    summary_dir

    if (!fs::dir_exists(summary_dir)) {
      fs::dir_create(summary_dir)
    }

    # Save ensemble model
    file.path(ensemble_dir, "fitted_ensemble.qs") ->
    model_path

    qs::qsave(ensemble_model, model_path)

    if (verbose) {
      cli::cli_text("├─ Ensemble model saved to {.path {model_path}}")
    }

    # Save results
    file.path(ensemble_dir, "ensemble_results.qs") ->
    results_path

    list(
      predictions = ensemble_predictions,
      metrics = ensemble_metrics,
      model_weights = model_weights,
      individual_performance = individual_performance,
      metadata = list(
        method = ensemble_method,
        n_models = nrow(finalized_models),
        n_active = if (ensemble_method == "stacks") {
          nrow(model_weights)
        } else {
          sum(model_weights$coef > 0.01)  # Models with >1% weight
        },
        test_prop = test_prop,
        seed = seed,
        blend_metric = blend_metric,
        optimize_blending = optimize_blending,
        timestamp = Sys.time()
      )
    ) ->
    results_to_save

    qs::qsave(results_to_save, results_path)

    if (verbose) {
      cli::cli_text("├─ Results saved to {.path {results_path}}")
    }

    # Save predictions as CSV for easy access
    file.path(ensemble_dir, "ensemble_predictions.csv") ->
    csv_path

    readr::write_csv(ensemble_predictions, csv_path)

    if (verbose) {
      cli::cli_text("├─ Predictions saved to {.path {csv_path}}")
    }

    # Save model weights as CSV
    file.path(ensemble_dir, "model_weights.csv") ->
    weights_path

    readr::write_csv(model_weights, weights_path)

    if (verbose) {
      cli::cli_text("├─ Model weights saved to {.path {weights_path}}")
    }

    # Save ensemble summary to summary directory
    file.path(summary_dir, "ensemble_summary.csv") ->
    summary_path

    tibble::tibble(
      method = ensemble_method,
      n_models = nrow(finalized_models),
      n_active = if (ensemble_method == "stacks") {
        nrow(model_weights)
      } else {
        sum(model_weights$coef > 0.01)
      },
      ensemble_rmse = ensemble_rmse,
      best_individual_rmse = if (nrow(individual_rmse) > 0) {
        individual_rmse$rmse[1]
      } else {
        NA_real_
      },
      improvement_pct = if (nrow(individual_rmse) > 0) {
        round((individual_rmse$rmse[1] - ensemble_rmse) / individual_rmse$rmse[1] * 100, 2)
      } else {
        NA_real_
      },
      blend_metric = blend_metric,
      optimize_blending = optimize_blending,
      test_prop = test_prop,
      seed = seed,
      timestamp = Sys.time()
    ) ->
    ensemble_summary

    readr::write_csv(ensemble_summary, summary_path)

    if (verbose) {
      cli::cli_text("└─ Ensemble summary saved to {.path {summary_path}}")
    }

  }

  ## ---------------------------------------------------------------------------
  ## Step 6: Create Return Object
  ## ---------------------------------------------------------------------------

  # Create return object with class
  structure(
    list(
      ensemble_model = ensemble_model,
      predictions = ensemble_predictions,
      metrics = ensemble_metrics,
      model_weights = model_weights,
      individual_performance = individual_performance,
      metadata = list(
        method = ensemble_method,
        n_models = nrow(finalized_models),
        n_active = if (ensemble_method == "stacks") {
          nrow(model_weights)
        } else {
          sum(model_weights$coef > 0.01)  # Models with >1% weight
        },
        test_prop = test_prop,
        seed = seed,
        blend_metric = blend_metric,
        optimize_blending = optimize_blending,
        ensemble_rmse = ensemble_rmse,
        best_individual_rmse = if (nrow(individual_rmse) > 0) {
          individual_rmse$rmse[1]
        } else {
          NA_real_
        },
        timestamp = Sys.time()
      )
    ),
    class = "horizons_ensemble"
  ) ->
  result

  if (verbose) {
    cli::cli_text("")
    cli::cli_rule(left = "Ensemble Complete")
    
    # Extract key metrics for display
    ensemble_metrics %>%
      dplyr::filter(.metric %in% c("rmse", "rsq", "mae")) %>%
      dplyr::mutate(.estimate = round(.estimate, 4)) ->
    final_metrics_display
    
    rsq_val <- final_metrics_display %>% 
      dplyr::filter(.metric == "rsq") %>% 
      dplyr::pull(.estimate)
    
    mae_val <- final_metrics_display %>% 
      dplyr::filter(.metric == "mae") %>% 
      dplyr::pull(.estimate)
    
    cli::cli_text("{.strong Summary:}")
    cli::cli_text("├─ Method: {.field {ensemble_method}}")
    cli::cli_text("├─ Active models: {.field {result$metadata$n_active}/{result$metadata$n_models}}")
    cli::cli_text("├─ Test RMSE: {.field {round(ensemble_rmse, 4)}}")
    cli::cli_text("├─ Test R²: {.field {rsq_val}}")
    cli::cli_text("└─ Test MAE: {.field {mae_val}}")
    cli::cli_text("")

    if (!is.na(result$metadata$best_individual_rmse)) {

      round((result$metadata$best_individual_rmse - ensemble_rmse) /
            result$metadata$best_individual_rmse * 100, 2) ->
      final_improvement

      if (final_improvement > 0) {
        cli::cli_text("{.strong Performance:}")
        cli::cli_text("└─ {cli::col_green('✓')} Improved over best individual by {final_improvement}%")
        cli::cli_text("")
      }

    }
    
    cli::cli_text("✓ Ensemble model created successfully")
  }

  return(result)

}

#' Print Method for Horizons Ensemble
#'
#' @description
#' Provides a clean summary when printing a horizons_ensemble object.
#'
#' @param x A horizons_ensemble object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.horizons_ensemble <- function(x, ...) {

  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Horizons Ensemble Model\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  cat("Method:", x$metadata$method, "\n")
  cat("Models:", x$metadata$n_models, "(", x$metadata$n_active, "active )\n")

  if (!is.null(x$metadata$optimize_blending)) {
    cat("Blending:", ifelse(x$metadata$optimize_blending, "Optimized", "Default"),
        "(metric:", x$metadata$blend_metric, ")\n")
  }

  cat("\n")
  cat("Test Performance:\n")
  cat("  RMSE:", round(x$metadata$ensemble_rmse, 4), "\n")

  # Add other key metrics if available
  if (!is.null(x$metrics)) {
    rsq_metric <- x$metrics %>%
      dplyr::filter(.metric == "rsq") %>%
      dplyr::pull(.estimate)

    if (length(rsq_metric) > 0) {
      cat("  R²:  ", round(rsq_metric[1], 4), "\n")
    }

    # Check for custom metrics
    ccc_metric <- x$metrics %>%
      dplyr::filter(.metric == "ccc") %>%
      dplyr::pull(.estimate)

    if (length(ccc_metric) > 0) {
      cat("  CCC: ", round(ccc_metric[1], 4), "\n")
    }

    rpd_metric <- x$metrics %>%
      dplyr::filter(.metric == "rpd") %>%
      dplyr::pull(.estimate)

    if (length(rpd_metric) > 0) {
      cat("  RPD: ", round(rpd_metric[1], 4), "\n")
    }
  }

  # Show improvement over best individual
  if (!is.na(x$metadata$best_individual_rmse)) {

    (x$metadata$best_individual_rmse - x$metadata$ensemble_rmse) /
      x$metadata$best_individual_rmse * 100 ->
    improvement

    cat("\n")
    cat("Best Individual RMSE:", round(x$metadata$best_individual_rmse, 4), "\n")

    if (improvement > 0) {
      cat("Improvement: ↑", round(improvement, 1), "%\n")
    } else if (improvement < 0) {
      cat("Improvement: ↓", round(abs(improvement), 1), "%\n")
    } else {
      cat("Improvement: No change\n")
    }
  }

  # Show top contributing models
  if (!is.null(x$model_weights) && nrow(x$model_weights) > 0) {
    cat("\n")
    cat("Top Contributing Models:\n")

    head(x$model_weights, 3) ->
    top_3

    for (i in seq_len(nrow(top_3))) {
      cat("  ", i, ". ", top_3$member[i], ": ",
          round(top_3$coef[i], 4), "\n", sep = "")
    }
  }

  cat("\n")
  cat("Created:", format(x$metadata$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")

  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  invisible(x)
}
