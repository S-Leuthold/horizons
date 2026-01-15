#' Library Prediction Helper Functions
#'
#' @description
#' Internal helper functions for library-based prediction workflow.
#' Handles prediction for both texture (2-model ILR with back-transform) and
#' standard (1-model) properties.
#'
#' @importFrom cli cli_text cli_alert_warning style_bold
#' @importFrom dplyr mutate select
#' @importFrom tibble tibble
#' @importFrom stats predict
#' @keywords internal

## -----------------------------------------------------------------------------
## Section 1: Prediction Functions
## -----------------------------------------------------------------------------

#' Predict Texture from ILR Models
#'
#' @description
#' Generates predictions for texture properties using paired ILR models,
#' back-transforms to texture space, and ensures mass balance.
#'
#' @param unknowns Tibble with unknown spectra (Sample_ID + spectral columns)
#' @param models List with ilr_1 and ilr_2 trained workflows
#' @param property Character. Property name (any texture property)
#' @param cluster_id Integer. Cluster identifier
#' @param config Data frame row. Config used for training
#' @param verbose Logical. Print progress?
#'
#' @return Tibble with Sample_ID, property (sand/silt/clay), pred, cluster_id, config_id
#' @keywords internal
predict_texture_from_models <- function(unknowns,
                                       models,
                                       property,
                                       cluster_id,
                                       config,
                                       verbose = TRUE) {

  ## Verbose output handled by orchestrator

  ## ---------------------------------------------------------------------------
  ## Step 1: Prepare unknowns data (add Project if missing)
  ## ---------------------------------------------------------------------------

  ## Recipe expects Project column ---------------------------------------------

  if (!"Project" %in% names(unknowns)) {
    unknowns <- unknowns %>% dplyr::mutate(Project = "library")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Generate predictions for both ILR coordinates
  ## ---------------------------------------------------------------------------

  ## Predict ilr_1 -------------------------------------------------------------

  ilr1_pred <- predict(models$ilr_1, new_data = unknowns)$.pred

  ## Predict ilr_2 -------------------------------------------------------------

  ilr2_pred <- predict(models$ilr_2, new_data = unknowns)$.pred

  ## Verbose output handled by orchestrator

  ## ---------------------------------------------------------------------------
  ## Step 2: Back-transform to texture space
  ## ---------------------------------------------------------------------------

  texture_pred <- ilr_to_texture(
    ilr_1  = ilr1_pred,
    ilr_2  = ilr2_pred,
    as_gkg = TRUE
  )

  ## Verbose output handled by orchestrator

  ## ---------------------------------------------------------------------------
  ## Step 3: Validate mass balance
  ## ---------------------------------------------------------------------------

  total <- texture_pred$sand + texture_pred$silt + texture_pred$clay
  max_deviation <- max(abs(total - 1000))

  if (max_deviation > 1) {
    cli::cli_alert_warning("Texture mass balance deviation: {round(max_deviation, 2)} g/kg")
  }

  ## ---------------------------------------------------------------------------
  ## Step 4: Format results (long format: one row per property)
  ## ---------------------------------------------------------------------------

  config_id <- paste0(config$model, "_", config$preprocessing, "_", config$feature_selection)

  ## Create 3 rows per sample (sand, silt, clay) ------------------------------

  results <- tibble::tibble(
    Sample_ID  = rep(unknowns$Sample_ID, 3),
    property   = rep(c("sand", "silt", "clay"), each = nrow(unknowns)),
    pred       = c(texture_pred$sand, texture_pred$silt, texture_pred$clay),
    cluster_id = cluster_id,
    config_id  = config_id
  )

  return(results)
}

#' Predict Standard Property from Model
#'
#' @description
#' Generates predictions for non-texture properties, applies bounds enforcement.
#'
#' @param unknowns Tibble with unknown spectra
#' @param model Trained workflow
#' @param property Character. Property name
#' @param cluster_id Integer. Cluster identifier
#' @param config Data frame row. Config used
#' @param verbose Logical. Print progress?
#'
#' @return Tibble with Sample_ID, property, pred, cluster_id, config_id
#' @keywords internal
predict_standard_from_model <- function(unknowns,
                                       model,
                                       property,
                                       cluster_id,
                                       config,
                                       verbose = TRUE) {

  ## Verbose output handled by orchestrator

  ## ---------------------------------------------------------------------------
  ## Step 1: Prepare unknowns data (add Project if missing)
  ## ---------------------------------------------------------------------------

  if (!"Project" %in% names(unknowns)) {
    unknowns <- unknowns %>%
      dplyr::mutate(Project = "library")
  }

  ## ---------------------------------------------------------------------------
  ## Step 2: Generate predictions
  ## ---------------------------------------------------------------------------

  predictions <- predict(model, new_data = unknowns)$.pred

  ## ---------------------------------------------------------------------------
  ## Step 2: Apply bounds enforcement
  ## ---------------------------------------------------------------------------

  predictions_bounded <- apply_bounds(
    values   = predictions,
    property = property,
    verbose  = FALSE
  )

  ## ---------------------------------------------------------------------------
  ## Step 3: Format results
  ## ---------------------------------------------------------------------------

  config_id <- paste0(config$model, "_", config$preprocessing, "_", config$feature_selection)

  tibble::tibble(
    Sample_ID  = unknowns$Sample_ID,
    property   = property,
    pred       = predictions_bounded,
    cluster_id = cluster_id,
    config_id  = config_id
  )
}

## -----------------------------------------------------------------------------
