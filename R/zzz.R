## -----------------------------------------------------------------------------
## zzz.R
## Startup configuration for horizons
## -----------------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {

  # Startup message
  packageStartupMessage("horizons v0.7.5 loaded. Please flag bugs on Github (www.github.com/S-Leuthold/horizons)")

  # Optional: Register future plan if not already set
  if (requireNamespace("future", quietly = TRUE) &&
      is.null(future::plan("list"))) {
    future::plan(sequential)
  }

  # Optional: Silence verbose packages used internally
  if (requireNamespace("tidymodels", quietly = TRUE)) {
    options(tidymodels.dark = TRUE)
  }

  # Optional: Configure cli progress style
  options(cli.progress_show_after = 0.2)

}

utils::globalVariables(c(
  ".", ".metric", ".pred",
  "3992", "4000", "600", "608",
  "Absorbance", "Analyte_Base", "Cluster", "Covariate", "File_Name",
  "Fraction", "Layer_ID", "Measured_Value", "Predicted_Values",
  "Project", "Response", "SNV_SG0_Absorbance", "Sam_Include", "Sample_ID",
  "Sample_Index", "Source", "Top_Depth", "Wavenumber", "analyte",
  "baked_data", "best_model", "dataset.code_ascii_txt", "efferv_usda.a479_class",
  "extract_preprocessor", "extract_spec_parsnip", "final_metrics",
  "final_variable", "final_wf", "finalize_workflow", "finalized_models",
  "fitted_wf", "id.layer_uuid_txt", "info", "metrics", "name", "passed",
  "plan", "predict", "predictions", "quantile", "result", "rmse", "rmse_vec",
  "rsq", "scan.mir.model.name_utf8_txt", "select_best", "sequential",
  "step_add_covariates", "step_transform_spectra", "target_unit",
  "testing", "training", "tune", "une", "value", "vfold_cv", "wflow_id",
  "workflow", "workflow_map", "workflow_set"
))
