pkgname <- "horizons"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('horizons')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build_ensemble_stack")
### * build_ensemble_stack

flush(stderr()); flush(stdout())

### Name: build_ensemble_stack
### Title: Build, Blend, and Evaluate a Stacked Ensemble from Top Workflows
### Aliases: build_ensemble_stack

### ** Examples

## Not run: 
##D model_results <- build_ensemble_stack(
##D   top_workflows = filtered_workflows,
##D   input_data    = my_data,
##D   variable      = "SOC_pct"
##D )
##D 
##D model_results$evaluation_metrics
##D model_results$predictions
## End(Not run)




cleanEx()
nameEx("build_recipe")
### * build_recipe

flush(stderr()); flush(stdout())

### Name: build_recipe
### Title: Build Tidymodels Recipe for Spectral + Covariate Modeling
### Aliases: build_recipe
### Keywords: internal

### ** Examples

## Not run: 
##D build_recipe(
##D   input_data              = df,
##D   spectral_transformation = "snv_deriv1",
##D   response_transformation = "Log Transformation",
##D   covariate_selection     = c("Clay", "pH"),
##D   covariate_data          = covariates
##D )
## End(Not run)




cleanEx()
nameEx("cluster_spectral_data")
### * cluster_spectral_data

flush(stderr()); flush(stdout())

### Name: cluster_spectral_data
### Title: Cluster Mid-Infrared Spectra via PCA and K-Means
### Aliases: cluster_spectral_data

### ** Examples

## Not run: 
##D clustered <- cluster_spectral_data(my_spectral_data)
##D head(clustered$input_data)
## End(Not run)




cleanEx()
nameEx("cluster_spectral_predictors")
### * cluster_spectral_predictors

flush(stderr()); flush(stdout())

### Name: cluster_spectral_predictors
### Title: Cluster Spectral Predictors to Reduce Redundancy
### Aliases: cluster_spectral_predictors

### ** Examples

## Not run: 
##D result <- cluster_spectral_predictors(spectra, k = 300)
##D head(result$reduced_mat)
## End(Not run)




cleanEx()
nameEx("create_clustered_subsets")
### * create_clustered_subsets

flush(stderr()); flush(stdout())

### Name: create_clustered_subsets
### Title: Create Clustered Training Subsets from PCA Scores
### Aliases: create_clustered_subsets

### ** Examples

## Not run: 
##D training_subsets <- create_clustered_subsets(
##D   training_data = my_training_data,
##D   pca_model     = my_pca_model,
##D   kmeans_model  = my_kmeans_model,
##D   n_components  = 50,
##D   coverage      = 0.8
##D )
## End(Not run)




cleanEx()
nameEx("create_project_configurations")
### * create_project_configurations

flush(stderr()); flush(stdout())

### Name: create_project_configurations
### Title: Create Model Configuration Grid for Horizons Workflows
### Aliases: create_project_configurations

### ** Examples

## Not run: 
##D config_grid <- create_project_configurations(
##D   project_data       = my_project_data,
##D   models             = c("xgboost", "cubist"),
##D   transformations    = c("none", "log"),
##D   preprocessing      = c("SNV", "SG-1D"),
##D   soil_covariates    = "all",
##D   climate_covariates = c("MAT", "MAP"),
##D   expand_covariate_grid = TRUE
##D )
##D 
##D config_grid$project_configurations
##D config_grid$covariate_data
## End(Not run)




cleanEx()
nameEx("create_project_data")
### * create_project_data

flush(stderr()); flush(stdout())

### Name: create_project_data
### Title: Create Unified Input Data for Spectral Modeling
### Aliases: create_project_data

### ** Examples

## Not run: 
##D projects <- project_list(
##D   FFAR = project_entry("data/FFAR/spectra", "data/FFAR/soil.csv"),
##D   AONR = project_entry("data/AONR/OPUS", "data/AONR/soil.csv", default_fraction = "Clay")
##D )
##D 
##D spectra_data <- create_project_data(
##D   projects      = projects,
##D   variables     = c("Sand", "pH"),
##D   save_spectra  = TRUE,
##D   save_locale   = "data/processed"
##D )
## End(Not run)




cleanEx()
nameEx("create_summary_table")
### * create_summary_table

flush(stderr()); flush(stdout())

### Name: create_summary_table
### Title: Generate Summary Table of Candidate and Ensemble Model Metrics
### Aliases: create_summary_table
### Keywords: internal

### ** Examples

## Not run: 
##D create_summary_table(ensemble_metrics = best_stack_metrics,
##D                      candidate_metrics = best_candidate_metrics)
## End(Not run)




cleanEx()
nameEx("define_model_specifications")
### * define_model_specifications

flush(stderr()); flush(stdout())

### Name: define_model_specifications
### Title: Define a Parsnip Model Specification for Soil Spectral Modeling
### Aliases: define_model_specifications
### Keywords: internal

### ** Examples

## Not run: 
##D define_model_specifications("random_forest")
##D define_model_specifications("plsr")
##D define_model_specifications("mlp_nn")
## End(Not run)




cleanEx()
nameEx("download_horizons_data")
### * download_horizons_data

flush(stderr()); flush(stdout())

### Name: download_horizons_data
### Title: Download and Cache OSSL Core Datasets
### Aliases: download_horizons_data

### ** Examples

## Not run: 
##D download_horizons_data()
## End(Not run)




cleanEx()
nameEx("download_ossl_data")
### * download_ossl_data

flush(stderr()); flush(stdout())

### Name: download_ossl_data
### Title: Download and Preprocess OSSL Spectral and Covariate Data
### Aliases: download_ossl_data

### ** Examples

## Not run: 
##D # Download and preprocess OSSL data for pH and Sand
##D ossl_data <- download_ossl_data(covariates = c("pH", "Sand"))
##D 
##D # Preview structure
##D glimpse(ossl_data)
## End(Not run)




cleanEx()
nameEx("evaluate_final_models")
### * evaluate_final_models

flush(stderr()); flush(stdout())

### Name: evaluate_final_models
### Title: Evaluate Final Models on Holdout Data
### Aliases: evaluate_final_models
### Keywords: internal

### ** Examples

## Not run: 
##D evaluate_final_models(
##D   finalized_wf_sets = fitted_model_tbl,
##D   holdout_data      = test_samples
##D )
## End(Not run)




cleanEx()
nameEx("evaluate_model_config")
### * evaluate_model_config

flush(stderr()); flush(stdout())

### Name: evaluate_model_config
### Title: Evaluate a Single Model Configuration on Spectral Data
### Aliases: evaluate_model_config

### ** Examples

## Not run: 
##D config_result <- evaluate_model_config(
##D   input_data        = my_spectral_data,
##D   covariate_data    = my_covs,
##D   variable          = "MAOM_C_g_kg",
##D   model             = "cubist",
##D   transformation    = "Log",
##D   preprocessing     = "snv",
##D   covariates        = c("Clay", "pH"),
##D   include_covariates = TRUE
##D )
##D 
##D config_result$evaluation_results
## End(Not run)




cleanEx()
nameEx("evaluate_predictions")
### * evaluate_predictions

flush(stderr()); flush(stdout())

### Name: evaluate_predictions
### Title: Evaluate Predicted Covariates Against Measured Values
### Aliases: evaluate_predictions

### ** Examples

## Not run: 
##D results <- evaluate_predictions(
##D   measured_data = my_measured_data,
##D   modeled_data  = my_predicted_data
##D )
## End(Not run)




cleanEx()
nameEx("fetch_climate_covariates")
### * fetch_climate_covariates

flush(stderr()); flush(stdout())

### Name: fetch_climate_covariates
### Title: Retrieve and Summarize Climate Covariates from Daymet
### Aliases: fetch_climate_covariates

### ** Examples

## Not run: 
##D fetch_climate_covariates(
##D   input_data = my_soil_data,
##D   start_year = 2010,
##D   end_year = 2020,
##D   gdd_base = 5
##D )
## End(Not run)




cleanEx()
nameEx("fit_cubist_model")
### * fit_cubist_model

flush(stderr()); flush(stdout())

### Name: fit_cubist_model
### Title: Fit a Cubist Model Using PCA-Transformed Spectral Data
### Aliases: fit_cubist_model

### ** Examples

## Not run: 
##D df <- tibble::tibble(
##D   Dim.1 = rnorm(100),
##D   Dim.2 = rnorm(100),
##D   Dim.3 = rnorm(100),
##D   Sand  = runif(100, 50, 80)
##D )
##D 
##D result <- fit_cubist_model(input_data = df, covariate = "Sand", verbose = TRUE)
##D result$Evaluation
## End(Not run)




cleanEx()
nameEx("get_ossl_data_path")
### * get_ossl_data_path

flush(stderr()); flush(stdout())

### Name: get_ossl_data_path
### Title: Get Path to Cached OSSL Dataset File
### Aliases: get_ossl_data_path
### Keywords: internal

### ** Examples

## Not run: 
##D get_ossl_data_path("lab")
## End(Not run)




cleanEx()
nameEx("get_processed_mir_path")
### * get_processed_mir_path

flush(stderr()); flush(stdout())

### Name: get_processed_mir_path
### Title: Get Path to Processed MIR Spectra File
### Aliases: get_processed_mir_path

### ** Examples

## Not run: 
##D get_processed_mir_path()
## End(Not run)




cleanEx()
nameEx("plot_ensemble_biplot")
### * plot_ensemble_biplot

flush(stderr()); flush(stdout())

### Name: plot_ensemble_biplot
### Title: Plot Ensemble Predictions and Model Weights
### Aliases: plot_ensemble_biplot

### ** Examples

## Not run: 
##D results <- build_ensemble_stack(...)
##D plots <- plot_ensemble_results(results)
##D print(plots$Obs_vs_Pred)
##D print(plots$Model_Weights)
## End(Not run)




cleanEx()
nameEx("predict_covariates")
### * predict_covariates

flush(stderr()); flush(stdout())

### Name: predict_covariates
### Title: Predict Soil Covariates from MIR Spectra Using Clustered Cubist
###   Models
### Aliases: predict_covariates

### ** Examples

## Not run: 
##D preds <- predict_covariates(
##D   covariates = c("Sand", "pH"),
##D   input_data = my_mir_data,
##D   refresh = TRUE
##D )
##D 
##D preds$Predicted_Values
##D preds$Evaluation_Statistics
## End(Not run)




cleanEx()
nameEx("project_entry")
### * project_entry

flush(stderr()); flush(stdout())

### Name: project_entry
### Title: Define a Project for Spectral Data Ingestion
### Aliases: project_entry

### ** Examples

project_entry(
  spectra_path        = "data/FFAR/spectra",
  sample_obs          = "data/FFAR/soil.csv",
  file_name_format    = "project_sampleid_fraction_ignore_ignore",
  file_name_delimiter = "_",
  default_fraction    = "Bulk"
)




cleanEx()
nameEx("project_list")
### * project_list

flush(stderr()); flush(stdout())

### Name: project_list
### Title: Define a Named List of Projects for Spectral Ingestion
### Aliases: project_list

### ** Examples

project_list(
  FFAR = project_entry("data/FFAR/spectra", "data/FFAR/soil.csv", "sampleid_fraction", "_"),
  AONR = project_entry("data/AONR/OPUS", "data/AONR/soil.csv", "sampleid_fraction", "_", "Clay")
)




cleanEx()
nameEx("reduce_dimensions_pca")
### * reduce_dimensions_pca

flush(stderr()); flush(stdout())

### Name: reduce_dimensions_pca
### Title: Reduce Spectral Dimensionality Using PCA
### Aliases: reduce_dimensions_pca

### ** Examples

## Not run: 
##D reduced <- reduce_dimensions_pca(training_data = ossl_data, new_data = unknown_data)
##D 
##D names(reduced)
##D head(reduced$training_data)
##D head(reduced$new_data)
## End(Not run)




cleanEx()
nameEx("rrmse_vec")
### * rrmse_vec

flush(stderr()); flush(stdout())

### Name: rrmse_vec
### Title: Relative Root Mean Squared Error (RRMSE)
### Aliases: rrmse_vec rrmse
### Keywords: internal

### ** Examples

rrmse_vec(
  data = data.frame(truth = c(1, 2, 3), estimate = c(1.1, 1.9, 3.2)),
  truth = truth,
  estimate = estimate
)

library(yardstick)
data <- tibble::tibble(obs = c(100, 120, 140), pred = c(105, 118, 135))
rrmse_vec(data, truth = obs, estimate = pred)

metric_set(rrmse, rsq)(data, truth = obs, estimate = pred)




cleanEx()
nameEx("run_model_evaluation")
### * run_model_evaluation

flush(stderr()); flush(stdout())

### Name: run_model_evaluation
### Title: Run Full Batch Model Evaluation Across Configuration Grid
### Aliases: run_model_evaluation

### ** Examples

## Not run: 
##D results <- run_model_evaluation(
##D   config                = model_config_grid,
##D   input_data            = spectral_data,
##D   covariate_data        = predicted_covs,
##D   variable              = "MAOM_C_g_kg",
##D   grid_size_eval        = 10,
##D   bayesian_iter_eval    = 15,
##D   number_models_retained = 20
##D )
## End(Not run)




cleanEx()
nameEx("safe_run_model")
### * safe_run_model

flush(stderr()); flush(stdout())

### Name: safe_run_model
### Title: Safely Run a Single Model Configuration and Log Results
### Aliases: safe_run_model

### ** Examples

## Not run: 
##D result <- safe_run_model(
##D   config_row     = config[1, ],
##D   input_data     = spectral_data,
##D   covariate_data = predicted_covs,
##D   variable       = "MAOM_C_g_kg",
##D   row_index      = 1,
##D   output_dir     = "outputs"
##D )
##D 
##D result$status_summary
## End(Not run)




cleanEx()
nameEx("safely_execute")
### * safely_execute

flush(stderr()); flush(stdout())

### Name: safely_execute
### Title: Safely Evaluate an Expression with Optional Logging and Tracing
### Aliases: safely_execute
### Keywords: internal

### ** Examples

## Not run: 
##D safely_execute(log("oops"), default_value = NA, error_message = "Failed to take log")
## End(Not run)




cleanEx()
nameEx("step_add_covariates")
### * step_add_covariates

flush(stderr()); flush(stdout())

### Name: step_add_covariates
### Title: Add Scaled Covariates to a Recipe Dataset
### Aliases: step_add_covariates

### ** Examples

## Not run: 
##D recipe(data = soil_data) %>%
##D   update_role(Sample_ID, new_role = "id") %>%
##D   step_add_covariates(covariate_data = predicted_covs)
## End(Not run)




cleanEx()
nameEx("tune_blend")
### * tune_blend

flush(stderr()); flush(stdout())

### Name: tune_blend
### Title: Tune Ensemble Blending Hyperparameters via Grid Search
### Aliases: tune_blend
### Keywords: internal

### ** Examples

## Not run: 
##D tune_blend(model_stack = my_stack,
##D            test_data   = holdout_samples)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
