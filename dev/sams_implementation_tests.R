rm(list = ls())

devtools::document()
devtools::install(build_vignettes = FALSE, upgrade = "never", quick = TRUE)
devtools::load_all()
devtools::check()


## -----------------------------------------------------------------------------
## Step 1. Build horizons projects object
## -----------------------------------------------------------------------------

projects <- project_list("FFAR" = project_entry(spectra_path        = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/FFAR/opus_files/",
                                                sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/FFAR/fraction_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"),
                         "MOYS" = project_entry(spectra_path        = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/MOYS/opus_files/",
                                                sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/MOYS/fraction_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"),
                         "AONR" = project_entry(spectra_path        = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/AONR/opus_files/",
                                                sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/AONR/speco_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"))

projects <- project_list("AONR" = project_entry(spectra_path        = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/AONR/opus_files/",
                                                sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/AONR/speco_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"))

## -----------------------------------------------------------------------------
## Step 2: Load the input data
## -----------------------------------------------------------------------------

create_project_data(projects  = projects,
                    variables = "MAOM_C_g_kg")  -> maom_data

## -----------------------------------------------------------------------------
## Step 3: Set up project configurations
## -----------------------------------------------------------------------------

create_project_configurations(project_data       = maom_data,
                              models             = c("random_forest",
                                                     "cubist",
                                                     "xgboost",
                                                     "elastic_net",
                                                     "svm_rbf",
                                                     "mars",
                                                     "plsr",
                                                     "mlp_nn"),
                              transformations    = c("No Transformation",
                                                     "Log Transformation",
                                                     "Square Root Transformation"),
                              preprocessing      = c("raw",
                                                     "sg",
                                                     "snv",
                                                     "deriv1",
                                                     "deriv2",
                                                     "snv_deriv1",
                                                     "snv_deriv2",
                                                     "msc_deriv1"),
                              soil_covariates    = c("pH",
                                                     "Clay",
                                                     "CEC",
                                                     "Nitrogen"),
                              climate_covariates = c("all"),
                              spatial_covariates = NULL,
                              refresh            = TRUE,
                              verbose            = TRUE) -> project_configs


## -----------------------------------------------------------------------------
## Step 3.5: Randomly select a distribution of samples
## -----------------------------------------------------------------------------

project_configs %>%
  pluck("project_configurations") %>%
  group_by(model) %>%
  slice_sample(n = 100) %>%
  ungroup() %>%
  slice_sample(prop = 1) -> random_configurations

## -----------------------------------------------------------------------------
## Step 4: Run the model evaluation
## -----------------------------------------------------------------------------

run_model_evaluation(config                 = random_configurations,
                     input_data             = maom_data,
                     covariate_data         = project_configs$covariate_data,
                     variable               = "MAOM_C_g_kg",
                     output_dir             = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/test_1_2",
                     grid_size_eval         = 10,
                     bayesian_iter_eval     = 15,
                     cv_folds_eval          = 10,
                     retrain_top_models     = TRUE,
                     number_models_retained = 10,
                     grid_size_final        = 25,
                     bayesian_iter_final    = 20,
                     cv_folds_final         = 10,
                     pruning                = FALSE) -> maom_results

## -----------------------------------------------------------------------------
## Step 5: Stack the models
## -----------------------------------------------------------------------------

build_ensemble_stack(results_dir    = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/test_1_2",
                     input_data     = maom_data,
                     variable       = "MAOM_C_g_kg",
                     filter_metric  = "rsq",
                     n_best         = 4,
                     test_prop      = 0.2,
                     cv_folds       = 10,
                     verbose        = TRUE) -> stack_results

## -----------------------------------------------------------------------------
## Step 5: Visualize results
## -----------------------------------------------------------------------------

plot_ensemble_biplot(ensemble_results = stack_results)

plot_ensemble_upset(results_dir = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/test_1_2",
                    ensemble_results     = NULL,
                    num_models           = 30,
                    metric               = "rsq",
                    collapse_rare_traits = FALSE,
                    simplify             = FALSE)

plot_ensemble_sankey()

## -----------------------------------------------------------------------------

##  _____   ____  __  __    ______          _             _   _
## |  __ \ / __ \|  \/  |  |  ____|        | |           | | (_)
## | |__) | |  | | \  / |  | |____   ____ _| |_   _  __ _| |_ _  ___  _ __
## |  ___/| |  | | |\/| |  |  __\ \ / / _` | | | | |/ _` | __| |/ _ \| '_ \
## | |    | |__| | |  | |  | |___\ V / (_| | | |_| | (_| | |_| | (_) | | | |
## |_|     \____/|_|  |_|  |______\_/ \__,_|_|\__,_|\__,_|\__|_|\___/|_| |_|

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Step 1. Build horizons projects object
## -----------------------------------------------------------------------------

projects <- project_list("FFAR" = project_entry(spectra_path        = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/FFAR/opus_files/",
                                                sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/FFAR/fraction_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"),
                         "MOYS" = project_entry(spectra_path        = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/MOYS/opus_files/",
                                                sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/MOYS/fraction_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"))

projects <- project_list("AONR" = project_entry(spectra_path        = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/AONR/opus_files/",
                                                sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/AONR/speco_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"))

## -----------------------------------------------------------------------------
## Step 2: Load the input data
## -----------------------------------------------------------------------------

create_project_data(projects  = projects,
                    variables = "POM_C_g_kg")  -> pom_data

## -----------------------------------------------------------------------------
## Step 3: Set up project configurations
## -----------------------------------------------------------------------------

create_project_configurations(project_data       = pom_data,
                              models             = c("random_forest",
                                                     "cubist",
                                                     "xgboost",
                                                     "elastic_net",
                                                     "svm_rbf",
                                                     "mars",
                                                     "plsr",
                                                     "mlp_nn"),
                              transformations    = c("No Transformation",
                                                     "Log Transformation",
                                                     "Square Root Transformation"),
                              preprocessing      = c("raw",
                                                     "snv",
                                                     "deriv1",
                                                     "deriv2",
                                                     "snv_deriv1",
                                                     "snv_deriv2",
                                                     "msc_deriv1"),
                              feature_selection  = c("pca",
                                                     "correlation"),
                              soil_covariates    = c("pH",
                                                     "Clay",
                                                     "CEC",
                                                     "Nitrogen"),
                              climate_covariates = c("AI", "GDD"),
                              spatial_covariates = NULL,
                              refresh            = FALSE,
                              verbose            = TRUE) -> project_configs

create_project_configurations(project_data       = pom_data,
                              models             = c("cubist"),
                              transformations    = c("No Transformation"),
                              preprocessing      = c("snv"),
                              feature_selection  = c("pca",
                                                     "correlation"),
                              soil_covariates    = "Clay",
                              climate_covariates = NULL,
                              spatial_covariates = NULL,
                              refresh            = FALSE,
                              verbose            = TRUE) -> project_configs


## -----------------------------------------------------------------------------
## Step 3.5: Randomly select a distribution of samples
## -----------------------------------------------------------------------------

pom_data %>%
  filter(!is.na(POM_C_g_kg),
         POM_C_g_kg > 0) -> pom_data

project_configs %>%
  pluck("project_configurations") %>%
  group_by(model) %>%
  slice_sample(n = 100) %>%
  ungroup() %>%
  slice_sample(prop = 1) -> random_pom_configurations

## -----------------------------------------------------------------------------
## Step 4: Run the model evaluation
## -----------------------------------------------------------------------------

run_model_evaluation(config                 = project_configs$project_configurations,
                     input_data             = pom_data,
                     covariate_data         = project_configs$covariate_data,
                     variable               = "POM_C_g_kg",
                     output_dir             = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/feat_selection_test_250703",
                     grid_size_eval         = 15,
                     bayesian_iter_eval     = 15,
                     cv_folds_eval          = 10,
                     retrain_top_models     = FALSE,
                     number_models_retained = 15,
                     grid_size_final        = 25,
                     bayesian_iter_final    = 20,
                     cv_folds_final         = 10,
                     pruning                = FALSE) -> pom_results

## -----------------------------------------------------------------------------
## Step 5: Stack the models
## -----------------------------------------------------------------------------

build_ensemble_stack(results_dir    = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/POM_Results_250621",
                     input_data     = pom_data,
                     variable       = "POM_C_g_kg",
                     filter_metric  = "rsq",
                     n_best         = 4,
                     test_prop      = 0.2,
                     cv_folds       = 10,
                     verbose        = TRUE) -> pom_stack_results

## -----------------------------------------------------------------------------
## Step 5: Visualize results
## -----------------------------------------------------------------------------

plot_ensemble_biplot(ensemble_results = pom_stack_results)
plot_ensemble_upset(results_dir = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/POM_Results_250621",,
                    ensemble_results     = NULL,
                    num_models           = 20,
                    metric               = "rsq",
                    collapse_rare_traits = FALSE,
                    simplify             = TRUE)


## -----------------------------------------------------------------------------
#       _____  ____   ______ ______ ____
#      / ___/ / __ \ / ____// ____// __ \
#     \__ \ / /_/ // __/  / /    / / / /
#    ___/ // ____// /___ / /___ / /_/ /
#   /____//_/    /_____/ \____/ \____/
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Step 1. Build horizons projects object
## -----------------------------------------------------------------------------

projects <- project_list("AONR" = project_entry(spectra_path        = "../../2_Data/1_Input_Data/1_Spectral_Data/AONR/",
                                                sample_obs          = "../../2_Data/1_Input_Data/2_Physicochemical_Data/aonr_updated.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_"))

## -----------------------------------------------------------------------------
## Step 2: Load the input data
## -----------------------------------------------------------------------------

create_project_data(projects  = projects,
                    variables = "Relative_Yield")  -> yield_data

yield_data %>% drop_na() -> yield_data
## -----------------------------------------------------------------------------
## Step 3: Set up project configurations
## -----------------------------------------------------------------------------

create_project_configurations(project_data       = yield_data,
                              models             = c("random_forest",
                                                     "cubist",
                                                     "xgboost",
                                                     "elastic_net",
                                                     "svm_rbf",
                                                     "mars",
                                                     "plsr",
                                                     "mlp_nn"),
                              transformations    = c("No Transformation",
                                                     "Log Transformation",
                                                     "Square Root Transformation"),
                              preprocessing      = c("raw",
                                                     "snv",
                                                     "deriv1",
                                                     "deriv2",
                                                     "snv_deriv1",
                                                     "snv_deriv2",
                                                     "msc_deriv1"),
                              feature_selection  = c("pca",
                                                     "correlation",
                                                     )
                              soil_covariates    = c("pH",
                                                     "Clay",
                                                     "CEC",
                                                     "Nitrogen"),
                              climate_covariates = c("AI", "GDD"),
                              spatial_covariates = NULL,
                              refresh            = FALSE,
                              verbose            = TRUE) -> yield_project_configs


## -----------------------------------------------------------------------------
## Step 3.5: Randomly select a distribution of samples
## -----------------------------------------------------------------------------

yield_project_configs %>%
  pluck("project_configurations") %>%
  group_by(model) %>%
  slice_sample(n = 4) %>%
  ungroup() %>%
  slice_sample(prop = 1) -> random_yield_configurations

## -----------------------------------------------------------------------------
## Step 4: Run the model evaluation
## -----------------------------------------------------------------------------

run_model_evaluation(config                 = random_yield_configurations,
                     input_data             = yield_data,
                     covariate_data         = yield_project_configs$covariate_data,
                     variable               = "Relative_Yield",
                     output_dir             = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/yield_test_250701",
                     grid_size_eval         = 15,
                     bayesian_iter_eval     = 15,
                     cv_folds_eval          = 10,
                     retrain_top_models     = TRUE,
                     number_models_retained = 5,
                     grid_size_final        = 20,
                     bayesian_iter_final    = 15,
                     cv_folds_final         = 10,
                     pruning                = FALSE) -> yield_results

## -----------------------------------------------------------------------------
## Step 5: Stack the models
## -----------------------------------------------------------------------------

build_ensemble_stack(results_dir    = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/yield_test_250701",
                     input_data     = yield_data,
                     variable       = "Relative_Yield",
                     filter_metric  = "rsq",
                     n_best         = 4,
                     test_prop      = 0.2,
                     cv_folds       = 10,
                     verbose        = TRUE) -> yield_stack_results

## -----------------------------------------------------------------------------
## Step 5: Visualize results
## -----------------------------------------------------------------------------

plot_ensemble_biplot(ensemble_results = stack_results)

plot_ensemble_upset(results_dir = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/yield_test_250701",
                    ensemble_results     = NULL,
                    num_models           = 10,
                    metric               = "rsq",
                    collapse_rare_traits = FALSE,
                    simplify             = TRUE)



library(recipes)

# Minimal data
df <- tibble::tibble(
  Response = rnorm(100),
  `600` = rnorm(100),
  `602` = rnorm(100),
  `604` = rnorm(100),
  `606` = rnorm(100),
  `608` = rnorm(100)
)

# Dummy recipe
rec <- recipe(Response ~ ., data = df) %>%
  step_select_correlation(all_predictors(), outcome = "Response")

# Now prep
prep(rec)

