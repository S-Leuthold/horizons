#-------------------------------------------------------------------------------
#    ______     ______     __   __     _____     ______     ______     __  __
#   /\  ___\   /\  __ \   /\ "-.\ \   /\  __-.  /\  == \   /\  __ \   /\_\_\_\
#   \ \___  \  \ \  __ \  \ \ \-.  \  \ \ \/\ \ \ \  __<   \ \ \/\ \  \/_/\_\/_
#   \/\_____\  \ \_\ \_\  \ \_\\"\_\  \ \____-  \ \_____\  \ \_____\   /\_\/\_\
#   \/_____/   \/_/\/_/   \/_/ \/_/   \/____/   \/_____/   \/_____/   \/_/\/_/
#
# ------------------------------------------------------------------------------

## -----------------------------------------------------------------------------
devtools::document()
devtools::install(build_vignettes = FALSE, upgrade = "never", quick = TRUE)
devtools::load_all()
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
                                                sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/AONR/soils_data.csv",
                                                file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                                file_name_delimiter = "_")) %>%
  create_project_data(projects  = .,
                      variables = "MAOM_C_g_kg")  -> proj_data

## -----------------------------------------------------------------------------
## Step 3: Create project configurations
## -----------------------------------------------------------------------------

## TODO: Implement RPD as a metric.

create_project_configurations(project_data       = proj_data,
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
                              feature_selection = c("pca",
                                                    "correlation",
                                                    "shap"),
                              soil_covariates    = c("pH",
                                                     "Clay",
                                                     "CEC"),
                              climate_covariates = c("all"),
                              spatial_covariates = NULL,
                              refresh            = FALSE,
                              verbose            = TRUE) -> configs


## Debugging the MARS and elastic_net models -----------------------------------

create_project_configurations(project_data       = proj_data,
                              models             = c("elastic_net",
                                                     "mars"),
                              transformations    = c("No Transformation",
                                                     "Log Transformation"),
                              preprocessing      = c("snv_deriv1",
                                                     "raw"),
                              feature_selection = c("pca",
                                                    "shap"),
                              soil_covariates    = NULL,
                              climate_covariates = c("MAP"),
                              spatial_covariates = NULL,
                              refresh            = FALSE,
                              verbose            = TRUE) -> test_configs


## -----------------------------------------------------------------------------
## Step 4: Create random configurations
## -----------------------------------------------------------------------------

set.seed(0307)

configs %>%
  purrr::pluck(., "project_configurations") %>%
  dplyr::group_by(model) %>%
  dplyr::slice_sample(n = 50) %>%
  dplyr::ungroup() %>%
  dplyr::slice_sample(prop = 1) -> random_configs


## -----------------------------------------------------------------------------
## Step 5: Run the model evaluation
## -----------------------------------------------------------------------------

run_model_evaluation(config                 = random_configs,
                     input_data             = proj_data,
                     covariate_data         = configs$covariate_data,
                     variable               = "MAOM_C_g_kg",
                     output_dir             = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729",
                     grid_size_eval         = 10,
                     bayesian_iter_eval     = 15,
                     cv_folds_eval          = 10,
                     retrain_top_models     = TRUE,
                     number_models_retained = 15,
                     grid_size_final        = 15,
                     bayesian_iter_final    = 15,
                     cv_folds_final         = 10,
                     pruning                = FALSE) -> results
