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

project_list("MOYS" = project_entry(spectra_path        = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/MOYS/opus_files/",
                                    sample_obs          = "../../../../../../../Desktop/_brain/1_Current_Projects/AI-CLIMATE/2_Data_Processed/MOYS/fraction_data.csv",
                                    file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                    file_name_delimiter = "_")) %>%
  create_project_data(projects  = .,
                      variables = "MAOM_C_g_kg")  -> proj_data

create_project_configurations(project_data       = proj_data,
                              models             = c("cubist"),
                              transformations    = c("No Transformation"),
                              preprocessing      = c("snv"),
                              feature_selection  = c("pca",
                                                     "correlation",
                                                     "boruta"),
                              soil_covariates    = c("pH"),
                              climate_covariates = NULL,
                              spatial_covariates = NULL,
                              refresh            = FALSE,
                              verbose            = TRUE) -> configs

run_model_evaluation(config                 = configs$project_configurations,
                     input_data             = proj_data,
                     covariate_data         = configs$covariate_data,
                     variable               = "MAOM_C_g_kg",
                     output_dir             = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/feat_selection_test_250715",
                     grid_size_eval         = 15,
                     bayesian_iter_eval     = 15,
                     cv_folds_eval          = 10,
                     retrain_top_models     = FALSE,
                     number_models_retained = 15,
                     grid_size_final        = 25,
                     bayesian_iter_final    = 20,
                     cv_folds_final         = 10,
                     pruning                = FALSE) -> pom_results
