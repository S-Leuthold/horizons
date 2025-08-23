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



project_list("FFAR" = project_entry(spectra_path        = "~/Desktop/_brain/1_Current_Projects/AI-CLIMATE/data/processed/FFAR/opus_files",
                                    sample_obs          = "~/Desktop/_brain/1_Current_Projects/AI-CLIMATE/data/processed/FFAR/fraction_data.csv",
                                    file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                    file_name_delimiter = "_"),
             "MOYS" = project_entry(spectra_path        = "~/Desktop/_brain/1_Current_Projects/AI-CLIMATE/data/processed/MOYS/opus_files",
                                    sample_obs          = "~/Desktop/_brain/1_Current_Projects/AI-CLIMATE/data/processed/MOYS/fraction_data.csv",
                                    file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                    file_name_delimiter = "_"),
             "AONR" = project_entry(spectra_path        = "~/Desktop/_brain/1_Current_Projects/AI-CLIMATE/data/processed/AONR/opus_files",
                                    sample_obs          = "~/Desktop/_brain/1_Current_Projects/AI-CLIMATE/data/processed/AONR/soils_data.csv",
                                    file_name_format    = "project_sampleid_fraction_scanid_wellid",
                                    file_name_delimiter = "_")) %>%
  create_project_data(projects  = .,
                      variables = "POM_C_g_kg",
                      drop_na = FALSE)  -> proj_data_POM

## -----------------------------------------------------------------------------
## Step 3: Create project configurations
## -----------------------------------------------------------------------------

create_project_configurations(project_data       = test_data_full,
                              models             = c(
                                                     "random_forest",
                                                     "cubist",
                                                    # "xgboost",
                                                     "elastic_net",
                                                    # "svm_rbf",
                                                    # "mars",
                                                     "plsr",
                                                     "mlp_nn"
                                                     ),
                              transformations    = c(
                                                     "No Transformation",
                                                     "Log Transformation"
                                                     #"Square Root Transformation"
                                                     ),
                              preprocessing      = c(
                                                     "raw",
                                                     "sg",
                                                     "snv",
                                                     "deriv1"
                                                    # "deriv2",
                                                    # "snv_deriv1",
                                                    # "snv_deriv2",
                                                    # "msc_deriv1"
                                                    ),
                              feature_selection = c(
                                                    "none",
                                                    "pca",
                                                    "correlation",
                                                   # "shap",
                                                    "none"
                                                   ),
                              soil_covariates    = c("pH", "Clay"),
                              climate_covariates = c("AI", "MAP"),
                              spatial_covariates = NULL,
                              refresh            = FALSE,
                              verbose            = TRUE) -> configs


## -----------------------------------------------------------------------------
## Step 4: Create random configurations
## -----------------------------------------------------------------------------

sample_configs(configs         = configs$project_configurations,
               n_per_group     = 3,
               ensure_baseline = TRUE,
               factorial_pairs = NULL,
               verbose         = TRUE)  -> downsampled_configs

sessionInfo()['BLAS']
## -----------------------------------------------------------------------------
## Step 5: Run the model evaluation
## -----------------------------------------------------------------------------

run_model_evaluation(config                 = downsampled_configs,
                     input_data             = pom_proj_data,
                     covariate_data         = configs$covariate_data,
                     variable               = "POM_C_g_kg",
                     output_dir             = "~/Desktop/_brain/1_Current_Projects/AI-CLIMATE/results/POM_C_g_kg_0818",
                     grid_size_eval         = 10,
                     bayesian_iter_eval     = 15,
                     cv_folds_eval          = 10,
                     retrain_top_models     = TRUE,
                     number_models_retained = 15,
                     grid_size_final        = 15,
                     bayesian_iter_final    = 15,
                     cv_folds_final         = 10,
                     pruning                = FALSE,
                     parallel_strategy      = "cv_folds",
                     workers                = NULL,
                     chunk_size             = 50,
                     checkpoint_dir         = NULL,
                     resume                 = FALSE) -> pom_results

## -----------------------------------------------------------------------------
## Step 6: Create Plots
## -----------------------------------------------------------------------------

qread("../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/batch_summary_MAOM_C_g_kg_20250730_210657.qs") -> maom_results

## Figure 1: UpSet Plot

plot_ensemble_upset("../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729/",
                    ensemble_results = NULL)
## Figure 2:

## Figure 3:

## Figure 4: Component Interactions (Network Plot)

plot_component_chord_diagram(results_data          = maom_results,
                             metric                = "rrmse",
                             performance_threshold = 0.75,
                             covariate_handling    = "simplified",
                             top_interactions      = 400,
                             min_co_occurrence     = 5,
                             show_significance     = FALSE)

## -----------------------------------------------------------------------------
##
## -----------------------------------------------------------------------------

hierarchical_interaction_decomposition(maom_results,
                                       metric = "rrmse",
                                       covariate_handling = c("simplified"),
                                       max_interaction_level = 5) -> decomp_results_maom

analyze_component_synergy_effects(decomp_results_maom) -> x

plot_component_synergy_effects(x)

x$model_info
plot_models_radar(maom_results,
                  metric = "rrmse")

plot_component_value_interactions(
  results$full_summary %>% filter(status == "success"),
  metric = "rrmse",
  focus_components = c("models", "preprocessing")
)

plot_all_component_interactions(
  results$full_summary,
  metric = "rrmse",
  layout = "facet",
  max_interactions_per_panel = 25
)

plot_all_component_interactions(
  results$full_summary %>% filter(status == "success"),
  metric = "rrmse",
  layout = "ranked"
)

plot_covariate_effects(results_data     = results$full_summary %>% filter(status == "success"),
                       metric           = "rrmse",
                       analysis_type    = c("model_specific"),
                       min_observations = 5,
                       effect_threshold = 0)

plot_covariate_effects(results_data     = pom_results$full_summary %>% filter(status == "success"),
                       metric           = "rmse",
                       analysis_type    = c("individual"),
                       min_observations = 5,
                       effect_threshold = 0)


## -----------------------------------------------------------------------------
## Step 7: Ensemble Model
## -----------------------------------------------------------------------------

build_ensemble_stack(results_dir    = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729",
                    input_data     = proj_data,
                    variable       = "MAOM_C_g_kg",
                    filter_metric  = "rsq",
                    n_best         = 4,
                    test_prop      = 0.2,
                    cv_folds       = 10,
                    verbose        = TRUE) -> stack_results

build_ensemble_stack(results_dir    = "../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/POM_250804",
                    input_data     = proj_data,
                    variable       = "POM_C_g_kg",
                    filter_metric  = "rsq",
                    n_best         = 4,
                    test_prop      = 0.2,
                    cv_folds       = 10,
                    verbose        = TRUE) -> pom_stack_results


plot_ensemble_biplot(stack_results)
plot_ensemble_upset("../../../../../../../Desktop/_brain/1_Current_Projects/horizons/4_Results/MAOM_250729",
                    ensemble_results = stack_results)



configs_output <- create_project_configurations(
  project_data = test_data,
  models = c(
    "random_forest",
    "cubist",
    "plsr"
  ),
  transformations = c(
    "No Transformation"  # Start with just one
  ),
  preprocessing = c(
    "raw",
    "snv"
  ),
  feature_selection = c(
    "none",
    "pca"
  ),
  soil_covariates = NULL,     # Start without covariates
  climate_covariates = NULL,   # Start without covariates
  spatial_covariates = NULL,
  expand_covariate_grid = FALSE,
  include_covariates = FALSE,  # Explicitly FALSE for now
  refresh = FALSE,
  verbose = TRUE,
  parallel = FALSE,       # Safety parameters for covariate prediction
  n_workers = 1,
  allow_nested = FALSE
)
