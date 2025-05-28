# 📖 *horizons* Function Audit

_Last updated: 2025-05-23_

---
## 📂 File: `covar_cluster_input.R`
| Function             | Exported | Used By            | Category       | Status     | Description                                                    | Priority   | Notes                                         |
|----------------------|----------|--------------------|----------------|------------|----------------------------------------------------------------|------------|-----------------------------------------------|
| cluster_input_data   | ❌        | predict_covariates | preprocessing  | ✅ stable  | Clusters input spectral data using PCA and k-means             | high       | Cluster cap hardcoded to 3 if silhouette > 5  |



---

## 📂 File: `covar_create_training_subsets.R`
| Function                 | Exported | Used By            | Category       | Status     | Description                                                                    | Priority   | Notes                                  |
|--------------------------|----------|--------------------|----------------|------------|--------------------------------------------------------------------------------|------------|----------------------------------------|
| create_training_subsets  | ❌        | predict_covariates | preprocessing  | ✅ stable  | Assigns samples to clusters and selects central points by coverage threshold  | medium     | Works well as a modular utility        |


---

## 📂 File: `covar_evaluate_predictions.R`
| Function              | Exported | Used By                                       | Category     | Status     | Description                                                   | Priority   | Notes                               |
|-----------------------|----------|-----------------------------------------------|--------------|------------|---------------------------------------------------------------|------------|--------------------------------------|
| evaluate_predictions  | ✅        | predict_covariates, cubist_model_function_covar | evaluation | ✅ stable  | Computes RMSE, R², CCC, RPIQ using `soilspec::eval()`         | high       | Gracefully skips invalid/missing data |


---

## 📂 File: `covar_predict.R`
| Function            | Exported | Used By                                                            | Category            | Status     | Description                                                                                          | Priority   | Notes                                                   |
|---------------------|----------|--------------------------------------------------------------------|----------------------|------------|------------------------------------------------------------------------------------------------------|------------|----------------------------------------------------------|
| predict_covariates  | ✅        | cubist_model_function_covar, create_input_data, download_ossl_data | model orchestration | ✅ stable  | Full pipeline to predict covariates from MIR spectra using PCA + clustering + Cubist stacking       | very high | Central driver for covariate prediction functionality    |

---

## 📂 File: `covar_reduce_pca.R`
| Function                     | Exported | Used By             | Category       | Status     | Description                                                            | Priority   | Notes                                             |
|------------------------------|----------|----------------------|----------------|------------|------------------------------------------------------------------------|------------|----------------------------------------------------|
| reduce_dimensions_pca_covpred| ❌        | predict_covariates   | preprocessing  | ✅ stable  | Projects training and new data onto shared PCA space (FactoMineR)      | high       | Handles both `training_data` and `new_data` consistently |


---

## 📂 File: `covar_train_model_cubist.R`
| Function                   | Exported | Used By            | Category       | Status     | Description                                                                 | Priority   | Notes                                         |
|----------------------------|----------|--------------------|----------------|------------|-----------------------------------------------------------------------------|------------|----------------------------------------------|
| cubist_model_function_covar| ❌        | predict_covariates | modeling       | ✅ stable  | Builds, tunes, and evaluates Cubist models via grid + Bayesian tuning       | high       | Hyperparameter tuning pipeline is robust; parallelism included |

---

## 📂 File: `ensemble_build_model_stack.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `build_ensemble_stack` | ✅ |  |  |  |  |  |  |

---

## 📂 File: `input_create_raw.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `create_input_data` | ✅ | download_ossl_data, get_file_location, join_physicochemical_data, read_spectral_data |  |  |  |  |  |

---

## 📂 File: `input_download_ossl.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `download_ossl_data` | ❌ | predict_covariates |  |  |  |  |  |

---

## 📂 File: `input_get_file_locations.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `get_file_location` | ❌ | create_input_data, read_spectral_data |  |  |  |  |  |

---

## 📂 File: `input_join_physchem_data.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `join_physicochemical_data` | ❌ | create_input_data |  |  |  |  |  |

---

## 📂 File: `input_read_opus_spectra.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `read_spectral_data` | ❌ | create_input_data, get_file_location, join_physicochemical_data |  |  |  |  |  |

---

## 📂 File: `input_transform_response.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `transform_outcome` | ✅ |  |  |  |  |  |  |

---

## 📂 File: `metric_rrmse.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `rrmse_vec` | ✅ | evaluate_final_models, filter_workflows |  |  |  |  |  |

---

## 📂 File: `model_backtransform_tune_results.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `backtransform_tune_results` | ❌ |  |  |  |  |  |  |

---

## 📂 File: `model_build_grid.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `build_model_grid` | ❌ | build_recipe, define_model_specifications, full_model_evaluation |  |  |  |  |  |

---

## 📂 File: `model_build_recipe.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `build_recipe` | ❌ | build_model_grid, full_model_evaluation |  |  |  |  |  |

---

## 📂 File: `model_define_specs.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `define_model_specifications` | ❌ | full_model_evaluation |  |  |  |  |  |

---

## 📂 File: `model_evaluate_holdout.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `evaluate_final_models` | ❌ | full_model_evaluation |  |  |  |  |  |

---

## 📂 File: `model_filter_workflows.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `filter_workflows` | ❌ | evaluate_final_models, run_bayesian_tuning, full_model_evaluation |  |  |  |  |  |

---

## 📂 File: `model_generate_workflow_ids.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `clean_workflow_id` | ❌ | build_model_grid, full_model_evaluation |  |  |  |  |  |

---

## 📂 File: `model_run_bayes_tuning.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `run_bayesian_tuning` | ❌ | full_model_evaluation |  |  |  |  |  |

---

## 📂 File: `model_run_full_evaluation.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `full_model_evaluation` | ✅ | build_model_grid, evaluate_final_models |  |  |  |  |  |

---

## 📂 File: `step_join_covariates.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `step_add_covariates` | ✅ | build_recipe, step_add_covariates_new, prep.step_add_covariates, bake.step_add_covariates, print.step_add_covariates, .onLoad |  |  |  |  |  |
| `step_add_covariates_new` | ✅ | step_add_covariates, prep.step_add_covariates, bake.step_add_covariates, print.step_add_covariates |  |  |  |  |  |
| `prep.step_add_covariates` | ❌ | step_add_covariates, step_add_covariates_new, bake.step_add_covariates, print.step_add_covariates |  |  |  |  |  |
| `bake.step_add_covariates` | ❌ | step_add_covariates, step_add_covariates_new, prep.step_add_covariates, print.step_add_covariates |  |  |  |  |  |
| `print.step_add_covariates` | ❌ | step_add_covariates, step_add_covariates_new, prep.step_add_covariates, bake.step_add_covariates |  |  |  |  |  |

---

## 📂 File: `step_transform_spectra.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `process_spectra` | ✅ | step_transform_spectra, step_transform_spectra_new, prep.step_transform_spectra, bake.step_transform_spectra, print.step_transform_spectra |  |  |  |  |  |
| `step_transform_spectra` | ✅ | build_recipe, run_bayesian_tuning, process_spectra, step_transform_spectra_new, prep.step_transform_spectra, bake.step_transform_spectra, print.step_transform_spectra, .onLoad |  |  |  |  |  |
| `step_transform_spectra_new` | ✅ | process_spectra, step_transform_spectra, prep.step_transform_spectra, bake.step_transform_spectra, print.step_transform_spectra |  |  |  |  |  |
| `prep.step_transform_spectra` | ❌ | process_spectra, step_transform_spectra, step_transform_spectra_new, bake.step_transform_spectra, print.step_transform_spectra |  |  |  |  |  |
| `bake.step_transform_spectra` | ❌ | process_spectra, step_transform_spectra, step_transform_spectra_new, prep.step_transform_spectra, print.step_transform_spectra |  |  |  |  |  |
| `print.step_transform_spectra` | ❌ | process_spectra, step_transform_spectra, step_transform_spectra_new, prep.step_transform_spectra, bake.step_transform_spectra |  |  |  |  |  |

---

## 📂 File: `zzz.R`
| Function | Exported | Used By | Category | Status | Description | Priority | Notes |
|----------|----------|---------|----------|--------|-------------|----------|-------|
| `.onLoad` | ❌ |  |  |  |  |  |  |

---

