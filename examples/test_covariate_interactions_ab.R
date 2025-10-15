rm(list = ls())

## ---------------------------------------------------------------------------
## A/B Test: Covariate × Spectral Interactions
## ---------------------------------------------------------------------------
## Purpose: Test whether covariate × spectral interactions improve predictions
## beyond covariate main effects alone
##
## Hypothesis: Environmental covariates (pH, clay, MAP, AI) can refine spectral
## predictions in edge cases where spectral features alone are ambiguous

devtools::document()
devtools::install(build_vignettes = FALSE, upgrade = "never", quick = TRUE)
devtools::load_all()

## ---------------------------------------------------------------------------
## Step 0: Quick Verification Test (5-10 minutes)
## ---------------------------------------------------------------------------
## Tests that covariate_interactions parameter works before full evaluation

cli::cli_h1("Quick Verification Test")
cli::cli_alert_info("Testing that interactions parameter flows through evaluation...")

# Load minimal test data (skip if you already have test_data in environment)
if (!exists("test_data")) {

  read_spectra(source       = "opus",
               spectra_path = "../../AI-CLIMATE/data/processed/FFAR/opus_files",
               spectra_type = "MIR",
               verbose      = TRUE) %>%
    preprocess_spectra(spectra_data      = .,
                       resample_interval = 2,
                       baseline_method   = "none") %>%
    create_dataset(response_data      = "../../AI-CLIMATE/data/processed/FFAR/fraction_data.csv",
                   parse_ids          = TRUE,
                   include_coords     = TRUE,
                   coord_columns      = c("Longitude", "Latitude"),
                   id_format          = "project_sampleid_fraction_scanid_wellid",
                   aggregate_by       = c("Project", "Sample_ID"),
                   response_variables = c("POM_C_g_kg"),
                   verbose            = TRUE) %>%
    finalize_dataset(dataset                  = .,
                     response_variable        = "POM_C_g_kg",
                     spectral_outlier_method  = "mahalanobis",
                     detect_response_outliers = TRUE,
                     spectral_cutoff          = 0.975,
                     response_cutoff          = 1.5,
                     remove_outliers          = FALSE,
                     enforce_positive         = TRUE,
                     verbose                  = TRUE) -> test_data

}

# Fetch or load covariates
if (!exists("covariates")) {

  fetch_covariates(input_data         = test_data,
                   configurations     = NULL,
                   n_similar          = 30000,
                   soil_covariates    = c("ph", "clay"),
                   climate_covariates = c("MAP", "AI"),
                   variance_threshold = 0.99,
                   bayesian_iter      = 10,
                   prop_train         = 0.85,
                   climate_start_year = 2003,
                   climate_end_year   = 2024,
                   allow_par          = TRUE,
                   n_workers          = 8,
                   cache_dir          = tools::R_user_dir("horizons", "cache"),
                   refresh_soil       = FALSE,
                   refresh_climate    = FALSE,
                   verbose            = TRUE) -> covariates

}

## ---------------------------------------------------------------------------
## Step 1: Create Full A/B Test Configurations
## ---------------------------------------------------------------------------

create_configs(models             = c("random_forest", "cubist", "xgboost", "elastic_net"),
               transformations    = c("none", "log"),
               preprocessing      = c("snv", "snv_deriv1"),
               feature_selection  = c("pca", "cars", "correlation"),
               soil_covariates    = c("ph", "clay"),
               climate_covariates = c("MAP", "AI"),
               spatial_covariates = NULL,
               verbose            = TRUE) -> configs_base

## Group A: Main effects only
configs_base %>%
  mutate(covariate_interactions = FALSE,
         test_group             = "A_main_effects") -> configs_main

## Group B: With interactions
configs_base %>%
  mutate(covariate_interactions = TRUE,
         test_group             = "B_interactions") -> configs_interact

## Combine for evaluation
bind_rows(configs_main, configs_interact) %>%
  arrange(model, transformation, preprocessing, test_group) -> configs_ab

cli::cli_alert_success("Created {nrow(configs_ab)} total configurations")
cli::cli_ul(c(
  "{sum(configs_ab$test_group == 'A_main_effects')} with covariate main effects",
  "{sum(configs_ab$test_group == 'B_interactions')} with covariate interactions"
))

## ---------------------------------------------------------------------------
## Step 2: Run A/B Test Evaluation
## ---------------------------------------------------------------------------

cli::cli_h1("Running A/B Test Evaluation")

## Quick test: random sample of paired configs
## Strategy: Sample N rows from configs_base, then create BOTH versions (interact=TRUE/FALSE)

n_pairs_to_test <- 25  # Number of pairs to test

set.seed(307)

configs_base %>%
  slice_sample(n = n_pairs_to_test) -> sampled_base

## Create matched pairs from sampled base configs
sampled_base %>%
  mutate(covariate_interactions = FALSE,
         test_group             = "A_main_effects") -> test_main

sampled_base %>%
  mutate(covariate_interactions = TRUE,
         test_group             = "B_interactions") -> test_interact

bind_rows(test_main, test_interact) %>%
  arrange(model, transformation, preprocessing, feature_selection) -> configs_test

cli::cli_alert_info("Sampled {n_pairs_to_test} base configs → {nrow(configs_test)} total configs ({n_pairs_to_test} matched pairs)")
cli::cli_ul(c(
  "Test A: Spectra + covariate main effects only",
  "Test B: Spectra + covariates + interactions"
))

## Full test: uncomment for paper
# configs_test <- configs_ab
# cli::cli_alert_info("Running FULL A/B test: {nrow(configs_ab)} configs ({nrow(configs_ab)/2} matched pairs)")

evaluate_models_local(config          = configs_test,
                      input_data      = test_data,
                      covariate_data  = covariates$covariate_data,
                      variable        = "POM_C_g_kg",
                      output_dir      = "../results/ab_test_interactions",
                      grid_size       = 10,
                      bayesian_iter   = 15,
                      cv_folds        = 5,
                      allow_par       = TRUE,
                      n_cv_cores      = 10,
                      prune_models    = FALSE,
                      prune_threshold = 0.9,
                      seed            = 0307,
                      resume          = FALSE,
                      verbose         = TRUE) -> ab_results

## ---------------------------------------------------------------------------
## Step 3: Analyze A/B Test Results
## ---------------------------------------------------------------------------

cli::cli_h1("A/B Test Results Analysis")

## Calculate improvement metrics
ab_results %>%
  filter(!is.na(rmse), !is.na(covariate_interactions)) %>%
  group_by(model, transformation, preprocessing, feature_selection) %>%
  summarise(main_rmse     = rmse[!covariate_interactions][1],
            main_rsq      = rsq[!covariate_interactions][1],
            interact_rmse = rmse[covariate_interactions][1],
            interact_rsq  = rsq[covariate_interactions][1],
            .groups       = "drop") %>%
  mutate(interaction_benefit = (main_rmse - interact_rmse) / main_rmse * 100,
         rsq_improvement     = interact_rsq - main_rsq,
         interactions_help   = interaction_benefit > 2) -> ab_comparison

## ---------------------------------------------------------------------------
## Step 4: Summary Statistics
## ---------------------------------------------------------------------------

cli::cli_h2("Summary Statistics")

cli::cli_alert_info("Interaction effects (B vs A):")
cli::cli_text("  Mean RMSE improvement: {round(mean(ab_comparison$interaction_benefit, na.rm=TRUE), 2)}%")
cli::cli_text("  Mean R² improvement: {round(mean(ab_comparison$rsq_improvement, na.rm=TRUE), 3)}")
cli::cli_text("  Configs improved >2%: {sum(ab_comparison$interactions_help, na.rm=TRUE)}/{nrow(ab_comparison)}")

cli::cli_h2("Best Configurations")

ab_comparison %>%
  arrange(interact_rmse) %>%
  head(5) %>%
  select(model, preprocessing, interact_rmse, interact_rsq, interaction_benefit) %>%
  print()

cli::cli_h2("Where Interactions Helped Most")

ab_comparison %>%
  arrange(desc(interaction_benefit)) %>%
  head(5) %>%
  select(model, preprocessing, main_rmse, interact_rmse, interaction_benefit) %>%
  print()

## ---------------------------------------------------------------------------
## Step 5: Visualization
## ---------------------------------------------------------------------------

library(ggplot2)

## Plot 1: RMSE comparison boxplots
ab_results %>%
  filter(!is.na(rmse), !is.na(covariate_interactions)) %>%
  mutate(test_group = ifelse(covariate_interactions, "B_interactions", "A_main_effects")) %>%
  ggplot(aes(x = test_group, y = rmse, fill = test_group)) +
  geom_boxplot() +
  geom_line(aes(group = paste(model, transformation, preprocessing)),
            alpha = 0.3) +
  facet_wrap(~model, scales = "free_y") +
  labs(title    = "A/B Test: Do Covariate Interactions Improve RMSE?",
       subtitle = "Comparing main effects only (A) vs with interactions (B)",
       x        = "Test Group",
       y        = "RMSE (g/kg POM-C)") +
  scale_fill_manual(values = c("A_main_effects" = "#E69F00",
                                "B_interactions" = "#56B4E9")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 45, hjust = 1)) -> p1

print(p1)
ggsave("../results/ab_test_interactions/rmse_comparison.png", width = 12, height = 8)

## Plot 2: Interaction benefit by model
ab_comparison %>%
  filter(!is.na(interaction_benefit)) %>%
  ggplot(aes(x = model, y = interaction_benefit, fill = model)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "blue", alpha = 0.5) +
  labs(title    = "Additional Benefit of Interactions Over Main Effects",
       subtitle = "Positive = interactions help, >2% = meaningful improvement",
       x        = "Model Type",
       y        = "Additional RMSE Improvement (%)") +
  theme_minimal() +
  theme(legend.position = "none") -> p2

print(p2)
ggsave("../results/ab_test_interactions/interaction_benefit.png", width = 10, height = 6)

## ---------------------------------------------------------------------------
## Step 6: Save Results
## ---------------------------------------------------------------------------

write_csv(ab_comparison, "../results/ab_test_interactions/ab_comparison.csv")
qs::qsave(ab_results, "../results/ab_test_interactions/full_ab_results.qs")

cli::cli_h1("A/B Test Complete")
cli::cli_alert_success("Results saved to ../results/ab_test_interactions/")

## ---------------------------------------------------------------------------
## Step 7: Paper-Ready Summary
## ---------------------------------------------------------------------------

cli::cli_h2("Paper-Ready Summary")

ab_comparison %>%
  summarise(n_configs             = n(),
            mean_main_rmse        = mean(main_rmse, na.rm = TRUE),
            mean_interact_rmse    = mean(interact_rmse, na.rm = TRUE),
            mean_benefit          = mean(interaction_benefit, na.rm = TRUE),
            mean_rsq_improvement  = mean(rsq_improvement, na.rm = TRUE),
            n_help                = sum(interactions_help, na.rm = TRUE),
            pct_help              = n_help / n_configs * 100) -> summary_stats

## Paired t-test
if (nrow(ab_comparison) > 2) {

  t_test  <- t.test(ab_comparison$main_rmse, ab_comparison$interact_rmse, paired = TRUE)
  p_value <- t_test$p.value

} else {

  p_value <- NA

}

cli::cli_text("")
cli::cli_rule("Paper Text")
cli::cli_text("")
cli::cli_text("We evaluated whether covariate × spectral interaction terms improved")
cli::cli_text("predictions beyond covariate main effects alone. Across {summary_stats$n_configs}")
cli::cli_text("matched model configurations, aanadding interaction terms")
cli::cli_text("{ifelse(summary_stats$mean_benefit > 0, 'reduced', 'increased')}")
cli::cli_text("RMSE by {abs(round(summary_stats$mean_benefit, 1))}%")
cli::cli_text("relative to main effects only{ifelse(!is.na(p_value), paste0(' (p = ', round(p_value, 3), ', paired t-test)'), '')}.")
cli::cli_text("")
cli::cli_text("Interactions showed meaningful improvement (>2% RMSE reduction) in")
cli::cli_text("{summary_stats$n_help} of {summary_stats$n_configs} configurations")
cli::cli_text("({round(summary_stats$pct_help, 0)}%).")
cli::cli_text("")
cli::cli_text("{ifelse(summary_stats$mean_benefit > 2,
              'This suggests environmental context meaningfully modifies spectral signatures,',
              ifelse(summary_stats$mean_benefit > 0,
                     'This indicates modest context-dependent effects in spectral expression,',
                     'This suggests covariates primarily act as independent predictors,'))}")
cli::cli_text("{ifelse(summary_stats$mean_benefit > 2,
              'supporting the hypothesis that covariates refine spectral edge cases.',
              'with limited evidence for context-dependent spectral modification.')}")
cli::cli_text("")
cli::cli_rule()

