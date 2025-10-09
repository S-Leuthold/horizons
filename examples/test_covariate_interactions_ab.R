rm(list=ls())

## -----------------------------------------------------------------------------
## A/B Test: Covariate Interactions vs Main Effects Only
## -----------------------------------------------------------------------------
## Purpose: Test whether covariate × spectral interactions improve predictions
## beyond covariate main effects alone

devtools::document()
devtools::install(build_vignettes = FALSE, upgrade = "never", quick = TRUE)
devtools::load_all()

## -----------------------------------------------------------------------------
## Build test data
## -----------------------------------------------------------------------------

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

## -----------------------------------------------------------------------------
## Create covariate data
## -----------------------------------------------------------------------------

fetch_covariates(input_data         = test_data,
                 configurations     = NULL,  # Will create configs after
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

## -----------------------------------------------------------------------------
## Create A/B Test Configurations
## -----------------------------------------------------------------------------
## Strategy: Compare identical configs with/without covariate interactions
## - Focus on PCA feature selection (optimal for interactions)
## - Test subset of models for quick turnaround
## - Create matched pairs for direct comparison

cli::cli_h1("Creating A/B Test Configurations")

# Create base configs WITH covariates
configs_all <- create_configs(
  models             = c("random_forest", "cubist", "xgboost", "elastic_net"),
  transformations    = c("none", "log"),
  preprocessing      = c("snv", "snv_deriv1"),
  feature_selection  = "pca",  # PCA best for interactions
  soil_covariates    = c("ph", "clay"),
  climate_covariates = c("MAP", "AI"),
  spatial_covariates = NULL,
  verbose            = TRUE
)

# Create A/B comparison by duplicating configs
# Group A: Covariate main effects only (interactions = FALSE or missing)
configs_main <- configs_all %>%
  mutate(
    covariate_interactions = FALSE,  # Explicit: no interactions
    test_group = "A_main_effects"
  )

# Group B: With covariate interactions (interactions = TRUE)
configs_interact <- configs_all %>%
  mutate(
    covariate_interactions = TRUE,   # NEW: enable interactions
    test_group = "B_interactions"
  )

# Combine for evaluation
configs_ab <- bind_rows(configs_main, configs_interact) %>%
  arrange(model, transformation, preprocessing, test_group)

cli::cli_alert_success("Created {nrow(configs_ab)} total configurations for A/B test")
cli::cli_ul(c(
  "{sum(configs_ab$test_group == 'A_main_effects')} configs with covariate main effects only",
  "{sum(configs_ab$test_group == 'B_interactions')} configs with covariate interactions"
))
cli::cli_alert_info("Each config tested with and without interactions for direct comparison")

## -----------------------------------------------------------------------------
## Run A/B Test Evaluation
## -----------------------------------------------------------------------------
## Use smaller sample for quick testing, or full set for publication

cli::cli_h1("Running A/B Test Evaluation")

# Quick test: random sample (20 configs = 10 pairs)
configs_test <- configs_ab %>%
  group_by(model, transformation, preprocessing) %>%
  slice_sample(n = 2) %>%  # Sample 2 per group (main + interaction)
  ungroup()

# Full test: uncomment for paper
# configs_test <- configs_ab

cli::cli_alert_info("Evaluating {nrow(configs_test)} configurations ({nrow(configs_test)/2} matched pairs)")
cli::cli_alert_info("Comparing:")
cli::cli_ul(c(
  "Test A: Spectra + covariate main effects",
  "Test B: Spectra + covariates + interactions"
))

evaluate_models_local(
  config         = configs_test,
  input_data     = test_data,
  covariate_data = covariates$covariate_data,
  variable       = "POM_C_g_kg",
  output_dir     = "../results/ab_test_interactions",
  grid_size      = 10,
  bayesian_iter  = 15,
  cv_folds       = 5,
  allow_par      = TRUE,
  n_cv_cores     = 6,
  prune_models   = TRUE,
  prune_threshold = 0.9,
  seed           = 0307,
  resume         = FALSE,
  verbose        = TRUE
) -> ab_results

## -----------------------------------------------------------------------------
## Analyze A/B Test Results
## -----------------------------------------------------------------------------

cli::cli_h1("A/B Test Results Analysis")

# Calculate improvement metrics
ab_comparison <- ab_results %>%
  filter(!is.na(rmse)) %>%
  group_by(model, transformation, preprocessing) %>%
  summarise(
    # Test A: Main effects only
    main_rmse = rmse[test_group == "A_main_effects"][1],
    main_rsq  = rsq[test_group == "A_main_effects"][1],

    # Test B: With interactions
    interact_rmse = rmse[test_group == "B_interactions"][1],
    interact_rsq  = rsq[test_group == "B_interactions"][1],

    .groups = "drop"
  ) %>%
  mutate(
    # Key comparison: Do interactions help beyond main effects?
    interaction_benefit = (main_rmse - interact_rmse) / main_rmse * 100,

    # Also look at R² improvement
    rsq_improvement = interact_rsq - main_rsq,

    # Flag significant improvements (>2% RMSE reduction)
    interactions_help = interaction_benefit > 2
  )

# Print summary
cli::cli_h2("Summary Statistics")

cli::cli_alert_info("Interaction effects (comparing B vs A):")
cli::cli_text("  Mean RMSE improvement: {round(mean(ab_comparison$interaction_benefit, na.rm=TRUE), 2)}%")
cli::cli_text("  Mean R² improvement: {round(mean(ab_comparison$rsq_improvement, na.rm=TRUE), 3)}")
cli::cli_text("  Configs where interactions help (>2% RMSE): {sum(ab_comparison$interactions_help, na.rm=TRUE)}/{nrow(ab_comparison)}")

# Show best performers
cli::cli_h2("Best Configurations")

ab_comparison %>%
  arrange(interact_rmse) %>%
  head(5) %>%
  select(model, preprocessing, interact_rmse, interact_rsq, interaction_benefit) %>%
  print()

# Show where interactions helped most
cli::cli_h2("Where Interactions Helped Most")

ab_comparison %>%
  arrange(desc(interaction_benefit)) %>%
  head(5) %>%
  select(model, preprocessing, main_rmse, interact_rmse, interaction_benefit) %>%
  print()

## -----------------------------------------------------------------------------
## Visualization
## -----------------------------------------------------------------------------

library(ggplot2)

# Plot 1: RMSE comparison
p1 <- ab_results %>%
  filter(!is.na(rmse)) %>%
  ggplot(aes(x = test_group, y = rmse, fill = test_group)) +
  geom_boxplot() +
  geom_line(aes(group = paste(model, transformation, preprocessing)),
            alpha = 0.3) +  # Connect matched pairs
  facet_wrap(~model, scales = "free_y") +
  labs(
    title = "A/B Test: Do Covariate Interactions Improve RMSE?",
    subtitle = "Comparing main effects only (A) vs with interactions (B)",
    x = "Test Group",
    y = "RMSE (g/kg POM-C)"
  ) +
  scale_fill_manual(values = c("A_main_effects" = "#E69F00",
                                "B_interactions" = "#56B4E9")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)
ggsave("../results/ab_test_interactions/rmse_comparison.png",
       width = 12, height = 8)

# Plot 2: Interaction benefit by model
p2 <- ab_comparison %>%
  filter(!is.na(interaction_benefit)) %>%
  ggplot(aes(x = model, y = interaction_benefit, fill = model)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "blue", alpha = 0.5) +
  labs(
    title = "Additional Benefit of Interactions Over Main Effects",
    subtitle = "Positive = interactions help, >2% = meaningful improvement",
    x = "Model Type",
    y = "Additional RMSE Improvement (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)
ggsave("../results/ab_test_interactions/interaction_benefit.png",
       width = 10, height = 6)

## -----------------------------------------------------------------------------
## Save Results
## -----------------------------------------------------------------------------

# Save detailed comparison
write_csv(ab_comparison, "../results/ab_test_interactions/ab_comparison.csv")

# Save full results
qs::qsave(ab_results, "../results/ab_test_interactions/full_ab_results.qs")

cli::cli_h1("A/B Test Complete")
cli::cli_alert_success("Results saved to ../results/ab_test_interactions/")
cli::cli_alert_info("Review ab_comparison.csv for detailed metrics")

## -----------------------------------------------------------------------------
## Paper-Ready Summary
## -----------------------------------------------------------------------------

cli::cli_h2("Paper-Ready Summary")

summary_stats <- ab_comparison %>%
  summarise(
    n_configs = n(),
    mean_main_rmse = mean(main_rmse, na.rm = TRUE),
    mean_interact_rmse = mean(interact_rmse, na.rm = TRUE),
    mean_interaction_benefit = mean(interaction_benefit, na.rm = TRUE),
    mean_rsq_improvement = mean(rsq_improvement, na.rm = TRUE),
    n_interactions_help = sum(interactions_help, na.rm = TRUE),
    pct_interactions_help = n_interactions_help / n_configs * 100
  )

# Run paired t-test
if (nrow(ab_comparison) > 2) {
  t_test <- t.test(ab_comparison$main_rmse, ab_comparison$interact_rmse, paired = TRUE)
  p_value <- t_test$p.value
} else {
  p_value <- NA
}

cli::cli_text("")
cli::cli_rule("Paper-Ready Summary")
cli::cli_text("")
cli::cli_text("We evaluated whether covariate × spectral interaction terms improved")
cli::cli_text("predictions beyond covariate main effects alone. Across {summary_stats$n_configs}")
cli::cli_text("matched model configurations, adding interaction terms")
cli::cli_text("{ifelse(summary_stats$mean_interaction_benefit > 0, 'reduced', 'increased')}")
cli::cli_text("RMSE by {abs(round(summary_stats$mean_interaction_benefit, 1))}%")
cli::cli_text("relative to main effects only{ifelse(!is.na(p_value), paste0(' (p = ', round(p_value, 3), ', paired t-test)'), '')}.")
cli::cli_text("")
cli::cli_text("Interactions showed meaningful improvement (>2% RMSE reduction) in")
cli::cli_text("{summary_stats$n_interactions_help} of {summary_stats$n_configs} configurations")
cli::cli_text("({round(summary_stats$pct_interactions_help, 0)}%).")
cli::cli_text("")
cli::cli_text("{ifelse(summary_stats$mean_interaction_benefit > 2,
              'This suggests that environmental context meaningfully modifies spectral signatures,',
              ifelse(summary_stats$mean_interaction_benefit > 0,
                     'This indicates modest context-dependent effects in spectral expression,',
                     'This suggests covariates primarily act as independent predictors,'))}")
cli::cli_text("{ifelse(summary_stats$mean_interaction_benefit > 2,
              'supporting the hypothesis that covariates refine predictions in spectral edge cases.',
              'with limited evidence for context-dependent spectral modification.')}")
cli::cli_text("")
cli::cli_rule()
