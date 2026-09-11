# 2026-09 local-strategy experiment

**Question.** For predicting soil properties from a large public MIR library (the KSSL subset of OSSL v1.2), which local-modeling strategy should `fit(local = )` implement, if any? This experiment is the gate for that design decision. The design it feeds is `dev/specs/v1-refactor/library-design.md`; the evidence behind the candidate strategies is `library-prior-art.md` and `library-seam-inventory.md` in the same directory.

**Pre-registration.** The protocol and the decision rule below were written on 2026-09-11 before any strategy ran. Changes after the first result are recorded in the changelog at the bottom, with the reason.

**Dogfooding.** Every step that horizons can do, horizons does: `spectra()`, `standardize()`, `add_response()`, `configure()`, `validate()`, `evaluate()`, `fit()`, `predict()`, `horizons_metric_set()`, `compute_c_alpha()`. Only the GMM/PCA step (not yet in the package) and strategy D (`resemble::mbl`) sit outside it. Every place horizons cannot be used, forces a workaround, or misbehaves goes into `dev/roadmaps/DOGFOOD_FINDINGS.md`.

## Data

`01-build-kssl-snapshot.R` builds the registered source `"kssl"` from OSSL v1.2 (CSV.gz, MD5-verified): dataset `KSSL.SSL`, instrument `Bruker Vertex 70 with HTS-XT accessory`, upper depth < 30 cm, complete spectra on the 600–4000 cm⁻¹ @ 2 cm⁻¹ grid. Result: **45,957 samples × 1,701 wavenumbers**; lab values in OSSL-native units (weight % for texture and carbon, pH unitless). Manifest with filters, counts, and hashes: `data/kssl/snapshot/kssl_v1.2/manifest.json`.

Properties, chosen for different behaviour:

| property | OSSL column | n non-NA | transformation | physical range |
|---|---|---|---|---|
| clay | `clay.tot_usda.a334_w.pct` | 26,168 | none | [0, 100] |
| oc | `oc_usda.c729_w.pct` | 45,792 | log | [0, ∞) |
| ph | `ph.h2o_usda.a268_index` | 28,413 | none | [0, 14] |

## Splits (`02-splits.R`), per property

1. Keep rows with a non-NA outcome inside the physical range (counts of impossible values reported).
2. Stratified 80/20 holdout (`rsample::initial_split(strata = outcome)`, seed 307). The 20 % is **test**, identical for every strategy.
3. From the 80 %, carve 15 % as **calib_ext**, *before any clustering*. The GMM, the PCA, and every model are fitted without these rows; every reported interval's conformal margin is computed on them.
4. The remainder is **train_core**.

Row-id sets are saved to `results/splits/<property>.qs2` with a SHA of each set; every strategy script asserts the sets match and writes the SHA into its results row.

## Shared settings

- Config set (all strategies): cubist × {snv, snv_deriv1}, rf × {snv, snv_deriv1}; feature selection none; transformation per property.
- Tuning (all horizons chains): `cv_folds = 5, grid_size = 5, bayesian_iter = 0`. Grid only. This is a comparison of strategies, not of tuned models.
- `HORIZONS_THREAD_CONTROL=TRUE` so ranger is single-threaded under multisession.
- Conformal wrapper (all strategies, `helpers.R::conformalize()`): scores `pmax(lower − y, y − upper)` on calib_ext, `c = compute_c_alpha(scores, 0.90)`, reported interval `[lower − c, upper + c]`. For B/C/E the margin is per cluster on calib_ext rows assigned to that cluster, pooled to the global margin below 200 rows (`n_k` and `pooled` recorded). For D the base interval is `[ŷ, ŷ]` (absolute-residual conformal).
- horizons' own `predict(interval = TRUE)` bounds for A/B/C are reported separately as `coverage_90_native`.

## Strategies

- **A — global.** One model per property on train_core: `spectra() |> standardize() |> add_response() |> configure(4 configs) |> validate() |> evaluate() |> fit(n_best = 1, compute_uq = TRUE, compute_ad = TRUE)`. Records the winning config, which B and E reuse.
- **B — clustered local, one config.** Clustering space on train_core: water bands removed (v1 `WATER_BANDS`), SNV, Savitzky–Golay 1st derivative (m = 1, p = 2, w = 11), PCA to 99 % variance (≤ 100 components), `mclust::Mclust(G = 5:11)`. Test and calib_ext rows assigned by `predict.Mclust` (argmax; posterior `z` and entropy ÷ log K stored). Per cluster with ≥ 300 train_core rows: the same chain with A's winning config only. Clusters below 300 rows use A's global model (count reported).
- **C — clustered local, per-cluster config.** Same clusters; per cluster `configure(4 configs) |> evaluate() |> fit(n_best = 1)`; the winner per cluster is recorded.
- **D — memory-based learning.** `resemble::mbl()` with train_core as the library, neighbours in PCA space, `k ∈ {50, 100, 200, 400}`, local PLS with 5–20 components, NNv validation to choose k and the preprocessing (SNV or SNV + 1st derivative). Predict time measured on 100 unknowns single-threaded. Artifact bytes reported two ways: the full preprocessed library, and PCA scores + rotation.
- **E — soft assignment.** From B: every test row predicted through every cluster model, blended by posterior; blended bounds then conformalized. Reported with the caveat that the blend has no per-cluster guarantee.

## Metrics (per property × strategy)

`n_train, n_calib, n_test, config, rmse, rpd, ccc, coverage_90, coverage_90_native, mean_width, predict_secs_per_100, artifact_bytes, k_clusters, n_fallback, n_pooled_clusters, split_sha`. Per-cluster table for B/C/E: `cluster_id, n_train, n_calib, n_test, metrics`, plus A's and D's metrics on the same test rows. `07-collect.R` adds a 500-draw paired bootstrap of ΔRPD and ΔRMSE over identical test rows. Figures: predicted vs observed grid, interval width vs |error|, per-cluster RPD bars, coverage dot plot.

## Decision rule (pre-registered)

- **Eligibility.** `coverage_90` within 87–93 on all three properties. A strategy outside that band is reported but cannot win; a miss means a bug in the wrapper or a broken exchangeability assumption and is fixed before comparing.
- **Primary criterion.** RPD on the holdout, paired on identical rows, decided on ≥ 2 of 3 properties. CCC may not be lower by more than 0.02.
- **Thresholds.** ΔRPD ≥ 0.10 is a win; |ΔRPD| < 0.05 is a tie. At n_test ≈ 4–5k the bootstrap SE of ΔRPD is ≈ 0.02–0.03, so 0.05 is the noise floor and 0.10 is a gain a user would notice.
- In order:
  1. **D wins** if RPD_D ≥ RPD_B + 0.10 on ≥ 2 properties, no property worse than B by > 0.05, predict time ≤ 60 s per 100 unknowns, and the compressed library ≤ 150 MB. Otherwise D is a documented v1.1 option.
  2. **B ships** if RPD_B ≥ RPD_A + 0.05 on ≥ 2 properties and none worse by > 0.05. B stays over D when within 0.05 of D: O(1) predict and the per-cluster reliability story break the tie. **E replaces hard assignment** if E ≥ B + 0.02 on ≥ 2 properties with mean width ≤ 1.05 × B's. **C replaces B** only if C ≥ B + 0.05 on ≥ 2 properties.
  3. **A ships** (reliability columns only, locality deferred to v1.1) if neither B/E nor D beats A by ≥ 0.05 on ≥ 2 properties.
- **Tiebreaks**, in order: mean width at equal coverage, artifact bytes, predict time.
- The cross-instrument external test (`09-external-invenio.R`, AONR Invenio-R spectra vs a total-carbon model) is reported, not gating, unless one strategy collapses (RPD < 1.0 where another is ≥ 1.4), which is raised as a veto candidate.

## Known deviations and asymmetries, stated before the run

- **Mondrian condition.** The reported intervals satisfy it (calib_ext is carved before clustering and never seen by the GMM or any model). horizons' *native* intervals for B/C do not, because `fit()` carves its own calibration inside train_core after the GMM has seen those rows (`pipeline-fit.R:184-191`; DOGFOOD #3). That is why the native coverage is a secondary column.
- **D's library size.** D uses 100 % of train_core as its library; a horizons final model trains on ~64 % of it (split F then split C). The 0.10 threshold absorbs part of this. A 64 %-library sensitivity run for D is optional.
- **Tuning budget.** Grid-only, five points. Absolute RPDs are below what a tuned model would reach; the comparison is between strategies under one budget.
- **Pre-registered fallback.** A pilot (clay × rf+snv, `evaluate(workers = 5)`, 45-min cap) times one fold at 2 cm⁻¹. If a Cubist fold exceeds ~10 min, `EXPERIMENT_RESAMPLE <- 4` in `00-config.R` resamples every strategy's input to 4 cm⁻¹ (851 predictors) before the run. If used, it is recorded here.

## Compute

30 cores, 62 GB. A(property) on ~20 cores (`evaluate()` needs `output_dir` when `workers > cv_folds`); when A finishes, B/C/E for that property run alongside A for the next; D on the spare cores. Everything runs as background `Rscript` processes, never in the shared kernel, with a checkpoint per (property × strategy) in `results/checkpoints/`. Expected critical path ≈ 8 h.

## Files

`00-config.R` · `01-build-kssl-snapshot.R` · `02-splits.R` · `03-A-global.R` · `04-B-gmm-local.R` (also E) · `05-C-gmm-perconfig.R` · `06-D-mbl.R` · `07-collect.R` · `08-figures.R` · `09-external-invenio.R` · `helpers.R` · `results/`

## Changelog

- 2026-09-11 — protocol and decision rule written; snapshot built (45,957 × 1,701).
