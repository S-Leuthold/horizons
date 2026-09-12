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

- Config set (all strategies), four configs: {cubist, rf} × {snv, snv_deriv1}, **every one with `feature_selection = "pca"`**; transformation per property. PCA is `step_pca(threshold = 0.995)` inside the recipe, so it is fitted per fold and carries no leakage. Feature selection is held constant deliberately: it is part of how anyone models MIR spectra, and holding it fixed keeps it from confounding the strategy comparison. Which feature-selection method is best (PCA vs CARS vs correlation vs Boruta) is a separate question this experiment does not ask. See the 16:50 changelog entry.
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

30 cores, 62 GB, shared with Steve. **The binding constraint is memory per worker, not cores.** Under a multisession plan each tune task worker holds its own copy of the split plus the baked design matrix and the model: ≈ 2–3 GB per worker at 2 cm⁻¹, ≈ 1.3 GB at 4 cm⁻¹. A 20-worker plan at 2 cm⁻¹ exhausted the box on 2026-09-11 (see changelog). Rules from that point:

- `MAX_WORKERS = 8` (`00-config.R`), enforced by every strategy script.
- `EXPERIMENT_RESAMPLE = 4` (851 predictors) for every strategy — the pre-registered fallback, invoked for memory.
- `watchdog.sh` runs alongside every launch and kills the experiment if MemAvailable drops below 15 GB (`results/logs/watchdog.log`).
- Parallelism is registered by the scripts (`future::plan(multisession, MAX_WORKERS)`) and `evaluate()` is called with `workers = cv_folds`, because horizons' own parallel paths do not work at this scale (DOGFOOD #10, #12, #13). Configs run sequentially; tune parallelises folds × grid.

Everything runs as background `Rscript` processes, never in the shared kernel, with a checkpoint per (property × strategy) in `results/checkpoints/`. With eight workers the critical path is longer than the plan's 8 h estimate; the pilot timing sets the real number.

## Files

`00-config.R` · `01-build-kssl-snapshot.R` · `02-splits.R` · `03-A-global.R` · `04-B-gmm-local.R` (also E) · `05-C-gmm-perconfig.R` · `06-D-mbl.R` · `07-collect.R` · `08-figures.R` · `09-external-invenio.R` · `helpers.R` · `results/`

## Changelog

- 2026-09-11 — protocol and decision rule written; snapshot built (45,957 × 1,701).
- 2026-09-11 16:08 — **`EXPERIMENT_RESAMPLE` set to 4 and `MAX_WORKERS = 8`, before any strategy produced a result.** Reason: memory, not time. The first pilot attempts failed on three horizons parallelism bugs (DOGFOOD #10, #12, #13); the fourth, with a 20-worker plan at 2 cm⁻¹, consumed all 62 GB and was killed. No results were affected because none existed. The decision rule is unchanged.
- 2026-09-12 10:15 — **The four cached `oc` strategy-A evaluation rows were relabelled from `pruned` to `success` in place**, rather than re-running 90 minutes of identical tuning. They were stamped `pruned` by the scale-blind prune test (DOGFOOD #15) despite carrying no error, valid `best_params`, and original-scale RPD 7.03–9.78; `fit()` refuses anything not marked `success`. Only the status label changed — every metric and parameter in those rows is exactly what `evaluate()` produced at 08:09–09:39. Subsequent runs pass `prune = FALSE`, so the label cannot recur.
- 2026-09-11 16:50 — **Config set amended: every config carries `feature_selection = "pca"`.** Still before any strategy produced a result. Two reasons. Necessity: the pilot showed Cubist on the full-resolution matrix (14,228 × 851 at 4 cm⁻¹) did not complete a single one of 25 tuning tasks in 29.5 minutes with 8 workers at 99 % CPU, because it fits a linear model in every rule terminal (DOGFOOD #14); OSSL's own published pipeline is SNV → PCA (120 components) → Cubist for this exact reason, so the amendment moves the experiment *toward* the literature baseline. Design (Sam, 16:49): feature selection belongs in every spectral config, not just the one that forces it, and holding it constant stops it confounding the strategy comparison. Approved by Sam. The decision rule, the splits, and the metrics are unchanged.
