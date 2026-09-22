# Experiment 2 — the configuration × structure factorial

**Status:** superseded on 2026-09-16. The five-arm factorial below was designed on 2026-09-14 and never run. What ran instead was a shakedown, a learning curve, the arm-P-versus-arm-M pivot, and a locality curve, and their results ended the factorial: locality is real, larger than the learner effect, and the design moved to a training-set selection verb, specified in `../../specs/v1-refactor/select-training-design.md`. The design section is kept as the record of what was planned and why it was dropped.
**Predecessor:** `../2026-09-local-strategy/` — read its README and `results/verdict.md` first.

---

## What ran (2026-09-15 to 16)

Every script runs from `package/` against the installed package, reuses experiment 1's snapshot, splits and helpers, and writes only under this directory's `results/` (gitignored, so the summary tables are reproduced here). All at 2 cm⁻¹ on the KSSL snapshot; `test` is experiment 1's fixed held-out split (clay 5,239 rows, oc 9,162, pH 5,686).

| script | question | result dir |
|---|---|---|
| `00-shakedown.R` (`run_shakedown.sh`) | what the M2/M3 parallelism costs on the real library | `results/shakedown*/` |
| `01-learning-curve.R` (`run_learning_curve.sh`), `02-learning-curve-figure.R` | does accuracy need all 17,788 clay rows | `results/learning-curve/` |
| `03-pivot-pm.R` (`run_pivot.sh`) | global PLS against memory-based learning, learner held to PLS | `results/pivot/` |
| `04-locality-curve.R` (`run_locality.sh`) | how coarse can the neighbourhood be | `results/locality-curve/` |
| `pm-helpers.R` | shared by 03 and 04; first block verbatim from experiment 1's `06-D-mbl.R` | |
| `08-verify-verb.R` (2026-09-21) | does `select_training()` reproduce 05's hand-built KSSL selection | prints PASS/FAIL; no result dir |

`08-verify-verb.R` ran 2026-09-21 against the 9/17 checkpoint: the verb's k = 100 union on the KSSL control batch is exactly 3,426 rows, the space retains 33 components, every one of the 101 targets' neighbour sets and their order match `kssl-targets.qs2`, and the nearest distances agree to 4e-15. 8 s for the draw on the 31,130-row pool. The MOYS batch is not reproduced by construction: 05 interpolated MOYS onto the pool's grid, the verb resamples the pool onto the targets'.

### Shakedown

The 9/15 multisession run was killed by the watchdog: persistent workers never return memory to the OS (glibc keeps the fold's sub-mmap-threshold allocations resident), so finished plsr workers sat at 3 to 4 GB and oc dispatched onto memory already spent. `00-shakedown.R` now defaults to `future.callr`, one fresh process per config, which reproduced the multisession numbers exactly and released 7 GB within two minutes of the plsr configs finishing. Evaluate's peak is the startup spike when every worker preps the full table at once (clay 18.5 GB at startup, 10 to 11 GB steady, `fit()` with UQ and AD 23.3 GB), not steady state. Vectorizing `step_transform_spectra` (branch `feat/vectorize-transform-spectra`) is bit-identical and 3 to 5× faster per bake but does not move the peak. A tuning-only hoist of SNV and the derivative out of the folds was built, proved equivalent, made the startup spike worse (24.7 GB), and was deleted when the pivot showed the memory work was serving the arms that lose on accuracy. Its measurements survive in the 2026-09-16 session log and horizons #62.

### Learning curve (clay, nested subsamples, fixed test rows)

| n_train | cubist_snv_pca RPD | RMSE | plsr_snv_deriv1_pca RPD | RMSE |
|---|---|---|---|---|
| 556 | 2.40 | 6.32 | 2.47 | 6.16 |
| 1,112 | 2.65 | 5.74 | 2.54 | 5.99 |
| 2,224 | 2.86 | 5.31 | 2.69 | 5.65 |
| 4,447 | 3.15 | 4.82 | 2.75 | 5.53 |
| 8,894 | 3.50 | 4.35 | 2.80 | 5.42 |
| 17,788 | 3.87 | 3.93 | 2.83 | 5.37 |

No plateau for Cubist: each doubling still buys 0.3 to 0.4 RPD and the last doubling is the largest step. plsr saturates by a quarter of the rows, but plsr is not the winning learner. So thinning the library does not dissolve the memory problem. Full-n cubist on the resamples axis: evaluate 341 s, fit 539 s; the whole curve ran in 31 min on 5 callr workers.

### Pivot (learner held to PLS on both sides)

| property | M: mbl, wapls 5-20, k by NNv | best global PLS (120 comps) | exp. 1 global Cubist, 4 cm⁻¹ | exp. 1 mbl, 4 cm⁻¹ |
|---|---|---|---|---|
| clay (RPD / RMSE) | **4.63** / 3.29 (snv_deriv1, k = 400) | 3.38 / 4.49 (snv) | 4.28 | 4.70 |
| oc | **11.27** / 1.38 (snv, k = 200) | 7.08 / 2.20 (snv, wapls) | 8.70 / 1.79 | 11.42 / 1.36 |
| pH | **3.67** / 0.338 (snv_deriv1, k = 400) | 2.75 / 0.451 (snv_deriv1) | 3.41 / 0.363 | 3.69 / 0.336 |

Global PLS was still improving at 60 components on clay and flat by 90 to 120. Coverage at the 90 % level held between 0.897 and 0.908 on every arm. Locality beats the best global PLS on every property by more than a full RPD unit, and beats the global Cubist too, so the effect is locality and not supervised dimension reduction. Full resolution buys nothing over experiment 1's 4 cm⁻¹ runs for the local model.

### Locality curve (clay, 1,000 held-out unknowns, wapls 5-20 fixed)

| k | 50 | 100 | 200 | 400 | 800 | 1,600 | 3,200 | 6,400 | 12,800 |
|---|---|---|---|---|---|---|---|---|---|
| RPD | 4.56 | 4.77 | 4.82 | 4.84 | 4.62 | 4.41 | 4.14 | 3.76 | 3.31 |
| RMSE | 3.26 | 3.12 | 3.09 | 3.08 | 3.22 | 3.37 | 3.59 | 3.95 | 4.50 |

The peak is at k = 200 to 400, about 2 % of the library, and the curve decays monotonically to the global value (3.38 at 120 components) by k = 12,800. Experiment 1's GMM library clusters, 1,600 to 3,500 rows under Cubist, sit on this curve at 4.04 to 4.09: coarse locality gives most of the gain back. Neighbour validation (NNv) was optimistic at large k.

### Coherent batch (2026-09-17): does one pool drawn around a real batch keep the per-sample gain?

`05-coherent-batch.R` (`run_coherent.sh`) and `06-mbl-sweep.R` (`run_mbl_sweep.sh`); results under `results/coherent-batch/`. Pool: oc `train_core`, 31,130 rows, at 4 cm⁻¹. Two batches, each scored on its own rows only. **moys**: 99 MOYS samples (Michigan, 10 farms, 0-10 cm, bulk C 0.44 to 3.6 %, scanned at CSU, so an instrument transfer on top of locality; replicate scans averaged, interpolated onto the pool grid). **kssl**: the within-library control, 101 oc test rows nearest a seeded sample at 40.57 N 99.69 W (Nebraska, 201 km radius, C 0.11 to 5.56 %). Nearest-pool distance, median: moys 3.4, kssl 1.1 scaled-score units. Arms: **G** experiment 1's global Cubist (loaded from its checkpoint); **B** the union of every target's k nearest pool rows, then an eight-config grid (cubist, rf, plsr × snv, snv_deriv1 on PCA, plus plsr on the full width), evaluate() picks, fit() refits; **C** the targets clustered first (k-means, silhouette, floor 30), one pool and model per cluster; **M** per-sample `resemble::mbl` on the full pool, wapls 5-20. Headline metric RMSE (% C) with bias; RPD for continuity only, the MOYS range is narrow.

| batch | arm | k | pool rows | RMSE | bias | CCC |
|---|---|---|---|---|---|---|
| moys | G global | | 31,130 | 0.354 | +0.104 | 0.877 |
| moys | B batch | 100 / 200 / 400 / 800 | 1,891 / 2,999 / 4,660 / 6,830 | 0.343 / 0.343 / 0.345 / 0.477 | -0.004 / +0.036 / +0.063 / +0.282 | 0.885 / 0.877 / 0.877 / 0.811 |
| moys | C cluster (2) | 100 / 200 / 400 / 800 | 2,174 / 3,585 / 5,850 / 9,168 | **0.321** / 0.337 / 0.344 / 0.339 | -0.003 / +0.055 / +0.081 / +0.033 | 0.894 / 0.882 / 0.879 / 0.877 |
| moys | M mbl, snv | 50 … 1,600 | 31,130 | 0.528 → 0.398 monotone | +0.34 → +0.19 | |
| moys | M mbl, snv_deriv1 | 50 … 1,600 | 31,130 | 0.482 → 0.395 monotone | +0.23 → +0.14 | |
| kssl | G global | | 31,130 | 0.120 | -0.009 | 0.993 |
| kssl | B batch | 100 / 200 / 400 / 800 | 3,426 / 5,130 / 7,336 / 9,960 | 0.118 / 0.123 / 0.117 / 0.119 | ≤ +0.02 | 0.993 |
| kssl | C cluster (collapsed to 1) | 400 | 7,336 | **0.113** | +0.002 | 0.994 |
| kssl | M mbl, snv | 50 … 1,600 | 31,130 | 0.138 (k 100) to 0.168 | ≤ +0.02 | |
| kssl | M mbl, snv_deriv1 | 50 … 1,600 | 31,130 | 0.130 (k 100, 1,600) to 0.159 | ≤ +0.02 | |

Cross-check on the kssl batch, scored on exactly its 101 rows: yesterday's pivot M (2 cm⁻¹, snv, k 200, full library) 0.159; experiment 1's D (snv_deriv1, k 400) 0.153; experiment 1's A global Cubist 0.120. So MBL's loss on this batch is the batch, not today's setup, and its errors are spread across the C range rather than in a tail.

Findings:

- **The union pool does not keep the per-sample gain on-library; it is coarse locality.** On the Nebraska batch the pool equals global (0.113 to 0.123 against 0.120), which is what the locality curve predicted for pools of thousands of rows. Clustering the targets first added nothing on either batch (C ≈ B; kssl collapsed to one cluster).
- **Per-sample MBL is not reliably better than global.** It beat global Cubist by a wide margin on the full 9,162-row oc test set (pivot, 2026-09-16) and loses to it by a third on this coherent regional batch at every k and both preprocessings. The pivot's locality gain is an average over a heterogeneous library. Distance does not predict it: this batch is close to the library.
- **Off-library, per-sample locality hurts and the pool wins.** On MOYS, MBL's RMSE and bias fall monotonically as k grows and are still falling at 1,600, the reverse of the within-library curve: a tight neighbourhood around an off-library sample is a one-sided slice of the library's edge. The derivative does not fix it. The pool with one model beats global by about 10 % RMSE and removes a +0.10 bias.
- **Pool-internal CV can choose the wrong model for the targets.** moys B at k 800 switched its winner from snv_deriv1 Cubist to snv Cubist and took a +0.28 bias; the same preprocessing that failed for MBL. CV inside the pool answers "which model predicts library rows like these", not "which predicts this batch". Off-library, honest model choice needs the user's references (`spike()`).
- **The union pool at k contains every target's k nearest by construction**, so `mbl` run on the pool is `mbl` on the library at a twentieth of the size. The selection verb is not where accuracy comes from; it is what makes both a single model and per-sample fits affordable on a laptop, with validation on the batch choosing between them.

**Evening follow-ups (same day), from existing predictions unless noted.**

- **MBL's advantage is a tail effect.** Binning yesterday's full-test-set predictions (pivot M against experiment 1's global Cubist) by property level: on oc, rows above 20 % C are 19 % of the test set and carry **92 %** of MBL's squared-error gain (RMSE 2.96 against 3.89 there); every mineral bin is a coin flip row by row (52 to 54 % MBL wins) with small RMSE differences. On clay, rows above 50 % clay (277 of 5,239) carry 86 % of the gain, and MBL is *worse* than Cubist in the 10 to 20 % band (3.88 against 3.49). pH, which has no tail, shows a small gain spread evenly. So the pivot's headline is per-sample locality rescuing organic soils and heavy clays where the global model's form fails; on mineral soils it was never much. Both of today's batches are mineral-soil batches. Global PLS on the KSSL control batch scores 0.162, with MBL (0.130 to 0.159), not with Cubist (0.120): on mineral soils the learner separates the models and locality adds little.
- **The similarity space finds soil, not instrument.** MOYS's ten nearest KSSL rows per target are Hapludalfs, Haplorthods and Glossudalfs centred on 41 N, 90 W (Great Lakes), across the instrument change. The KSSL control batch is Haplustolls and Argiustolls at 38.8 to 41.7 N, 101.3 to 97.6 W.
- **Spike, leave one farm out** (`07-spike-lofo.R`, `run_spike.sh`, `results/coherent-batch/spike-lofo.csv`; compute). Fixed k = 100 MOYS pool (1,891 rows); for each of the ten farms, the other nine farms' referenced samples (87 to 92 rows) added to the pool before the grid, against the same pool unspiked, predicting the held-out farm. Pooled over the 99 targets: unspiked 0.343 / bias -0.004; **spiked 0.319 / +0.013**; 7 of 10 farms improved. Per-farm biases are large and farm-specific in both directions (S8 +0.32, S10 -0.18 unspiked), and 88 rows among 1,891 move a Cubist fit only so far. Farm S5's 0.80 RMSE is two samples (S5-8 measured 3.60, predicted 1.93 by every model; S5-9 measured 1.03, predicted 2.25 by every model) and looks like label noise. Unweighted spiking is real but modest; the spec's "no case weights" position now has evidence against it and should be revisited with weighted or two-stage (bias-correction) spiking as the next test.

Two horizons defects surfaced by the MOYS transfer, filed as #64 (`standardize()` anchors its resampling grid at the data's own max wavenumber, so two sources never share an axis) and #65 (`predict()` on a deserialized fit fails unless the `workflows` namespace is loaded). The verb spec was revised the same evening to own the reconciliation of pool and targets.

### Where this leaves the design

- Arms G, L-fixed and L-select are dropped. Clustering the *library* is measured (experiment 1 arms B, C, E; the curve above) and loses to tight neighbourhoods for structural reasons.
- Arm M becomes a model in the configuration grid (`mbl`, resemble engine, k a tuning parameter), not a structural mode. Separate design, not yet written.
- The product problem is a user with a batch of spectra and a laptop, not maximum accuracy on a 30-core box. The design is a selection verb that draws a training set from the pool as the union of per-target k-nearest neighbourhoods and hands it to the unchanged grid: `../../specs/v1-refactor/select-training-design.md`, with `mbl-prior-art.md` and `selection-prior-art.md` beside it.
- The coherent-batch experiment ran 2026-09-17 (section above) and overturned the verb's accuracy premise while keeping its purpose: the pool is the affordable substrate, and a grid on it with `mbl` as a model, validated on the batch, is the shape. Still owed: the metric swap (Mahalanobis-on-PCA against Euclidean and a kNN-average floor), the similarity-versus-search comparison against `resemble::gesearch()` and an RS-LOCAL-style subset, and now the `spike()` validation design, since pool-internal CV cannot choose honestly for off-library targets.

Operational notes from the run: the `mbl` stage exceeds R's 2 GB serialization limit as a future global, so the dissimilarity is computed once in the parent and sliced per chunk (bit-identical to mbl's own path), and it runs on forked multicore rather than callr. Ten mbl workers at full resolution pushed MemAvailable under 15 GB; `watchdog.sh` now also reaps orphaned callr workers by PPID, because the first kill left about 53 GB of orphans behind.

---

## Why there was going to be a second experiment (written 2026-09-14)

Experiment 1 compared five modelling *structures* (global, clustered, clustered with per-cluster config selection, memory-based learning, soft assignment) on three properties, using a deliberately small configuration grid. It produced a verdict — global models ship, locality deferred — but three things about that verdict do not survive scrutiny.

**The verdict turned on a criterion that was invented, not required.** Memory-based learning beat the global model significantly on all three properties (paired bootstrap, 500 draws: clay +0.43 [0.18, 0.76], organic carbon +2.72 [1.74, 3.74], pH +0.28 [0.15, 0.40]) and lost on exactly one thing, a 60-second-per-100-samples predict ceiling written into the decision rule by the assistant with no grounding in a real requirement. **Sam's call, 2026-09-14: latency is not a constraint. Accurate models beat fast models.** With that gate removed the experiment-1 verdict is unsupported.

**The per-cluster config question was not actually answered.** Experiment 1 reported that 30 of 31 fitted clusters chose the same configuration as the global screen, and concluded that per-cluster selection buys nothing. But the grid it chose from was four configurations — two tree models × two preprocessings, all with PCA — that were nearly interchangeable. A grid with almost no variation cannot demonstrate that variation does not matter. The question is open.

**The comparison was confounded three ways.** Memory-based learning ran weighted-average PLS on supervised PLS components. Every other arm ran Cubist or random forest on unsupervised PCA components — and the PCA was *forced*, because Cubist does not finish on a full-resolution spectral matrix (issue #40). So memory-based learning's advantage could be locality, could be the learner, could be supervised versus unsupervised dimension reduction. Nothing in experiment 1 separates them.

**One correction carried forward.** Experiment 1 reported per-property artifact sizes for memory-based learning of 50, 88, and 54 MB, as if they were separate objects. They are not: all three properties selected the same preprocessing and the same 841-column feature space, and the row sets differ only by which samples have that measurement. **One library serves every property.** For a fifteen-property library that is under 100 MB total, against three to six gigabytes for fifteen independent global models. The prior-art review (`../../specs/v1-refactor/library-prior-art.md`) named artifact size the binding constraint on the whole distribute-a-trained-object design; this inverts it.

---

## The design as written on 2026-09-14 (not run)

Two crossed grids, plus one structure that does not decompose into them.

**Configuration grid:** preprocessing × feature selection × model.
**Structure grid:** global × clustered.
**Alongside:** memory-based learning, which has no configuration grid in the same sense — its analogues are the neighbourhood size and the local fit method.

### Arms

| arm | what it fits | answers |
|---|---|---|
| **G** | full configuration grid, one global model per property | configuration main effects; the best global model |
| **L-fixed** | G's winning configuration, fitted separately in each cluster | locality alone, configuration held constant |
| **L-select** | full configuration grid *within* each cluster | whether config selection should be per cluster |
| **M** | memory-based learning over a wider neighbourhood grid | locality at its most granular |
| **P** | global PLS (and global weighted-average PLS) | **the control that breaks the confound** |

Arm **P** is the pivot and must not be dropped. If global PLS lands near memory-based learning, then locality contributes little and the real finding is that supervised dimension reduction beats unsupervised for this data — which would be obtainable in a fast, small, global model. If memory-based learning still leads clearly over global PLS, locality is real.

### Cost, and why L-select is cheaper than it looks

The clustered arms partition the same rows. Eleven clusters at roughly a tenth the size each cost about what one global fit costs, and *less* for learners whose cost grows superlinearly in n. So L-select ≈ G in total compute, not 11 × G. The expensive thing is the size of the grid itself, not the clustering.

Rough shape at 80 configurations per property × 3 properties, from experiment-1 timings (Cubist + PCA on 14k × 851 ≈ 6 min per config): G is order 24 hours of single-stream compute, L-select comparable, L-fixed and M cheap. At genuine 30-way parallelism that is an overnight run. At the 8-way workaround experiment 1 had to use, it is most of a week.

---

## Prerequisite: the parallelism fixes (done 2026-09-14/15)

Experiment 1 ran 8 workers on a 30-core box through a script-level workaround, because horizons' own parallelism was broken in three independent ways (#35 nested plan, #36 serialization payload, #37 silent no-plan path). All three are fixed on `development`:

- #36 by the serialization fixes (per-worker payload 2,086.6 MB → 417.6 MB);
- #35 and #37 by the user-managed design: `evaluate()` no longer builds a plan. Register one and opt in:

```r
future::plan(future::multisession, workers = 30)
hz |> evaluate(allow_par = TRUE, output_dir = "output/run")   # parallelize_over = "auto" -> "configs"
```

The scripts must run the **installed** package (`R CMD INSTALL`), not `devtools::load_all()`: the configs axis dispatches to workers that load the installed horizons and refuses to run under pkgload. `helpers.R::require_fresh_install()` guards this. Tracking issue **#48**.

**Shakedown (2026-09-15).** `00-shakedown.R` runs the merged design once on the real library at 2 cm⁻¹ before anything here is built on it: six configs (cubist, rf, plsr × snv, snv_deriv1, all PCA), six workers, three properties, `parallelize_over = "auto"` resolving to the configs loop. It records the loop used, seconds per config, `fit()` time, fitted-object size, and memory from the watchdog and RSS logs, all under `results/shakedown/`. It touches nothing in experiment 1. Those numbers, not experiment 1's eight-worker timings, are the cost model for the grid decisions below. Launch with `run_shakedown.sh`.

Two more from experiment 1 that will bite this run specifically:

- **#38** (pruned configs unusable + scale-blind prune test) — cost 90 minutes of discarded tuning on the log-transformed property. Workaround in the experiment-1 scripts is `prune = FALSE`; the real fix is better.
- **#39** (`add_response()` silently keeps NA-outcome rows) — the splits script must keep dropping them by hand until this lands.

---

## Open decisions as of 2026-09-14 (overtaken by the pivot)

**1. The configuration grid.** Horizons offers 9 models × 7 preprocessings × 5 feature-selection methods = 315 per property, which is not sensible. The assistant's instinct is ~5 models spanning families, 4 preprocessings, 4 feature-selection methods = 80 per property. Specifically to decide:

- *Which models.* Experiment 1 used `cubist` and `rf`. `plsr` is mandatory for arm P. `elastic_net` is a cheap linear baseline. One gradient booster (`xgboost` or `lightgbm`). That is five. `svm_rbf`, `mlp`, `mars` are the candidates for a sixth.
- *Which preprocessings.* `raw`, `snv`, `snv_deriv1`, `snv_deriv2` is the natural four.
- *Which feature-selection methods.* `none`, `pca`, `correlation`, `cars` is four. **`boruta` is expensive** and its inclusion should be a deliberate choice, not a default. Note that `none` is now meaningful to include — experiment 1 forced PCA everywhere for compute reasons, so the unsupervised-reduction effect was never measured.

**2. The primary metric.** RPD misbehaved badly on organic carbon (values of 8.7 to 11.4, inflated because RPD divides by the standard deviation and `validate()` flagged 5,854 response outliers). A metric that ranks 80 configurations needs to be defensible. Candidates: RMSE (honest but scale-dependent, so it cannot be pooled across properties without normalising), CCC, or a rank-based aggregation computed per property and then combined.

**3. Whether per-cluster selection gets a middle option.** L-select as described re-screens everything per cluster. A cheaper middle exists: let the global screen fix the model family and let each cluster choose only its preprocessing. Worth including as a third locality arm, or worth skipping.

**4. Scope.** Three properties (clay, organic carbon, pH) answers the design question. A claim in the paper about when locality helps needs more properties, repeated splits rather than one, and a real tuning budget. These are different experiments; do not try to make one run serve both.

---

## What to reuse from experiment 1

Nearly all of the harness. `../2026-09-local-strategy/` has:

- `00-config.R` — paths, the KSSL source definition, the property map, split proportions.
- `helpers.R` — snapshot access, split SHAs and assertions, horizons chain builders (`build_hz`, `select_configs`, `eval_exp`), the shared conformal wrapper (`conformalize`, `conformalize_by_cluster` with pooling below 200 calibration rows), metrics rows, checkpointing, artifact sizing, and the clustering-space PCA + GMM (`clustering_matrix`, `fit_clustering`, `assign_clusters`).
- `02-splits.R` — stratified holdout with an external calibration set carved *before* clustering, which is what makes the reported intervals satisfy the Mondrian condition.
- `07-collect.R` — paired bootstrap and the decision-rule evaluator.
- `watchdog.sh` — memory guard. **Keep it.** A 20-worker plan at full resolution exhausted a 62 GB box on 2026-09-11.

The data snapshot at `/data/workshop/projects/horizons/data/kssl/snapshot/kssl_v1.2/` (45,957 samples × 1,701 wavenumbers, with `manifest.json`) is unchanged and does not need rebuilding.

Two harness bugs fixed late in experiment 1 and worth not reintroducing: `mclust` must be *attached*, not namespaced (`Mclust()` dispatches to `mclustBIC()` through the calling environment), and a bootstrap vector must not share a name with the tibble column defined above it.

---

## Decision rule

To be written **before** the run, as in experiment 1, and with the latency gate removed. Predict time and artifact size become descriptive columns reported for every arm; accuracy decides, with artifact size as the tiebreak. Coverage eligibility (90 ± 3 on the held-out set) should stay — it held across all fifteen cells in experiment 1 and is what makes the intervals comparable between arms.

---

## Overnight run (2026-09-21 → 22): is the library really unthinnable, and does the laptop really need selection?

Pre-registered before the run. Parameters live in `overnight-config.R`; the verdict tables read them, so a budget or margin can be changed tomorrow without refitting. Chain: `run_overnight.sh` runs 12, 13, 14 in order after `11-metric-followup.R` exits, each under `/usr/bin/time -v` and the memory watchdog, logs under `results/overnight-logs/`.

**Why.** Today's metric experiments (10, 11) put a global random forest ahead of every selection arm on-library (Iowa clay: 3.02 against 3.10 at best), while off-library (MOYS oc) selection beat the global forest by 30%. Two outside reads (Gemini 3.1 Pro, GPT-6 Astra, `/outside-opinion`, 18:04) agreed the earlier conclusion that the library cannot be thinned or partitioned rests on random subsampling plus one hard GMM partition, and that the memory premise under library mode was never measured sequentially. The follow-up (11) added that larger k hurts on-library and that the PLS learner is where selection's literature gains live.

**12 — the memory premise.** Sequential `configure → validate → evaluate → fit(UQ, AD) → predict` on the full clay train core (17,788 rows) with the 9/17 four-config set; peak RSS from `/proc` and `time -v`; fit and pool artifact sizes. Reading rule: peak under 16 GB means "the laptop cannot run the library" is inference, not fact, and library mode's argument becomes size and speed; over 32 GB and the premise stands.

**13 — the thinning competition.** Pool = clay train core minus the Iowa fixture. Sizes 2,000 / 6,000 / 12,000 against the full core. Selectors from the pool only: random (three seeds), Kennard-Stone and DUPLEX on the 13-component similarity space, k-means medoids, and clay-decile × spectral-cluster stratification with a per-cell floor. Learners rf and Cubist, snv + PCA, experiment-1 tuning. Scored on the fixed 5,239-row test split, the Iowa batch, and the tail bins (clay < 10, > 50). Reading rule: a subset passes when its test RMSE is within 5% of the full-core reference for that learner and every tail bin within 10%; the smallest passing size is the shippable library at that learner. Artifact sizes as qs2 recorded against the 50 MB budget. Random seeds report the spread so a diversity selector must beat random's mean, not its luckiest draw.

**14 — global plus local bias correction.** For Iowa (clay) and MOYS (oc): a global rf on the pool with out-of-fold residuals kept; each target corrected by the mean OOF residual over its 100 nearest pool rows (offset, inverse-distance weighted, and a local slope variant); scored against the uncorrected global and the best selection arm from 10. Reading rule: if correction recovers most of selection's off-library gain, selection's accuracy contribution is bias correction and the product should ship the correction with or without selection.

Not run tonight, pending these results: response-driven or soft partitions (PLS-space clusters, property strata, mixtures of experts), transfer-aware CV, applicability-domain routing, instrument standardization before selection.

### Results (read 2026-09-22)

The chain ran 19:22 to 22:44 and every script exited 0. The watchdog never fired: MemAvailable never dropped below 28 GB, peak RSS was 12.6 GB for 13 (8 callr workers), 6.1 GB for 14 and 4.9 GB for 12. The one skipped arm is by construction, DUPLEX at 12,000 needs 2k ≤ pool rows and 24,000 > 17,590. 42 thinning arms, 8 correction rows and the single full memory-premise row are all present. Results are gitignored, so the tables below are the record; the CSVs are `results/memory-premise/memory-premise.csv`, `results/thinning/{thinning,verdict,sizes}.csv` and `results/bias-correction/bias-correction.csv`.

| experiment | pre-registered rule | result | clears |
|---|---|---|---|
| 12 memory premise | peak under 16 GB: the laptop claim is inference, not fact; over 32 GB: it stands | peak RSS 4.66 GB (`/proc`), 4.88 GB (`time -v`), sequential and single-threaded; wall 3 h 22 min for four configs on clay (evaluate 2 h 13 min, fit 1 h 09 min, predict 14 s on 5,239 rows); fit artifact 227 MB, pool 53 MB | yes, by three times; the premise falls |
| 13 thinning | pass = test RMSE ≤ 1.05 × the full core for that learner and both tail bins ≤ 1.10 ×; smallest passing size ships; a diversity selector must beat random's mean | 3 of 28 cells pass, all at 12,000: cubist k-medoids 1.029 (tails 1.03 / 0.95), cubist Kennard-Stone 1.048 (0.99 / 1.04), rf k-medoids 1.042 (1.08 / 1.04); random at 12,000 fails on both learners (1.085 and 1.063, max 1.099); best at 6,000 is 1.12, at 2,000 is 1.32 | yes, at 12,000 only |
| 14 bias correction | if correction recovers most of selection's off-library gain, selection's contribution is bias correction and the product ships the correction | MOYS oc: global rf 0.526, best selection arm from 10 (Euclidean batch) 0.348, offset 0.565, weighted 0.565, slope 0.571; bias +0.35 → +0.37. Iowa clay: global 3.131, best selection (cosine batch) 3.140, offset 2.827, weighted 2.820, slope 2.960; bias −0.52 → −0.10 | no; off-library the correction recovers none of the gain and makes it worse. On-library it beats global and selection by 10 %, which the rule did not anticipate |

**12, the memory premise.** The full sequential pipeline on the 17,788-row clay core (`configure → validate → evaluate → fit(UQ, AD) → predict`, four configs, 4 cm⁻¹, ranger on one thread) peaks at 4.7 GB. Memory is not what keeps the library off a laptop. Time is: three and a half hours for one property and four configs on the box's CPU, of which evaluate is two-thirds. Artifact is the other half of the argument: the fit with UQ and AD is 227 MB, the pool it was built from is 53 MB, just over the 50 MB budget. The cost model in `select-training-design.md` should be rewritten in those terms: the verb exists so a batch does not cost a multi-hour fit per property, and so the shipped object can be the pool rather than the fit. The RAM argument is gone.

**13, thinning.** Full thinning table, test RMSE (clay %) with the ratio to the full-core reference for that learner; random rows are the mean of three seeds with the range in brackets.

| size | selector | cubist RMSE (ratio) | cubist Iowa | rf RMSE (ratio) | rf Iowa | MB |
|---|---|---|---|---|---|---|
| 17,590 | full | 3.872 (1.000) | 2.587 | 5.428 (1.000) | 3.131 | 52.6 |
| 12,000 | k-medoids | **3.983 (1.029)** | 2.664 | **5.657 (1.042)** | 3.105 | 36.1 |
| 12,000 | Kennard-Stone | **4.058 (1.048)** | 2.736 | 5.742 (1.058) | 3.240 | 36.2 |
| 12,000 | random | 4.199 (1.085) [4.124 to 4.254] | 2.579 | 5.768 (1.063) [5.740 to 5.805] | 3.229 | 36.0 |
| 12,000 | stratified | 4.389 (1.134) | 2.560 | 5.804 (1.069) | 3.193 | 36.0 |
| 6,000 | Kennard-Stone | 4.483 (1.158) | 3.357 | 6.077 (1.120) | 3.738 | 18.4 |
| 6,000 | DUPLEX | 4.567 (1.179) | 2.815 | 6.308 (1.162) | 3.764 | 18.2 |
| 6,000 | k-medoids | 4.587 (1.185) | 2.680 | 6.235 (1.149) | 3.479 | 18.2 |
| 6,000 | stratified | 4.619 (1.193) | 2.429 | 6.426 (1.184) | 3.294 | 18.2 |
| 6,000 | random | 4.676 (1.208) [4.620 to 4.786] | 2.618 | 6.304 (1.161) [6.270 to 6.374] | 3.597 | 18.2 |
| 2,000 | Kennard-Stone | 5.199 (1.343) | 3.726 | 7.149 (1.317) | 5.171 | 6.2 |
| 2,000 | stratified | 5.214 (1.347) | 2.851 | 7.327 (1.350) | 3.972 | 6.1 |
| 2,000 | DUPLEX | 5.233 (1.352) | 3.516 | 7.232 (1.332) | 5.093 | 6.2 |
| 2,000 | k-medoids | 5.299 (1.369) | 2.698 | 7.145 (1.316) | 3.977 | 6.1 |
| 2,000 | random | 5.425 (1.401) [5.345 to 5.515] | 3.045 | 7.389 (1.361) [7.276 to 7.516] | 4.000 | 6.1 |

Bold cells pass. The library thins by about a third: 12,000 k-medoids rows of 17,590 pass on both learners, at 36 MB against the 50 MB budget where the full core is 53 MB. Nothing at 6,000 or 2,000 is within reach of the margin with any selector, which is the learning curve of 9/16 again (random 8,894 rows scored 4.35 there; random 12,000 scores 4.20 here). The diversity selectors do beat random, and by more than random's spread: k-medoids is 0.22 RMSE under random's mean at 12,000 for Cubist against a random range of 0.13, so the selector effect is real but it is worth a few percent, not a factor. Stratification by clay decile is the worst selector at 12,000 and no better than random below it. Kennard-Stone and DUPLEX have the worst Iowa and high-tail errors at 2,000, the coverage-by-extremes design pulling the fit toward the hull. The Iowa column is not gated and is noisy at 300 rows: random 12,000 beats the full core there under Cubist. Timing: the 12,000 k-medoids Cubist arm took 538 s against 852 s for the full core. Caveats before building on the pass: one property, one split, and one seed for every selector except random.

**14, bias correction.** Off-library the hypothesis is rejected outright. On MOYS the global random forest on the 31,130-row oc pool scores 0.526 with a +0.35 bias, and every correction variant makes it worse (0.565 to 0.571) while moving the bias the wrong way, to +0.37. Selection's 0.348 is untouched. The mechanism is legible from the numbers: the pool's own out-of-fold residuals in MOYS's neighbourhood carry the wrong sign for an instrument transfer, because a library-internal misfit and an off-library offset are different quantities, and only references from the batch can see the second one. That is the `spike()` argument from 9/17 restated from the other side. On-library the picture inverts. On the Iowa fixture, offset and inverse-distance corrections take the global forest from 3.131 to 2.82 (10 % RMSE, bias −0.52 → −0.10), and beat the best selection arm from 10 (cosine batch, 3.140) that had been the on-library argument for cosine. The local-slope variant is weaker (2.960) with no fallbacks triggered. Two notes on the setup: the correction's neighbours are drawn from the 80 % of pool rows that `fit()` cross-validated, because `fit()` holds the other 20 % out as its own test split (Split F; UQ and AD were off, or a further 20 % would have gone to calibration), so this is consistent across blocks but not the whole pool; and the on-library result is one fixture of 300 rows, not the 5,239-row test split, so it is a finding to test, not a product decision.

**What this settles, and what it moves.**

- The library can be thinned, by a third and no further, with k-medoids. The packaged-pool design reopens in exactly that form, a two-thirds core under the size budget, and not as a small pool.
- The laptop premise was wrong on memory and right on cost. The verb's cost model becomes time per batch and the size of the shipped object.
- Off-library, selection's gain is not bias correction; it stands as locality plus, when references exist, spiking. On-library, selection is dominated by global plus local residual correction. The morning read of this was that it removed cosine's only win and left Euclidean, which won off-library (0.348 against cosine 0.372), as the natural single default; the afternoon's 15 (below) overturned that, because without a global fit the on-library base is the pool and there cosine is worth 11 %. The `sdev_floor` question is untouched: no PLSR arm ran.
- Owed before any of it becomes design: the on-library correction on the full clay test split and the other two properties. The k-medoids replication is not owed: Sam's call the same afternoon (below) was that the library ships whole, which closes thinning as a design input.

### Same afternoon (2026-09-22): the library ships whole, and the correction from the pool fit

Sam's call after reading the results: the library always ships in full; the only question is what enters the model per batch, optimized for the batch's accuracy and the fit's runtime. That closes thinning as a design question (the 12,000 k-medoids pass stands as a finding nothing hangs on), drops the 50 MB budget as a library budget, and makes the residual correction's dependence on a global fit the one open coupling: 14's on-library gain came from a global fit's out-of-fold residuals, and under this design there is no global fit at predict time.

**15 — the correction from the pool fit** (`15-pool-correction.R`, `run_pool_correction.sh`, `results/pool-correction/`). Pool as 10 built it (every snapshot row but the fixture, the property attached). `select_training()` at k = 100 and 400, scope batch; the one rf config on the drawn pool; fit()'s out-of-fold residuals on the pool rows it cross-validated (80 %, as in 14: the rest is fit()'s held-out Split F); 14's three corrections from each target's 100 nearest OOF rows, Euclidean. Euclidean draws on both blocks, then a cosine draw on Iowa at k = 100 because the Euclidean result reopened the metric. 16 minutes for all five draws at 8 workers, peak RSS 6.3 GB. Every k = 100 pool fit reproduces 10's arm to three decimals (3.541, 3.140, 0.348), so the harness is sound. RMSE:

| block | draw | pool rows | pool fit | + offset | + weighted | + slope | bars from 10 and 14 |
|---|---|---|---|---|---|---|---|
| Iowa clay | Euclidean k 100 | 6,606 | 3.541 | 3.177 | 3.165 | 3.297 | global 3.131; global + offset 2.827; best selection (cosine) 3.140 |
| Iowa clay | Euclidean k 400 | 11,290 | 3.875 | 3.453 | 3.445 | 3.553 | |
| Iowa clay | **cosine k 100** | 7,027 | 3.140 | **2.827** | **2.817** | 2.904 | |
| MOYS oc | Euclidean k 100 | 2,627 | **0.348** | 0.403 | 0.402 | 0.402 | global 0.526; global + offset 0.565; best selection 0.348 |
| MOYS oc | Euclidean k 400 | 6,244 | **0.329** | 0.376 | 0.376 | 0.375 | |

Findings:

- **The correction's sign is a property of the batch, not of the fit that produced the residuals.** From a pool fit it is a 10 to 11 % gain on-library at every draw and a 14 to 16 % loss off-library at every draw, the same as from the global fit in 14. The nearest-pool distance separates the two batches (1.1 against 3.4 scaled-score units on 9/17), so the correction can be gated by the applicability signal the verb already computes.
- **On-library the product path reaches the global-fit number, but only with cosine.** The cosine pool at k = 100 plus the offset correction is 2.827, identical to 14's global fit plus correction, from a fit on 7,027 rows instead of 17,590. The Euclidean pool starts 13 % worse (3.541 against 3.140) and the correction closes a proportional slice of whatever it is handed, so it lands at 3.17.
- **The metric split is real and the correction does not dissolve it.** Cosine on-library (3.14 against 3.54), Euclidean off-library (0.348 against 0.372 in 10). The 9/22 morning recommendation of Euclidean as a single default rested on the on-library path being served by a global fit; without one, cosine is worth 11 % on-library and Euclidean 7 % off-library. Since the same distance that gates the correction separates the regimes, routing the metric by it is the natural design; two batches is thin evidence for the threshold.
- **k stays small on-library and is unsettled off-library.** 400 was worse than 100 on Iowa again (3.875 against 3.541, bias −0.41). On MOYS 400 edged 100 (0.329 against 0.348), against 9/17's flat curve under Cubist. One batch each.

**What this settles for the design** (recorded in `../../specs/v1-refactor/select-training-design.md` the same day): the library ships whole and `build_library()` is not a user verb; `select_training()` takes one routing argument, `library =`, resolving a registered name, a path or a `horizons_data`; the residual correction is a designed step gated by the applicability signal; the metric defaults are cosine on-library and Euclidean off-library, routed by the same signal, pending more batches.
