# Experiment 2 — the configuration × structure factorial

**Status:** designed, not built. Written 2026-09-14 to be picked up in a later session.
**Predecessor:** `../2026-09-local-strategy/` — read its README and `results/verdict.md` first.

---

## Why there is a second experiment

Experiment 1 compared five modelling *structures* (global, clustered, clustered with per-cluster config selection, memory-based learning, soft assignment) on three properties, using a deliberately small configuration grid. It produced a verdict — global models ship, locality deferred — but three things about that verdict do not survive scrutiny.

**The verdict turned on a criterion that was invented, not required.** Memory-based learning beat the global model significantly on all three properties (paired bootstrap, 500 draws: clay +0.43 [0.18, 0.76], organic carbon +2.72 [1.74, 3.74], pH +0.28 [0.15, 0.40]) and lost on exactly one thing, a 60-second-per-100-samples predict ceiling written into the decision rule by the assistant with no grounding in a real requirement. **Sam's call, 2026-09-14: latency is not a constraint. Accurate models beat fast models.** With that gate removed the experiment-1 verdict is unsupported.

**The per-cluster config question was not actually answered.** Experiment 1 reported that 30 of 31 fitted clusters chose the same configuration as the global screen, and concluded that per-cluster selection buys nothing. But the grid it chose from was four configurations — two tree models × two preprocessings, all with PCA — that were nearly interchangeable. A grid with almost no variation cannot demonstrate that variation does not matter. The question is open.

**The comparison was confounded three ways.** Memory-based learning ran weighted-average PLS on supervised PLS components. Every other arm ran Cubist or random forest on unsupervised PCA components — and the PCA was *forced*, because Cubist does not finish on a full-resolution spectral matrix (issue #40). So memory-based learning's advantage could be locality, could be the learner, could be supervised versus unsupervised dimension reduction. Nothing in experiment 1 separates them.

**One correction carried forward.** Experiment 1 reported per-property artifact sizes for memory-based learning of 50, 88, and 54 MB, as if they were separate objects. They are not: all three properties selected the same preprocessing and the same 841-column feature space, and the row sets differ only by which samples have that measurement. **One library serves every property.** For a fifteen-property library that is under 100 MB total, against three to six gigabytes for fifteen independent global models. The prior-art review (`../../specs/v1-refactor/library-prior-art.md`) named artifact size the binding constraint on the whole distribute-a-trained-object design; this inverts it.

---

## The design

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

## Prerequisite: fix the parallelism bugs first

Experiment 1 ran 8 workers on a 30-core box through a script-level workaround, because horizons' own parallelism is broken in three independent ways:

- **#35** — the nested plan (`workers > cv_folds`) trips `parallelly`'s localhost limit; every config dies in ~15 s.
- **#36** — the parallel branch exports ~6 copies of the training table; >2 GB of globals kills the socket transfer above ~3-4k rows.
- **#37** — at or below `cv_folds`, no `future::plan()` is ever registered, so the documented inner parallelism silently does not exist and the run is single-threaded.

Together: **no value of `workers` gives working parallelism at library scale.** Fixing them is maybe half a day and is the difference between this experiment being an overnight run and a multi-day one. Do it first. Tracking issue **#48**.

Two more from experiment 1 that will bite this run specifically:

- **#38** (pruned configs unusable + scale-blind prune test) — cost 90 minutes of discarded tuning on the log-transformed property. Workaround in the experiment-1 scripts is `prune = FALSE`; the real fix is better.
- **#39** (`add_response()` silently keeps NA-outcome rows) — the splits script must keep dropping them by hand until this lands.

---

## Open decisions — Sam's calls before this can be built

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
