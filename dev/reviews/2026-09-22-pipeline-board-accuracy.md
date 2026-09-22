# Pipeline board accuracy review, 2026-09-22

Three fresh-context reviewers (Claude Opus, no conversation context) each took a slice of `vignettes/board/content.yml` and verified every claim against the source at its pointer, with the S3-contract and leakage-lifecycle lenses from `.claude/commands/horizons-review/` applied where they bite. Slices: A `spectra`, `standardize`, `average`, `add_response`, `select_training`; B `configure`, `validate`, `evaluate`; C `fit`, `ensemble`, both `predict` methods, and the two divergence blocks.

| slice | claims checked | findings | wording fixed | code defects surfaced |
|---|---|---|---|---|
| A | 153 | 11 | 11 | 2 |
| B | 94 | 7 | 7 | 3 |
| C | 105 | 8 | 8 | 2 |

Every wording finding was applied to `content.yml` the same afternoon, the board regenerated, and `tests/testthat/test-board.R` passes (299 expectations). The defects below are recorded on the relevant cards as notes until fixed. Filed the same afternoon as #67 to #72; defect 5 was already open as #62.

## Code defects the review surfaced

1. **`fit()` splits the unfiltered table.** `evaluate()` drops NA-outcome rows into a local frame and never writes it back; `fit()` then draws Split F from `x$data$analysis` unfiltered (`R/pipeline-fit.R:155,170`). NA-outcome rows reach the fit, and the coincidence guard compares `in_id` vectors over two different frames, so the warning is meaningless. Bites whenever #39 does.
2. **`response_bound` is taken over every row of the object** (`R/pipeline-fit.R:619`), Split F's test rows and NA-outcome rows included, while the roxygen says "max training outcome". A small leak into a deploy-time guardrail.
3. **`cov_fusion = "late"` is unimplemented.** The value is validated, stored and printed, and nothing reads it; `build_recipe()` does early fusion only (`R/utils-recipes.R:8,278`). `"late"` is bit-identical to `"early"`.
4. **Re-configuring after `ensemble()` aborts.** `configure()` clears a fixed key list (`R/pipeline-configure.R:306-326`) that includes a non-existent `ensemble$stack` and misses `ensemble$model`; `promoted_state()` (`R/utils-subset.R:53`) then treats the object as promoted and `set_analysis()` refuses it. Also leaves `models$uq`, `models$ad`, `models$results` and others in place, so `has_uq()` stays TRUE on the demoted object.
5. **`config$defaults` is not in force.** It records a PCA threshold of 0.99 and a Savitzky-Golay window of 11; `build_recipe()` hardcodes 0.995 (`R/utils-recipes.R:217`) and `step_transform_spectra()` defaults to 9. The three per-configuration override list-columns are never read.
6. **Two constructors.** `spectra()` builds through `create_horizons_data()` (`R/pipeline-spectra.R:1060`), which omits `data$n_responses`, carries pre-contract `models` and `ensemble` stubs, and writes `models$uq` as a non-empty list, so `has_uq()` (`R/class-core.R:1883`) returns TRUE on a raw object. `new_horizons_data()` in class-core is the contract.
7. **The twin rule's reference width differs by scope.** `draw_neighbours()` uses the 50 nearest measured rows capped at a quarter of the measured rows (`R/select-neighbours.R:469`); the `scope = "global"` branch uses `max(k, 50)` uncapped (`R/pipeline-select-training.R:519`), 400 at the default. The control arm runs a looser twin rule than the arms it is compared with, and the in-code comment says otherwise.

Lesser, recorded on the cards: `validation$passed` is advisory (no verb reads it); `select_training()` does not refuse a merely configured pool; the `twin_ratio` roxygen describes the retired rule; the `prune` advice for transformed outcomes was stale by one release (#49 fixed it); `fit()`'s n_best cap and calibration disable are tree lines rather than warnings, so `verbose = FALSE` silences them.

## What the review confirmed

The `models$` and `ensemble$` field names and every class vector; the shared UQ/AD calibration split and its 30-row joint disable; the degradation rule; signed CQR scores and the Ledoit-Wolf AD with held-out thresholds, both pre-butcher; back-transform once, clamp once, floor at zero on the single-model path and unclamped members with one guardrail at the ensemble output; CV+ as order-statistic aggregation at `seed + 1000`; the reconciliation rules (hole, half-spacing clamp, finer-targets warning); the union subtraction and `retained = FALSE` bookkeeping; the per-property draw on measured rows; every evidence number against the factorial README and the select-training spec.
