# 16 — The threshold experiment: does one distance route the correction, the metric and k?

**Status:** design draft, 2026-09-29. Internally reviewed, and the review put the premise in question (last section). Held there pending Sam's call on scope; not yet revised; nothing has run. It was written before the script so that building it section by section would expose what is wrong with it.
**Script (not yet written):** `16-threshold.R`, launched by `run_threshold.sh`. **Results:** `results/threshold/` (gitignored, so the tables below become the record).
**Predecessors:** `14-bias-correction.R` and `15-pool-correction.R`, read in the README sections of 2026-09-21 and 22. **The design it serves:** `select-training-design.md` (horizons `dev/specs/v1-refactor/`), findings 5, 7 and 8 and the open questions on the correction's threshold and the metric threshold.

---

## Objective

Three defaults of `select_training()` currently rest on two batches. On the Iowa clay fixture (on-library), correcting each target by the mean out-of-fold residual of its 100 nearest pool rows took the cosine pool fit from 3.140 to 2.827, and cosine beat Euclidean by 11 %. On MOYS (off-library, a different instrument), the same correction made the Euclidean pool fit worse, 0.348 to 0.403, Euclidean beat cosine by 7 %, and k = 400 edged k = 100 where on Iowa it lost. The design spec turned this into two paths, both switched by one signal, the batch's distance to the library:

- **near:** cosine draw at small k, fit, residual correction
- **far:** Euclidean draw, fit, no correction (and `spike()` when references exist)

The threshold that separates them is unset. Two batches, one on each side, cannot place it.

The objective is to measure, on enough batches spread along the distance axis, three things. First, whether each decision (correct or not, cosine or Euclidean, small or large k) changes sign monotonically with a batch-level distance. Second, whether the correction and the metric change sign at the same distance, so that one threshold routes both. Third, whether a threshold placed on batches from inside the library, which share KSSL's instrument and differ only in how well the library covers them, sorts batches from another instrument correctly. The useful outcome is either a threshold with an interval around it, or a clear finding that distance does not route one or more of the decisions. The second is as much a design input as the first.

Out of scope: `spike()`, the PLS space, `sdev_floor`, model choice (one configuration throughout), and per-target routing as a product feature (analysed as a secondary question only).

## Hypotheses

For each batch *b* with distance signal *d_b* (defined under Methods), four contrasts, each a log RMSE ratio on the batch's own targets. Negative means the first arm wins.

| contrast | definition | what it decides |
|---|---|---|
| *C_b* | log(RMSE pool fit + offset / RMSE pool fit), k = 100 | the correction gate |
| *M_b* | log(RMSE cosine pool / RMSE Euclidean pool), k = 100, uncorrected | the metric |
| *K_b* | log(RMSE k = 400 / RMSE k = 100), Euclidean, uncorrected | k |
| *P_b* | log(RMSE near path / RMSE far path): cosine k = 100 + offset against Euclidean k = 100 uncorrected | the routing decision the product actually makes |

Priors from 15, measured before #81, #67 and #72 and so directional only: Iowa *C* −0.105 (cosine pool), *M* −0.120, *K* +0.090, *P* −0.225; MOYS *C* +0.146 (Euclidean pool), *M* +0.067 (cosine from 10), *K* −0.056, *P* not measured.

- **H1, the correction gate.** *C_b* rises with *d_b* and crosses zero: the correction helps near batches and hurts far ones.
- **H2, the metric.** *M_b* rises with *d_b* and crosses zero: cosine wins near, Euclidean far.
- **H3, one threshold.** The zero crossings of *C* and *M* fall at the same distance, within their intervals. If H1 and H2 hold with separate crossings, the product needs three regimes, not two.
- **H4, k.** Below the threshold *K_b* is positive (k = 100 wins); above it *K_b* is zero or negative.
- **H5, transfer across instruments.** A threshold placed on KSSL batches alone classifies the external batches correctly on *P_b*. If it fails, the sign of the correction off-library is driven by the instrument rather than by coverage, and a score-space distance cannot gate it.

The alternative each rule guards against is that the effects are idiosyncratic to the batch: two batches at the same distance, opposite signs.

## Methods

### Fixed across every arm

| setting | value | source |
|---|---|---|
| package | horizons at 77e69a1, reinstalled with `R CMD INSTALL` before the run | the 9/24 merges |
| library | `kssl_v1.2` snapshot: 45,957 rows, upper depth < 30 cm, one instrument (Bruker Vertex 70, HTS-XT) | manifest |
| grid, mask | 4 cm⁻¹; 1580–1720 and 3100–3700 cm⁻¹ masked | `overnight-config.R` |
| space | PCA to 99 %, `sdev_floor` 0.10, `space_rows = "all"` | as shipped |
| `twin_ratio` | the shipped constant, uncalibrated; exclusions recorded per batch | as shipped |
| draw | `scope = "batch"`, `properties` = the batch's property | 15 |
| configuration | rf, snv, pca; experiment 1's tuning (5-fold CV, grid 5, no Bayesian iterations) | 10, 14, 15 |
| chain | `configure → evaluate → fit(n_best = 1, UQ and AD off)`, seed 307 | 15's `pool_fit()` |
| correction | each target's 100 nearest out-of-fold rows (evaluate()'s training partition, about 80 % of the pool), Euclidean, in a space fitted on those rows; offset, inverse-distance weighted, local slope | 15's steps 3–5, unchanged |

### The distance signal

**Primary:** *d_b* is the median over the batch's targets of *r_i*, target *i*'s nearest-pool distance divided by the pool's own median nearest-neighbour distance. Both distances are Mahalanobis (Euclidean on sdev-scaled scores) in the all-rows space. The pool reference is the one `check_resemblance()` already draws (2,000 rows, seed 1). It comes from one extra `select_training(metric = "mahalanobis")` call per batch, whose drawn rows are discarded. Raw distances are recorded beside it.

Three reasons for this definition. The signal cannot depend on the metric it routes, so it is computed in one fixed metric regardless of the draw. The raw scaled-score distance is not stable: it grows with the number of retained components, which ran from 12 to 15 across 15's pools. And the 9/17 logs show MOYS at 3.39 and at 2.91 and the KSSL control batch at 2.05 and at 1.08, from runs hours apart on the same day. The spec's "1.1 against 3.4" pairs numbers from two different runs, in a 33-component space the verb no longer builds, so neither number is a prior for this run. A ratio to the library's own neighbour spacing is the scale the resemblance check already uses, and it is the form that would carry to a different library.

**Secondary signals (reported, not used for any decision):**

- *s1*, the fraction of targets beyond the verb's resemblance threshold (the 99th percentile of the pool's nearest-neighbour distances). This is the warning users already see.
- *s2*, the projection residual: the part of each target's preprocessed spectrum that the retained components do not reconstruct, as a median ratio to the pool's own residuals. The space keeps `center` and `rotation`, so this is a helper in the script, not a package change. It is the one signal that could see an instrument shift lying outside the library's components, which a score distance cannot.

If a secondary signal orders the batches better than the primary, that is a finding for a follow-up, not a basis for the default.

### Batches

Four families. The first three are drawn from KSSL and calibrate the threshold. The fourth is external and only tests it.

**1. Anchors.** Iowa (the 300 layers with point coordinates nearest Ames) and MOYS (99 samples, replicate scans averaged), exactly as 15 built them.

- **Continuity cells:** each is run on its original property (Iowa clay, MOYS oc) in 15's five cells. This is the re-baseline the pre-#81 numbers need: Table 4 gives the size of the shift at 77e69a1 before anything else is read.
- **Primary analysis:** both enter on the primary property, with all four draws. Iowa counts as a calibration batch. MOYS is external.

**2. Regional KSSL batches.** A batch is the 150 layers from the whole sites nearest a centre. A site is `id.dataset.site_ascii_txt`, and all of its layers stay together. Location is the point coordinate where one exists (18,923 rows) and the county centroid otherwise (a further 25,181 rows). The pool is the snapshot minus every row sharing a site with a target. Candidate centres, fixed before the scan:

| | centre | lat, lon | | centre | lat, lon |
|---|---|---|---|---|---|
| 1 | eastern Nebraska | 40.8, −96.7 | 9 | Palouse | 46.7, −117.0 |
| 2 | western Kansas | 38.9, −100.9 | 10 | central California | 36.7, −119.8 |
| 3 | North Dakota | 47.0, −100.0 | 11 | Arizona | 32.9, −111.5 |
| 4 | Mississippi Delta | 33.4, −90.9 | 12 | Colorado Front Range | 40.5, −105.0 |
| 5 | Georgia Piedmont | 34.0, −83.4 | 13 | interior Alaska | 64.8, −147.7 |
| 6 | Florida peninsula | 28.5, −81.8 | 14 | Hawaii | 19.7, −155.5 |
| 7 | New England | 43.5, −72.0 | 15 | Puerto Rico | 18.2, −66.5 |
| 8 | southern Michigan | 42.7, −84.5 | 16 | central Ohio | 40.0, −83.0 |
| | | | 17 | central South Dakota | 44.4, −100.3 |

Centres 8, 16 and 17 sit where MOYS, AONR and FFAR were sampled. They are always run, as region-matched controls: an external batch and its KSSL twin from the same region differ mainly in instrument and lab. The scan computes *d_b* for all 17 centres, about 15 s each with no fitting. The run then takes the three matched controls plus seven chosen to spread the range, with at least two batches in every quarter of the scanned range. Selection reads *d_b* only, never an outcome, so choosing after the scan does not bias the contrasts. The full scan, including unselected centres, is recorded. Hawaii (about 290 rows) and Puerto Rico (about 160) will be close to exhausted by a 150-layer batch, which is the point: little coverage is left behind.

**3. Buffer series.** On two near batches (Iowa and one other), the pool also loses every row located within *R* of the batch centroid, for *R* in {0, 250, 750} km. The targets are held fixed and only coverage changes, so a buffer series is the dose response of coverage with the soils held constant. The buffer levels share targets, so they are not independent batches. Only *R* = 0 enters the threshold analysis, and the series are read as trajectories. 1,853 rows (4 %) have neither a point nor a county coordinate and cannot be excluded by a buffer. They are dropped from every pool in the experiment, so pools differ between arms only by what the design removes.

**4. External batches (test set, never used to place the threshold).**

| set | where | n | property measured | readiness |
|---|---|---|---|---|
| MOYS | Michigan cropland, 0–10 cm | 99 | bulk (total) C | loader exists (15) |
| AONR | Ohio N-rate trial | 628 → 150, seeded, whole samples | bulk C | OPUS + CSV, MOYS's loader pattern |
| FFAR | South Dakota grazing lands | 431 → 150, top depth only if the suffix resolves | bulk C | OPUS + CSV; depth suffix unverified |
| JRC2 (LUCAS 2018) | EU | 368 | OC back-calculated from SOC stock and bulk density; carbonate flagged | averaged CSV built; truth is a proxy |

MOYS and JRC2 were scanned at CSU, and the others probably were. That has to be verified from the OPUS headers before the run, because the design reads them as one outside instrument. Not used: Syngenta's Cerrado set (Sam's call, 2026-09-29: it is in Brazil), CQuester IB (client data), NRCS.csv (its IDs look like KSSL pedons, and the spectra are not on the box), NEON (spectra not on the box), pyom-mir (char-spiked blends), and several sets too small to score.

### Property

**Primary: total C everywhere.** KSSL `total_carbon` (45,900 rows) is paired with the bulk C the external sets measured, log-transformed as 15 did for oc. That puts one property across the calibration and test sets, and it is the property the external labs actually reported. 15 scored MOYS bulk C against KSSL oc, a mismatch that is small in Michigan's non-calcareous soils but not zero, and that grows wherever carbonate is present (7,330 KSSL rows carry more than 0.5 % carbonate). JRC2 measures organic C, so it runs on KSSL oc and is reported apart from the primary test set. The helpers' `property_rows()` has no physical-range spec for `total_carbon` yet; it needs one.

**Replication: clay on every KSSL batch.** Under `space_rows = "all"` the similarity space is built on every pool row, so *d_b* is the same for clay and for total C. Only the drawable rows and the response change. Clay therefore tests whether the threshold belongs to the batch or to the batch-and-property. It has no external test, because no external set has lab-measured clay.

### Arms per batch

Four pool fits: metric {cosine, Euclidean} × draw k {100, 400}. Each is scored uncorrected and with the three corrections, which cost nothing extra. The signal draw adds one `select_training()` call with no fit.

**Seed replicate.** Iowa and MOYS on the primary property, k = 100, both metrics, at seed 308: four fits, to measure how much a contrast moves between fits that differ only in seed.

**Cost**, from 15's timings at 8 workers (a 2,627-row pool took 49 s to evaluate and fit; 6,244 rows took 124 s; 11,290 rows took 322 s): a 150-target batch is about 9 minutes for its four fits. Primary property, about 20 batch units including buffers and externals: about 3 h. Clay replication: about 1.7 h. Continuity cells and seed replicate: about 25 min. Overnight in all; the primary pass first.

### Scoring and uncertainty

For each cell: RMSE, bias, CCC and RPD on the batch's own targets, plus the truth's sd and range. Each contrast gets a paired bootstrap 95 % CI (2,000 draws, seed 307). Resampling is by site for KSSL batches and by farm or field for external ones, because layers from one site are not independent. The CI covers target sampling, not fit noise. If the seed replicate moves a contrast by more than half the median CI width, the margin δ below grows by that amount, fixed before the calibration batches are read.

### Decision rules (pre-registered)

**Classifying a batch on a contrast**, with margin δ = log(1.02):

- **for** (the first arm wins): the CI lies entirely below zero and the estimate is below −δ
- **against:** the CI lies entirely above zero and the estimate is above +δ
- **neutral:** otherwise

**Calibration set:** the KSSL batches, Iowa included, on the primary property.

- **H1** holds if both of these hold. First, the Spearman correlation of *d_b* with *C_b* is positive at one-sided p < 0.05. Second, "for" and "against" batches separate by distance with at most one misordered batch, named. The threshold interval runs from the largest *d_b* among "for" to the smallest among "against", and *d\*_C* is its midpoint. *C* is judged in the cosine pool, where the correction would live; the Euclidean version is reported. If no KSSL batch is "against", the correction never hurts inside the library. That is a result: the gate is then about instruments, not coverage, and H5 carries the question.
- **H2:** the same rule on *M_b*, giving *d\*_M*.
- **H3** holds if the two intervals overlap. The shared threshold is the overlap's midpoint.
- **H4** holds if the median *K_b* is positive below the threshold and zero or negative above it, with at least two classified batches above.
- **H5:** apply the threshold to the external batches. H5 holds if every external batch classified on *P_b* falls on the predicted side. A single miss is reported as a miss, not waved through.

**Adaptive second round (committed 2026-09-29).** After the primary pass, three or four more KSSL batches are run whose *d_b* falls inside the widest of the threshold intervals from H1 and H2. They come from the unselected scan centres, or from new centres scanned the same way, and are chosen on *d_b* alone. They get the same arms, and the thresholds are re-estimated on the union. The first-pass estimates are reported beside the final ones.

**What each outcome does to the design:**

| outcome | default that goes into the spec |
|---|---|
| H1–H3 and H5 hold | two-path routing at the shared threshold |
| H1–H3 hold, H5 fails | routing for KSSL-instrument batches only; other instruments take the far path, and *s2* becomes the next experiment |
| H1 or H2 fails | one default, Euclidean without correction (the spec's stated fallback); the distance is still reported to users |
| H1 and H2 hold, H3 fails | three regimes, with both thresholds in the spec |
| H4 | k set per side of the threshold, or left at 100 if H4 fails |

### Secondary analysis: per target

For each target: did the offset move it closer to the truth, and how does that depend on its own *r_i*? Answered with a logistic regression on log *r_i* with batch fixed effects, over the calibration batches. It says whether a per-target gate would add anything to the per-batch one. It decides nothing.

### Operation

On the box: 8 `future.callr` workers under the watchdog and `/usr/bin/time -v`. Free space and `/tmp` Rtmp directories are checked before launch (the 9/21 failure). Every cell is checkpointed so the run resumes. A `--dry` pilot comes first (4,000-row pool, 40 targets, two batches). Outputs in `results/threshold/`:

- `scan.csv`: every candidate centre, *d_b*, raw distance, *s1*, *s2*, selected or not
- `signal.csv`: one row per run batch
- `threshold.csv`: one row per batch × property × metric × k × method
- `preds/` and `checkpoints/`

## Results

Not run. The shells below are what the run fills in, in this order.

**Table 1, the scan.** All 17 centres: n, radius spanned, *d_b*, raw distance, *s1*, *s2*, selected.

**Table 2, per batch** (calibration then external). Family, n, truth sd, *d_b*, and each of *C_b* (cosine and Euclidean pools), *M_b*, *K_b* and *P_b* with its CI and class.

**Table 3, verdicts.**

| hypothesis | rule | result | holds |
|---|---|---|---|
| H1 correction gate | | | |
| H2 metric | | | |
| H3 one threshold | | | |
| H4 k | | | |
| H5 transfer | | | |

**Table 4, re-baseline.** 15's five cells against the same cells at 77e69a1.

**Figure 1.** *C*, *M*, *K* and *P* against *d_b*, calibration batches filled and external ones open, each external set joined to its region-matched KSSL twin, with the threshold intervals shaded.

**Figure 2.** The buffer trajectories.

## Weaknesses found in writing this, and calls for review

1. **The external test set may not span the distance axis.** MOYS, AONR and FFAR are US Midwest and Plains soils on one instrument, and their *d_b* may cluster near MOYS's. H5 would then test the threshold at a single point. JRC2 is the only set that would spread it, and its truth is a back-calculated proxy on a different property (oc).
2. **One outside instrument.** A pass on H5 is a pass for CSU's instrument, not for "other instruments". The region-matched twins isolate that one instrument's effect cleanly, but they cannot generalize it.
3. **Ten or so calibration batches place a threshold only coarsely.** Under the midpoint rule the interval is as wide as the gap between neighbouring batches, and where the sign changes is unknown until the run. The adaptive second round (Decision rules) narrows the gap. It is committed to now, so the decision to run it does not depend on the results.
4. **One configuration.** The correction and the metric may interact with the learner; Cubist won off-library on 9/17. This is stated scope, not fixed.
5. **Batch composition varies.** 150 layers around a centre are geographically coherent but differ in depth mix and C range. RMSE ratios can move with the truth's spread, which is reported but not controlled.
6. **Not quite the library as shipped.** Dropping the 1,853 unlocatable rows means no arm uses the whole library. 4 %, but a deviation from "the library ships whole".
7. **The anchors change property.** Iowa and MOYS enter the primary analysis on total C, not on 15's clay and oc. The continuity cells are the only bridge back to 15.
8. **Whether 150 targets is enough is unknown.** The pilot will show the CI widths. If they approach the roughly 10 % effects seen in 15, the batches need to be larger, and that changes the cost.
9. **`twin_ratio`** should seldom fire once whole sites are excluded. Exclusions are recorded per batch, and any batch where the rule fires on more than a handful of targets gets looked at before its contrasts are read.

**Calls, settled with Sam on 2026-09-29:**

- Syngenta is out: it is in Brazil.
- Everything else is as written:
  - total C is the primary property
  - the clay replication runs
  - the adaptive second round is committed
  - the unlocatable rows are dropped from every pool
  - CQuester IB is not used


## Internal review, 2026-09-29: the premise is in question

Two cold reviewers read the draft above, one on statistical design and one on spectroscopy and leakage. Between them they raised eight blocking findings. Six are design repairs. Two question whether the experiment should run in this form at all, and a check made while merging the reviews sharpens the second.

**The external batches are not far.** The spectroscopy reviewer rebuilt the shipped space outside the verb: 4 cm⁻¹, SNV, first derivative, the mask, 13 components after the floor. In that space MOYS, FFAR and AONR sit in the middle of the KSSL candidate batches (*d_b* 1.57, 1.63 and 1.33, against 0.43 to 2.54). Their projection residual is no larger than the library's own. MOYS stays mid-range in the unfloored 33-component space too (1.78). The rebuild is approximate, so its second decimal is uncertain, but the ordering is what matters. All four external sets were scanned on one INVENIO-R (serial 667), so "one outside instrument" is now verified.

**MOYS's truth is a fraction sum, not a bulk measurement.** In `MOYS.csv`, `Bulk_C_g_kg` equals POM + CHAOM + MAOM C to within 0.01 g/kg on 100 of 100 rows. The same holds for FFAR on 91 % of 432. AONR, from the same lab, has both a direct bulk C and fraction C on 62 samples, and there the fractions recover a median 83.1 % of direct bulk C (IQR 79.6 to 85.6 %). A KSSL-trained model predicts on KSSL's direct-measurement scale, so against MOYS it should over-predict by about a fifth. The observed pool bias was +0.18 % C. Re-scoring the existing MOYS predictions from 10 and 15 against truth ÷ 0.831:

| quantity | as recorded | truth ÷ 0.831 |
|---|---|---|
| correction, *C* (Euclidean pool, k = 100) | +0.146 | +0.014 |
| metric, *M* (cosine / Euclidean) | +0.066 | +0.024 |
| selection against global, log RMSE ratio | −0.350 | −0.110 |
| global rf bias | +0.32 | +0.03 |

This is a sensitivity check on results already seen, and MOYS's own recovery is unknown. But at AONR's recovery, most of the off-library regime the design was built around goes away: the correction's harm, most of Euclidean's edge, and two-thirds of selection's gain over global. That regime is findings 4, 7 and 8 of the spec on their off-library side. If they are largely a reference-method offset, there may be no far side for a threshold to separate. A score-space distance could not see the offset in any case, because it lives in the lab values, not the spectra.

**The design repairs, whatever form the experiment takes:**

- **Statistics.** At 150 targets the site-clustered CI half-widths on the contrasts are about 0.10 (C 0.104, M 0.105, K 0.097, P 0.139; 0.07 to 0.10 at 300). That is as large as the prior effects, so most batches would classify as neutral, and H1 as written holds only 40 to 46 % of the time when it is true.
  - Estimate each crossing by an inverse-variance regression of the contrast on log *d_b*, with a batch-and-site bootstrap. Keep the classes as description only, and make verdicts three-valued: holds, fails, undetermined.
  - Estimate the routing threshold on *P*. It is exactly *C* (cosine pool) + *M*.
  - Test H3 by a paired bootstrap of *d\*_C − d\*_M* against an equivalence band. Interval overlap passes 86 % of the time when the crossings sit half the range apart.
  - Close the gaps in the outcome table. In particular, H5 must not pass when every external is neutral or every external is far.
  - Make the adaptive round mechanical: a reserve list scanned now, and first-pass verdicts.
- **Composition.** The simulated KSSL batches run from Arizona (median total C 0.76 %) to southern Michigan (37.9 %, 61 % of rows above 8 % C). The contrasts would follow soil type as much as distance.
  - Restrict calibration batches to mineral surface soils (total C at most about 8 %, upper depth at most 10 cm).
  - Report carbonate.
  - Score on the log scale too.
- **Leakage at the near end.** Site exclusion leaves 2 to 1,534 rows of the batch's own KSSL project in the pool, and North Dakota's *d_b* moves from 0.43 to 1.37 under project exclusion. Exclude by `id.project_ascii_txt` at *R* = 0, and keep site-only exclusion as a nearer dose level.
- **The twins do not match.**
  - The southern Michigan KSSL batch is restored wetlands and Histosols.
  - AONR is 44 fields across five states (24 of them in Ohio), with 519 samples carrying bulk C, not 628.
  - FFAR spans both Dakotas, and its A–D suffixes are depth increments: top layer only gives 108 samples, and its OPUS files carry the sample name "Cquester".
  - A twin, if kept, should be built from KSSL layers near the external set's own fields and matched on C range and depth.
- **Smaller repairs.**
  - The signal's denominator should exclude same-project neighbours. 58.5 % of pool rows' nearest neighbour shares their project.
  - The selection record does not keep the space or every target's nearest distance, so the signal needs a named helper.
  - The experiment's water-band mask is not the shipped default, which is `mask = NULL`.
  - Clay-measured rows per batch run from 0 (Hawaii) to 154, so the clay replication needs a floor on scored targets.
  - Buffers should be measured from the nearest target, not the centroid: the "Iowa" fixture is 168 of 300 rows in Nebraska.
  - The continuity cells must use 15's pool exactly.
  - The draw space was 13 components in all of 15's pools. The 12 to 15 were the correction spaces.
  - The 9/17 figures 3.39 and 2.05 came from the dry pilot on a 3,000-row pool, so the difference from the real run's 2.91 and 1.08 is pool density, not run-to-run instability.
