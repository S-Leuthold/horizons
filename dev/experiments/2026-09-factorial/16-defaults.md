# 16 — One default each for the correction, the metric and k

**Status:** launched 2026-09-29 as a quick read, not a pre-registered test. It replaces the threshold design in `16-threshold.md`. That draft and its internal review stay as the record of why the routing idea was dropped: a public package cannot condition on instrument, geography keeps the non-US instrument sets out, and in the shipped space the distance did not separate the batches we have.
**Script:** `16-defaults.R`, launched by `run_defaults.sh`. **Results:** `results/defaults/`.

## Objective

Pick one default each for the residual correction, the draw metric and k in `select_training()`. The defaults should hold up on a US batch scanned on an instrument other than KSSL's, which describes nearly every user.

## Hypothesis

The on-library wins seen on Iowa (the correction by 10 %, cosine by 11 %, k = 100 over 400) do not carry to batches from another instrument. If so, the default that is safe off-instrument is a Euclidean draw at k = 100 with no correction.

## Methods

**Batches.**

- **Off-instrument, the user case.** Three US sets, all scanned on CSU's INVENIO-R, truth as bulk C (g/kg ÷ 10 = %):
  - MOYS: Michigan, 99 samples, grouped by farm.
  - AONR: 40 fields over five states, 521 joinable samples, at most 4 per field (seeded), grouped by field.
  - FFAR: Dakotas grazing lands, top (A) layer only, 107 samples.
- **Matched instrument, the reference.** Six KSSL regional batches of about 150 layers each: central Iowa, western Kansas, the Mississippi Delta, the Georgia Piedmont, the Palouse, and central California.
  - Targets are mineral surface layers: total C above 0 and at most 8 %, upper depth at most 10 cm.
  - Layers are taken from the whole sites nearest the centre, located by point coordinate or else county centroid.
  - The pool loses every row of the targets' KSSL projects, so a batch cannot find its own project in the library.

**Property.** KSSL `total_carbon` against the external sets' bulk C, log-transformed. MOYS and FFAR bulk C are fraction sums; that caveat is known and stated, not chased here.

**Arms per batch.**

- Four pool fits: the draw metric (cosine or Euclidean) × draw k (100 or 400).
- Everything else is as in 15:
  - one rf/snv/pca configuration with experiment 1's tuning
  - `configure → evaluate → fit`, seed 307
- Each fit is scored uncorrected and with 15's offset, weighted and slope corrections.
- A Mahalanobis draw at k = 100 records the distance: the median nearest-pool distance over the resemblance threshold. It is recorded and not used.

**Reading.** Per batch, three log RMSE ratios (negative means the first setting wins):

- correction: offset against uncorrected
- metric: cosine against Euclidean
- k: 400 against 100

For each knob, the default is the setting that wins across the off-instrument batches, provided it loses by no more than about 5 % on any KSSL batch. This is a read of the vibe, not a test. Three off-instrument batches on one instrument settle a default for now, not for good.

## Results (run 2026-09-29, 17:37 to 19:23, peak RSS 10.2 GB, exit 0)

The table gives log RMSE ratios; negative means the first setting wins. The correction column is the offset in the Euclidean pool, with the cosine pool in brackets. *d* is the median nearest-pool distance over the resemblance threshold, recorded and not used.

| batch | family | n | median C % | *d* | correction | cosine / Euclidean | k 400 / 100 (Euclidean) |
|---|---|---|---|---|---|---|---|
| AONR | external | 141 | 1.45 | 0.38 | +0.12 (+0.14) | −0.04 | +0.10 |
| MOYS | external | 99 | 1.21 | 0.50 | +0.18 (+0.08) | +0.08 | +0.03 |
| FFAR | external | 107 | 3.35 | 0.52 | +0.15 (+0.11) | +0.16 | +0.53 |
| Iowa | KSSL | 150 | 2.21 | 0.36 | −0.17 (−0.20) | +0.05 | −0.06 |
| Mississippi Delta | KSSL | 150 | 1.45 | 0.39 | +0.01 (+0.04) | −0.18 | −0.01 |
| western Kansas | KSSL | 151 | 1.67 | 0.41 | −0.10 (−0.08) | +0.17 | −0.01 |
| Palouse | KSSL | 151 | 1.61 | 0.48 | −0.24 (−0.31) | +0.17 | +0.13 |
| central California | KSSL | 150 | 1.37 | 0.50 | −0.00 (−0.04) | +0.01 | +0.01 |
| Georgia Piedmont | KSSL | 150 | 2.30 | 0.60 | −0.03 (−0.05) | +0.01 | +0.02 |

**The read: Euclidean, k = 100, no correction.**

- **The correction is the one knob that splits cleanly.** It helps or is neutral on five of six KSSL batches, by up to 22 % on the Palouse, and hurts all three off-instrument batches, by 8 to 19 %. Every off-instrument batch is over-predicted (Euclidean pool bias: MOYS +0.21, AONR +0.25, FFAR +1.08 % C), and the library's residuals push the same way. AONR's truth is a direct bulk C measurement, so the over-prediction is not only MOYS's fraction-sum caveat. The distance cannot tell the two families apart (*d* 0.38 to 0.52 off-instrument, against 0.36 to 0.60 on KSSL), so there is nothing spectral to gate the correction on. It should not be on by default.
- **The metric does not split by regime.** "Cosine on-library" did not replicate: on this Iowa batch (surface mineral layers, total C, its own projects removed) Euclidean wins by 5 %. Across all nine batches Euclidean wins or ties in six, cosine clearly in one (Mississippi Delta), and the median contrast is +0.05. Euclidean is the default.
- **k = 100.** 400 is worse on all three off-instrument batches (FFAR by 53 %) and a wash on KSSL. That reverses 15's MOYS edge for 400.
- FFAR is poor under every setting (RMSE 1.75 % C, bias +1.08): high-C grassland topsoils, over-predicted.

Caveats: one outside instrument, one seed, one configuration, and no CIs. It is a read, not a test.
