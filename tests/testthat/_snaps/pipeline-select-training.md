# targets on an instrument's own axis share no column and are refused, naming the standardize() call (#236)

    Code
      select_training(targets, pool, k = 10)
    Condition
      Error in `select_training()`:
      ! `x` and `library` share no wavenumber column.
      x The targets' 1763 columns run from 599.7431 to 3997.885202 cm-1, about 1.929 cm-1 apart; the library's 851 run from 600 to 4000, 4 cm-1 apart.
      i The training set is the library's own columns at the wavenumbers the targets share with it. Put the targets on the library's grid with `standardize(x, resample = 4, trim = c(600, 4000), force = TRUE)`, or on a multiple of its 4 cm-1 step.

# hulls fitted over ranges an end apart by a step or more are refused (#107)

    Code
      select_training(trimmed, pool, k = 5)
    Condition
      Error in `select_training()`:
      ! `x` and `library` were standardized differently.
      x Both were baseline-corrected, the library's hull over 600 to 4000 cm-1 and the targets' over 1000 to 3000 cm-1, so the hulls are pinned at different ends.
      i A model trained on one baseline cannot predict the other.
      i Fit both hulls over the range both reach, 1000 to 3000 cm-1, standardizing from `spectra()`:
      * the library with `standardize(library, resample = 4, trim = c(1000, 3000), baseline = TRUE)`

# verbose = TRUE reports the pool, the space and the draw

    Code
      out <- select_training(fx$targets, fx$pool, k = 5, control = control_horizons(
        verbose = TRUE))
    Output
      ├─ Selecting training set...
      │  ├─ Pool: 60 rows; searched on its own 4 cm⁻¹ grid, targets resampled onto it
      │  ├─ Space: SNV → SG d1 w11 (40 cm⁻¹) p2 → PCA, 11 components (21 by variance, floored at 0.1 of PC1 sd) on all 60 rows, euclidean
      │  ├─ Depth: not recorded in the library, every row eligible
      │  ├─ clay: k = 5 × 8 targets → 29 of 60 measured rows
      │  ├─ oc: k = 5 × 8 targets → 23 of 30 measured rows
      │  ├─ Twins flagged: 1 (T01)
      │  ├─ Twin rows: 1 removed from the union
      │  ├─ Targets beyond the pool's spread: 0
      │  └─ 39 rows in 1 group (scope = batch)
      │

