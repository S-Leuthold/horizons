# verbose = TRUE reports the pool, the space and the draw

    Code
      out <- select_training(fx$targets, fx$pool, k = 5)
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

