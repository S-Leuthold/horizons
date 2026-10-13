# fit() - the progress tree / prints each member's result, the counts, the selected member and the runtime

    Code
      invisible(run())
    Output
      
      ┌ fit ─────────────────────────────────────────────────────────
      │
      │  Re-tuning top 2 of 2 configurations
      │  Split: 48 train / 12 test (evaluate()'s held-out rows)
      │  CV: 3-fold stratified on SOC
      │  Bayesian: 25 iterations with warm-start
      │
      │  ├─ [1/2] Random Forest + none + raw + none
      │  │  ├─ Test: RPD = 1.1, R² = 0.61, RMSE = 0.512
      │  │  ├─ CV:   RPD = 2 ± 0.12, RMSE = 0.457 ± 0.032
      │  │  ├─ ⚠ DEGRADED: test RPD 1.100 (95% bootstrap interval [0.900, 1.300], 12 test rows) is below the CV mean RPD 2.000
      │  │  ├─ The final fit warned
      │  │  └─ ✓ 12.3s
      │  └─ [2/2] Cubist + none + raw + none
      │     ├─ FAILED: Warm-start tuning failed: mocked
      │     └─ ✗ 3.4s
      │
      │  Summary
      │  ├─ Successful: 1 / 2
      │  ├─ Degraded: 1 / 2 (cfg_001)
      │  ├─ Failed: 1 / 2
      │  ├─ CV-selected: cfg_001 (test RPD 1.1)
      │  └─ Runtime: 1.5 min
      │
      └─ Class: horizons_eval → horizons_fit
      ──────────────────────────────────────────────────────────────

