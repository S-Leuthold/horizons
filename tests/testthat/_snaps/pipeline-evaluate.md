# evaluate() - the progress tree / prints each config's result, the counts and the best config, fresh and resumed, and nothing when quiet

    Code
      invisible(run())
    Output
      
      ┌ Evaluation ──────────────────────────────────────────────────
      │
      │  Dropped 2 rows with NA outcome
      │  Split: 46 train / 12 test (80/20, stratified)
      │  Tuning: 3-fold CV (stratified), grid = 2, bayesian = 0
      │  Configs: 3 total
      │
      │  ├─ [1/3] Random Forest + none + raw + none
      │  │  ├─ Test metrics: RPD = 2.12, R² = 0.6, RMSE = 0.5
      │  │  └─ ✓ 3.2s
      │  ├─ [2/3] Cubist + none + raw + none
      │  │  ├─ Pruned (grid RPD below threshold)
      │  │  ├─ Test metrics: RPD = 0.88, R² = 0.6, RMSE = 0.5
      │  │  ├─ Pruned config warned
      │  │  └─ ✓ 0.5s
      │  └─ [3/3] Random Forest + none + raw + none
      │     ├─ FAILED: Grid search failed: mocked
      │     ├─ Failed config warned
      │     └─ ✗ 0.1s
      │
      │  Results: 1 success, 1 pruned, 1 failed
      └─ Best: cfg_001 (Random Forest) — CV RPD = 2.373 (test RPD = 2.123)
      ──────────────────────────────────────────────────────────────

---

    Code
      invisible(run())
    Output
      
      ┌ Evaluation ──────────────────────────────────────────────────
      │
      │  Dropped 2 rows with NA outcome
      │  Split: 46 train / 12 test (80/20, stratified)
      │  Tuning: 3-fold CV (stratified), grid = 2, bayesian = 0
      │  Configs: 3 total (2 from checkpoint)
      │
      │  ├─ [1/3] Random Forest + none + raw + none
      │  │  └─ loaded from checkpoint
      │  ├─ [2/3] Cubist + none + raw + none
      │  │  └─ loaded from checkpoint
      │  └─ [3/3] Random Forest + none + raw + none
      │     ├─ FAILED: Grid search failed: mocked
      │     ├─ Failed config warned
      │     └─ ✗ 0.1s
      │
      │  Results: 1 success, 1 pruned, 1 failed
      └─ Best: cfg_001 (Random Forest) — CV RPD = 2.373 (test RPD = 2.123)
      ──────────────────────────────────────────────────────────────

