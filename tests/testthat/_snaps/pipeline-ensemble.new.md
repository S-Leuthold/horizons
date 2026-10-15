# ensemble() - the summary it prints / shows the top members, the score, the improvement over the best member, the UQ and the runtime

    Code
      render_ensemble_summary(contract, rank_metric)
    Output
      │  Top members (by weight)
      │  ├─ cubist_raw_log_none_ad3e74: 0.3333
      │  ├─ plsr_raw_log_none_f34a30: 0.3333
      │  └─ plsr_raw_sqrt_none_8af1ca: 0.3333
      │
      │  Summary
      │  ├─ Ensemble rpd: 1.21
      │  ├─ Improvement over best member: -0.0144
      │  ├─ UQ: CV+ conformal (level 0.9, n_calib = 43)
      │  └─ Runtime: <time>
      │
      └─ Class: horizons_fit → horizons_ensemble

---

    Code
      render_ensemble_summary(contract, rank_metric)
    Output
      │  Top members (by weight)
      │  ├─ plsr_raw_log_none_f34a30: -0.5
      │  ├─ plsr_raw_sqrt_none_8af1ca: 0.3
      │  └─ cubist_raw_log_none_ad3e74: 0.2
      │
      │  Summary
      │  ├─ Ensemble rpd: 1.21
      │  ├─ Improvement over best member: -0.0123
      │  ├─ UQ: not computed
      │  └─ Runtime: 2.1 min
      │
      └─ Class: horizons_fit → horizons_ensemble

