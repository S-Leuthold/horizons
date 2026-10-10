# ensemble() - the summary it prints / shows the top members, the score, the improvement over the best member, the UQ and the runtime

    Code
      render_ensemble_summary(contract, rank_metric)
    Output
      │  Top members (by weight)
      │  ├─ cubist_raw_log_none_d2a226: 0.3333
      │  ├─ cubist_raw_none_none_7b491d: 0.3333
      │  └─ rf_raw_none_none_d572c9: 0.3333
      │
      │  Summary
      │  ├─ Ensemble rpd: 1.192
      │  ├─ Improvement over best member: +0.0227
      │  ├─ UQ: CV+ conformal (level 0.9, n_calib = 43)
      │  └─ Runtime: <time>
      │
      └─ Class: horizons_fit → horizons_ensemble

---

    Code
      render_ensemble_summary(contract, rank_metric)
    Output
      │  Top members (by weight)
      │  ├─ cubist_raw_none_none_7b491d: -0.5
      │  ├─ rf_raw_none_none_d572c9: 0.3
      │  └─ cubist_raw_log_none_d2a226: 0.2
      │
      │  Summary
      │  ├─ Ensemble rpd: 1.192
      │  ├─ Improvement over best member: -0.0123
      │  ├─ UQ: not computed
      │  └─ Runtime: 2.1 min
      │
      └─ Class: horizons_fit → horizons_ensemble

