# check_training_outcome_variance() / names the outcome, the value, the count, and where the other rows went

    Code
      check_training_outcome_variance(rep(0, 12), to_calibration, "SOC", "fit")
    Condition
      Error:
      ! `fit()` cannot fit SOC: all 12 training rows have the value 0.
      x The 3 modelled rows with another value are all outside the training rows: 1 in the test set, 2 in the calibration set.
      i Every model would be fitted to a constant.
      i To keep the calibration rows in training, fit with `compute_uq = FALSE, compute_ad = FALSE`.
      i Otherwise SOC needs more samples where it varies.

# check_training_outcome_variance() / offers undoing the trim when the trim holds the other rows, and only more data when the test set does

    Code
      check_training_outcome_variance(rep(0, 12), to_trim, "SOC", "evaluate")
    Condition
      Error:
      ! `evaluate()` cannot fit SOC: all 12 training rows have the value 0.
      x The 1 modelled row with another value is outside the training rows: 1 trimmed as response outliers.
      i Every model would be fitted to a constant.
      i To keep the trimmed rows in training, re-run `validate()` without `remove_outliers = "response"`, then `evaluate()`.
      i Otherwise SOC needs more samples where it varies.

---

    Code
      check_training_outcome_variance(rep(0, 12), to_test_set, "SOC", "evaluate")
    Condition
      Error:
      ! `evaluate()` cannot fit SOC: all 12 training rows have the value 0.
      x The 1 modelled row with another value is outside the training rows: 1 in the test set.
      i Every model would be fitted to a constant.
      i SOC needs more samples where it varies.

