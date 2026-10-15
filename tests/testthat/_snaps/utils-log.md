# signal_log() / words a multi-warning topic and the warn = 2 error readably

    Code
      signal_log(do.call(closed, c("evaluate", events)))
    Condition
      Warning:
      3 of 40 configurations failed.
      Warning:
      2 quality warnings in `evaluate()`:
      * The smallest assessment fold has 8 rows.
      * Every member is below the prune threshold.

---

    Code
      withr::with_options(list(warn = 2), signal_log(do.call(closed, c("evaluate",
        events))))
    Condition
      Error:
      ! `evaluate()` raised 2 warnings, and `options(warn = 2)` turns warnings into errors.
      * 3 of 40 configurations failed.
      * 2 quality warnings in `evaluate()`: The smallest assessment fold has 8 rows. Every member is below the prune threshold.
      i The finished result is in this error's object field, and the warnings in its log field.

