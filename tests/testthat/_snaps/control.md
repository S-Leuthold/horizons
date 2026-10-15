# control_horizons() / words a refusal as the user's call, naming the field

    Code
      control_horizons(min_calib_rows = 8L)
    Condition
      Error in `control_horizons()`:
      ! `min_calib_rows` must be a whole number larger than or equal to 9, not the number 8.

---

    Code
      evaluate(make_eval_object(), control = control_horizons(chunk_size = 10L))
    Condition
      Error in `evaluate()`:
      ! `evaluate()` does not use the control field chunk_size.
      i chunk_size: select_training()

