# control_horizons() / words a refusal as the user's call, naming the field

    Code
      control_horizons(min_calib_rows = 8L)
    Condition
      Error in `control_horizons()`:
      ! `min_calib_rows` must be a whole number larger than or equal to 9, not the number 8.

---

    Code
      control_horizons(TRUE, alow_par = TRUE)
    Condition
      Error in `control_horizons()`:
      ! `control_horizons()` takes its fields by name.
      x It has no field alow_par.
      x 1 value was passed by position.
      i Its arguments are the fields, e.g. `control_horizons(allow_par = TRUE)`.

---

    Code
      control_horizons(parallelize_over = "both")
    Condition
      Error in `control_horizons()`:
      ! `parallelize_over` = "both" is not supported.
      i Nested parallelism is the user's to register: set a nested `future::plan()` and use "configs".

---

    Code
      evaluate(make_eval_object(), control = control_horizons(chunk_size = 10L))
    Condition
      Error in `evaluate()`:
      ! `evaluate()` does not use the control field chunk_size.
      i chunk_size: select_training()

---

    Code
      evaluate(make_eval_object(), control = list(allow_par = TRUE))
    Condition
      Error in `evaluate()`:
      ! `control` must be made by `control_horizons()`.
      x Got a list.

---

    Code
      evaluate(make_eval_object(), control = ctrl)
    Condition
      Error in `evaluate()`:
      ! `control` has a field alow_par that `control_horizons()` does not know.
      i Make it with `control_horizons()`, whose arguments are the fields.

# control_horizons() / prints the fields that are set, and returns the control invisibly

    Code
      print(control_horizons(allow_par = TRUE, parallelize_over = "resamples",
        chunk_size = 10L))
    Output
      <horizons_control>
        allow_par: TRUE
        parallelize_over: resamples
        chunk_size: 10

