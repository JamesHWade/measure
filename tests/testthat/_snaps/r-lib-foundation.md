# arg_match() produces informative caller-attributed errors

    Code
      step_measure_smooth_ma(rec, edge_method = "refelct")
    Condition
      Error in `step_measure_smooth_ma()`:
      ! `edge_method` must be one of "reflect", "constant", or "NA", not "refelct".
      i Did you mean "reflect"?

---

    Code
      validate_measure(new_measure_tbl(1:5, 1:5), checks = c("monotonic", "bogus"))
    Condition
      Error in `validate_measure()`:
      ! `checks` must be one of "monotonic", "duplicates", "missing", or "spacing", not "bogus".

# shared validators attribute errors to user-facing callers

    Code
      step_measure_smooth_ma(rec, window = 4L)
    Condition
      Error in `step_measure_smooth_ma()`:
      ! `window` must be a single finite odd integer >= 3.

---

    Code
      validate_measure(tibble::tibble(a = 1:3))
    Condition
      Error in `validate_measure()`:
      ! No measure column found in data frame.
      i Use `step_measure_input_wide()` or `step_measure_input_long()` first.

---

    Code
      measure_quantify_batch(mini_rec, manifest = "not-a-df", window = c(1, 3))
    Condition
      Error in `measure_quantify_batch()`:
      ! `manifest` must be a data frame, not the string "not-a-df".

---

    Code
      measure_quantify_batch(mini_rec, manifest = tibble::tibble(injection_id = "INJ-1",
        injection_type = "standard", nominal_conc = 10), window = c(3, 1))
    Condition
      Error in `measure_quantify_batch()`:
      ! `window` must be a length-2 finite numeric vector with `window[1] < window[2]`.

