# savitzky-golay inputs

    The window size (3) should be greater than or equal to `degree` (5). `window_side` was increased from 1 to 3.

---

    The window size (3) should be greater than or equal to `degree` (5). `window_side` was increased from 1 to 3.

---

    The window size (3) should be greater than or equal to `degree` (5). `window_side` was increased from 1 to 3.

---

    The window size (3) should be greater than or equal to `degree` (5). `window_side` was increased from 1 to 3.

---

    The window size (3) should be greater than or equal to `degree` (6). `window_side` was increased from 1 to 3.

---

    The window size (3) should be greater than or equal to `degree` (6). `window_side` was increased from 1 to 3.

---

    The window size (3) should be greater than or equal to `degree` (6). `window_side` was increased from 1 to 3.

---

    The window size (3) should be greater than or equal to `degree` (6). `window_side` was increased from 1 to 3.

---

    The `degree` argument to `step_measure_savitzky_golay()` should be greater than or equal to `differentiation_order` (1). The polynomial degree was increased to 2.

---

    The `degree` argument to `step_measure_savitzky_golay()` should be greater than or equal to `differentiation_order` (2). The polynomial degree was increased to 3.

---

    The `degree` argument to `step_measure_savitzky_golay()` should be greater than or equal to `differentiation_order` (3). The polynomial degree was increased to 4.

---

    The `degree` argument to `step_measure_savitzky_golay()` should be greater than or equal to `differentiation_order` (2). The polynomial degree was increased to 3.

---

    The `degree` argument to `step_measure_savitzky_golay()` should be greater than or equal to `differentiation_order` (3). The polynomial degree was increased to 4.

---

    The `degree` argument to `step_measure_savitzky_golay()` should be greater than or equal to `differentiation_order` (3). The polynomial degree was increased to 4.

---

    Code
      rec %>% step_measure_savitzky_golay(differentiation_order = bad_inputs$diffs[i],
      window_side = bad_inputs$wn[i], degree = bad_inputs$deg[i]) %>% prep()
    Condition
      Error in `step_measure_savitzky_golay()`:
      Caused by error in `prep()`:
      ! The `differentiation_order` argument to `step_measure_savitzky_golay()` should be a single integer greater than -1.

---

    Code
      rec %>% step_measure_savitzky_golay(differentiation_order = bad_inputs$diffs[i],
      window_side = bad_inputs$wn[i], degree = bad_inputs$deg[i]) %>% prep()
    Condition
      Error in `step_measure_savitzky_golay()`:
      Caused by error in `prep()`:
      ! The `degree` argument to `step_measure_savitzky_golay()` was 0 and should be a single integer greater than zero.

---

    Code
      rec %>% step_measure_savitzky_golay(differentiation_order = bad_inputs$diffs[i],
      window_side = bad_inputs$wn[i], degree = bad_inputs$deg[i]) %>% prep()
    Condition
      Error in `step_measure_savitzky_golay()`:
      Caused by error in `prep()`:
      ! The `window_side` argument to `step_measure_savitzky_golay()` should be an integer greater than 0.

# savitzky-golay tuning parameters

    Code
      window_side()
    Output
      Window Size (one side) (quantitative)
      Range: [1, 5]

---

    Code
      window_side(c(2, 10))
    Output
      Window Size (one side) (quantitative)
      Range: [2, 10]

---

    Code
      differentiation_order()
    Output
      Differentiation Order (quantitative)
      Range: [0, 4]

---

    Code
      recipe(water + fat + protein ~ ., data = meats_long) %>% update_role(id,
        new_role = "id") %>% step_measure_input_long(transmittance, location = vars(
        channel)) %>% step_measure_savitzky_golay() %>% tunable()
    Output
      # A tibble: 3 x 5
        name                  call_info        source component           component_id
        <chr>                 <list>           <chr>  <chr>               <chr>       
      1 window_side           <named list [2]> recipe step_measure_savit~ measure_sav~
      2 differentiation_order <named list [2]> recipe step_measure_savit~ measure_sav~
      3 degree                <named list [3]> recipe step_measure_savit~ measure_sav~

