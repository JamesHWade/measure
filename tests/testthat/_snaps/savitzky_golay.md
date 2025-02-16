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

