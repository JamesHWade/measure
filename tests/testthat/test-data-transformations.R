test_that("transform measure to matrix", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  spect_start <- measure:::measure_to_matrix(rec$template$.measures)

})
