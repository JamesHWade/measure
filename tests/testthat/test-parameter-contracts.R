test_that("the less-traveled parameter constructors expose dials metadata", {
  contracts <- tibble::tribble(
    ~name                   , ~type     , ~lower , ~upper ,
    "baseline_lambda"       , "double"  ,  2     ,   9    ,
    "baseline_asymmetry"    , "double"  ,  0.001 ,   0.1  ,
    "baseline_degree"       , "integer" ,  1     ,   6    ,
    "baseline_half_window"  , "integer" ,  5     , 100    ,
    "baseline_span"         , "double"  ,  0.1   ,   0.9  ,
    "baseline_alpha"        , "double"  ,  0     ,   1    ,
    "baseline_window"       , "integer" , 10     , 200    ,
    "peak_location_min"     , "double"  ,  0     , 100    ,
    "peak_location_max"     , "double"  ,  0     , 100    ,
    "differentiation_order" , "integer" ,  0     ,   4    ,
    "derivative_order"      , "integer" ,  1     ,   2    ,
    "derivative_gap"        , "integer" ,  1     ,  10    ,
    "derivative_segment"    , "integer" ,  1     ,   5    ,
    "smooth_window"         , "integer" ,  3     ,  21    ,
    "smooth_sigma"          , "double"  ,  0.5   ,   5    ,
    "fourier_cutoff"        , "double"  ,  0.01  ,   0.5  ,
    "despike_threshold"     , "double"  ,  2     ,  10    ,
    "align_max_shift"       , "integer" ,  1     ,  50    ,
    "align_segment_length"  , "integer" , 10     , 100    ,
    "outlier_threshold"     , "double"  ,  2     ,   5    ,
    "bin_width"             , "double"  ,  1     ,  20    ,
    "emsc_degree"           , "integer" ,  0     ,   4    ,
    "osc_n_components"      , "integer" ,  1     ,  10
  )

  for (i in seq_len(nrow(contracts))) {
    row <- contracts[i, ]
    parameter <- get(row$name[[1]], mode = "function")()

    expect_true(inherits(parameter, "quant_param"), info = row$name[[1]])
    expect_identical(parameter$type, row$type[[1]], info = row$name[[1]])
    expect_equal(
      unname(unlist(parameter$range)),
      c(row$lower[[1]], row$upper[[1]]),
      info = row$name[[1]]
    )
  }

  expect_s3_class(baseline_lambda(), "param")
  expect_identical(baseline_lambda()$trans$name, "log-10")
  expect_equal(unname(cwt_noise_quantile()$inclusive), c(FALSE, FALSE))
})

test_that("parameter constructors forward custom ranges and transformations", {
  custom <- window_side(
    range = c(3L, 9L),
    trans = scales::transform_log10()
  )

  expect_equal(unname(unlist(custom$range)), c(3, 9))
  expect_identical(custom$type, "integer")
  expect_identical(custom$trans$name, "log-10")

  no_transform <- baseline_lambda(range = c(4, 6), trans = NULL)
  expect_equal(unname(unlist(no_transform$range)), c(4, 6))
  expect_null(no_transform$trans)
})

test_that("executable tunable recipe methods discover their parameters", {
  withr::local_package("tune")
  data <- data.frame(outcome = 1:5, predictor = 1:5)

  cases <- tibble::tribble(
    ~step                           , ~fields                                  ,
    "step_measure_baseline_als"     , list(c("lambda", "p"))                   ,
    "step_measure_baseline_poly"    , list("degree")                           ,
    "step_measure_baseline_rf"      , list("span")                             ,
    "step_measure_detrend"          , list("degree")                           ,
    "step_measure_normalize_peak"   , list(c("location_min", "location_max"))  ,
    "step_measure_smooth_gaussian"  , list("sigma")                            ,
    "step_measure_filter_fourier"   , list("cutoff")                           ,
    "step_measure_qc_outlier"       , list("threshold")                        ,
    "step_measure_peaks_deconvolve" , list(c("quality_threshold", "max_iter"))
  )

  for (i in seq_len(nrow(cases))) {
    recipe <- recipes::recipe(outcome ~ predictor, data = data)
    fields <- unlist(cases$fields[[i]])
    args <- setNames(lapply(fields, tune::tune), fields)
    recipe <- do.call(
      get(cases$step[[i]], mode = "function"),
      c(list(recipe), args)
    )
    parameters <- tune::extract_parameter_set_dials(recipe)
    expect_setequal(parameters$id, fields)
    expect_true(all(vapply(parameters$object, inherits, logical(1), "param")))
  }

  recipe <- recipes::recipe(outcome ~ predictor, data = data) |>
    step_measure_baseline_als(
      lambda = tune::tune(),
      p = tune::tune()
    )
  parameters <- tune::extract_parameter_set_dials(recipe)
  lambda <- parameters$object[[match("lambda", parameters$id)]]
  expect_equal(unname(unlist(lambda$range)), c(2, 9))
  expect_identical(lambda$trans$name, "log-10")
})
