# ==============================================================================
# Parametrized tidymodels / recipes S3 contract tests across all steps (#213)
# ==============================================================================

test_that("every exported step_*() implements the full recipes S3 contract", {
  exports <- getNamespaceExports("measure")
  # step_measure_normalize_istd() is a documented alias returning
  # step_measure_normalize_peak
  aliases <- "step_measure_normalize_istd"
  step_fns <- setdiff(sort(grepv("^step_", exports)), aliases)

  expect_gte(length(step_fns), 100L)

  ns <- asNamespace("measure")
  for (step_name in step_fns) {
    for (generic in c("prep", "bake", "print", "tidy", "required_pkgs")) {
      method_name <- paste0(generic, ".", step_name)
      expect_true(
        exists(method_name, envir = ns, inherits = FALSE),
        info = paste("Missing S3 method:", method_name)
      )
    }

    req_fn <- get(
      paste0("required_pkgs.", step_name),
      envir = ns,
      inherits = FALSE
    )
    pkgs <- req_fn(structure(list(), class = c(step_name, "step")))
    expect_type(pkgs, "character")
    expect_true(
      "measure" %in% pkgs,
      info = paste0("required_pkgs.", step_name, "() must include 'measure'")
    )
  }

  # Optional backend steps declare their engine packages
  expect_true(
    "multiway" %in%
      recipes::required_pkgs(structure(
        list(),
        class = c("step_measure_parafac", "step")
      ))
  )
  expect_true(
    "multiway" %in%
      recipes::required_pkgs(structure(
        list(),
        class = c("step_measure_tucker", "step")
      ))
  )
  expect_true(
    "dtw" %in%
      recipes::required_pkgs(structure(
        list(),
        class = c("step_measure_align_dtw", "step")
      ))
  )
  expect_true(
    "ptw" %in%
      recipes::required_pkgs(structure(
        list(),
        class = c("step_measure_align_ptw", "step")
      ))
  )
  expect_true(
    "prospectr" %in%
      recipes::required_pkgs(structure(
        list(),
        class = c("step_measure_savitzky_golay", "step")
      ))
  )
  expect_true(
    "wavethresh" %in%
      recipes::required_pkgs(structure(
        list(),
        class = c("step_measure_smooth_wavelet", "step")
      ))
  )
  expect_true(
    "reticulate" %in%
      recipes::required_pkgs(structure(
        list(),
        class = c("step_measure_baseline_py", "step")
      ))
  )
})

test_that("core 1D preprocessing steps enforce recipes runtime contracts", {
  times <- seq(1, 10, length.out = 25)
  make_spec <- function(offset) {
    new_measure_tbl(times, offset + 2 * exp(-0.5 * ((times - 5) / 0.8)^2))
  }
  df <- tibble::tibble(
    id = paste0("S", 1:6),
    y = c(1.1, 2.0, 2.9, 4.1, 5.0, 6.2),
    .measures = new_measure_list(lapply(
      c(0.5, 0.7, 0.9, 1.1, 1.3, 1.5),
      make_spec
    ))
  )

  step_builders <- list(
    snv = function(r) step_measure_snv(r),
    msc = function(r) step_measure_msc(r),
    smooth_ma = function(r) step_measure_smooth_ma(r, window = 5L),
    smooth_median = function(r) step_measure_smooth_median(r, window = 5L),
    smooth_gaussian = function(r) step_measure_smooth_gaussian(r, sigma = 1),
    detrend = function(r) step_measure_detrend(r, degree = 1),
    center = function(r) step_measure_center(r),
    scale_auto = function(r) step_measure_scale_auto(r),
    normalize_sum = function(r) step_measure_normalize_sum(r),
    normalize_max = function(r) step_measure_normalize_max(r),
    baseline_als = function(r) step_measure_baseline_als(r),
    baseline_poly = function(r) step_measure_baseline_poly(r, degree = 2),
    trim = function(r) step_measure_trim(r, range = c(2, 9)),
    despike = function(r) step_measure_despike(r),
    derivative = function(r) step_measure_derivative(r)
  )

  base_rec <- recipes::recipe(y ~ ., data = df) |>
    recipes::update_role(id, new_role = "id")

  for (nm in names(step_builders)) {
    rec <- step_builders[[nm]](base_rec)

    # required_pkgs() works on recipes containing measure steps
    expect_true("measure" %in% recipes::required_pkgs(rec), info = nm)

    # Untrained print() and tidy() succeed
    expect_output(print(rec))
    untidy <- recipes::tidy(rec, number = 1)
    expect_s3_class(untidy, "tbl_df")
    expect_true("id" %in% names(untidy), info = nm)

    # Prep, trained print(), trained tidy(), and bake() succeed
    prepped <- recipes::prep(rec, training = df)
    expect_output(print(prepped))
    td <- recipes::tidy(prepped, number = 1)
    expect_s3_class(td, "tbl_df")
    expect_true("id" %in% names(td), info = nm)

    baked <- recipes::bake(prepped, new_data = df)
    expect_s3_class(baked, "tbl_df")
    expect_equal(nrow(baked), nrow(df), info = nm)

    # Missing required measure column fails via recipes::check_new_data()
    expect_error(
      recipes::bake(prepped, new_data = df[, c("id", "y")]),
      "missing",
      info = nm
    )
  }
})

test_that("bake() check_new_data() reports missing columns cleanly", {
  times <- seq(1, 5, length.out = 10)
  df <- tibble::tibble(
    id = c("S1", "S2"),
    .measures = new_measure_list(list(
      new_measure_tbl(times, sin(times)),
      new_measure_tbl(times, cos(times))
    ))
  )

  prepped <- recipes::recipe(~., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_snv() |>
    recipes::prep(training = df)

  expect_snapshot(
    error = TRUE,
    recipes::bake(prepped$steps[[1]], new_data = df["id"])
  )
})
