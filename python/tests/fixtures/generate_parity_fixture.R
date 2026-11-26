#!/usr/bin/env Rscript

# Generate deterministic signal reference values from the repository sources.
#
# The script sources the implementation in this checkout instead of
# reimplementing the algorithms. Run it from any working directory with:
#
#   R_LIBS=/path/to/rlib Rscript \
#     python/tests/fixtures/generate_parity_fixture.R
#
# prospectr is required for the Savitzky-Golay values.

args <- commandArgs()
script_arg <- args[startsWith(args, "--file=")]
if (!length(script_arg)) {
  cli::cli_abort("Run this script with Rscript so its path can be resolved.")
}

fixture_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
repo_root <- normalizePath(file.path(fixture_dir, "../../.."))
output <- file.path(fixture_dir, "spectra_parity.json")

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to generate the parity fixture.", call. = FALSE)
}
if (!requireNamespace("prospectr", quietly = TRUE)) {
  stop("prospectr is required to generate the parity fixture.", call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate the parity fixture.", call. = FALSE)
}

pkgload::load_all(repo_root, quiet = TRUE)

locations <- c(100, 103, 107, 112, 118, 125, 133, 142, 152)
base <- c(-2, -0.5, 1, 2.25, 3.75, 5.5, 6.25, 8.5, 10)

# Training includes one missing point so the reference calculation exercises
# R's column-wise na.rm=TRUE behavior.  The held-out rows include a constant
# spectrum (MSC's zero-slope fallback).  A separate missing spectrum is kept
# for policies that can be exercised without aborting the full MSC batch.
training <- rbind(
  base,
  1.8 * base + 2.5,
  -0.75 * base + 4,
  c(0.25, 1.2, 2.1, NA, 4.4, 5.2, 6.8, 7.4, 9.3)
)
held_out <- rbind(
  1.4 * base - 3.2,
  -0.6 * base + 7,
  rep(4.2, length(base))
)
missing <- replace(base + 1.75, 5, NA_real_)

measure_list <- function(x) {
  measure::new_measure_list(
    lapply(seq_len(nrow(x)), function(i) {
      measure::new_measure_tbl(locations, x[i, ])
    })
  )
}

values_matrix <- function(x) {
  do.call(rbind, lapply(x, function(measure) measure$value))
}

measurement_data <- function(x) {
  tibble::tibble(signal = measure_list(x))
}

snv_values <- function(x) {
  data <- measurement_data(x)
  recipe <- recipes::recipe(~signal, data = data) |>
    measure::step_measure_snv()
  fitted <- recipes::prep(recipe, training = data, verbose = FALSE)
  values_matrix(recipes::bake(fitted, new_data = data)$signal)
}

# MSC cannot prep a recipe containing a missing response because the R step
# delegates each row to stats::lm.fit(). Compute its training reference and
# correction kernels directly after loading the package namespace.
reference <- .compute_reference_spectrum(measure_list(training))
msc_values <- function(x) {
  values_matrix(
    suppressWarnings(.compute_msc(measure_list(x), reference))
  )
}

# A quadratic and a linear trace make the smoothing and first/second
# derivative contracts easy to inspect, while a third trace exercises the
# ordinary 2-D matrix path with non-polynomial values.
sg_values <- rbind(
  (seq_along(locations) - 1)^2 + 2 * (seq_along(locations) - 1) + 3,
  3 * (seq_along(locations) - 1) + 4,
  c(2, 5, 4, 8, 7, 11, 10, 14, 13)
)
sg_measure_values <- function(deriv) {
  data <- measurement_data(sg_values)
  recipe <- recipes::recipe(~signal, data = data) |>
    measure::step_measure_savitzky_golay(
      degree = 2,
      window_side = 2,
      differentiation_order = deriv
    )
  fitted <- recipes::prep(recipe, training = data, verbose = FALSE)
  values_matrix(recipes::bake(fitted, new_data = data)$signal)
}

sg_missing <- sg_values
sg_missing[3, 5] <- NA_real_
sg_missing_values <- function() {
  data <- measurement_data(sg_missing)
  recipe <- recipes::recipe(~signal, data = data) |>
    measure::step_measure_savitzky_golay(
      degree = 2,
      window_side = 2,
      differentiation_order = 0
    )
  fitted <- recipes::prep(recipe, training = data, verbose = FALSE)
  values_matrix(recipes::bake(fitted, new_data = data)$signal)
}

sg_missing_result <- suppressWarnings(
  sg_missing_values()
)

fixture <- list(
  schema = list(
    locations = locations,
    sg_window = 5L,
    sg_polyorder = 2L,
    sg_derivatives = c(0L, 1L, 2L),
    missing_encoding = "null means NA/NaN and is converted to numpy.nan"
  ),
  train = training,
  held_out = held_out,
  constant = rep(4.2, length(base)),
  missing = missing,
  snv = list(
    train = snv_values(training),
    held_out = snv_values(held_out),
    constant = snv_values(matrix(rep(4.2, length(base)), nrow = 1)),
    missing = snv_values(matrix(missing, nrow = 1))
  ),
  msc = list(
    reference = reference,
    held_out = msc_values(held_out),
    constant = msc_values(matrix(rep(4.2, length(base)), nrow = 1)),
    missing_policy = "error: R lm.fit rejects a per-spectrum missing response"
  ),
  sg = list(
    values = sg_values,
    locations = locations,
    output_locations = locations[3:7],
    smooth = sg_measure_values(0),
    first_derivative = sg_measure_values(1),
    second_derivative = sg_measure_values(2),
    missing_values = sg_missing,
    missing_smooth = sg_missing_result
  )
)

jsonlite::write_json(
  fixture,
  output,
  auto_unbox = TRUE,
  digits = 17,
  na = "null",
  pretty = TRUE
)
cat("Wrote ", output, "\n", sep = "")
