# Run against an installed measure backend:
# Rscript tools/benchmark-whittaker.R output.csv
# Timings include recipe construction, preparation and baking, but not startup.
# This fixed synthetic workload measures execution, not baseline accuracy.

args <- commandArgs(trailingOnly = TRUE)
destination <- if (length(args)) args[[1]] else ""
n <- 50000L
axis <- seq(0, 10, length.out = n)
signal <- 0.2 +
  0.02 * axis +
  exp(-0.5 * ((axis - 2) / 0.18)^2) +
  0.7 * exp(-0.5 * ((axis - 5.2) / 0.4)^2) +
  0.85 * exp(-0.5 * ((axis - 8.2) / 0.24)^2)
data <- tibble::tibble(sample_id = "synthetic-memory-workload")
data$signal <- measure::new_measure_list(
  list(measure::new_measure_tbl(axis, signal))
)

run_case <- function(method, lambda) {
  elapsed <- system.time({
    recipe <- recipes::recipe(~., data = data)
    if (method == "als") {
      recipe <- measure::step_measure_baseline_als(
        recipe,
        measures = "signal",
        lambda = lambda
      )
    } else {
      recipe <- measure::step_measure_baseline_airpls(
        recipe,
        measures = "signal",
        lambda = lambda
      )
    }
    result <- recipes::bake(recipes::prep(recipe), new_data = NULL)
  })
  values <- result$signal[[1]]$value
  stopifnot(length(values) == n, all(is.finite(values)))
  data.frame(
    method = method,
    points = n,
    lambda = lambda,
    iteration_limit = if (method == "als") 20L else 50L,
    elapsed_seconds = unname(elapsed[["elapsed"]]),
    all_values_finite = TRUE,
    r_version = as.character(getRversion()),
    measure_version = as.character(utils::packageVersion("measure")),
    matrix_version = as.character(utils::packageVersion("Matrix"))
  )
}

results <- do.call(
  rbind,
  list(
    run_case("als", 1e6),
    run_case("als", 1e12),
    run_case("airpls", 1e5),
    run_case("airpls", 1e12)
  )
)
utils::write.csv(results, file = destination, row.names = FALSE)
