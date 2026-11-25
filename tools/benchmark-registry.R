#!/usr/bin/env Rscript

# Fresh process, deterministic synthetic registry. Examples:
#   Rscript tools/benchmark-registry.R
#   Rscript tools/benchmark-registry.R --packs 250 --steps-per-pack 40 --reps 20

args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default) {
  bare <- paste0("--", name)
  exact <- match(bare, args)
  if (!is.na(exact)) {
    return(args[[exact + 1L]])
  }
  prefixed <- grepv(paste0("^", bare, "="), args)
  if (length(prefixed) == 0L) {
    return(default)
  }
  sub(paste0("^", bare, "="), "", prefixed[[1L]])
}

n_packs <- as.integer(option("packs", "100"))
steps_per_pack <- as.integer(option("steps-per-pack", "25"))
repetitions <- as.integer(option("reps", "20"))
if (
  anyNA(c(n_packs, steps_per_pack, repetitions)) ||
    any(c(n_packs, steps_per_pack, repetitions) < 1L)
) {
  stop(
    "packs, steps-per-pack, and reps must be positive integers",
    call. = FALSE
  )
}
if (!requireNamespace("measure", quietly = TRUE)) {
  stop("The installed measure package is required", call. = FALSE)
}

pack_names <- sprintf("benchmark.pack.%03d", seq_len(n_packs))
techniques <- c("SEC/GPC", "FTIR", "Raman", "NMR")
categories <- c(
  "preprocessing",
  "calculation",
  "detector",
  "baseline",
  "calibration",
  "quality",
  "integration",
  "normalization"
)

time_call <- function(fun) {
  gc(FALSE)
  started <- proc.time()[["elapsed"]]
  value <- fun()
  list(
    value = value,
    elapsed_ms = unname((proc.time()[["elapsed"]] - started) * 1000)
  )
}

registration <- time_call(function() {
  for (pack_index in seq_along(pack_names)) {
    measure::register_measure_pack(
      pack_names[[pack_index]],
      techniques[[(pack_index - 1L) %% length(techniques) + 1L]],
      version = "0.0.0.9000",
      description = "Synthetic benchmark technique pack"
    )
    for (step_index in seq_len(steps_per_pack)) {
      measure::register_measure_step(
        sprintf("step_benchmark_%03d_%03d", pack_index, step_index),
        pack_names[[pack_index]],
        categories[[(step_index - 1L) %% length(categories) + 1L]],
        "Synthetic benchmark step"
      )
    }
  }
  invisible(NULL)
})

all_steps <- measure::measure_steps()
target_pack <- pack_names[[ceiling(n_packs / 2)]]
target_category <- categories[[3L]]
target_technique <- techniques[[2L]]
queries <- list(
  all = function() measure::measure_steps(),
  pack = function() measure::measure_steps(packs = target_pack),
  category = function() measure::measure_steps(categories = target_category),
  technique = function() measure::measure_steps(techniques = target_technique),
  pack_and_category = function() {
    measure::measure_steps(
      packs = target_pack,
      categories = target_category
    )
  }
)
invisible(lapply(queries, function(query) query()))

filter_rows <- lapply(names(queries), function(name) {
  times <- numeric(repetitions)
  rows <- integer(repetitions)
  for (iteration in seq_len(repetitions)) {
    timed <- time_call(queries[[name]])
    times[[iteration]] <- timed$elapsed_ms
    rows[[iteration]] <- nrow(timed$value)
  }
  data.frame(
    operation = name,
    repetitions = repetitions,
    result_rows = rows[[1L]],
    median_ms = stats::median(times),
    min_ms = min(times),
    max_ms = max(times),
    stringsAsFactors = FALSE
  )
})

results <- rbind(
  data.frame(
    operation = "registration",
    repetitions = 1L,
    result_rows = nrow(all_steps),
    median_ms = registration$elapsed_ms,
    min_ms = registration$elapsed_ms,
    max_ms = registration$elapsed_ms,
    stringsAsFactors = FALSE
  ),
  do.call(rbind, filter_rows)
)
cat(
  "registry benchmark; packs=",
  n_packs,
  "; synthetic_steps=",
  n_packs * steps_per_pack,
  "; total_steps=",
  nrow(all_steps),
  "; reps=",
  repetitions,
  "\n",
  sep = ""
)
write.table(results, file = "", sep = ",", row.names = FALSE, quote = FALSE)
