# Package-level globals for Python integration
.measure_py <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Initialize the technique pack registry
  .measure_registry_reset()

  # Initialize the peak algorithm registry
  .peak_algorithm_registry_reset()

  # Initialize the peak model registry
  .peak_model_registry_reset()

  # Register measure itself as a pack
  register_measure_pack(
    pack_name = "measure",
    technique = "general",
    description = "Core measurement preprocessing steps"
  )

  # Register all core steps
  .register_core_steps()

  # Register core peak detection algorithms
  .register_core_peak_algorithms()

  # Register core peak models
  .register_core_peak_models()
}

#' Check if pybaselines is available
#'
#' @return Logical indicating whether pybaselines is installed and available.
#' @noRd
.pybaselines_available <- function() {
  if (!rlang::is_installed("reticulate")) {
    return(FALSE)
  }

  tryCatch(
    {
      reticulate::py_module_available("pybaselines")
    },
    error = function(e) FALSE
  )
}

#' Get pybaselines module (lazy loading)
#'
#' @return The pybaselines Python module.
#' @noRd
.get_pybaselines <- function(call = rlang::caller_env()) {
  if (!is.null(.measure_py$pybaselines)) {
    return(.measure_py$pybaselines)
  }

  rlang::check_installed(
    "reticulate",
    reason = "for Python-based baseline correction",
    call = call
  )

  # Request pybaselines installation if not available
  if (!.pybaselines_available()) {
    cli::cli_abort(
      c(
        "The Python package {.pkg pybaselines} is not installed.",
        "i" = "Install it with: {.code reticulate::py_require('pybaselines')}"
      ),
      call = call
    )
  }

  .measure_py$pybaselines <- reticulate::import(
    "pybaselines",
    delay_load = TRUE
  )
  .measure_py$pybaselines
}
