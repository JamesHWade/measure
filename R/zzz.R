# Package-level globals for Python integration
.measure_py <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Set up reticulate to use managed Python environment
  # Only if reticulate is available (it's in Suggests)
  if (requireNamespace("reticulate", quietly = TRUE)) {
    Sys.setenv(RETICULATE_PYTHON = "managed")
  }
}

#' Check if pybaselines is available
#'
#' @return Logical indicating whether pybaselines is installed and available.
#' @noRd
.pybaselines_available <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
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
.get_pybaselines <- function() {
  if (!is.null(.measure_py$pybaselines)) {
    return(.measure_py$pybaselines)
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The {.pkg reticulate} package is required for Python-based baseline correction.",
        "i" = "Install it with: {.code install.packages('reticulate')}"
      )
    )
  }

  # Request pybaselines installation if not available
  if (!.pybaselines_available()) {
    cli::cli_abort(
      c(
        "The Python package {.pkg pybaselines} is not installed.",
        "i" = "Install it with: {.code reticulate::py_require('pybaselines')}"
      )
    )
  }

  .measure_py$pybaselines <- reticulate::import("pybaselines", delay_load = TRUE)
  .measure_py$pybaselines
}
