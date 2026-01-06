# ==============================================================================
# Peak Algorithm Registry
#
# Registration and discovery system for peak detection algorithms.
# Enables technique packs to register custom algorithms.
# ==============================================================================

# Internal registry environment (stored in .measure_registry from registry.R)
# Uses: .measure_registry$peak_algorithms

#' Reset the peak algorithm registry
#'
#' Clears all registered peak detection algorithms. Used internally by
#' `.onLoad()` and in tests.
#'
#' @return Invisible `NULL`.
#' @noRd
.peak_algorithm_registry_reset <- function() {
  .measure_registry$peak_algorithms <- list()
  invisible(NULL)
}

#' Register Core Peak Detection Algorithms
#'
#' Registers the built-in peak detection algorithms. Called from `.onLoad()`.
#'
#' @return Invisible `NULL`.
#' @noRd
.register_core_peak_algorithms <- function() {
  # Prominence method - robust to noise
  register_peak_algorithm(
    name = "prominence",
    algorithm_fn = .detect_peaks_prominence,
    pack_name = "measure",
    description = "Finds local maxima by prominence (height above surrounding signal)",
    default_params = list(
      min_prominence = 0,
      min_height = 0,
      min_distance = 0
    ),
    param_info = list(
      min_prominence = "Minimum peak prominence (how much peak stands out)",
      min_height = "Minimum peak height",
      min_distance = "Minimum distance between peaks in x-axis units"
    )
  )

  # Derivative method - simple and fast
  register_peak_algorithm(
    name = "derivative",
    algorithm_fn = .detect_peaks_derivative,
    pack_name = "measure",
    description = "Finds peaks by zero-crossings in first derivative",
    default_params = list(
      min_height = 0,
      min_distance = 0
    ),
    param_info = list(
      min_height = "Minimum peak height",
      min_distance = "Minimum distance between peaks in x-axis units"
    )
  )

  # Local maxima method - very simple
  register_peak_algorithm(
    name = "local_maxima",
    algorithm_fn = .detect_peaks_local_maxima,
    pack_name = "measure",
    description = "Finds all local maxima above a threshold",
    default_params = list(
      min_height = 0,
      min_distance = 0
    ),
    param_info = list(
      min_height = "Minimum peak height",
      min_distance = "Minimum distance between peaks in x-axis units"
    )
  )

  invisible(NULL)
}

# ==============================================================================
# Registration Functions (exported)
# ==============================================================================

#' Register a Peak Detection Algorithm
#'
#' Registers a peak detection algorithm with the measure package. This function
#' can be called from technique pack packages to add specialized algorithms.
#'
#' @param name Algorithm name (e.g., `"cwt"`, `"finderskeepers"`). Must be unique.
#' @param algorithm_fn The algorithm function. Must accept `location`, `value`,
#'   and return a `peaks_tbl` object. Additional parameters are passed via `...`.
#' @param pack_name Source package name. Use `pkgname` from `.onLoad()`.
#' @param description Brief description of the algorithm.
#' @param default_params Named list of default parameter values.
#' @param param_info Named list of parameter descriptions (for documentation).
#' @param technique Optional technique name (e.g., `"SEC/GPC"`). If `NULL`,
#'   algorithm is considered general-purpose.
#'
#' @return Invisible `TRUE`.
#'
#' @examples
#' \dontrun{
#' # In a technique pack's R/zzz.R file:
#' .onLoad <- function(libname, pkgname) {
#'   if (requireNamespace("measure", quietly = TRUE)) {
#'     measure::register_peak_algorithm(
#'       name = "sec_loess_ist",
#'       algorithm_fn = .detect_peaks_sec_loess_ist,
#'       pack_name = pkgname,
#'       description = "LOESS smoothing with iterative soft thresholding",
#'       default_params = list(loess_span = 0.01, ist_points = 50),
#'       technique = "SEC/GPC"
#'     )
#'   }
#' }
#' }
#'
#' @seealso [peak_algorithms()], [get_peak_algorithm()]
#' @export
register_peak_algorithm <- function(
  name,
  algorithm_fn,
  pack_name,
  description = "",
  default_params = list(),
  param_info = list(),
  technique = NULL
) {
  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    cli::cli_abort("{.arg name} must be a non-empty string.")
  }
  if (!is.function(algorithm_fn)) {
    cli::cli_abort("{.arg algorithm_fn} must be a function.")
  }
  if (!is.character(pack_name) || length(pack_name) != 1) {
    cli::cli_abort("{.arg pack_name} must be a string.")
  }
  if (!is.list(default_params)) {
    cli::cli_abort("{.arg default_params} must be a list.")
  }

  # Initialize registry if needed

  if (is.null(.measure_registry$peak_algorithms)) {
    .measure_registry$peak_algorithms <- list()
  }

  # Store algorithm info
  .measure_registry$peak_algorithms[[name]] <- list(
    name = name,
    algorithm_fn = algorithm_fn,
    pack_name = pack_name,
    description = description,
    default_params = default_params,
    param_info = param_info,
    technique = technique,
    registered_at = Sys.time()
  )

  invisible(TRUE)
}

#' Unregister a Peak Detection Algorithm
#'
#' Removes a peak detection algorithm from the registry.
#'
#' @param name Algorithm name to remove.
#'
#' @return Invisible `TRUE` if removed, `FALSE` if not found.
#'
#' @seealso [register_peak_algorithm()]
#' @export
unregister_peak_algorithm <- function(name) {
  if (is.null(.measure_registry$peak_algorithms)) {
    return(invisible(FALSE))
  }
  if (!name %in% names(.measure_registry$peak_algorithms)) {
    return(invisible(FALSE))
  }

  .measure_registry$peak_algorithms[[name]] <- NULL
  invisible(TRUE)
}

# ==============================================================================
# Discovery Functions (exported)
# ==============================================================================

#' List Available Peak Detection Algorithms
#'
#' Returns a tibble of all registered peak detection algorithms.
#'
#' @param packs Character vector of pack names to include. If `NULL`, includes
#'   all packs.
#' @param techniques Character vector of techniques to include. If `NULL`,
#'   includes all techniques (including general-purpose algorithms).
#'
#' @return A tibble with columns:
#'   - `name`: Algorithm name (e.g., "prominence", "derivative")
#'   - `pack_name`: Source package name
#'   - `description`: Brief description
#'   - `technique`: Technique (or `NA` for general-purpose)
#'   - `default_params`: List column of default parameter values
#'
#' @examples
#' # List all algorithms
#' peak_algorithms()
#'
#' # List only algorithms from a specific pack
#' peak_algorithms(packs = "measure")
#'
#' @seealso [register_peak_algorithm()], [get_peak_algorithm()]
#' @export
peak_algorithms <- function(packs = NULL, techniques = NULL) {
  algos <- .measure_registry$peak_algorithms

  if (is.null(algos) || length(algos) == 0) {
    return(
      tibble::tibble(
        name = character(),
        pack_name = character(),
        description = character(),
        technique = character(),
        default_params = list()
      )
    )
  }

  result <- tibble::tibble(
    name = vapply(algos, `[[`, character(1), "name"),
    pack_name = vapply(algos, `[[`, character(1), "pack_name"),
    description = vapply(algos, `[[`, character(1), "description"),
    technique = vapply(
      algos,
      function(x) x$technique %||% NA_character_,
      character(1)
    ),
    default_params = lapply(algos, `[[`, "default_params")
  )

  # Filter by pack
  if (!is.null(packs)) {
    result <- result[result$pack_name %in% packs, , drop = FALSE]
  }

  # Filter by technique
  if (!is.null(techniques)) {
    result <- result[
      is.na(result$technique) | result$technique %in% techniques,
      ,
      drop = FALSE
    ]
  }

  result
}

#' Get a Peak Detection Algorithm
#'
#' Retrieves a registered peak detection algorithm by name.
#'
#' @param name Algorithm name.
#'
#' @return A list with components:
#'   - `name`: Algorithm name
#'   - `algorithm_fn`: The algorithm function
#'   - `pack_name`: Source package name
#'   - `description`: Brief description
#'   - `default_params`: List of default parameter values
#'   - `param_info`: List of parameter descriptions
#'   - `technique`: Technique name (or `NULL`)
#'
#'   Returns `NULL` if algorithm not found.
#'
#' @examples
#' algo <- get_peak_algorithm("prominence")
#' if (!is.null(algo)) {
#'   print(algo$description)
#' }
#'
#' @seealso [peak_algorithms()], [register_peak_algorithm()]
#' @export
get_peak_algorithm <- function(name) {
  if (is.null(.measure_registry$peak_algorithms)) {
    return(NULL)
  }
  .measure_registry$peak_algorithms[[name]]
}

#' Check if a Peak Algorithm Exists
#'
#' Checks whether a peak detection algorithm is registered.
#'
#' @param name Algorithm name.
#'
#' @return Logical `TRUE` if algorithm exists, `FALSE` otherwise.
#'
#' @examples
#' has_peak_algorithm("prominence")  # TRUE
#' has_peak_algorithm("nonexistent") # FALSE
#'
#' @seealso [peak_algorithms()]
#' @export
has_peak_algorithm <- function(name) {
  !is.null(get_peak_algorithm(name))
}

# ==============================================================================
# Algorithm execution helper
# ==============================================================================

#' Execute a Peak Detection Algorithm
#'
#' Internal function to run a registered peak detection algorithm with given
#' parameters.
#'
#' @param name Algorithm name.
#' @param location Numeric vector of x-axis values.
#' @param value Numeric vector of y-axis values (signal).
#' @param ... Additional parameters passed to the algorithm function.
#'
#' @return A `peaks_tbl` object, or error if algorithm not found.
#' @noRd
.run_peak_algorithm <- function(name, location, value, ...) {
  algo <- get_peak_algorithm(name)

  if (is.null(algo)) {
    available <- names(.measure_registry$peak_algorithms)
    cli::cli_abort(
      c(
        "Peak algorithm {.val {name}} not found.",
        "i" = "Available algorithms: {.val {available}}"
      )
    )
  }

  # Merge default params with provided params
  params <- algo$default_params
  provided <- list(...)
  for (nm in names(provided)) {
    params[[nm]] <- provided[[nm]]
  }

  # Call the algorithm function
  do.call(
    algo$algorithm_fn,
    c(list(location = location, value = value), params)
  )
}

# ==============================================================================
# Additional built-in algorithms
# ==============================================================================

#' Detect peaks using simple local maxima method
#' @noRd
.detect_peaks_local_maxima <- function(
  location,
  value,
  min_height = 0,
  min_distance = 0
) {
  n <- length(value)
  if (n < 3) {
    return(new_peaks_tbl())
  }

  # Find all local maxima (points higher than both neighbors)
  is_peak <- c(
    FALSE,
    value[-c(1, n)] > value[-c(n - 1, n)] & value[-c(1, n)] > value[-c(1, 2)],
    FALSE
  )
  peaks_idx <- which(is_peak)

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Filter by minimum height
  if (min_height > 0) {
    peaks_idx <- peaks_idx[value[peaks_idx] >= min_height]
  }

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Filter by minimum distance (O(n) incremental approach)
  if (min_distance > 0 && length(peaks_idx) > 1) {
    # Sort by height descending
    height_order <- order(value[peaks_idx], decreasing = TRUE)
    sorted_idx <- peaks_idx[height_order]
    n_peaks <- length(sorted_idx)

    keep <- logical(n_peaks)
    keep[1] <- TRUE
    kept_locs <- numeric(n_peaks)
    kept_locs[1] <- location[sorted_idx[1]]
    n_kept <- 1L

    for (i in seq_len(n_peaks)[-1]) {
      current_loc <- location[sorted_idx[i]]
      if (all(abs(current_loc - kept_locs[seq_len(n_kept)]) >= min_distance)) {
        keep[i] <- TRUE
        n_kept <- n_kept + 1L
        kept_locs[n_kept] <- current_loc
      }
    }

    kept_idx <- sorted_idx[keep]
    peaks_idx <- kept_idx[order(location[kept_idx])]
  }

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Find simple peak bases (nearest local minima on each side)
  left_bases <- integer(length(peaks_idx))
  right_bases <- integer(length(peaks_idx))

  for (i in seq_along(peaks_idx)) {
    pk <- peaks_idx[i]

    # Left base
    left_idx <- pk - 1
    while (left_idx > 1 && value[left_idx] >= value[left_idx - 1]) {
      left_idx <- left_idx - 1
    }
    left_bases[i] <- left_idx

    # Right base
    right_idx <- pk + 1
    while (right_idx < n && value[right_idx] >= value[right_idx + 1]) {
      right_idx <- right_idx + 1
    }
    right_bases[i] <- right_idx
  }

  new_peaks_tbl(
    peak_id = seq_along(peaks_idx),
    location = location[peaks_idx],
    height = value[peaks_idx],
    left_base = location[left_bases],
    right_base = location[right_bases],
    area = rep(NA_real_, length(peaks_idx))
  )
}
