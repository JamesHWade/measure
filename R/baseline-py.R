#' Python-Based Baseline Correction via pybaselines
#'
#' `step_measure_baseline_py()` creates a *specification* of a recipe step
#' that applies baseline correction using the Python pybaselines library,
#' which provides 50+ baseline correction algorithms.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param method The pybaselines method to use. Common methods include:
#'   - Whittaker methods: `"asls"`, `"iasls"`, `"airpls"`, `"arpls"`, `"drpls"`, `"psalsa"`
#'   - Polynomial methods: `"poly"`, `"modpoly"`, `"imodpoly"`, `"loess"`, `"quant_reg"`
#'   - Morphological: `"mor"`, `"imor"`, `"rolling_ball"`, `"tophat"`
#'   - Spline: `"pspline_asls"`, `"pspline_airpls"`, `"mixture_model"`
#'   - Smooth: `"snip"`, `"swima"`, `"noise_median"`
#'   - Classification: `"dietrich"`, `"golotvin"`, `"fastchrom"`
#'   - See pybaselines documentation for the full list.
#' @param ... Additional arguments passed to the pybaselines method. Common
#'   parameters include:
#'   - `lam`: Smoothness parameter for Whittaker methods (default varies by method)
#'   - `p`: Asymmetry parameter for ALS methods (default ~0.01)
#'   - `poly_order`: Polynomial degree for polynomial methods
#'   - `half_window`: Window size for morphological methods
#'   - `max_half_window`: Maximum window for SNIP method
#' @param subtract If `TRUE` (default), the baseline is subtracted from the
#'   signal. If `FALSE`, the baseline values replace the original values
#'   (useful for extracting baselines).
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked?
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' This step provides access to the comprehensive pybaselines Python library,
#' which implements over 50 baseline correction algorithms across several
#' categories:
#'
#' ## Whittaker Methods
#' Based on penalized least squares with asymmetric weights:
#' - `asls`: Asymmetric Least Squares (good general-purpose method)
#' - `iasls`: Improved ALS with automatic smoothness selection
#' - `airpls`: Adaptive iteratively reweighted penalized least squares
#' - `arpls`: Asymmetrically reweighted penalized least squares
#' - `psalsa`: Peaked Signal's Asymmetric Least Squares Algorithm
#'
#' ## Polynomial Methods
#' Fit polynomials to baseline regions:
#' - `poly`: Simple polynomial fitting
#' - `modpoly`: Modified polynomial (iterative)
#' - `imodpoly`: Improved modified polynomial
#' - `loess`: Local regression (LOESS)
#'
#' ## Morphological Methods
#' Based on mathematical morphology:
#' - `mor`: Morphological opening
#' - `imor`: Improved morphological
#' - `rolling_ball`: Rolling ball algorithm
#' - `tophat`: Top-hat transform
#'
#' ## Requirements
#'
#' This step requires the `reticulate` package and Python with pybaselines
#' installed. Install pybaselines with:
#'
#' ```r
#' reticulate::py_require("pybaselines")
#' ```
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `method`, `subtract`, and `id` is returned.
#'
#' @seealso [step_measure_baseline_als()], [step_measure_baseline_custom()] for
#'   R-based alternatives.
#'
#' @family measure-baseline
#' @export
#'
#' @examplesIf measure:::.pybaselines_available()
#' library(recipes)
#'
#' # Asymmetric Least Squares baseline correction
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_py(method = "asls", lam = 1e6, p = 0.01) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' # Using SNIP algorithm
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_py(method = "snip", max_half_window = 40) |>
#'   prep()
step_measure_baseline_py <- function(
  recipe,
  method = "asls",
  ...,
  subtract = TRUE,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_py")
) {
  # Capture additional arguments as quosures
  method_args <- rlang::enquos(...)

  recipes::add_step(
    recipe,
    step_measure_baseline_py_new(
      method = method,
      method_args = method_args,
      subtract = subtract,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_py_new <- function(
  method,
  method_args,
  subtract,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_py",
    method = method,
    method_args = method_args,
    subtract = subtract,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_py <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate parameters
  if (!is.character(x$method) || length(x$method) != 1) {
    cli::cli_abort(
      "{.arg method} must be a single character string, not {.cls {class(x$method)}}."
    )
  }

  if (!is.logical(x$subtract) || length(x$subtract) != 1) {
    cli::cli_abort(
      "{.arg subtract} must be a single logical value, not {.val {x$subtract}}."
    )
  }

  # Check pybaselines availability (will error with helpful message if not)
  .get_pybaselines()

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_py_new(
    method = x$method,
    method_args = x$method_args,
    subtract = x$subtract,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_baseline_py <- function(object, new_data, ...) {
  # Evaluate quosures to get actual argument values
  method_args <- lapply(object$method_args, rlang::eval_tidy)

  for (col in object$measures) {
    result <- .compute_baseline_py(
      new_data[[col]],
      method = object$method,
      method_args = method_args,
      subtract = object$subtract
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_py <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Python pybaselines (", x$method, ") on ")

  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_baseline_py <- function(x, ...) {
  if (recipes::is_trained(x)) {
    terms_val <- x$measures
  } else {
    terms_val <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms_val,
    method = x$method,
    subtract = x$subtract,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_baseline_py <- function(x, ...) {
  c("measure", "reticulate")
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_py <- function(x, ...) {
  # Define tunable parameters based on method
  method_params <- list(
    # Whittaker methods
    asls = list(
      lam = list(pkg = "measure", fun = "baseline_lambda"),
      p = list(pkg = "measure", fun = "baseline_asymmetry")
    ),
    iasls = list(
      lam = list(pkg = "measure", fun = "baseline_lambda"),
      p = list(pkg = "measure", fun = "baseline_asymmetry")
    ),
    airpls = list(
      lam = list(pkg = "measure", fun = "baseline_lambda")
    ),
    arpls = list(
      lam = list(pkg = "measure", fun = "baseline_lambda")
    ),
    drpls = list(
      lam = list(pkg = "measure", fun = "baseline_lambda")
    ),
    psalsa = list(
      lam = list(pkg = "measure", fun = "baseline_lambda"),
      p = list(pkg = "measure", fun = "baseline_asymmetry")
    ),
    # Polynomial methods
    poly = list(
      poly_order = list(pkg = "measure", fun = "baseline_degree")
    ),
    modpoly = list(
      poly_order = list(pkg = "measure", fun = "baseline_degree")
    ),
    imodpoly = list(
      poly_order = list(pkg = "measure", fun = "baseline_degree")
    ),
    # Morphological methods
    mor = list(
      half_window = list(pkg = "measure", fun = "baseline_half_window")
    ),
    imor = list(
      half_window = list(pkg = "measure", fun = "baseline_half_window")
    ),
    rolling_ball = list(
      half_window = list(pkg = "measure", fun = "baseline_half_window")
    ),
    tophat = list(
      half_window = list(pkg = "measure", fun = "baseline_half_window")
    ),
    # Smooth methods
    snip = list(
      max_half_window = list(pkg = "measure", fun = "baseline_half_window")
    )
  )

  params <- method_params[[x$method]]

  if (is.null(params) || length(params) == 0) {
    return(
      tibble::tibble(
        name = character(0),
        call_info = list(),
        source = character(0),
        component = character(0),
        component_id = character(0)
      )
    )
  }

  param_names <- names(params)
  tibble::tibble(
    name = param_names,
    call_info = unname(params),
    source = rep("recipe", length(param_names)),
    component = rep("step_measure_baseline_py", length(param_names)),
    component_id = rep(x$id, length(param_names))
  )
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute pybaselines baseline correction for a measure_list
#'
#' @param dat A measure_list (list of measure_tbl objects).
#' @param method The pybaselines method name.
#' @param method_args Additional arguments for the method.
#' @param subtract Whether to subtract the baseline.
#' @return A list of measure_tbl objects with baseline-corrected values.
#' @noRd
.compute_baseline_py <- function(dat, method, method_args, subtract) {
  purrr::map(
    dat,
    .py_baseline_single,
    method = method,
    method_args = method_args,
    subtract = subtract
  )
}

#' Apply pybaselines baseline correction to a single spectrum
#'
#' @param x A measure_tbl with location and value columns.
#' @param method The pybaselines method name.
#' @param method_args Additional arguments for the method.
#' @param subtract Whether to subtract the baseline.
#' @return A measure_tbl with baseline-corrected values.
#' @noRd
.py_baseline_single <- function(x, method, method_args, subtract) {
  y <- x$value
  locations <- x$location
  n <- length(y)

  # Handle edge cases
  if (n < 2) {
    cli::cli_warn(
      "Spectrum has fewer than 2 points; returning unchanged."
    )
    return(x)
  }

  if (all(is.na(y))) {
    cli::cli_warn("Spectrum is all NA; returning unchanged.")
    return(x)
  }

  # Get pybaselines module
  pybaselines <- .get_pybaselines()

  # Create Baseline fitter object with x_data
  baseline <- tryCatch(
    {
      # Create Baseline object with location data
      fitter <- pybaselines$Baseline(x_data = locations)

      # Get the method function
      method_fn <- tryCatch(
        fitter[[method]],
        error = function(e) {
          cli::cli_abort(
            c(
              "Unknown pybaselines method: {.val {method}}.",
              "i" = "See pybaselines documentation for available methods."
            )
          )
        }
      )

      # Call the method with y values and additional arguments
      result <- do.call(method_fn, c(list(y), method_args))

      # pybaselines returns (baseline, params) tuple
      # The baseline is the first element
      as.numeric(result[[1]])
    },
    error = function(e) {
      cli::cli_warn(
        "pybaselines {.val {method}} failed: {e$message}; returning unchanged."
      )
      return(NULL)
    }
  )

  if (is.null(baseline)) {
    return(x)
  }

  # Validate baseline output
  if (length(baseline) != n) {
    cli::cli_warn(
      "pybaselines {.val {method}} returned {length(baseline)} values, expected {n}; returning unchanged."
    )
    return(x)
  }

  # Apply baseline correction
  if (subtract) {
    x$value <- y - baseline
  } else {
    x$value <- baseline
  }

  x
}
