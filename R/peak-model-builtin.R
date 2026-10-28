# ==============================================================================
# Built-in Peak Models
#
# Core peak shape models: Gaussian, EMG, Bi-Gaussian, Lorentzian, Voigt
# ==============================================================================

# ==============================================================================
# Gaussian Model
# ==============================================================================

#' Create Gaussian Peak Model
#'
#' Creates a symmetric Gaussian peak model with three parameters:
#' height, center, and width (sigma).
#'
#' @return A `gaussian_peak_model` object.
#'
#' @details
#' The Gaussian function is:
#' \deqn{f(x) = h \cdot \exp\left(-\frac{(x - c)^2}{2\sigma^2}\right)}
#'
#' where `h` is height, `c` is center, and `sigma` is width.
#'
#' @examples
#' model <- gaussian_peak_model()
#' x <- seq(0, 10, by = 0.1)
#' params <- list(height = 1, center = 5, width = 1)
#' y <- peak_model_value(model, x, params)
#' plot(x, y, type = "l")
#'
#' @family peak-models
#' @export
gaussian_peak_model <- function() {
  new_peak_model(
    name = "gaussian",
    n_params = 3,
    param_names = c("height", "center", "width"),
    description = "Symmetric Gaussian peak"
  )
}

#' @export
peak_model_value.gaussian_peak_model <- function(model, x, params) {
  params$height * exp(-0.5 * ((x - params$center) / params$width)^2)
}

#' @export
peak_model_gradient.gaussian_peak_model <- function(model, x, params) {
  z <- (x - params$center) / params$width
  g <- params$height * exp(-0.5 * z^2)

  # Partial derivatives
  dg_dheight <- g / params$height
  dg_dcenter <- g * z / params$width
  dg_dwidth <- g * z^2 / params$width

  grad <- cbind(dg_dheight, dg_dcenter, dg_dwidth)
  colnames(grad) <- c("height", "center", "width")
  grad
}

#' @export
peak_model_bounds.gaussian_peak_model <- function(model, x_range, y_range) {
  list(
    lower = c(
      height = max(0, y_range[1]),
      center = x_range[1],
      width = 0.001
    ),
    upper = c(
      height = y_range[2] * 1.5,
      center = x_range[2],
      width = diff(x_range) / 2
    )
  )
}

#' @export
peak_model_initial_guess.gaussian_peak_model <- function(
  model,
  x,
  y,
  peak_idx
) {
  peak_height <- abs(y[peak_idx])
  peak_center <- x[peak_idx]

  # Estimate width from FWHM
  half_height <- peak_height / 2

  # Find left half-height
  left_idx <- peak_idx
  while (
    left_idx > 1 && !is.na(y[left_idx]) && abs(y[left_idx]) > half_height
  ) {
    left_idx <- left_idx - 1
  }

  # Find right half-height
  right_idx <- peak_idx
  n <- length(y)
  while (
    right_idx < n && !is.na(y[right_idx]) && abs(y[right_idx]) > half_height
  ) {
    right_idx <- right_idx + 1
  }

  # FWHM to sigma: sigma = FWHM / (2 * sqrt(2 * ln(2)))
  fwhm <- x[right_idx] - x[left_idx]
  width <- fwhm / (2 * sqrt(2 * log(2)))
  width <- max(width, diff(range(x)) / 100)

  list(
    height = peak_height,
    center = peak_center,
    width = width
  )
}

#' @export
peak_model_area.gaussian_peak_model <- function(model, params, x_range = NULL) {
  # Analytical integral of Gaussian: height * width * sqrt(2 * pi)
  params$height * params$width * sqrt(2 * pi)
}

# ==============================================================================
# EMG (Exponentially Modified Gaussian) Model
# ==============================================================================

#' Create EMG Peak Model
#'
#' Creates an Exponentially Modified Gaussian peak model with four parameters:
#' height, center, width (sigma), and tau (exponential decay constant).
#'
#' @return An `emg_peak_model` object.
#'
#' @details
#' The EMG function models asymmetric peaks with tailing, common in
#' chromatography. It is the convolution of a precursor Gaussian with a
#' one-sided exponential decay density. For positive `tau`, its value is:
#'
#' \deqn{
#' f(x) = \frac{h \sigma \sqrt{2\pi}}{\tau}
#' \exp\left(\frac{\sigma^2}{2\tau^2} - \frac{x-c}{\tau}\right)
#' \Phi\left(\frac{x-c}{\sigma} - \frac{\sigma}{\tau}\right)
#' }
#'
#' Here, `height` is the peak height of the precursor Gaussian, not the
#' observed EMG maximum. The integrated area is therefore
#' `height * width * sqrt(2 * pi)`. A non-positive `tau` returns the Gaussian
#' limit. Fitting constrains precursor height to be nonnegative but leaves its
#' upper bound unbounded because a long tail can make the observed apex much
#' smaller than the precursor height.
#'
#' @examples
#' model <- emg_peak_model()
#' x <- seq(0, 15, by = 0.1)
#' params <- list(height = 1, center = 5, width = 0.5, tau = 0.3)
#' y <- peak_model_value(model, x, params)
#' plot(x, y, type = "l")
#'
#' @family peak-models
#' @export
emg_peak_model <- function() {
  new_peak_model(
    name = "emg",
    n_params = 4,
    param_names = c("height", "center", "width", "tau"),
    description = "Exponentially Modified Gaussian (tailing)"
  )
}

#' @export
peak_model_value.emg_peak_model <- function(model, x, params) {
  if (params$tau <= 0) {
    # Degenerate to Gaussian
    return(params$height * exp(-0.5 * ((x - params$center) / params$width)^2))
  }

  height <- params$height
  if (height == 0) {
    return(rep(0, length(x)))
  }

  delta <- x - params$center
  z <- delta / params$width
  overflow_delta <- is.infinite(delta) & is.finite(x) & is.finite(params$center)
  if (any(overflow_delta)) {
    z[overflow_delta] <- x[overflow_delta] /
      params$width -
      params$center / params$width
  }
  log_k <- log(params$width) - log(params$tau)
  result <- rep(NA_real_, length(x))
  result[is.infinite(z)] <- 0

  finite_z <- is.finite(z)
  positive_infinite_z <- which(
    is.infinite(z) &
      z > 0 &
      is.finite(x) &
      is.finite(params$center)
  )
  if (!any(finite_z) && length(positive_infinite_z) == 0) {
    return(result)
  }

  z_finite <- z[finite_z]
  log_height <- log(abs(height))

  # If width / tau is beyond the finite double range, the exponential
  # displacement is below floating-point resolution and the model is Gaussian.
  if (log_k >= log(.Machine$double.xmax)) {
    result[finite_z] <- height * exp(-0.5 * z_finite^2)
    return(result)
  }

  k <- exp(log_k)

  if (length(positive_infinite_z) > 0) {
    log_delta <- .log_positive_difference(
      x[positive_infinite_z],
      params$center
    )
    log_offset <- log_delta - log(params$tau)
    finite_offset <- log_offset < log(.Machine$double.xmax)

    if (any(finite_offset)) {
      offset <- exp(log_offset[finite_offset])
      log_value <- log_height +
        log_k +
        0.5 * log(2 * pi) +
        0.5 * k^2 -
        offset
      result[positive_infinite_z[finite_offset]] <- sign(height) *
        exp(log_value)
    }
  }

  if (!any(finite_z)) {
    return(result)
  }

  on_gaussian_side <- z_finite <= k
  log_value <- numeric(length(z_finite))

  if (any(on_gaussian_side)) {
    z_left <- z_finite[on_gaussian_side]
    q <- k - z_left
    log_q <- log(q)

    # k - z can overflow when both terms are large. In that case, compute
    # log(k - z) as log(k + abs(z)) without constructing the sum.
    overflow <- is.infinite(q)
    if (any(overflow)) {
      log_abs_z <- log(-z_left[overflow])
      larger <- pmax(log_k, log_abs_z)
      log_q[overflow] <- larger +
        log1p(
          exp(-abs(log_k - log_abs_z))
        )
    }

    log_mills <- .log_normal_mills_ratio(q, log_q)
    log_value[on_gaussian_side] <- log_height +
      log_k -
      0.5 * z_left^2 +
      log_mills
  }

  on_exponential_side <- !on_gaussian_side
  if (any(on_exponential_side)) {
    z_right <- z_finite[on_exponential_side]
    log_value[on_exponential_side] <- log_height +
      log_k +
      0.5 * log(2 * pi) +
      k * (0.5 * k - z_right) +
      stats::pnorm(z_right - k, log.p = TRUE)
  }

  result[finite_z] <- sign(height) * exp(log_value)
  result
}

.log_positive_difference <- function(x, center) {
  center <- rep_len(center, length(x))
  delta <- x - center
  result <- rep(NA_real_, length(delta))
  finite_positive <- is.finite(delta) & delta > 0
  result[finite_positive] <- log(delta[finite_positive])

  overflow_positive <- is.infinite(delta) &
    delta > 0 &
    is.finite(x) &
    is.finite(center)
  if (any(overflow_positive)) {
    log_x <- log(x[overflow_positive])
    log_negative_center <- log(-center[overflow_positive])
    larger <- pmax(log_x, log_negative_center)
    result[overflow_positive] <- larger +
      log1p(exp(-abs(log_x - log_negative_center)))
  }

  result
}

.log_normal_mills_ratio <- function(q, log_q) {
  result <- numeric(length(q))
  use_log_cdf <- q <= 20

  if (any(use_log_cdf)) {
    q_moderate <- q[use_log_cdf]
    result[use_log_cdf] <- 0.5 *
      log(2 * pi) +
      0.5 * q_moderate^2 +
      stats::pnorm(-q_moderate, log.p = TRUE)
  }

  # For large q, the asymptotic expansion avoids cancellation in log(Phi(-q)).
  if (!all(use_log_cdf)) {
    inverse_q <- exp(-log_q[!use_log_cdf])
    inverse_q_squared <- inverse_q^2
    correction <- 1 +
      inverse_q_squared *
        (-1 +
          inverse_q_squared *
            (3 +
              inverse_q_squared *
                (-15 +
                  inverse_q_squared *
                    (105 +
                      inverse_q_squared *
                        (-945 +
                          inverse_q_squared *
                            (10395 - 135135 * inverse_q_squared))))))
    result[!use_log_cdf] <- -log_q[!use_log_cdf] + log(correction)
  }

  result
}

.emg_peak_mode <- function(params) {
  if (params$tau <= 0) {
    return(params$center)
  }

  log_k <- log(params$width) - log(params$tau)
  if (log_k >= log(.Machine$double.xmax)) {
    return(params$center)
  }
  k <- exp(log_k)

  if (k > 21) {
    # Write the normal Mills ratio as C(q) / q, q = k - z. The mode solves
    # z = k * (1 - C(q)). Solve for s = k * z near 1 to avoid cancellation
    # and losing a small standardized displacement with a large physical width.
    scaled_equation <- function(s) {
      inverse_q <- 1 / (k - s / k)
      u <- inverse_q^2
      correction <- 1 +
        u * (-3 + u * (15 + u * (-105 + u * (945 + u * (-10395 + 135135 * u)))))
      s - (k * inverse_q)^2 * correction
    }
    # The root is below 1; leave slack above it so endpoint roundoff cannot
    # remove the sign change in the near-Gaussian limit.
    s <- stats::uniroot(scaled_equation, c(0, 2), tol = 1e-12)$root
    return(params$center + params$tau * s)
  }

  # For a positive exponential convolution the mode is right of the Gaussian
  # center. In standardized coordinates it solves R(k - z) = 1 / k.
  equation <- function(z) {
    q <- k - z
    .log_normal_mills_ratio(q, log(abs(q))) + log_k
  }
  upper <- 1
  while (equation(upper) < 0) {
    upper <- 2 * upper
  }
  z <- stats::uniroot(equation, c(0, upper), tol = 1e-12)$root
  location <- params$center + params$width * z
  if (is.infinite(location) && params$center < 0) {
    location <- params$width * (params$center / params$width + z)
  }
  location
}

#' @export
peak_model_bounds.emg_peak_model <- function(model, x_range, y_range) {
  list(
    lower = c(
      height = 0,
      center = x_range[1],
      width = 0.001,
      tau = 0.001
    ),
    upper = c(
      # EMG height is the precursor Gaussian height. Its observed apex can be
      # substantially lower when the exponential tail is long, so it has no
      # finite upper bound derived from the observed y range.
      height = Inf,
      center = x_range[2],
      width = diff(x_range) / 2,
      tau = diff(x_range)
    )
  )
}

#' @export
peak_model_initial_guess.emg_peak_model <- function(model, x, y, peak_idx) {
  # Start with Gaussian estimate
  gauss_model <- gaussian_peak_model()
  gauss_guess <- peak_model_initial_guess(gauss_model, x, y, peak_idx)

  peak_height <- abs(y[peak_idx])
  peak_center <- x[peak_idx]
  half_height <- peak_height / 2

  # Find half-height points
  left_idx <- peak_idx
  while (
    left_idx > 1 && !is.na(y[left_idx]) && abs(y[left_idx]) > half_height
  ) {
    left_idx <- left_idx - 1
  }

  right_idx <- peak_idx
  n <- length(y)
  while (
    right_idx < n && !is.na(y[right_idx]) && abs(y[right_idx]) > half_height
  ) {
    right_idx <- right_idx + 1
  }

  # Measure asymmetry
  left_width <- abs(peak_center - x[left_idx])
  right_width <- abs(x[right_idx] - peak_center)
  asymmetry <- right_width - left_width

  # Estimate tau from asymmetry
  tau_estimate <- max(asymmetry, diff(range(x)) / 200)
  tau_estimate <- min(tau_estimate, diff(range(x)) / 3)

  # EMG width should be narrower (use left side)
  width_estimate <- left_width / sqrt(2 * log(2))
  width_estimate <- max(width_estimate, diff(range(x)) / 200)

  list(
    height = peak_height * 1.2,
    center = peak_center,
    width = width_estimate,
    tau = tau_estimate
  )
}

# ==============================================================================
# Bi-Gaussian Model
# ==============================================================================

#' Create Bi-Gaussian Peak Model
#'
#' Creates a Bi-Gaussian peak model with four parameters:
#' height, center, width_left, and width_right.
#'
#' @return A `bigaussian_peak_model` object.
#'
#' @details
#' The Bi-Gaussian function uses different widths on the left and right
#' sides of the peak, providing flexible asymmetry.
#'
#' @examples
#' model <- bigaussian_peak_model()
#' x <- seq(0, 10, by = 0.1)
#' params <- list(height = 1, center = 5, width_left = 0.8, width_right = 1.2)
#' y <- peak_model_value(model, x, params)
#' plot(x, y, type = "l")
#'
#' @family peak-models
#' @export
bigaussian_peak_model <- function() {
  new_peak_model(
    name = "bigaussian",
    n_params = 4,
    param_names = c("height", "center", "width_left", "width_right"),
    description = "Bi-Gaussian (asymmetric with two widths)"
  )
}

#' @export
peak_model_value.bigaussian_peak_model <- function(model, x, params) {
  y <- numeric(length(x))

  # Left side (x <= center)
  left_mask <- x <= params$center
  if (any(left_mask)) {
    y[left_mask] <- params$height *
      exp(-0.5 * ((x[left_mask] - params$center) / params$width_left)^2)
  }

  # Right side (x > center)
  right_mask <- x > params$center
  if (any(right_mask)) {
    y[right_mask] <- params$height *
      exp(-0.5 * ((x[right_mask] - params$center) / params$width_right)^2)
  }

  y
}

#' @export
peak_model_bounds.bigaussian_peak_model <- function(model, x_range, y_range) {
  list(
    lower = c(
      height = max(0, y_range[1]),
      center = x_range[1],
      width_left = 0.001,
      width_right = 0.001
    ),
    upper = c(
      height = y_range[2] * 1.5,
      center = x_range[2],
      width_left = diff(x_range) / 2,
      width_right = diff(x_range) / 2
    )
  )
}

#' @export
peak_model_initial_guess.bigaussian_peak_model <- function(
  model,
  x,
  y,
  peak_idx
) {
  peak_height <- abs(y[peak_idx])
  peak_center <- x[peak_idx]
  half_height <- peak_height / 2

  # Find half-height points
  left_idx <- peak_idx
  while (
    left_idx > 1 && !is.na(y[left_idx]) && abs(y[left_idx]) > half_height
  ) {
    left_idx <- left_idx - 1
  }

  right_idx <- peak_idx
  n <- length(y)
  while (
    right_idx < n && !is.na(y[right_idx]) && abs(y[right_idx]) > half_height
  ) {
    right_idx <- right_idx + 1
  }

  # Separate widths for each side
  fwhm_left <- 2 * abs(x[peak_idx] - x[left_idx])
  fwhm_right <- 2 * abs(x[right_idx] - x[peak_idx])

  width_left <- fwhm_left / (2 * sqrt(2 * log(2)))
  width_right <- fwhm_right / (2 * sqrt(2 * log(2)))

  min_width <- diff(range(x)) / 100
  width_left <- max(width_left, min_width)
  width_right <- max(width_right, min_width)

  list(
    height = peak_height,
    center = peak_center,
    width_left = width_left,
    width_right = width_right
  )
}

# ==============================================================================
# Lorentzian Model
# ==============================================================================

#' Create Lorentzian Peak Model
#'
#' Creates a Lorentzian (Cauchy) peak model with three parameters:
#' height, center, and gamma (half-width at half-maximum).
#'
#' @return A `lorentzian_peak_model` object.
#'
#' @details
#' The Lorentzian function has heavier tails than Gaussian and is commonly
#' used in spectroscopy.
#'
#' @examples
#' model <- lorentzian_peak_model()
#' x <- seq(0, 10, by = 0.1)
#' params <- list(height = 1, center = 5, gamma = 0.5)
#' y <- peak_model_value(model, x, params)
#' plot(x, y, type = "l")
#'
#' @family peak-models
#' @export
lorentzian_peak_model <- function() {
  new_peak_model(
    name = "lorentzian",
    n_params = 3,
    param_names = c("height", "center", "gamma"),
    description = "Lorentzian (Cauchy) peak"
  )
}

#' @export
peak_model_value.lorentzian_peak_model <- function(model, x, params) {
  params$height / (1 + ((x - params$center) / params$gamma)^2)
}

#' @export
peak_model_gradient.lorentzian_peak_model <- function(model, x, params) {
  z <- (x - params$center) / params$gamma
  denom <- 1 + z^2

  dg_dheight <- 1 / denom
  dg_dcenter <- 2 * params$height * z / (params$gamma * denom^2)
  dg_dgamma <- 2 * params$height * z^2 / (params$gamma * denom^2)

  grad <- cbind(dg_dheight, dg_dcenter, dg_dgamma)
  colnames(grad) <- c("height", "center", "gamma")
  grad
}

#' @export
peak_model_bounds.lorentzian_peak_model <- function(model, x_range, y_range) {
  list(
    lower = c(
      height = max(0, y_range[1]),
      center = x_range[1],
      gamma = 0.001
    ),
    upper = c(
      height = y_range[2] * 1.5,
      center = x_range[2],
      gamma = diff(x_range) / 2
    )
  )
}

#' @export
peak_model_initial_guess.lorentzian_peak_model <- function(
  model,
  x,
  y,
  peak_idx
) {
  peak_height <- abs(y[peak_idx])
  peak_center <- x[peak_idx]
  half_height <- peak_height / 2

  # For Lorentzian, gamma = HWHM (half-width at half-max)
  left_idx <- peak_idx
  while (
    left_idx > 1 && !is.na(y[left_idx]) && abs(y[left_idx]) > half_height
  ) {
    left_idx <- left_idx - 1
  }

  right_idx <- peak_idx
  n <- length(y)
  while (
    right_idx < n && !is.na(y[right_idx]) && abs(y[right_idx]) > half_height
  ) {
    right_idx <- right_idx + 1
  }

  gamma <- (x[right_idx] - x[left_idx]) / 2
  gamma <- max(gamma, diff(range(x)) / 100)

  list(
    height = peak_height,
    center = peak_center,
    gamma = gamma
  )
}

#' @export
peak_model_area.lorentzian_peak_model <- function(
  model,
  params,
  x_range = NULL
) {
  # Analytical integral: height * gamma * pi
  params$height * params$gamma * pi
}

# ==============================================================================
# Registration
# ==============================================================================

#' Register Core Peak Models
#'
#' Registers the built-in peak models. Called from `.onLoad()`.
#'
#' @return Invisible `NULL`.
#' @noRd
.register_core_peak_models <- function() {
  register_peak_model(
    name = "gaussian",
    constructor = gaussian_peak_model,
    pack_name = "measure",
    description = "Symmetric Gaussian peak"
  )

  register_peak_model(
    name = "emg",
    constructor = emg_peak_model,
    pack_name = "measure",
    description = "Exponentially Modified Gaussian (tailing)"
  )

  register_peak_model(
    name = "bigaussian",
    constructor = bigaussian_peak_model,
    pack_name = "measure",
    description = "Bi-Gaussian (asymmetric with two widths)"
  )

  register_peak_model(
    name = "lorentzian",
    constructor = lorentzian_peak_model,
    pack_name = "measure",
    description = "Lorentzian (Cauchy) peak"
  )

  invisible(NULL)
}
