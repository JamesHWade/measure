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
#' chromatography. It is the convolution of a Gaussian with an exponential
#' decay function.
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

	# EMG formula using log-space for numerical stability
	# log(result) = log(coef) + exp_term + log(pnorm_term)
	z <- (x - params$center) / params$width + params$width / params$tau
	exp_term <- 0.5 *
		(params$width / params$tau)^2 -
		(x - params$center) / params$tau

	# Clamp exp_term to avoid overflow (exp(709) is ~max for double)
	exp_term <- pmin(exp_term, 700)

	coef <- params$height * params$width * sqrt(2 * pi) / params$tau
	result <- coef * exp(exp_term) * stats::pnorm(z)

	# For very negative z, pnorm is essentially 0, so result should be 0
	# For very large exp_term that was clamped, fallback to Gaussian tail
	gaussian_fallback <- params$height *
		exp(-0.5 * ((x - params$center) / params$width)^2)

	# Use Gaussian where EMG formula has numerical issues
	result[is.nan(result) | is.infinite(result)] <- gaussian_fallback[
		is.nan(result) | is.infinite(result)
	]

	result
}

#' @export
peak_model_bounds.emg_peak_model <- function(model, x_range, y_range) {
	list(
		lower = c(
			height = max(0, y_range[1]),
			center = x_range[1],
			width = 0.001,
			tau = 0.001
		),
		upper = c(
			height = y_range[2] * 1.5,
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
