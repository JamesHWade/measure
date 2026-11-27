"""Preprocessing functions for matrices of samples by spectral channels."""

from __future__ import annotations

import math
import numbers
import warnings

import numpy as np

__all__ = ["msc", "msc_reference", "savitzky_golay", "snv"]


def _as_float_matrix(values: object, *, name: str) -> np.ndarray:
    """Convert *values* to a two-dimensional float matrix.

    ``NaN`` is retained for the kernels that support missing observations.
    Infinite values are rejected uniformly so that a failed operation cannot
    silently turn an entire spectrum into ``NaN``.
    """

    try:
        raw = np.asarray(values)
    except (TypeError, ValueError) as error:
        raise ValueError(f"{name} must be a two-dimensional numeric array") from error

    if np.iscomplexobj(raw):
        raise ValueError(f"{name} must contain real-valued numbers")

    try:
        result = np.asarray(raw, dtype=np.float64)
    except (TypeError, ValueError) as error:
        raise ValueError(f"{name} must be a two-dimensional numeric array") from error

    if result.ndim != 2:
        raise ValueError(f"{name} must be a 2D array of samples by channels")
    if result.shape[1] == 0:
        raise ValueError(f"{name} must contain at least one channel")
    if np.isinf(result).any():
        raise ValueError(f"{name} must contain only finite values or NaN")
    return result


def _as_float_vector(values: object, *, name: str) -> np.ndarray:
    """Convert *values* to a one-dimensional finite float vector."""

    try:
        raw = np.asarray(values)
    except (TypeError, ValueError) as error:
        raise ValueError(f"{name} must be a one-dimensional numeric array") from error

    if np.iscomplexobj(raw):
        raise ValueError(f"{name} must contain real-valued numbers")

    try:
        result = np.asarray(raw, dtype=np.float64)
    except (TypeError, ValueError) as error:
        raise ValueError(f"{name} must be a one-dimensional numeric array") from error

    if result.ndim != 1:
        raise ValueError(f"{name} must be a one-dimensional array")
    if not np.isfinite(result).all():
        raise ValueError(f"{name} must contain only finite values")
    return result


def _integer_parameter(value: object, *, name: str, minimum: int) -> int:
    """Validate an integer-like scalar parameter and return a Python ``int``."""

    if isinstance(value, (bool, np.bool_)) or not isinstance(value, numbers.Real):
        raise ValueError(f"{name} must be a finite integer >= {minimum}")
    numeric = float(value)
    if not np.isfinite(numeric) or not numeric.is_integer() or numeric < minimum:
        raise ValueError(f"{name} must be a finite integer >= {minimum}")
    return int(numeric)


def snv(values: object) -> np.ndarray:
    """Apply Standard Normal Variate row by row.

    Parameters
    ----------
    values:
        A two-dimensional array with shape ``(samples, channels)``.

    Returns
    -------
    numpy.ndarray
        A new float array with the same shape.  Each row is centred by its
        mean and divided by its sample standard deviation.  Rows with fewer
        than two non-missing values, or zero variance, are returned centred
        and emit a :class:`RuntimeWarning`, matching ``.snv_single()``.
    """

    array = _as_float_matrix(values, name="values")
    result = array.copy()
    for row in result:
        observed = ~np.isnan(row)
        values_present = row[observed]
        if values_present.size:
            # Subtract a measured value first so a flat decimal signal centers
            # to exact zeros even when summing repeated values would round.
            offsets = values_present - values_present[0]
            row[observed] = offsets - offsets.mean()
        if values_present.size > 1:
            scale = np.linalg.norm(row[observed]) / math.sqrt(values_present.size - 1)
            if scale > 0:
                row[observed] /= scale
                continue
        warnings.warn(
            "Standard deviation is zero or NA for a spectrum. "
            "SNV cannot be applied; returning centered values.",
            RuntimeWarning,
            stacklevel=2,
        )
    return result


def msc_reference(training_values: object) -> np.ndarray:
    """Compute the column-wise MSC reference spectrum.

    ``NaN`` observations are omitted independently for each channel, just as
    ``colMeans(..., na.rm = TRUE)`` is in ``R/msc.R``.  A channel containing no
    observed values therefore has a ``NaN`` reference value.
    """

    array = _as_float_matrix(training_values, name="training_values")
    valid = ~np.isnan(array)
    counts = valid.sum(axis=0)
    sums = np.sum(np.where(valid, array, 0.0), axis=0)
    result = np.full(array.shape[1], np.nan, dtype=np.float64)
    with np.errstate(invalid="ignore", divide="ignore"):
        np.divide(sums, counts, out=result, where=counts > 0)
    return result


def msc(values: object, reference: object) -> np.ndarray:
    """Apply Multiplicative Scatter Correction to each sample.

    The intercept and slope for each row are obtained by regressing the row
    on ``reference``.  A zero or undefined slope follows the R fallback and
    returns the row centred by its fitted intercept, with a warning.

    Unlike :func:`msc_reference`, this operation requires complete finite
    spectra and a complete finite reference.  This is intentional: the R
    implementation calls ``stats::lm.fit()`` without a missing-value action,
    which errors for ``NA``, ``NaN``, or ``Inf`` in either regression input.
    """

    array = _as_float_matrix(values, name="values")
    ref = _as_float_vector(reference, name="reference")

    if array.shape[1] != ref.size:
        raise ValueError("reference must have the same number of channels as values")
    if not np.isfinite(array).all():
        raise ValueError("msc values and reference must contain only finite values")

    if array.shape[0] == 0:
        return array.copy()

    ref_mean = float(np.mean(ref))
    ref_centred = ref - ref_mean
    denominator = float(np.dot(ref_centred, ref_centred))
    levels = np.mean(array, axis=1)
    constant_rows = np.all(array == array[:, :1], axis=1)
    levels[constant_rows] = array[constant_rows, 0]
    centred = array - levels[:, None]

    # stats::lm.fit() declares the second column rank deficient when its
    # residual norm after projecting out the intercept falls below its
    # tolerance (1e-7 times the original column norm).  Without this check,
    # NumPy's normal-equation slope can turn a near-constant reference into a
    # large, meaningless multiplicative correction.
    reference_rank_deficient = denominator == 0.0 or math.sqrt(denominator) <= (
        1e-7 * float(np.linalg.norm(ref))
    )
    if reference_rank_deficient:
        slopes = np.full(array.shape[0], np.nan, dtype=np.float64)
        intercepts = levels.copy()
    else:
        slopes = (centred @ ref_centred) / denominator
        intercepts = levels - slopes * ref_mean

    result = np.empty_like(array)
    threshold = np.finfo(np.float64).eps
    for row, slope in enumerate(slopes):
        intercept = intercepts[row]
        if np.isnan(slope) or abs(slope) < threshold:
            warnings.warn(
                "Multiplicative coefficient is zero or NA for a spectrum. "
                "MSC cannot be applied; returning centered values.",
                RuntimeWarning,
                stacklevel=2,
            )
            result[row] = array[row] - intercept
        else:
            result[row] = (array[row] - intercept) / slope

    return result


def _sg_parameters(n_channels, window, polyorder, deriv):
    width = _integer_parameter(window, name="window", minimum=1)
    order = _integer_parameter(polyorder, name="polyorder", minimum=0)
    derivative = _integer_parameter(deriv, name="deriv", minimum=0)

    if width % 2 != 1:
        raise ValueError("window must be odd")
    if width >= n_channels:
        raise ValueError("window must be lower than the number of channels in values")
    if order >= width:
        raise ValueError("window must be greater than polyorder")
    if order < derivative:
        raise ValueError("polyorder must be greater than or equal to deriv")

    return width, order, derivative


def savitzky_golay(
    values: object,
    locations: object,
    *,
    window: object = 5,
    polyorder: object = 2,
    deriv: object = 0,
) -> np.ndarray:
    """Smooth or differentiate spectra with a valid Savitzky--Golay window.

    Parameters
    ----------
    values:
        A two-dimensional array with shape ``(samples, channels)``.
    locations:
        Finite one-dimensional labels for the channels.  They must have the
        same length as the second dimension of ``values``.  Labels are not
        required to be equally spaced because the R measure step uses unit
        channel spacing (``delta.wav`` is omitted) when calculating its
        coefficients.
    window:
        Odd filter length.  It must be strictly smaller than the number of
        channels, as required by ``prospectr::savitzkyGolay()``.
    polyorder:
        Non-negative polynomial order strictly smaller than ``window``.
    deriv:
        Non-negative derivative order no larger than ``polyorder``.

    Returns
    -------
    numpy.ndarray
        The valid convolution result with shape
        ``(samples, channels - window + 1)``.  Thus, the surrounding step can
        label the result with the centred slice of ``locations``.  ``NaN`` is
        propagated by every window that touches it.
    """

    array = _as_float_matrix(values, name="values")
    axis = _as_float_vector(locations, name="locations")
    if axis.size != array.shape[1]:
        raise ValueError(
            "locations must have the same number of entries as values has channels"
        )

    width, order, derivative = _sg_parameters(axis.size, window, polyorder, deriv)

    half = (width - 1) // 2
    offsets = np.arange(-half, half + 1, dtype=np.float64)
    powers = np.arange(order + 1, dtype=np.float64)
    basis = offsets[:, None] ** powers[None, :]

    # This is the same least-squares construction used by prospectr: each row
    # of ``operator`` contains the coefficients for one polynomial derivative.
    operator = np.linalg.solve(basis.T @ basis, basis.T)
    coefficients = math.factorial(derivative) * operator[derivative]

    windows = np.lib.stride_tricks.sliding_window_view(
        array,
        window_shape=width,
        axis=1,
    )
    # windows has shape (samples, output_channels, width).  einsum keeps the
    # row/channel orientation explicit and naturally propagates NaN values.
    return np.einsum("sow,w->so", windows, coefficients, optimize=True)
