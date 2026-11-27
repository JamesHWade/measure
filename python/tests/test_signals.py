"""Numerical behavior of the spectroscopy kernels."""

import numpy as np
import pytest

from measure_spectra.signals import msc, msc_reference, savitzky_golay, snv


def test_snv_is_rowwise_sample_standardisation_and_preserves_missing_values():
    values = np.array(
        [
            [1.0, 2.0, 3.0, 4.0],
            [7.0, 7.0, 7.0, 7.0],
            [1.0, np.nan, 3.0, np.nan],
        ]
    )

    result = snv(values)

    expected = np.array(
        [
            [
                -1.161895003862225,
                -0.387298334620742,
                0.387298334620742,
                1.161895003862225,
            ],
            [0.0, 0.0, 0.0, 0.0],
            [-np.sqrt(0.5), np.nan, np.sqrt(0.5), np.nan],
        ]
    )
    np.testing.assert_allclose(result, expected, equal_nan=True)
    np.testing.assert_array_equal(values[0], [1.0, 2.0, 3.0, 4.0])


def test_snv_constant_decimal_rows_do_not_amplify_reduction_roundoff():
    values = np.full((2, 7), 0.1)

    with pytest.warns(RuntimeWarning, match="zero or NA"):
        result = snv(values)

    np.testing.assert_array_equal(result, np.zeros_like(values))


def test_snv_constant_rows_with_leading_missing_channels_are_centered():
    values = np.array([[np.nan, 2.0, 2.0], [np.nan, 2.0, np.nan]])

    with pytest.warns(RuntimeWarning, match="zero or NA"):
        result = snv(values)

    expected = np.array([[np.nan, 0.0, 0.0], [np.nan, 0.0, np.nan]])
    np.testing.assert_allclose(result, expected, equal_nan=True)


def test_msc_reference_is_columnwise_nan_omitting_mean():
    training = np.array(
        [
            [1.0, 2.0, np.nan],
            [3.0, np.nan, 9.0],
            [5.0, 6.0, 12.0],
        ]
    )

    result = msc_reference(training)

    np.testing.assert_allclose(result, [3.0, 4.0, 10.5], equal_nan=True)


def test_msc_recovers_affine_spectra_and_centers_zero_slope_spectra():
    reference = np.arange(1.0, 6.0)
    values = np.vstack(
        [
            2.0 * reference + 1.0,
            -0.5 * reference + 7.0,
            np.full(reference.shape, 4.0),
        ]
    )

    with pytest.warns(RuntimeWarning, match="zero or NA"):
        result = msc(values, reference)

    np.testing.assert_allclose(result[:2], [reference, reference])
    np.testing.assert_allclose(result[2], np.zeros(reference.shape))


def test_msc_rejects_missing_values_like_r_lm_fit():
    reference = np.arange(1.0, 6.0)
    values = np.array([[1.0, 2.0, np.nan, 4.0, 5.0]])

    with pytest.raises(ValueError, match="finite"):
        msc(values, reference)


def test_msc_handles_flat_and_rank_deficient_references_like_r_qr():
    flat_values = np.full((1, 7), 0.1)
    reference = np.arange(1.0, 8.0)
    with pytest.warns(RuntimeWarning, match="zero or NA"):
        flat_result = msc(flat_values, reference)
    np.testing.assert_array_equal(flat_result, np.zeros_like(flat_values))

    # R's lm.fit() drops a reference column whose residual norm is below its
    # 1e-7 rank tolerance.  The fallback is therefore centered, not a huge
    # correction from a numerically tiny slope.
    near_constant_reference = 1.0 + 1e-8 * np.arange(1.0, 9.0)
    values = np.arange(1.0, 9.0)[None, :]
    with pytest.warns(RuntimeWarning, match="zero or NA"):
        result = msc(values, near_constant_reference)
    np.testing.assert_allclose(result[0], values[0] - 4.5)


def test_signal_kernels_reject_lossy_complex_casts():
    with pytest.raises(ValueError, match="real-valued"):
        snv(np.array([[1.0 + 1.0j, 2.0 + 0.0j]]))
    with pytest.raises(ValueError, match="real-valued"):
        msc_reference(np.array([[1.0 + 1.0j, 2.0 + 0.0j]]))
    with pytest.raises(ValueError, match="real-valued"):
        savitzky_golay(
            np.array([[1.0 + 1.0j, 2.0 + 0.0j, 3.0 + 0.0j]]),
            [1.0, 2.0, 3.0],
            window=3,
        )


def test_savitzky_golay_returns_valid_centered_windows_and_uses_unit_index_delta():
    # The R step labels the output with the centered locations but deliberately
    # uses unit index spacing for its derivatives.  The labels here are
    # irregular to make that distinction observable.
    locations = np.array([100.0, 103.0, 107.0, 112.0, 118.0, 125.0, 133.0])
    index = np.arange(locations.size, dtype=float)
    values = np.vstack(
        [
            index**2 + 2.0 * index + 3.0,
            3.0 * index + 4.0,
        ]
    )

    smooth = savitzky_golay(values, locations, window=5, polyorder=2)
    first = savitzky_golay(values, locations, window=5, polyorder=2, deriv=1)
    second = savitzky_golay(values, locations, window=5, polyorder=2, deriv=2)

    assert smooth.shape == (2, 3)
    np.testing.assert_allclose(smooth[0], values[0, 2:5])
    np.testing.assert_allclose(smooth[1], values[1, 2:5])
    np.testing.assert_allclose(first[0], 2.0 * index[2:5] + 2.0)
    np.testing.assert_allclose(first[1], np.full(3, 3.0))
    np.testing.assert_allclose(second[0], np.full(3, 2.0))
    np.testing.assert_allclose(second[1], np.zeros(3), atol=1e-14)


def test_savitzky_golay_propagates_missing_values_only_through_touched_windows():
    locations = np.arange(9.0)
    values = np.vstack([np.arange(9.0), np.arange(9.0) ** 2])
    values[0, 4] = np.nan

    result = savitzky_golay(values, locations, window=5, polyorder=2)

    assert np.isnan(result[0]).all()
    assert np.isfinite(result[1]).all()


@pytest.mark.parametrize(
    ("kwargs", "message"),
    [
        ({"window": 4}, "odd"),
        ({"window": 9}, "lower than"),
        ({"window": 5, "polyorder": 5}, "greater than"),
        ({"window": 5, "polyorder": 1, "deriv": 2}, "greater than or equal"),
    ],
)
def test_savitzky_golay_matches_prospectr_window_rules(kwargs, message):
    values = np.arange(14.0).reshape(2, 7)
    locations = np.arange(7.0)

    with pytest.raises(ValueError, match=message):
        savitzky_golay(values, locations, **kwargs)


def test_savitzky_golay_validates_shape_and_axis():
    values = np.arange(14.0).reshape(2, 7)

    with pytest.raises(ValueError, match="2D"):
        savitzky_golay(values[0], np.arange(7.0))
    with pytest.raises(ValueError, match="same number"):
        savitzky_golay(values, np.arange(6.0))
    with pytest.raises(ValueError, match="finite"):
        savitzky_golay(values, [0.0, 1.0, np.nan, 3.0, 4.0, 5.0, 6.0])


def test_signal_kernels_reject_booleans_and_numeric_strings():
    bool_matrix = np.array([[True, False, True, False]])
    obj_bool_matrix = np.array([[True, 2.0, 3.0, 4.0]], dtype=object)
    str_matrix = np.array([["1.0", "2.0", "3.0", "4.0"]])

    for bad in (bool_matrix, obj_bool_matrix, str_matrix):
        with pytest.raises(ValueError, match="numeric|boolean"):
            snv(bad)
        with pytest.raises(ValueError, match="numeric|boolean"):
            msc_reference(bad)
        with pytest.raises(ValueError, match="numeric|boolean"):
            msc(bad, [1.0, 2.0, 3.0, 4.0])
        with pytest.raises(ValueError, match="numeric|boolean"):
            savitzky_golay(bad, [1.0, 2.0, 3.0, 4.0], window=3)

    with pytest.raises(ValueError, match="numeric|boolean"):
        msc([[1.0, 2.0, 3.0]], [True, False, True])
    with pytest.raises(ValueError, match="numeric|boolean"):
        msc([[1.0, 2.0, 3.0]], ["1.0", "2.0", "3.0"])
    with pytest.raises(ValueError, match="numeric|boolean"):
        savitzky_golay([[1.0, 2.0, 3.0, 4.0]], [True, False, True, False], window=3)
    with pytest.raises(ValueError, match="numeric|boolean"):
        savitzky_golay([[1.0, 2.0, 3.0, 4.0]], ["1", "2", "3", "4"], window=3)


def test_savitzky_golay_first_derivative_propagates_center_nan():
    # For window=5, polyorder=2, deriv=1, the central filter weight is 0.0.
    # A missing point at the center of a window must still propagate NaN.
    locations = np.arange(9.0)
    values = np.array([[1.0, 2.0, 3.0, 4.0, np.nan, 6.0, 7.0, 8.0, 9.0]])

    result = savitzky_golay(values, locations, window=5, polyorder=2, deriv=1)

    assert np.isnan(result[0]).all()


def test_savitzky_golay_wide_window_high_polyorder_stays_well_conditioned():
    # Normal equations (basis.T @ basis) lose accuracy rapidly for wide windows
    # with higher polynomial orders; SVD stays accurate on exact polynomials.
    locations = np.arange(61, dtype=float)
    x = (locations - 30.0) / 30.0
    poly = x**8 - 0.5 * x**5 + 2.0 * x**2 - 3.0 * x + 1.0
    dpoly = (8.0 * x**7 - 2.5 * x**4 + 4.0 * x - 3.0) / 30.0

    smooth = savitzky_golay(poly[None, :], locations, window=51, polyorder=8, deriv=0)
    first = savitzky_golay(poly[None, :], locations, window=51, polyorder=8, deriv=1)

    np.testing.assert_allclose(smooth[0], poly[25:-25], rtol=1e-10, atol=1e-10)
    np.testing.assert_allclose(first[0], dpoly[25:-25], rtol=1e-10, atol=1e-10)
