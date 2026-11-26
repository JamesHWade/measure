"""Numerical parity checks against the R ``measure`` implementation."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest
from measure_spectra.signals import msc, msc_reference, savitzky_golay, snv

FIXTURE = Path(__file__).with_name("fixtures") / "spectra_parity.json"


def _load_fixture() -> dict[str, object]:
    with FIXTURE.open(encoding="utf-8") as stream:
        return json.load(stream)


def _array(value: object) -> np.ndarray:
    """Convert stored values, including missing markers, to float arrays."""

    return np.asarray(value, dtype=float)


def _assert_r_close(actual: object, expected: object) -> None:
    # R and NumPy use the same IEEE-754 arithmetic here, but R's QR/linear
    # algebra path can differ by a few ulps from NumPy's implementation.
    np.testing.assert_allclose(
        _array(actual),
        _array(expected),
        rtol=3e-12,
        atol=3e-12,
        equal_nan=True,
    )


def test_snv_matches_r_for_matrix_constant_and_missing() -> None:
    fixture = _load_fixture()
    train = _array(fixture["train"])
    held_out = _array(fixture["held_out"])
    constant = _array(fixture["constant"])[None, :]
    missing = _array(fixture["missing"])
    expected = fixture["snv"]

    # The matrix path is row-wise. The training matrix includes an NA to pin
    # the measure implementation's na.rm=TRUE policy.
    _assert_r_close(snv(train), expected["train"])
    _assert_r_close(snv(held_out), expected["held_out"])

    # R returns centered values for a zero/undefined standard deviation.
    _assert_r_close(snv(constant), expected["constant"])

    # Missing points stay missing while the mean and sample SD use observed
    # points only.
    _assert_r_close(snv(missing[None, :]), expected["missing"])


def test_msc_reference_and_correction_match_r_for_matrix_and_constant() -> None:
    fixture = _load_fixture()
    train = _array(fixture["train"])
    held_out = _array(fixture["held_out"])
    constant = _array(fixture["constant"])[None, :]
    expected = fixture["msc"]

    # msc_reference is column-wise and ignores the one missing training point,
    # exactly as measure's colMeans(..., na.rm = TRUE) does.
    reference = msc_reference(train)
    _assert_r_close(reference, expected["reference"])

    _assert_r_close(msc(held_out, reference), expected["held_out"])

    # A constant response has a zero multiplicative coefficient; R's fallback
    # returns centered values rather than dividing by zero.
    _assert_r_close(msc(constant, reference), expected["constant"])

    # Unlike SNV and SG, R's per-spectrum lm.fit does not accept a missing
    # response.  The Python kernel makes this boundary explicit rather than
    # silently returning an untrustworthy correction.
    with pytest.raises(
        (ValueError, FloatingPointError), match="(?i)missing|finite|nan"
    ):
        msc(_array(fixture["missing"])[None, :], reference)


@pytest.mark.parametrize(
    ("deriv", "expected_key"),
    ((0, "smooth"), (1, "first_derivative"), (2, "second_derivative")),
)
def test_savitzky_golay_matches_r_derivatives_and_trims_boundaries(
    deriv: int, expected_key: str
) -> None:
    fixture = _load_fixture()
    sg = fixture["sg"]
    values = _array(sg["values"])
    locations = _array(sg["locations"])

    actual_values = _array(
        savitzky_golay(
            values,
            locations,
            window=int(fixture["schema"]["sg_window"]),
            polyorder=int(fixture["schema"]["sg_polyorder"]),
            deriv=deriv,
        )
    )
    _assert_r_close(actual_values, sg[expected_key])
    assert actual_values.shape == (values.shape[0], values.shape[1] - 4)


def test_savitzky_golay_missing_window_policy_matches_r() -> None:
    fixture = _load_fixture()
    sg = fixture["sg"]
    locations = _array(sg["locations"])

    # A complete-window convolution propagates a missing point to every
    # output window that contains it.  The third row's missing center lies in
    # all five windows for this nine-point, five-channel fixture.
    missing_values = _array(
        savitzky_golay(
            _array(sg["missing_values"]),
            locations,
            window=5,
            polyorder=2,
            deriv=0,
        )
    )
    _assert_r_close(missing_values, sg["missing_smooth"])
