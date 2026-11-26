"""Tests for the public ``mlfeatures`` spectroscopy adapters."""

from __future__ import annotations

from decimal import Decimal

import numpy as np
import pandas as pd
import polars as pl
import pytest
from measure_spectra import Spectra, msc, msc_reference
from measure_spectra.steps import StepMSC, StepSavitzkyGolay, StepSNV
from mlfeatures import FittedRecipe, Recipe, RecipeTransformer, Selectors, StepError
from mlfeatures.testing import StepConformanceTest
from sklearn.linear_model import Ridge
from sklearn.pipeline import make_pipeline


def _frame(
    *,
    channels: int = 7,
    extra: bool = False,
    offset=0.0,
    axis_unit="nm",
    value_unit="AU",
) -> pd.DataFrame:
    rows = pd.DataFrame(
        {
            "sample": ["s3", "s1", "s2"],
            **{
                f"x{i}": np.asarray([i + 1.0, i + 2.0, i + 3.0])
                + np.asarray([0.0, 0.25, -0.1])
                for i in range(channels)
            },
        }
    )
    channel_map = {f"x{i}": 100.0 + offset + i for i in range(channels)}
    if extra:
        rows["x-extra"] = [1.0, 2.0, 3.0]
        channel_map["x-extra"] = 100.0 + channels + 1.0
    return Spectra.from_wide(
        rows,
        sample="sample",
        channels=channel_map,
        axis_unit=axis_unit,
        value_unit=value_unit,
    ).to_frame()


def test_snv_replaces_selected_features_and_preserves_other_columns_and_index():
    frame = _frame()
    names = list(frame.columns)
    with_id = frame.assign(batch=["b", "a", "b"])
    recipe = Recipe().add(StepSNV(names))

    result = recipe.fit(with_id).transform(with_id)

    assert result.columns.tolist() == [*names, "batch"]
    assert result.index.tolist() == ["s3", "s1", "s2"]
    assert result["batch"].tolist() == ["b", "a", "b"]
    np.testing.assert_allclose(result[names].mean(axis=1), 0.0, atol=1e-12)


def test_msc_uses_training_reference_and_round_trips_through_registered_json():
    frame = _frame()
    names = list(frame.columns)
    fitted = Recipe().add(StepMSC(names)).fit(frame.iloc[:2])

    before = fitted.transform(frame.iloc[2:])
    restored = FittedRecipe.from_json(fitted.to_json())
    after = restored.transform(frame.iloc[2:])

    np.testing.assert_allclose(
        after, msc(frame.iloc[2:].to_numpy(), msc_reference(frame.iloc[:2].to_numpy()))
    )
    pd.testing.assert_frame_equal(after, before)
    assert restored.steps[0].id == fitted.steps[0].id


def test_savitzky_golay_trims_edges_and_dynamic_selector_can_follow_it():
    frame = _frame()
    names = list(frame.columns)
    recipe = (
        Recipe()
        .add(StepSavitzkyGolay(names))
        .add(StepSNV(Selectors.numeric_features()))
    )

    result = recipe.fit(frame).transform(frame)
    expected = names[2:-2]

    assert result.columns.tolist() == expected
    np.testing.assert_allclose(result[expected].mean(axis=1), 0.0, atol=1e-12)


def test_savitzky_golay_selected_subset_of_a_larger_grid_keeps_unselected_channels():
    frame = _frame()
    names = list(frame.columns)
    selected = names[:6]
    fitted = Recipe().add(StepSavitzkyGolay(selected)).fit(frame)

    result = fitted.transform(frame)

    assert result.columns.tolist() == [*names[2:4], *names[6:]]


@pytest.mark.parametrize("kind", [StepSNV, StepMSC, StepSavitzkyGolay])
def test_fitted_grid_rejects_reordered_or_shifted_channels(kind):
    frame = _frame()
    names = list(frame.columns)
    kwargs = {} if kind is not StepSavitzkyGolay else {"window": 5, "polyorder": 2}
    fitted = Recipe().add(kind(names, **kwargs)).fit(frame)

    reordered = frame[names[::-1]]
    with pytest.raises(StepError) as reordered_error:
        fitted.transform(reordered)
    assert isinstance(reordered_error.value.__cause__, ValueError)
    assert "ordering" in str(reordered_error.value.__cause__)

    shifted = _frame(offset=0.5)
    with pytest.raises(ValueError, match="missing|grid|schema"):
        fitted.transform(shifted)


def test_fitted_grid_rejects_a_new_same_unit_channel():
    frame = _frame()
    names = list(frame.columns)
    fitted = Recipe().add(StepSNV(names)).fit(frame)
    with_extra = _frame(extra=True)

    with pytest.raises(StepError) as extra_error:
        fitted.transform(with_extra)
    assert isinstance(extra_error.value.__cause__, ValueError)
    assert "additional spectral channel" in str(extra_error.value.__cause__)


def test_steps_work_inside_sklearn_recipe_transformer():
    frame = _frame()
    names = list(frame.columns)
    transformer = RecipeTransformer(Recipe().add(StepSNV(names))).fit(frame)

    result = transformer.transform(frame)

    assert result.index.tolist() == frame.index.tolist()
    assert transformer.get_feature_names_out().tolist() == names


@pytest.mark.parametrize(
    "changed", [{"axis_unit": "um"}, {"value_unit": "transmittance"}]
)
def test_measurement_unit_change_is_rejected(changed):
    frame = _frame()
    fitted = Recipe().add(StepMSC(list(frame.columns))).fit(frame)
    with pytest.raises(ValueError, match="missing|grid|schema"):
        fitted.transform(_frame(**changed))


def test_polars_derivative_round_trip_matches_pandas():
    pandas_frame = _frame()
    polars_frame = pl.DataFrame(pandas_frame.to_dict("list"))
    spec = Recipe().add(
        StepSavitzkyGolay(list(polars_frame.columns), window=3, deriv=1)
    )
    expected = spec.fit(pandas_frame).transform(pandas_frame)
    restored = FittedRecipe.from_json(spec.fit(polars_frame).to_json())
    observed = restored.transform(polars_frame)
    assert isinstance(observed, pl.DataFrame)
    assert observed.columns == list(expected.columns)
    np.testing.assert_allclose(observed.to_numpy(), expected.to_numpy())


def test_supervised_pipeline_predicts_held_out_spectrum():
    frame = _frame()
    training, held_out = frame.iloc[:2], frame.iloc[2:]
    target = pd.Series([2.0, 3.0], index=training.index, name="concentration")
    recipe = Recipe(
        data=training.assign(concentration=target), target="concentration"
    ).add(StepMSC(list(training.columns)))
    pipeline = make_pipeline(RecipeTransformer(recipe), Ridge())
    prediction = pipeline.fit(training, target).predict(held_out)
    assert prediction.shape == (1,)
    assert np.isfinite(prediction).all()


def test_savitzky_golay_removes_dropped_boundary_columns_from_roles():
    frame = _frame()
    names = list(frame.columns)
    fitted = Recipe().add(StepSavitzkyGolay(names, window=5)).fit(frame)

    expected = names[2:-2]
    assert fitted.roles.columns_with_role("feature") == expected
    assert fitted.roles.all_columns() == expected
    for dropped in (*names[:2], *names[-2:]):
        assert fitted.roles.of(dropped) == set()


def test_steps_handle_nullable_pandas_and_extended_polars_numeric_dtypes():
    base = _frame()
    names = list(base.columns)

    # Pandas nullable integer/float dtypes with missing values for SNV and SG.
    nullable_pd = pd.DataFrame(
        {
            name: pd.Series(
                [
                    int(round(v * 10)) if i != 2 or j != 0 else pd.NA
                    for j, v in enumerate(base[name])
                ],
                dtype="Int64",
            )
            for i, name in enumerate(names)
        }
    )
    snv_out = Recipe().add(StepSNV(names)).fit(nullable_pd).transform(nullable_pd)
    assert np.isnan(snv_out.iloc[0, 2])
    assert np.isfinite(snv_out.iloc[1:].to_numpy()).all()

    sg_out = (
        Recipe()
        .add(StepSavitzkyGolay(names, window=5))
        .fit(nullable_pd)
        .transform(nullable_pd)
    )
    assert np.isnan(sg_out.iloc[0]).all()
    assert np.isfinite(sg_out.iloc[1:].to_numpy()).all()

    # Complete nullable pandas Int64 for MSC.
    complete_nullable_pd = pd.DataFrame(
        {
            name: pd.Series([int(round(v * 10)) for v in base[name]], dtype="Int64")
            for name in names
        }
    )
    msc_pd = (
        Recipe()
        .add(StepMSC(names))
        .fit(complete_nullable_pd)
        .transform(complete_nullable_pd)
    )
    assert np.isfinite(msc_pd.to_numpy()).all()

    # Polars Int128 and Decimal(10, 2) columns (both valid mlfeatures NUMERIC_DTYPES).
    polars_extended = pl.DataFrame(
        {
            name: (
                pl.Series(
                    [int(round(v * 100)) for v in base[name]],
                    dtype=pl.Int128,
                )
                if i % 2 == 0
                else pl.Series(
                    [Decimal(f"{v:.2f}") for v in base[name]],
                    dtype=pl.Decimal(10, 2),
                )
            )
            for i, name in enumerate(names)
        }
    )
    for step in (
        StepSNV(names),
        StepMSC(names),
        StepSavitzkyGolay(names, window=5),
    ):
        transformed = Recipe().add(step).fit(polars_extended).transform(polars_extended)
        assert isinstance(transformed, pl.DataFrame)
        assert np.isfinite(transformed.to_numpy()).all()


_CONFORMANCE_FRAME = _frame()
_CONFORMANCE_CHANNELS = list(_CONFORMANCE_FRAME.columns)
_CONFORMANCE_DATA = {
    **_CONFORMANCE_FRAME.to_dict("list"),
    "batch": ["a", "b", "a"],
}


class TestStepSNVConformance(StepConformanceTest):
    training_data = _CONFORMANCE_DATA
    estimates_attr = None

    def make_step(self):
        return StepSNV(_CONFORMANCE_CHANNELS)


class TestStepMSCConformance(StepConformanceTest):
    training_data = _CONFORMANCE_DATA

    def make_step(self):
        return StepMSC(_CONFORMANCE_CHANNELS)


class TestStepSavitzkyGolayConformance(StepConformanceTest):
    training_data = _CONFORMANCE_DATA
    estimates_attr = None

    def make_step(self):
        return StepSavitzkyGolay(_CONFORMANCE_CHANNELS, window=5, polyorder=2)
