from __future__ import annotations

import numpy as np
import pandas as pd
import polars as pl
import pytest

from measure_spectra.spectra import Spectra, decode_feature_name


def test_long_and_wide_have_the_same_grid_and_feature_names() -> None:
    long = pd.DataFrame(
        {
            "sample_id": ["b", "b", "a", "a"],
            "wavelength": [10.5, 9.0, 10.5, 9.0],
            "absorbance": [4.0, 3.0, 2.0, 1.0],
        }
    )
    wide = pd.DataFrame(
        {
            "sample_id": ["b", "a"],
            "x_10_5": [4.0, 2.0],
            "x_9": [3.0, 1.0],
        }
    )

    from_long = Spectra.from_long(
        long,
        sample="sample_id",
        location="wavelength",
        value="absorbance",
        axis_unit="nm",
        value_unit="AU",
    )
    from_wide = Spectra.from_wide(
        wide,
        sample="sample_id",
        channels={"x_10_5": 10.5, "x_9": 9.0},
        axis_unit="nm",
        value_unit="AU",
    )

    np.testing.assert_array_equal(from_long.locations, [10.5, 9.0])
    np.testing.assert_array_equal(from_long.values, [[4.0, 3.0], [2.0, 1.0]])
    np.testing.assert_array_equal(from_long.sample_ids, ["b", "a"])
    assert from_long.feature_names == from_wide.feature_names
    pd.testing.assert_frame_equal(from_long.to_frame(), from_wide.to_frame())


def test_feature_names_round_trip_coordinate_and_units() -> None:
    spectra = Spectra.from_wide(
        pd.DataFrame({"id": [1], "x": [2.5]}),
        sample="id",
        channels={"x": 2.5},
        axis_unit="wavenumber / cm^-1",
        value_unit="abs__unit",
    )

    assert decode_feature_name(spectra.feature_names[0]) == (
        2.5,
        "wavenumber / cm^-1",
        "abs__unit",
    )


def test_long_preserves_descending_nonuniform_grid_and_rejects_reordering() -> None:
    frame = pd.DataFrame(
        {
            "id": ["first", "first", "first", "second", "second", "second"],
            "x": [5.0, 2.5, 1.0, 5.0, 2.5, 1.0],
            "y": [1, 2, 3, 4, 5, 6],
        }
    )
    spectra = Spectra.from_long(frame, sample="id", location="x", value="y")
    np.testing.assert_array_equal(spectra.locations, [5.0, 2.5, 1.0])

    reordered = frame.copy()
    reordered.loc[3:, "x"] = [1.0, 2.5, 5.0]
    with pytest.raises(ValueError, match="shared location grid"):
        Spectra.from_long(reordered, sample="id", location="x", value="y")


def test_long_rejects_duplicate_locations_and_missing_points() -> None:
    duplicate = pd.DataFrame({"id": ["a", "a"], "x": [1.0, 1.0], "y": [2.0, 3.0]})
    with pytest.raises(ValueError, match="duplicate location"):
        Spectra.from_long(duplicate, sample="id", location="x", value="y")

    mismatch = pd.DataFrame(
        {
            "id": ["a", "a", "b"],
            "x": [1.0, 2.0, 1.0],
            "y": [2.0, 3.0, 4.0],
        }
    )
    with pytest.raises(ValueError, match="shared location grid"):
        Spectra.from_long(mismatch, sample="id", location="x", value="y")


def test_wide_preserves_row_order_and_channel_order() -> None:
    frame = pd.DataFrame(
        {
            "id": ["z", "z", "a"],
            "right": [3.0, 4.0, 5.0],
            "left": [1.0, 2.0, 6.0],
        }
    )
    spectra = Spectra.from_wide(
        frame,
        sample="id",
        channels={"right": 20.0, "left": 10.0},
    )
    np.testing.assert_array_equal(spectra.sample_ids, ["z", "z", "a"])
    np.testing.assert_array_equal(spectra.locations, [20.0, 10.0])
    np.testing.assert_array_equal(spectra.values, [[3.0, 1.0], [4.0, 2.0], [5.0, 6.0]])


def test_wide_rejects_duplicate_channel_locations_and_non_numeric_columns() -> None:
    frame = pd.DataFrame({"id": ["a"], "x": [1.0], "y": [2.0]})
    with pytest.raises(ValueError, match="unique physical location"):
        Spectra.from_wide(
            frame,
            sample="id",
            channels={"x": 1.0, "y": 1.0},
        )

    bad = pd.DataFrame({"id": ["a"], "x": ["not numeric"]})
    with pytest.raises(TypeError, match="numeric"):
        Spectra.from_wide(bad, sample="id", channels={"x": 1.0})


def test_polars_input_is_supported_without_requiring_arrow() -> None:
    frame = pl.DataFrame(
        {
            "id": ["b", "b", "a", "a"],
            "x": [2.0, 1.0, 2.0, 1.0],
            "y": [4.0, 3.0, 2.0, 1.0],
        }
    )
    spectra = Spectra.from_long(frame, sample="id", location="x", value="y")
    np.testing.assert_array_equal(spectra.sample_ids, ["b", "a"])
    np.testing.assert_array_equal(spectra.values, [[4.0, 3.0], [2.0, 1.0]])


def test_units_and_grid_are_part_of_feature_names() -> None:
    frame = pd.DataFrame({"id": ["a"], "x": [1.0], "y": [2.0]})
    base = Spectra.from_long(
        frame,
        sample="id",
        location="x",
        value="y",
        axis_unit="nm",
        value_unit="AU",
    )
    shifted = Spectra.from_long(
        frame.assign(x=[1.1]),
        sample="id",
        location="x",
        value="y",
        axis_unit="nm",
        value_unit="AU",
    )
    changed_unit = Spectra.from_long(
        frame,
        sample="id",
        location="x",
        value="y",
        axis_unit="um",
        value_unit="AU",
    )
    assert base.feature_names != shifted.feature_names
    assert base.feature_names != changed_unit.feature_names


def test_to_frame_contains_only_numeric_features_and_uses_sample_index() -> None:
    spectra = Spectra.from_wide(
        pd.DataFrame({"sample": ["one", "two"], "x": [1, 2], "y": [3, 4]}),
        sample="sample",
        channels={"x": 100.0, "y": 101.0},
    )
    result = spectra.to_frame()
    assert result.index.tolist() == ["one", "two"]
    assert result.index.name == "sample"
    assert result.columns.tolist() == spectra.feature_names
    assert all(pd.api.types.is_numeric_dtype(dtype) for dtype in result.dtypes)
    assert "sample" not in result.columns


@pytest.mark.parametrize("locations", [[True, 2.0], [1.0 + 0.0j, 2.0]])
def test_direct_constructor_rejects_lossy_coordinate_types(locations) -> None:
    with pytest.raises(TypeError, match="locations"):
        Spectra(locations, [[1.0, 2.0]], ["a"])


def test_direct_constructor_rejects_numeric_string_values() -> None:
    with pytest.raises(TypeError, match="numeric"):
        Spectra([1.0, 2.0], [["1", "2"]], ["a"])


def test_all_missing_numeric_signal_in_polars_long_and_wide_tables():
    frame = pl.DataFrame(
        {
            "id": [2**53 + 1, 2**53 + 3],
            "nm": [900, 900],
            "signal": pl.Series([None, None], dtype=pl.Float64),
        }
    )
    wide = Spectra.from_wide(frame, sample="id", channels={"signal": 900})
    long = Spectra.from_long(frame, sample="id", location="nm", value="signal")
    pd.testing.assert_frame_equal(wide.to_frame(), long.to_frame())
    assert list(wide.sample_ids) == [2**53 + 1, 2**53 + 3]
    assert np.isnan(wide.values).all()


def test_tuple_sample_ids_work_across_constructors_and_to_frame() -> None:
    ids = [("batch-1", 1), ("batch-1", 2)]
    direct = Spectra([900.0, 905.0], [[1.0, 2.0], [3.0, 4.0]], ids)
    assert direct.sample_ids.ndim == 1
    assert direct.sample_ids.tolist() == ids

    wide = pd.DataFrame({"sample": ids, "c1": [1.0, 3.0], "c2": [2.0, 4.0]})
    from_wide = Spectra.from_wide(
        wide, sample="sample", channels={"c1": 900.0, "c2": 905.0}
    )
    long = pd.DataFrame(
        {
            "sample": [ids[0], ids[0], ids[1], ids[1]],
            "nm": [900.0, 905.0, 900.0, 905.0],
            "val": [1.0, 2.0, 3.0, 4.0],
        }
    )
    from_long = Spectra.from_long(long, sample="sample", location="nm", value="val")

    frame = from_wide.to_frame()
    assert not isinstance(frame.index, pd.MultiIndex)
    assert frame.index.name == "sample"
    assert frame.index.tolist() == ids
    pd.testing.assert_frame_equal(from_long.to_frame(), frame)


def test_polars_ingestion_ignores_unused_nested_columns() -> None:
    wide_pl = pl.DataFrame(
        {
            "id": ["s1", "s2"],
            "c1": [1.0, 2.0],
            "c2": [3.0, 4.0],
            "tags": [["a", "b"], ["c"]],
            "meta": [{"k": 1}, {"k": 2}],
        }
    )
    spectra = Spectra.from_wide(wide_pl, sample="id", channels={"c1": 100, "c2": 105})
    np.testing.assert_array_equal(spectra.values, [[1.0, 3.0], [2.0, 4.0]])

    long_pl = pl.DataFrame(
        {
            "id": ["s1", "s1", "s2", "s2"],
            "nm": [100.0, 105.0, 100.0, 105.0],
            "val": [1.0, 3.0, 2.0, 4.0],
            "nested": [[1, 2], [3], [4, 5], []],
        }
    )
    from_long = Spectra.from_long(long_pl, sample="id", location="nm", value="val")
    pd.testing.assert_frame_equal(from_long.to_frame(), spectra.to_frame())
