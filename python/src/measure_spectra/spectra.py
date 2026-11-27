"""Spectra on a shared, ordered grid of measurement locations."""

from __future__ import annotations

import math
import numbers
from collections.abc import Iterable, Mapping
from dataclasses import dataclass, field
from typing import Any
from urllib.parse import quote, unquote

import numpy as np
import pandas as pd
import polars as pl

_FEATURE_PREFIX = "spectra"


def _escape_token(value: str) -> str:
    """Escape a textual feature-name token, including literal underscores."""

    # ``urllib.parse.quote`` always leaves ``_`` unescaped, even with an empty
    # safe set.  We use ``__`` as field separators, so escape it explicitly.
    return quote(value, safe="").replace("_", "%5F")


def _encode_unit(unit: str | None) -> str:
    if unit is None:
        return "n"
    return "s" + _escape_token(unit)


def _decode_unit(token: str) -> str | None:
    if token == "n":
        return None
    if token.startswith("s") and len(token) > 1:
        return unquote(token[1:])
    raise ValueError(f"invalid unit token in feature name: {token!r}")


def _format_location(location: float) -> str:
    """Return a round-trippable, canonical representation of a location."""

    # Normalise negative zero so equivalent physical coordinates have one name.
    if location == 0.0:
        location = 0.0
    return repr(float(location))


def _make_feature_name(
    location: float,
    axis_unit: str | None,
    value_unit: str | None,
) -> str:
    return (
        f"{_FEATURE_PREFIX}__loc={_escape_token(_format_location(location))}"
        f"__axis={_encode_unit(axis_unit)}"
        f"__value={_encode_unit(value_unit)}"
    )


def decode_feature_name(name: str) -> tuple[float, str | None, str | None]:
    """Decode a :class:`Spectra` feature name.

    Parameters
    ----------
    name:
        A name returned by :attr:`Spectra.feature_names` or
        :meth:`Spectra.to_frame`.

    Returns
    -------
    tuple
        ``(location, axis_unit, value_unit)``.  Unit entries are ``None`` when
        the corresponding unit was not supplied at construction time.

    Raises
    ------
    ValueError
        If ``name`` is not in the strict format emitted by this module.
    """

    if not isinstance(name, str):
        raise TypeError("feature name must be a string")

    parts = name.split("__")
    if len(parts) != 4 or parts[0] != _FEATURE_PREFIX:
        raise ValueError(f"not a Spectra feature name: {name!r}")

    fields: dict[str, str] = {}
    for part in parts[1:]:
        key, separator, value = part.partition("=")
        if not separator or key in fields or key not in {"loc", "axis", "value"}:
            raise ValueError(f"not a Spectra feature name: {name!r}")
        fields[key] = value

    if set(fields) != {"loc", "axis", "value"}:
        raise ValueError(f"not a Spectra feature name: {name!r}")

    try:
        location = float(unquote(fields["loc"]))
    except (TypeError, ValueError) as exc:
        raise ValueError(f"invalid location in feature name: {name!r}") from exc
    if not math.isfinite(location):
        raise ValueError(f"invalid location in feature name: {name!r}")

    axis_unit = _decode_unit(fields["axis"])
    value_unit = _decode_unit(fields["value"])
    if name != _make_feature_name(location, axis_unit, value_unit):
        raise ValueError(f"not a canonical Spectra feature name: {name!r}")
    return location, axis_unit, value_unit


def _as_pandas(
    frame: Any,
    required_columns: Iterable[Any],
    numeric_columns: set[Any],
) -> pd.DataFrame:
    """Convert a supported native frame without using a pyarrow round-trip."""

    if isinstance(frame, pd.DataFrame):
        result = frame.copy(deep=False)
    elif isinstance(frame, pl.DataFrame):
        # Materialize only the referenced columns and keep declared numeric
        # measurements numeric even when every value is null. Sample labels
        # retain their original scalar values.
        needed = set(required_columns)
        present = [name for name in frame.columns if name in needed]
        selected = frame.select(present)
        result = pd.DataFrame(
            {
                name: column.cast(pl.Float64).to_numpy()
                if name in numeric_columns and column.dtype.is_numeric()
                else column.to_list()
                for name, column in selected.to_dict().items()
            }
        )
    else:
        supported = "pandas.DataFrame or polars.DataFrame"
        raise TypeError(f"frame must be a {supported}")

    if not result.columns.is_unique:
        duplicates = result.columns[result.columns.duplicated()].tolist()
        raise ValueError(
            f"duplicate frame column names are not allowed: {duplicates!r}"
        )
    return result


def _numeric_series(frame: pd.DataFrame, column: Any, role: str) -> np.ndarray:
    series = frame[column]
    dtype = series.dtype
    if (
        not pd.api.types.is_numeric_dtype(dtype)
        or pd.api.types.is_bool_dtype(dtype)
        or pd.api.types.is_complex_dtype(dtype)
    ):
        raise TypeError(f"{role} column {column!r} must be numeric")
    try:
        result = series.to_numpy(dtype=float, na_value=np.nan)
    except TypeError:
        # ``na_value`` is not accepted by a few older pandas extension arrays.
        result = np.asarray(series, dtype=float)
    result = np.asarray(result, dtype=float)
    if np.isinf(result).any():
        raise ValueError(f"{role} column {column!r} must not contain infinite values")
    return result


def _validate_coordinates(
    values: Any,
    source: str,
    *,
    unique: bool = True,
) -> np.ndarray:
    """Validate and normalize coordinates for every construction path."""

    try:
        raw_values = list(values)
    except TypeError as exc:
        raise TypeError(f"{source} locations must be an ordered collection") from exc
    if not raw_values:
        raise ValueError(f"{source} must contain at least one channel")

    converted: list[float] = []
    for value in raw_values:
        if isinstance(value, (bool, np.bool_)):
            raise TypeError(f"{source} locations must be numeric, not boolean")
        if isinstance(value, (complex, np.complexfloating)):
            raise TypeError(f"{source} locations must be real numeric values")
        if not isinstance(value, numbers.Number):
            raise TypeError(f"{source} locations must be numeric")
        try:
            converted_value = float(value)
        except (TypeError, ValueError, OverflowError) as exc:
            raise TypeError(f"{source} locations must be numeric") from exc
        if not math.isfinite(converted_value):
            raise ValueError(f"{source} locations must be finite")
        converted.append(0.0 if converted_value == 0.0 else converted_value)

    result = np.asarray(converted, dtype=float)
    if unique and len(set(converted)) != len(converted):
        raise ValueError(
            f"{source} locations must have unique physical location values"
        )
    return result


def _validate_units(axis_unit: str | None, value_unit: str | None) -> None:
    for name, unit in (("axis_unit", axis_unit), ("value_unit", value_unit)):
        if unit is not None and (not isinstance(unit, str) or not unit):
            raise TypeError(f"{name} must be a non-empty string or None")


def _is_missing_identifier(identifier: Any) -> bool:
    try:
        missing = pd.isna(identifier)
    except (TypeError, ValueError):
        return False
    return isinstance(missing, (bool, np.bool_)) and bool(missing)


def _as_sample_id_array(values: Any) -> np.ndarray:
    """Store sample identifiers in a guaranteed 1D object array (including tuples)."""

    if isinstance(values, np.ndarray):
        if values.ndim != 1:
            raise ValueError("sample_ids must have one entry for every values row")
        items = list(values)
    elif isinstance(values, (str, bytes)):
        raise ValueError("sample_ids must have one entry for every values row")
    else:
        try:
            items = list(values)
        except TypeError as exc:
            raise ValueError(
                "sample_ids must have one entry for every values row"
            ) from exc

    result = np.empty(len(items), dtype=object)
    result[:] = items
    return result


def _ordered_groups(values: list[Any]) -> tuple[list[Any], dict[Any, list[int]]]:
    ordered: list[Any] = []
    groups: dict[Any, list[int]] = {}
    for index, identifier in enumerate(values):
        if _is_missing_identifier(identifier):
            raise ValueError("sample identifiers must not be missing")
        try:
            hash(identifier)
        except TypeError as exc:
            raise TypeError(
                "sample identifiers must be hashable scalar labels"
            ) from exc
        if identifier not in groups:
            groups[identifier] = []
            ordered.append(identifier)
        groups[identifier].append(index)
    return ordered, groups


def _check_sample_grid(
    coordinates: np.ndarray,
    sample_id: Any,
    *,
    reference: np.ndarray | None = None,
) -> None:
    if len(coordinates) == 0:
        raise ValueError(f"sample {sample_id!r} has an empty location grid")
    if len(set(coordinates.tolist())) != len(coordinates):
        raise ValueError(f"sample {sample_id!r} has duplicate location values")
    if reference is not None and (
        len(coordinates) != len(reference) or not np.array_equal(coordinates, reference)
    ):
        raise ValueError(
            "long-form spectra must use one shared location grid in identical "
            f"order; sample {sample_id!r} does not match the first sample"
        )


@dataclass
class Spectra:
    """A batch of numeric spectra on one ordered, shared location grid.

    ``values`` is shaped ``(n_samples, n_locations)`` and follows the order of
    ``sample_ids`` and ``locations``.  The arrays are copied and validated on
    construction.  Use :meth:`from_long` or :meth:`from_wide` at the frame
    boundary so sample and grid rules are applied consistently.

    The ``units`` property is a small mapping with ``"axis"`` and ``"value"``
    keys.  ``axis_unit`` and ``value_unit`` remain available as direct fields.
    """

    locations: np.ndarray
    values: np.ndarray
    sample_ids: np.ndarray
    axis_unit: str | None = None
    value_unit: str | None = None
    _sample_name: str | None = field(default=None, repr=False)

    def __post_init__(self) -> None:
        _validate_units(self.axis_unit, self.value_unit)

        locations = _validate_coordinates(self.locations, "locations")

        raw_values = np.asarray(self.values)
        if raw_values.ndim != 2:
            raise ValueError("values must be a two-dimensional numeric array")
        if raw_values.dtype.kind == "b":
            raise TypeError("values must be numeric, not boolean")
        if raw_values.dtype.kind == "c":
            raise TypeError("values must be real numeric values, not complex")
        if raw_values.dtype.kind == "O":
            for value in raw_values.flat:
                if isinstance(value, (bool, np.bool_)):
                    raise TypeError("values must be numeric, not boolean")
                if not isinstance(value, numbers.Number) or np.iscomplexobj(value):
                    raise TypeError("values must be real numeric values")
        elif raw_values.dtype.kind not in "iuf":
            raise TypeError("values must be a two-dimensional numeric array")
        values = raw_values.astype(float, copy=True)
        if values.shape[1] != len(locations):
            raise ValueError("values must have one column for every physical location")
        if np.isinf(values).any():
            raise ValueError("values must not contain infinite values")

        sample_ids = _as_sample_id_array(self.sample_ids)
        if sample_ids.ndim != 1 or len(sample_ids) != values.shape[0]:
            raise ValueError("sample_ids must have one entry for every values row")
        _ordered_groups(sample_ids.tolist())

        if self._sample_name is not None and not isinstance(self._sample_name, str):
            raise TypeError("sample column name must be a string")

        self.locations = locations.copy()
        self.values = values
        self.sample_ids = sample_ids.copy()

    @classmethod
    def from_long(
        cls,
        frame: Any,
        *,
        sample: str,
        location: str,
        value: str,
        axis_unit: str | None = None,
        value_unit: str | None = None,
    ) -> Spectra:
        """Construct spectra from one row per sample/location observation.

        The first sample encountered in ``frame`` establishes the ordered
        location grid.  Later samples must have the same locations in exactly
        that order.  This preserves descending and irregular grids while
        making an accidental row shuffle explicit instead of silently changing
        the correspondence between locations and values.
        """

        if (
            not isinstance(sample, str)
            or not isinstance(location, str)
            or not isinstance(value, str)
        ):
            raise TypeError("sample, location, and value must be column-name strings")
        data = _as_pandas(frame, {sample, location, value}, {location, value})
        if sample not in data.columns:
            raise KeyError(f"sample column {sample!r} was not found in frame")
        if location not in data.columns:
            raise KeyError(f"location column {location!r} was not found in frame")
        if value not in data.columns:
            raise KeyError(f"value column {value!r} was not found in frame")
        if sample == location or sample == value or location == value:
            raise ValueError("sample, location, and value columns must be distinct")
        if len(data) == 0:
            raise ValueError("long-form spectra require at least one observation")

        coordinates = _numeric_series(data, location, "location")
        if np.isnan(coordinates).any():
            raise ValueError("location column must not contain missing values")
        coordinates = _validate_coordinates(coordinates, "location", unique=False)
        responses = _numeric_series(data, value, "value")
        sample_values = data[sample].tolist()
        sample_ids, groups = _ordered_groups(sample_values)

        first_indices = groups[sample_ids[0]]
        grid = coordinates[first_indices]
        _check_sample_grid(grid, sample_ids[0])
        rows: list[np.ndarray] = []
        for sample_id in sample_ids:
            indices = groups[sample_id]
            sample_grid = coordinates[indices]
            _check_sample_grid(sample_grid, sample_id, reference=grid)
            rows.append(responses[indices])

        values = np.vstack(rows)
        return cls(
            grid,
            values,
            _as_sample_id_array(sample_ids),
            axis_unit,
            value_unit,
            _sample_name=sample,
        )

    @classmethod
    def from_wide(
        cls,
        frame: Any,
        *,
        sample: str,
        channels: Mapping[Any, float],
        axis_unit: str | None = None,
        value_unit: str | None = None,
    ) -> Spectra:
        """Construct spectra from one scalar column per physical location.

        ``channels`` is an ordered mapping from frame column name to physical
        location.  Mapping order is retained as the spectral axis order; the
        input frame's column order does not matter.  Every input row becomes a
        spectrum and rows are never sorted or grouped by ``sample``.
        """

        if not isinstance(sample, str):
            raise TypeError("sample must be a column-name string")
        if not isinstance(channels, Mapping):
            raise TypeError("channels must be a mapping from column names to locations")
        channel_names = list(channels.keys())
        grid = _validate_coordinates(list(channels.values()), "channels")

        data = _as_pandas(frame, {sample, *channel_names}, set(channels))
        if sample not in data.columns:
            raise KeyError(f"sample column {sample!r} was not found in frame")
        for channel in channel_names:
            if channel not in data.columns:
                raise KeyError(f"channel column {channel!r} was not found in frame")
        if sample in channel_names:
            raise ValueError("sample column must not also be a spectral channel")

        sample_ids = data[sample].tolist()
        columns = [
            _numeric_series(data, channel, "channel") for channel in channel_names
        ]
        values = np.column_stack(columns)
        return cls(
            grid,
            values,
            _as_sample_id_array(sample_ids),
            axis_unit,
            value_unit,
            _sample_name=sample,
        )

    @property
    def feature_names(self) -> list[str]:
        """Names encoding each location and the two physical units."""

        return [
            _make_feature_name(location, self.axis_unit, self.value_unit)
            for location in self.locations
        ]

    @property
    def units(self) -> dict[str, str | None]:
        """Return ``{"axis": axis_unit, "value": value_unit}``."""

        return {"axis": self.axis_unit, "value": self.value_unit}

    def to_frame(self) -> pd.DataFrame:
        """Return only numeric spectral features, indexed by sample labels."""

        index = pd.Index(
            self.sample_ids.copy(), name=self._sample_name, tupleize_cols=False
        )
        return pd.DataFrame(self.values.copy(), index=index, columns=self.feature_names)

    def __len__(self) -> int:
        return self.values.shape[0]


__all__ = ["Spectra", "decode_feature_name"]
