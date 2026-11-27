"""mlfeatures recipe steps for measurements on a shared spectral grid."""

from __future__ import annotations

import narwhals.stable.v2 as nw
import numpy as np
from mlfeatures import (
    NUMERIC_DTYPES,
    ColSchema,
    Frame,
    Mode,
    Roles,
    SchemaInfo,
    Selector,
    Step,
)

from .signals import _sg_parameters, msc, msc_reference, savitzky_golay, snv
from .spectra import decode_feature_name


def _block_to_float_matrix(block: Frame) -> np.ndarray:
    """Convert a selected numeric frame block to a float64 matrix with NaN nulls."""

    casted = block.select(nw.all().cast(nw.Float64))
    if casted.implementation.is_pandas():
        return casted.to_native().to_numpy(dtype=np.float64, na_value=np.nan)
    return casted.to_numpy()


class _SpectralStep(Step):
    mode = Mode.ONE_TO_ONE

    def __init__(self, columns: Selector, id: str | None, default_id: str):
        self.columns = columns
        super().__init__(id, default_id)

    def fit(self, training_data: Frame, info: SchemaInfo):
        selected = set(self.resolve_selection(training_data, info))
        # The frame preserves acquisition order, including descending axes.
        names = [name for name in training_data.columns if name in selected]
        if not names:
            raise ValueError("Select at least one spectral channel")
        units = {decode_feature_name(name)[1:] for name in names}
        if len(units) != 1:
            raise ValueError("Selected channels must share axis and signal units")

        self._training_columns = list(training_data.columns)
        block = training_data.select(names)
        self._schema_in = ColSchema(
            columns=names,
            dtypes={name: str(dtype) for name, dtype in block.schema.items()},
        )
        outputs = self._fit_channels(names, block)
        self._schema_out = ColSchema(
            columns=outputs, dtypes=dict.fromkeys(outputs, "Float64")
        )
        return self

    def _fit_channels(self, names: list[str], block: Frame) -> list[str]:
        return names

    def transform(self, new_data: Frame) -> Frame:
        names = self.schema_in().columns
        selected = set(names)
        current = [name for name in new_data.columns if name in selected]
        if current != names:
            raise ValueError(
                "Fitted spectral channels are missing or their ordering changed"
            )
        units = decode_feature_name(names[0])[1:]
        for name in set(new_data.columns).difference(self._training_columns):
            try:
                candidate_units = decode_feature_name(name)[1:]
            except ValueError:
                continue  # An ordinary metadata column.
            if candidate_units == units:
                raise ValueError(
                    f"Found an additional spectral channel on the fitted grid: {name!r}"
                )
        block = new_data.select(names)
        if any(type(dtype) not in NUMERIC_DTYPES for dtype in block.schema.values()):
            raise TypeError("Spectral channels must contain numeric values")
        values = self._apply(_block_to_float_matrix(block), names)
        outputs = self.schema_out().columns
        removed = selected.difference(outputs)
        result = new_data.drop(list(removed)) if removed else new_data
        return result.with_columns(
            *[
                nw.new_series(name, values[:, i], backend=result.implementation)
                for i, name in enumerate(outputs)
            ]
        )

    def _apply(self, values: np.ndarray, names: list[str]) -> np.ndarray:
        raise NotImplementedError

    def update_roles(self, roles: Roles) -> None:
        if self._schema_in is None or self._schema_out is None:
            return
        removed = set(self._schema_in.columns).difference(self._schema_out.columns)
        for col in removed:
            for role in list(roles.of(col)):
                roles.remove(col, role)

    def schema_in(self) -> ColSchema:
        if self._schema_in is None:
            raise RuntimeError(
                "Fit the spectral step before requesting its input schema"
            )
        return self._schema_in

    def schema_out(self) -> ColSchema:
        if self._schema_out is None:
            raise RuntimeError(
                "Fit the spectral step before requesting its output schema"
            )
        return self._schema_out

    def accepts_dtypes(self):
        return set(NUMERIC_DTYPES)

    @property
    def produces_sparse(self) -> bool:
        return False


class StepSNV(_SpectralStep):
    """Center and scale each spectrum by its own sample standard deviation."""

    def __init__(self, columns: Selector, *, id: str | None = None):
        super().__init__(columns, id, "measure_snv")

    def _apply(self, values, names):
        return snv(values)


class StepMSC(_SpectralStep):
    """Correct scatter using a reference estimated from the training spectra."""

    def __init__(self, columns: Selector, *, id: str | None = None):
        super().__init__(columns, id, "measure_msc")

    def _fit_channels(self, names, block):
        self._estimates = msc_reference(_block_to_float_matrix(block)).tolist()
        return names

    def _apply(self, values, names):
        return msc(values, self._estimates)


class StepSavitzkyGolay(_SpectralStep):
    """Fit local polynomials and retain the channels centered in a full window.

    Derivatives use unit channel-index spacing. Output names identify the
    original coordinates and source units.
    """

    def __init__(
        self,
        columns: Selector,
        *,
        window: int = 5,
        polyorder: int = 2,
        deriv: int = 0,
        id: str | None = None,
    ):
        self.window = window
        self.polyorder = polyorder
        self.deriv = deriv
        super().__init__(columns, id, "measure_savitzky_golay")

    def _fit_channels(self, names, block):
        width, _, _ = _sg_parameters(
            len(names), self.window, self.polyorder, self.deriv
        )
        edge = width // 2
        return names[edge : len(names) - edge]

    def _apply(self, values, names):
        locations = [decode_feature_name(name)[0] for name in names]
        return savitzky_golay(
            values,
            locations,
            window=self.window,
            polyorder=self.polyorder,
            deriv=self.deriv,
        )
