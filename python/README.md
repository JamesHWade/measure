# Measure for Python

`measure-spectra` preprocesses one-dimensional spectra on a shared measurement grid. It provides standard normal variate (SNV) normalization, multiplicative scatter correction (MSC), and Savitzky–Golay smoothing and derivatives. Use the steps in an mlfeatures recipe or call their numerical functions on a matrix of samples by channels.

## Install from a Measure checkout

With Python 3.10 or later and Git installed, run this from the repository root.

```sh
python -m pip install ./python
```

The package pins the mlfeatures Git revision that provides the third-party step API. Installation fetches that revision from GitHub.

## Fit a correction on training spectra

This example uses synthetic absorbance measurements at five wavelengths. MSC estimates a reference spectrum from the calibration samples. Applying the fitted recipe to a new sample uses that same reference.

```python
import pandas as pd
from mlfeatures import FittedRecipe, Recipe
from measure_spectra import Spectra, StepMSC

readings = pd.DataFrame(
    {
        "sample": ["standard-A", "standard-B", "standard-C", "unknown"],
        "a": [0.10, 0.13, 0.09, 0.11],
        "b": [0.18, 0.24, 0.16, 0.21],
        "c": [0.41, 0.52, 0.38, 0.47],
        "d": [0.28, 0.33, 0.30, 0.32],
        "e": [0.12, 0.15, 0.10, 0.13],
    }
)
spectra = Spectra.from_wide(
    readings,
    sample="sample",
    channels={"a": 900, "b": 905, "c": 910, "d": 915, "e": 920},
    axis_unit="nm",
    value_unit="absorbance",
)
X = spectra.to_frame()
X_train, X_new = X.iloc[:3], X.iloc[3:]
recipe = Recipe().add(StepMSC(list(X.columns)))
fitted = recipe.fit(X_train)
corrected = fitted.transform(X_new)

saved = fitted.to_json()
restored = FittedRecipe.from_json(saved)
pd.testing.assert_frame_equal(restored.transform(X_new), corrected)
```

For a long table with one row per observed sample and wavelength, construct the same representation using the names of the three measurement columns.

```python
long = readings.melt(id_vars="sample", var_name="channel", value_name="signal")
long["wavelength"] = long["channel"].map(
    {"a": 900, "b": 905, "c": 910, "d": 915, "e": 920}
)
from_long = Spectra.from_long(
    long,
    sample="sample",
    location="wavelength",
    value="signal",
    axis_unit="nm",
    value_unit="absorbance",
)
pd.testing.assert_frame_equal(from_long.to_frame(), X)
```

Both constructors accept pandas or Polars tables. The modeling frame uses the sample IDs as its pandas index. Channel names encode the coordinate and the original axis and signal units. Those names identify the source channel through preprocessing. SNV produces dimensionless values, and derivatives change the signal scale. Use the same units and grid at prediction time. Align or resample spectra from different grids before creating a shared-grid matrix.

## Use a scikit-learn pipeline

Place the recipe inside the pipeline so each training fold fits its own preprocessing. Here the target represents a concentration measured separately for each calibration sample.

```python
from mlfeatures import RecipeTransformer
from sklearn.linear_model import Ridge
from sklearn.pipeline import make_pipeline

reference_concentration = pd.Series(
    [1.0, 1.5, 0.8], index=X_train.index, name="concentration"
)
specification = Recipe(
    data=X_train.assign(concentration=reference_concentration), target="concentration"
).add(StepMSC(list(X_train.columns)))
model = make_pipeline(RecipeTransformer(specification), Ridge())
model.fit(X_train, reference_concentration)
prediction = model.predict(X_new)
```

## Choose a spectral transformation

| Step | What it does |
| --- | --- |
| `StepSNV(columns)` | Centers each spectrum and divides by its sample standard deviation. A constant spectrum produces centered values with a warning. |
| `StepMSC(columns)` | Fits the mean training spectrum, then regresses each sample on that reference to correct for additive and multiplicative scatter. |
| `StepSavitzkyGolay(columns, window=5, polyorder=2, deriv=0)` | Fits local polynomials to smooth spectra or calculate derivatives. The window is the full odd width in channels and must be shorter than the input spectrum. Only points with a complete window are returned. |

For example, a three-channel window retains the three middle wavelengths of the five-point grid above. A derivative is per channel index, matching R Measure. To express a first derivative per nm on a uniform 5 nm grid, divide by 5. On an irregular grid, use a resampled uniform axis before interpreting derivatives in physical units.

```python
from measure_spectra import StepSavitzkyGolay, snv

derivative = Recipe().add(
    StepSavitzkyGolay(list(X.columns), window=3, polyorder=2, deriv=1)
)
slope_per_channel = derivative.fit(X_train).transform(X_new)
normalized = snv(spectra.values)
```

The numerical functions accept a two-dimensional array (samples × channels). Call `msc_reference(train)` once and pass the result to `msc(new, reference)`. For `savitzky_golay(values, locations, ...)`, the output columns correspond to the central locations left after trimming half the window at each edge.

## Run the checks

From the repository root:

```sh
python -m pip install './python[test]'
python -m pytest python/tests
```
