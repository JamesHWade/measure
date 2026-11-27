# Quantify a scalar chromatography batch with external calibration

`measure_quantify_batch()` runs a complete single-analyte, external
calibration workflow over a scalar
[`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md):
peak detection and integration via
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md)
and
[`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md),
calibration fitting via
[`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md),
QC verification via
[`measure_calibration_verify()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_verify.md),
dilution back-calculation, and explicit per-injection outcome flagging.

## Usage

``` r
measure_quantify_batch(
  record,
  manifest,
  window,
  channel_id = NULL,
  analyte = "analyte",
  conc_unit = NULL,
  calibration_revision = "cal-v1",
  peak_algorithm = "prominence",
  min_height = 0,
  min_prominence = 0,
  min_distance = 0,
  algorithm_params = list(),
  integration_method = c("trapezoid", "simpson"),
  baseline = c("local", "none", "global"),
  model = c("linear", "quadratic"),
  weights = c("none", "1/x", "1/x2", "1/y", "1/y2"),
  origin = FALSE,
  lloq = NULL,
  uloq = NULL,
  qc_acceptance_pct = 15,
  blank_max_area = NULL,
  blank_max_pct_lloq = 20
)

# S3 method for class 'measure_batch_results'
print(x, ...)

# S3 method for class 'measure_batch_results'
summary(object, ...)

# S3 method for class 'measure_batch_results'
tidy(x, ...)
```

## Arguments

- record:

  A
  [`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md)
  containing one scalar chromatogram per injection on the target
  channel.

- manifest:

  A data frame describing the batch injections with required columns:

  - `injection_id`: Unique character IDs matching `record$injections`.

  - `injection_type`: One of `"standard"`, `"blank"`, `"qc"`, or
    `"unknown"`.

  - `nominal_conc`: Numeric nominal concentration (finite for
    `"standard"` and `"qc"`, `NA` or `0` for `"blank"`, `NA` for
    `"unknown"`). Optional columns:

  - `dilution_factor`: Positive finite numeric dilution factor (default
    `1`). Applied to back-calculate original sample concentration after
    checking in-vial concentration against `[lloq, uloq]`.

  - `batch_order`: Integer run order (default
    `seq_len(nrow(manifest))`).

- window:

  Length-2 finite numeric vector `c(min, max)` defining the target
  analyte retention-time window (`window[1] < window[2]`).

- channel_id:

  Optional channel ID when `record` has multiple channels. Required if
  `record$channels` has more than one row.

- analyte:

  Single non-empty string naming the target analyte.

- conc_unit:

  Optional single non-empty string declaring concentration units (e.g.,
  `"mg/L"`).

- calibration_revision:

  Single non-empty string identifying the calibration revision recorded
  on each quantified injection.

- peak_algorithm:

  Peak detection algorithm passed to
  [`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md).
  Default `"prominence"`.

- min_height:

  Minimum peak height passed to
  [`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md).

- min_prominence:

  Minimum peak prominence passed to
  [`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md).

- min_distance:

  Minimum peak distance passed to
  [`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md).

- algorithm_params:

  Optional list of algorithm-specific parameters passed to
  [`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md).

- integration_method:

  Integration rule passed to
  [`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md):
  `"trapezoid"` (default) or `"simpson"`.

- baseline:

  Peak baseline convention passed to
  [`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md):
  `"local"` (default), `"none"`, or `"global"`.

- model:

  Calibration model passed to
  [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md):
  `"linear"` (default) or `"quadratic"`.

- weights:

  Calibration weighting scheme passed to
  [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md).

- origin:

  Logical; force calibration curve through the origin? Default `FALSE`.

- lloq:

  Optional lower limit of quantitation (in-vial concentration). Defaults
  to the minimum positive fitted standard concentration.

- uloq:

  Optional upper limit of quantitation (in-vial concentration). Defaults
  to the maximum fitted standard concentration.

- qc_acceptance_pct:

  Maximum allowed absolute percent deviation from nominal for standards
  and QC samples. Default `15`.

- blank_max_area:

  Optional absolute peak area ceiling for blank injections. When `NULL`
  (default), blanks are checked against `blank_max_pct_lloq` percent of
  the fitted response at `lloq`.

- blank_max_pct_lloq:

  Percent of LLOQ response tolerated in a blank when `blank_max_area` is
  `NULL`. Default `20`.

- x:

  A `measure_batch_results` object.

- ...:

  Additional arguments (unused).

- object:

  A `measure_batch_results` object.

## Value

A `measure_batch_results` object (inheriting from
[`measure_results()`](https://jameshwade.github.io/measure/dev/reference/measure_results.md))
with components:

- `results`: Per-injection tibble linking provenance, peaks, areas,
  calibration revision, estimated/reported concentrations, and outcomes.

- `calibration`: Fitted
  [measure_calibration](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  object.

- `verification`:
  [measure_calibration_verify](https://jameshwade.github.io/measure/dev/reference/measure_calibration_verify.md)
  object for QC injections (or `NULL` if no QC injections are present).

- `method`: Named list of declared batch parameters and source digests.

- `peaks`: All detected peaks across injections for diagnostics.

- `data`, `measures`, `id_cols`: Inherited
  [`measure_results()`](https://jameshwade.github.io/measure/dev/reference/measure_results.md)
  fields for plotting and report rendering.

## Details

Every reported row links back to its `sample_id`, `injection_id`,
`curve_id`, `source_id`, `source_sha256`, parsed source-row bounds,
selected peak bounds, area, `calibration_revision`, estimated and
reported concentrations, and QC status. Missing peaks, multiple
coeluting/interfering peaks in the analyte window, blank interferences,
and out-of-range unknowns (`"below_lloq"` / `"above_uloq"`) are reported
as distinct outcomes rather than silently coerced to zero or
extrapolated as valid.

## See also

[`measure_record_curves()`](https://jameshwade.github.io/measure/dev/reference/measure_record_curves.md),
[`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md),
[`measure_calibration_verify()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_verify.md),
[`measure_results()`](https://jameshwade.github.io/measure/dev/reference/measure_results.md),
[`render_measure_report()`](https://jameshwade.github.io/measure/dev/reference/render_measure_report.md)

## Examples

``` r
path <- system.file(
  "extdata",
  "lc_batch_control.csv",
  package = "measure",
  mustWork = TRUE
)
manifest_path <- system.file(
  "extdata",
  "lc_batch_manifest.csv",
  package = "measure",
  mustWork = TRUE
)
cols <- c(
  sample_id = "sample_id",
  injection_id = "injection_id",
  channel_id = "channel_id",
  channel_name = "channel_name",
  curve_id = "curve_id",
  axis_name = "axis_name",
  axis_unit = "axis_unit",
  value_name = "value_name",
  value_unit = "value_unit",
  location = "location",
  value = "value"
)
rec <- read_measure_record_delim(path, cols, source_id = "lc-batch-control")
manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
batch <- measure_quantify_batch(
  rec,
  manifest,
  window = c(4.8, 5.6),
  analyte = "caffeine_control",
  conc_unit = "mg/L",
  calibration_revision = "cal-control-v1",
  min_prominence = 1.0
)
summary(batch)
#> # A tibble: 13 × 31
#>    batch_order sample_id injection_id curve_id channel_id injection_type analyte
#>          <int> <chr>     <chr>        <chr>    <chr>      <chr>          <chr>  
#>  1           1 BLK-01    INJ-01       CURVE-01 uv254      blank          caffei…
#>  2           2 STD-05    INJ-02       CURVE-02 uv254      standard       caffei…
#>  3           3 STD-10    INJ-03       CURVE-03 uv254      standard       caffei…
#>  4           4 STD-25    INJ-04       CURVE-04 uv254      standard       caffei…
#>  5           5 STD-50    INJ-05       CURVE-05 uv254      standard       caffei…
#>  6           6 STD-100   INJ-06       CURVE-06 uv254      standard       caffei…
#>  7           7 QC-LOW    INJ-07       CURVE-07 uv254      qc             caffei…
#>  8           8 QC-HIGH   INJ-08       CURVE-08 uv254      qc             caffei…
#>  9           9 UNK-01    INJ-09       CURVE-09 uv254      unknown        caffei…
#> 10          10 UNK-02    INJ-10       CURVE-10 uv254      unknown        caffei…
#> 11          11 UNK-03    INJ-11       CURVE-11 uv254      unknown        caffei…
#> 12          12 UNK-04    INJ-12       CURVE-12 uv254      unknown        caffei…
#> 13          13 UNK-05    INJ-13       CURVE-13 uv254      unknown        caffei…
#> # ℹ 24 more variables: nominal_conc <dbl>, dilution_factor <dbl>,
#> #   conc_unit <chr>, peak_count <int>, peak_status <chr>, peak_location <dbl>,
#> #   peak_height <dbl>, left_base <dbl>, right_base <dbl>, area <dbl>,
#> #   calibration_revision <chr>, estimated_conc <dbl>, reported_conc <dbl>,
#> #   accuracy_pct <dbl>, deviation_pct <dbl>, outcome <chr>, qc_status <chr>,
#> #   source_id <chr>, source_name <chr>, source_sha256 <chr>,
#> #   source_row_start <int>, source_row_end <int>, …
```
