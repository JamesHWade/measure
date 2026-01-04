# Create an Analytical Method Validation Report

Creates a structured validation report object that collects results from
various validation studies (calibration, precision, accuracy, etc.) and
can be rendered to HTML, PDF, or Word formats using standardized
templates.

This function supports two major validation frameworks:

- **ICH Q2(R2)**: International harmonized guidelines for analytical
  validation

- **USP \<1225\>**: United States Pharmacopeia compendial validation
  procedures

## Usage

``` r
measure_validation_report(
  title = "Analytical Method Validation Report",
  method_name = NULL,
  method_description = NULL,
  analyst = NULL,
  reviewer = NULL,
  lab = NULL,
  date = Sys.Date(),
  instrument = NULL,
  software = NULL,
  calibration = NULL,
  lod_loq = NULL,
  accuracy = NULL,
  precision = NULL,
  linearity = NULL,
  range = NULL,
  specificity = NULL,
  robustness = NULL,
  carryover = NULL,
  system_suitability = NULL,
  uncertainty = NULL,
  method_comparison = NULL,
  stability = NULL,
  criteria = NULL,
  conclusions = NULL,
  references = NULL,
  appendices = NULL,
  ...
)
```

## Arguments

- title:

  Report title. Default: "Analytical Method Validation Report"

- method_name:

  Name of the analytical method being validated.

- method_description:

  Brief description of the method (technique, analyte, matrix).

- analyst:

  Name of the analyst(s) performing validation.

- reviewer:

  Name of the reviewer (optional).

- lab:

  Laboratory name or identifier.

- date:

  Date of the validation study. Default: current date.

- instrument:

  Instrument details (name, model, serial number).

- software:

  Software used for data acquisition/processing.

- calibration:

  A `measure_calibration` object from
  [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md).

- lod_loq:

  LOD/LOQ results from
  [`measure_lod()`](https://jameshwade.github.io/measure/dev/reference/measure_lod.md),
  [`measure_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_loq.md),
  or
  [`measure_lod_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_lod_loq.md).
  Can be a single object or a list.

- accuracy:

  Accuracy results from
  [`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md).

- precision:

  A list containing precision study results:

  - `repeatability`: from
    [`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md)

  - `intermediate`: from
    [`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md)

  - `reproducibility`: from
    [`measure_reproducibility()`](https://jameshwade.github.io/measure/dev/reference/measure_reproducibility.md)
    (optional)

- linearity:

  Linearity results from
  [`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md).

- range:

  A list with `lower` and `upper` validated range limits, or results
  supporting range determination.

- specificity:

  User-provided specificity/selectivity assessment. Can be text, a data
  frame of interference results, or a list.

- robustness:

  User-provided robustness study results. Can be text, a data frame, or
  structured results.

- carryover:

  Carryover results from
  [`measure_carryover()`](https://jameshwade.github.io/measure/dev/reference/measure_carryover.md).

- system_suitability:

  System suitability results from
  [`measure_system_suitability()`](https://jameshwade.github.io/measure/dev/reference/measure_system_suitability.md).

- uncertainty:

  Uncertainty budget from
  [`measure_uncertainty_budget()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty_budget.md).

- method_comparison:

  Method comparison results (Bland-Altman, Deming, Passing-Bablok) from
  the corresponding functions.

- stability:

  User-provided stability data (solution stability, freeze-thaw, etc.).

- criteria:

  A `measure_criteria` object defining acceptance criteria, or a named
  list of criteria objects for different sections.

- conclusions:

  User-provided conclusions text or a list with `summary` and
  `recommendations`.

- references:

  Character vector of references cited.

- appendices:

  Named list of additional content to include as appendices.

- ...:

  Additional metadata to include in the report.

## Value

A `measure_validation_report` object containing:

- `metadata`: Report metadata (title, analyst, date, etc.)

- `sections`: Named list of validation results by section

- `criteria`: Acceptance criteria used

- `provenance`: Data provenance and computational environment info

- `call`: The function call

## Details

### Workflow

1.  Run individual validation studies using measure functions

2.  Collect results into a validation report object

3.  Render to desired format using
    [`render_validation_report()`](https://jameshwade.github.io/measure/dev/reference/render_validation_report.md)

### Supported Validation Characteristics (ICH Q2)

- **Specificity/Selectivity**: Ability to assess analyte in presence of
  interferences

- **Linearity**: Proportional response over concentration range

- **Range**: Validated concentration interval

- **Accuracy**: Closeness to true value (trueness)

- **Precision**: Repeatability, intermediate precision, reproducibility

- **Detection Limit (LOD)**: Lowest detectable amount

- **Quantitation Limit (LOQ)**: Lowest quantifiable amount with
  acceptable precision/accuracy

- **Robustness**: Capacity to remain unaffected by small method
  variations

### Data Provenance

The report automatically captures:

- R version and package versions

- Date/time of report generation

- Function calls used to generate each section

## See also

[`render_validation_report()`](https://jameshwade.github.io/measure/dev/reference/render_validation_report.md)
to generate the final report document.

Related validation functions:

- [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md),
  [`measure_calibration_predict()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_predict.md)

- [`measure_lod()`](https://jameshwade.github.io/measure/dev/reference/measure_lod.md),
  [`measure_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_loq.md),
  [`measure_lod_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_lod_loq.md)

- [`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md),
  [`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md),
  [`measure_carryover()`](https://jameshwade.github.io/measure/dev/reference/measure_carryover.md)

- [`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md),
  [`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md)

- [`measure_uncertainty_budget()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty_budget.md)

## Examples

``` r
# Create sample validation data
set.seed(123)
cal_data <- data.frame(
  nominal_conc = rep(c(1, 5, 10, 25, 50, 100), each = 3),
  response = c(1, 5, 10, 25, 50, 100) * 1000 +
    rnorm(18, sd = 50),
  sample_type = "standard"
)

# Fit calibration
cal_fit <- measure_calibration_fit(
  cal_data,
  formula = response ~ nominal_conc,
  weights = "1/x"
)

# Calculate LOD/LOQ (requires sample_type column)
blank_data <- data.frame(
  response = rnorm(10, mean = 50, sd = 15),
  sample_type = "blank"
)
lod_result <- measure_lod(blank_data, response_col = "response")

# Create precision data
precision_data <- data.frame(
  concentration = rep(c(10, 50, 100), each = 6),
  replicate = rep(1:6, 3),
  response = c(
    rnorm(6, 10000, 200),
    rnorm(6, 50000, 800),
    rnorm(6, 100000, 1500)
  )
)
repeatability <- measure_repeatability(
  precision_data,
  response_col = "response",
  group_col = "concentration"
)

# Create validation report
report <- measure_validation_report(
  title = "Validation of HPLC Method for Compound X",
  method_name = "HPLC-UV Assay",
  method_description = "Reversed-phase HPLC with UV detection at 254 nm",
  analyst = "J. Smith",
  lab = "Analytical Development Lab",
  calibration = cal_fit,
  lod_loq = lod_result,
  precision = list(repeatability = repeatability),
  conclusions = "Method meets all acceptance criteria for intended use."
)

print(report)
#> 
#> ── Validation Report ───────────────────────────────────────────────────────────
#> Title: Validation of HPLC Method for Compound X
#> Method: HPLC-UV Assay
#> Analyst: J. Smith
#> Lab: Analytical Development Lab
#> Date: 2026-01-04
#> 
#> 
#> ── Validation Sections ──
#> 
#> ℹ Calibration
#> ℹ LOD/LOQ
#> ℹ Precision
#> 
#> 
#> ── Conclusions ──
#> 
#> Method meets all acceptance criteria for intended use.
#> 
#> 
#> ── Provenance ──
#> 
#> Generated: 2026-01-04 14:13:48.44705
#> R version: 4.5.2
#> measure version: 0.0.1.9001
#> 
#> ℹ Use `render_validation_report()` to generate document
```
