# Validate Analytical Metadata

Validates that a data frame contains the required metadata columns for
analytical workflows. This function checks for column presence, correct
data types, and valid values (e.g., sample_type levels).

## Usage

``` r
measure_validate_metadata(
  data,
  require = NULL,
  sample_types = measure_sample_types,
  action = c("error", "warn", "message")
)
```

## Arguments

- data:

  A data frame to validate.

- require:

  Character vector of required columns. Common columns include:

  - `"sample_type"`: Sample classification (qc, standard, blank,
    unknown, reference)

  - `"run_order"`: Injection/measurement sequence (integer)

  - `"batch_id"`: Batch identifier (character/factor)

  - `"nominal_conc"`: Known concentration for standards (numeric)

  - `"sample_id"`: Unique sample identifier

  - `"analyst_id"`, `"day"`, `"instrument_id"`: Precision study factors

- sample_types:

  Allowed values for `sample_type` column. Default is
  [measure_sample_types](https://jameshwade.github.io/measure/dev/reference/measure_sample_types.md):
  "qc", "standard", "blank", "unknown", "reference".

- action:

  What to do when validation fails:

  - `"error"` (default): Stop with an informative error

  - `"warn"`: Issue warnings but continue

  - `"message"`: Issue messages but continue

## Value

Invisibly returns a list with validation results:

- `valid`: Logical, TRUE if all checks passed

- `checks`: List of individual check results

- `data`: The original data (unchanged)

## Details

### Canonical Columns

Milestone 2 functions expect specific column names with specific types:

|                   |                  |                                 |
|-------------------|------------------|---------------------------------|
| Column            | Type             | Description                     |
| `sample_type`     | character/factor | Sample classification           |
| `run_order`       | integer          | Injection sequence within batch |
| `batch_id`        | character/factor | Batch identifier                |
| `nominal_conc`    | numeric          | Known concentration (standards) |
| `sample_id`       | character/factor | Unique sample identifier        |
| `analyst_id`      | character/factor | Analyst performing measurement  |
| `day`             | character/Date   | Day of measurement              |
| `instrument_id`   | character/factor | Instrument identifier           |
| `dilution_factor` | numeric          | Sample dilution factor          |

### Sample Type Values

The `sample_type` column must contain only values from
[measure_sample_types](https://jameshwade.github.io/measure/dev/reference/measure_sample_types.md):

- `"qc"`: Quality control sample (pooled QC, system suitability)

- `"standard"`: Calibration standard with known concentration

- `"blank"`: Blank sample (solvent, matrix blank)

- `"unknown"`: Sample with unknown concentration

- `"reference"`: Reference material for batch correction

## See also

[`measure_standardize_sample_type()`](https://jameshwade.github.io/measure/dev/reference/measure_standardize_sample_type.md)
for converting non-standard sample type values to canonical form.

## Examples

``` r
# Create sample analytical data
data <- data.frame(
  sample_id = paste0("S", 1:10),
  sample_type = c("qc", "standard", "standard", "unknown", "unknown",
                  "unknown", "qc", "blank", "unknown", "qc"),
  run_order = 1:10,
  batch_id = "B001",
  nominal_conc = c(NA, 10, 50, NA, NA, NA, NA, 0, NA, NA),
  response = rnorm(10, mean = 100)
)

# Validate required columns
measure_validate_metadata(data, require = c("sample_type", "run_order"))

# Validate for calibration workflow
measure_validate_metadata(
  data,
  require = c("sample_type", "nominal_conc")
)

# More lenient validation (warnings only)
measure_validate_metadata(
  data,
  require = c("sample_type", "run_order", "missing_col"),
  action = "warn"
)
#> Warning: Metadata validation issues:
#> ! Missing required column(s): missing_col
```
