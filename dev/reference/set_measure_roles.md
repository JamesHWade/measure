# Set Measure Roles in a Recipe

Batch assign roles to columns based on their detected types or explicit
patterns. This is a convenience wrapper around
[`recipes::update_role()`](https://recipes.tidymodels.org/reference/roles.html)
for common analytical data patterns.

## Usage

``` r
set_measure_roles(
  recipe,
  id_cols = NULL,
  blank_cols = NULL,
  qc_cols = NULL,
  standard_cols = NULL,
  metadata_cols = NULL,
  measure_cols = NULL
)
```

## Arguments

- recipe:

  A recipe object.

- id_cols:

  Column(s) to assign "id" role. Accepts tidyselect.

- blank_cols:

  Column(s) to assign "blank" role. Accepts tidyselect.

- qc_cols:

  Column(s) to assign "qc" role. Accepts tidyselect.

- standard_cols:

  Column(s) to assign "standard" role. Accepts tidyselect.

- metadata_cols:

  Column(s) to assign "metadata" role. Accepts tidyselect.

- measure_cols:

  Column(s) to assign "measure" role. Accepts tidyselect.

## Value

Updated recipe object with roles assigned.

## Details

Common roles for analytical chemistry workflows:

|           |                                           |
|-----------|-------------------------------------------|
| Role      | Purpose                                   |
| id        | Sample identifiers (not used in modeling) |
| blank     | Blank/background samples for subtraction  |
| qc        | Quality control samples                   |
| standard  | Calibration standards                     |
| metadata  | Sample metadata (not used in modeling)    |
| measure   | Measurement columns for input steps       |
| predictor | Columns used as model predictors          |
| outcome   | Target variable(s) for modeling           |

## Examples

``` r
if (FALSE) { # \dontrun{
library(recipes)

# Basic role assignment
rec <- recipe(outcome ~ ., data = my_data) |>
  set_measure_roles(
    id_cols = sample_id,
    metadata_cols = c(batch, operator)
  )

# With QC and blank identification by column name patterns
rec <- recipe(outcome ~ ., data = my_data) |>
  set_measure_roles(
    id_cols = sample_id,
    blank_cols = starts_with("blank_"),
    qc_cols = starts_with("qc_")
  )
} # }
```
