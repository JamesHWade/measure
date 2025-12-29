# Check Measure Recipe Structure

Validates that a recipe is properly structured for measure operations.
Checks for common issues like missing input steps, incompatible column
types, and role conflicts.

## Usage

``` r
check_measure_recipe(recipe, strict = TRUE)
```

## Arguments

- recipe:

  A recipe object to validate.

- strict:

  Logical. If TRUE (default), returns errors as a tibble. If FALSE,
  issues cli warnings and returns the recipe invisibly.

## Value

If `strict = TRUE`, returns a tibble with columns:

- level:

  Severity: "error", "warning", or "info"

- check:

  Name of the check that triggered the message

- message:

  Description of the issue

If `strict = FALSE`, returns the recipe invisibly after printing
warnings.

## Details

The following checks are performed:

**Errors** (will cause failures):

- No input step (`step_measure_input_*`)

- Output step before input step

- Multiple input steps

**Warnings** (may cause issues):

- No output step (data stays in internal format)

- Processing steps after output step

- No predictor columns identified

**Info** (suggestions):

- Large number of measurement columns (consider dimension reduction)

- No ID column identified

## Examples

``` r
if (FALSE) { # \dontrun{
library(recipes)

# Check a properly structured recipe
rec <- recipe(outcome ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_snv() |>
  step_measure_output_wide()

check_measure_recipe(rec)

# Check a recipe with issues
bad_rec <- recipe(outcome ~ ., data = my_data) |>
  step_measure_snv()  # Missing input step!

check_measure_recipe(bad_rec)
} # }
```
