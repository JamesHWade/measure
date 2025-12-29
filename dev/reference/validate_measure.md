# Validate measure data

Performs comprehensive validation checks on measure data, including axis
monotonicity, duplicate detection, missing value detection, and spacing
regularity.

## Usage

``` r
validate_measure(
  x,
  checks = c("monotonic", "duplicates", "missing", "spacing"),
  tolerance = 1e-06,
  action = c("error", "warn", "message")
)
```

## Arguments

- x:

  A `measure_tbl`, `measure_list`, or data frame with measure column.

- checks:

  Character vector of checks to perform. Default is all checks:
  `"monotonic"`, `"duplicates"`, `"missing"`, `"spacing"`.

- tolerance:

  Numeric tolerance for spacing regularity check. Default is 1e-6.

- action:

  What to do when validation fails: `"error"` (default), `"warn"`, or
  `"message"`.

## Value

Invisibly returns a list with validation results. Each element is a list
with `valid` (logical), `message` (character), and `details`.

## Examples

``` r
# Create valid measure data
spec <- new_measure_tbl(location = 1:100, value = sin(1:100 / 10))
validate_measure(spec)

# Data with issues
spec_dup <- new_measure_tbl(location = c(1, 2, 2, 3), value = c(1, 2, 3, 4))
try(validate_measure(spec_dup))
#> Error in validate_measure(spec_dup) : Measure validation failed:
#> ✖ Duplicate locations found in 1 sample(s) Irregular spacing found in 1
#>   sample(s)

# Only check specific issues
validate_measure(spec, checks = c("monotonic", "missing"))
```
