# Standardize Sample Type Values

Converts non-standard sample type values to canonical form using a
user-specified mapping. This is useful when data uses different naming
conventions (e.g., "QC", "quality_control", "pooled_qc").

## Usage

``` r
measure_standardize_sample_type(
  data,
  col = "sample_type",
  mapping = NULL,
  unknown_action = c("error", "warn", "keep", "unknown")
)
```

## Arguments

- data:

  A data frame containing a sample type column.

- col:

  Name of the sample type column. Default is `"sample_type"`.

- mapping:

  A named list mapping canonical types to vectors of aliases. For
  example: `list(qc = c("QC", "quality_control", "pooled_qc"))`. If
  NULL, uses default case-insensitive matching.

- unknown_action:

  What to do with values that don't match any mapping:

  - `"error"` (default): Stop with error

  - `"warn"`: Warn and keep original value

  - `"keep"`: Silently keep original value

  - `"unknown"`: Convert to "unknown"

## Value

The data frame with standardized sample_type values.

## Examples

``` r
# Data with non-standard sample types
data <- data.frame(
  sample_id = 1:5,
  sample_type = c("QC", "STD", "BLK", "UNK", "REF")
)

# Standardize with custom mapping
measure_standardize_sample_type(
  data,
  mapping = list(
    qc = c("QC", "qc", "quality_control"),
    standard = c("STD", "std", "cal"),
    blank = c("BLK", "blk", "blank"),
    unknown = c("UNK", "unk", "sample"),
    reference = c("REF", "ref")
  )
)
#>   sample_id sample_type
#> 1         1          qc
#> 2         2    standard
#> 3         3       blank
#> 4         4     unknown
#> 5         5   reference
```
