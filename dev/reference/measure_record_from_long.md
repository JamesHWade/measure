# Import a measurement record from a long table

`measure_record_from_long()` turns an already parsed long table into a
relational analytical record. Explicit column mappings define sample,
injection, detector channel, curve, axis, and signal identity.
Identifier and label columns must already be character vectors; values
are not inferred from their appearance, and units are retained only as
declarations.

## Usage

``` r
measure_record_from_long(data, columns, source_id, allow_extra_columns = FALSE)
```

## Arguments

- data:

  A data frame containing one row per point in long form.

- columns:

  A named character vector mapping canonical fields to existing input
  column names. Required names are `sample_id`, `injection_id`,
  `channel_id`, `curve_id`, `location`, and `value`. Optional names are
  `channel_name`, `axis_name`, `axis_unit`, `value_name`, and
  `value_unit`. Input columns cannot be reused for different fields.

- source_id:

  A caller-declared, nonblank character source identifier.

- allow_extra_columns:

  Set to `TRUE` to ignore input columns not named in `columns`.

## Value

A `measure_record` containing samples, injections, channels, curves, and
one source declaration.

## Details

Curves and related entities retain first-appearance order. Points retain
their occurrence order within each curve, including interleaved curves.
Each curve may have its own finite, strictly monotone location grid.
Values must be finite numeric values or `NA`; locations must be finite
numeric values. The function does not sort points, infer missing
metadata from other rows, infer units, or perform locale conversion.
Conflicting declarations, including missing-versus-known mixtures within
an entity, are errors. Its source record contains the caller-declared
`source_id` only; this conversion does not compute a source digest or
claim a complete transformation history.

Unmapped input columns are errors by default. Set
`allow_extra_columns = TRUE` to explicitly ignore them. This API handles
one-dimensional measurement curves. It does not import nD data, DAD/MS
vendor structures, or vendor-specific file formats.

## Examples

``` r
long_data <- tibble::tibble(
  sample_id = c("sample-01", "sample-01"),
  injection_id = c("injection-01", "injection-01"),
  channel_id = c("uv", "uv"),
  curve_id = c("curve-01", "curve-01"),
  location = c(250, 251),
  value = c(0.02, NA_real_)
)
long_columns <- c(
  sample_id = "sample_id",
  injection_id = "injection_id",
  channel_id = "channel_id",
  curve_id = "curve_id",
  location = "location",
  value = "value"
)
record <- measure_record_from_long(long_data, long_columns, "source-01")
record
#> <measure_record>
#>   Samples: 1
#>   Injections: 1
#>   Channels: 1
#>   Curves: 1
#>   Sources: 1
```
