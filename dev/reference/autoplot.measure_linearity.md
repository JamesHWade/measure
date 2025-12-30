# Plot Linearity Assessment Results

Creates diagnostic plots for linearity assessment.

## Usage

``` r
# S3 method for class 'measure_linearity'
autoplot(object, type = c("fit", "residuals"), ...)
```

## Arguments

- object:

  A linearity assessment result from
  [`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md).

- type:

  Type of plot: `"fit"` for fitted vs actual, or `"residuals"`.

- ...:

  Additional arguments (unused).

## Value

A ggplot object.
