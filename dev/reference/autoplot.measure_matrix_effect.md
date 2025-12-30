# Plot Matrix Effects

Creates a visualization of matrix effects showing
suppression/enhancement.

## Usage

``` r
# S3 method for class 'measure_matrix_effect'
autoplot(object, type = c("bar", "point", "forest"), show_limits = TRUE, ...)
```

## Arguments

- object:

  A `measure_matrix_effect` object.

- type:

  Plot type: "bar", "point", or "forest". Default "bar".

- show_limits:

  Show acceptable limits (80-120%)? Default TRUE.

- ...:

  Additional arguments (unused).

## Value

A ggplot object.
