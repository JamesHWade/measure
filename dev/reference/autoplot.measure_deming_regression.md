# Plot Method Comparison Regression

Creates a scatter plot with regression line for method comparison.

## Usage

``` r
# S3 method for class 'measure_deming_regression'
autoplot(object, show_identity = TRUE, ...)

# S3 method for class 'measure_passing_bablok'
autoplot(object, show_identity = TRUE, ...)
```

## Arguments

- object:

  A `measure_deming_regression` or `measure_passing_bablok` object.

- show_identity:

  Show y = x identity line? Default TRUE.

- ...:

  Additional arguments (unused).

## Value

A ggplot object.
