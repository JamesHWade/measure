# Plot Proficiency Test Scores

Creates a bar chart or dot plot of proficiency scores with threshold
lines.

## Usage

``` r
# S3 method for class 'measure_proficiency_score'
autoplot(object, type = c("bar", "point"), ...)
```

## Arguments

- object:

  A `measure_proficiency_score` object.

- type:

  Plot type: "bar" or "point". Default "bar".

- ...:

  Additional arguments (unused).

## Value

A ggplot object.
