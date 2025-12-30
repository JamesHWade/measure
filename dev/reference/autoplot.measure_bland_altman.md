# Plot Bland-Altman Analysis

Creates a Bland-Altman plot showing differences vs means with limits of
agreement.

## Usage

``` r
# S3 method for class 'measure_bland_altman'
autoplot(object, show_loa = TRUE, show_ci = FALSE, ...)
```

## Arguments

- object:

  A `measure_bland_altman` object.

- show_loa:

  Show limits of agreement? Default TRUE.

- show_ci:

  Show confidence intervals for LOA? Default FALSE.

- ...:

  Additional arguments (unused).

## Value

A ggplot object.
