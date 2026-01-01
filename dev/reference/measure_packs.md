# List Registered Technique Packs

Returns a tibble of all registered technique packs, including the core
`measure` package.

## Usage

``` r
measure_packs()
```

## Value

A tibble with columns:

- `name`: Package name

- `technique`: Technique category (e.g., "general", "SEC/GPC")

- `version`: Package version

- `description`: Brief description

## See also

[`measure_steps()`](https://jameshwade.github.io/measure/dev/reference/measure_steps.md),
[`register_measure_pack()`](https://jameshwade.github.io/measure/dev/reference/register_measure_pack.md)

## Examples

``` r
measure_packs()
#> # A tibble: 1 × 4
#>   name    technique version    description                         
#>   <chr>   <chr>     <chr>      <chr>                               
#> 1 measure general   0.0.1.9001 Core measurement preprocessing steps
```
