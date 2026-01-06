# List Available Peak Models

Returns a tibble of all registered peak models.

## Usage

``` r
peak_models(packs = NULL, techniques = NULL)
```

## Arguments

- packs:

  Character vector of pack names to filter by. If `NULL`, includes all
  packs.

- techniques:

  Character vector of techniques to filter by. If `NULL`, includes all
  (including general-purpose models).

## Value

A tibble with columns: name, pack_name, description, technique.

## See also

[`register_peak_model()`](https://jameshwade.github.io/measure/dev/reference/register_peak_model.md),
[`create_peak_model()`](https://jameshwade.github.io/measure/dev/reference/create_peak_model.md)

## Examples

``` r
peak_models()
#> # A tibble: 4 × 4
#>   name       pack_name description                               technique
#>   <chr>      <chr>     <chr>                                     <chr>    
#> 1 gaussian   measure   Symmetric Gaussian peak                   NA       
#> 2 emg        measure   Exponentially Modified Gaussian (tailing) NA       
#> 3 bigaussian measure   Bi-Gaussian (asymmetric with two widths)  NA       
#> 4 lorentzian measure   Lorentzian (Cauchy) peak                  NA       
```
