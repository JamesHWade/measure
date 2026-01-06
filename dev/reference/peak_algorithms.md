# List Available Peak Detection Algorithms

Returns a tibble of all registered peak detection algorithms.

## Usage

``` r
peak_algorithms(packs = NULL, techniques = NULL)
```

## Arguments

- packs:

  Character vector of pack names to include. If `NULL`, includes all
  packs.

- techniques:

  Character vector of techniques to include. If `NULL`, includes all
  techniques (including general-purpose algorithms).

## Value

A tibble with columns:

- `name`: Algorithm name (e.g., "prominence", "derivative")

- `pack_name`: Source package name

- `description`: Brief description

- `technique`: Technique (or `NA` for general-purpose)

- `default_params`: List column of default parameter values

## See also

[`register_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/register_peak_algorithm.md),
[`get_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/get_peak_algorithm.md)

## Examples

``` r
# List all algorithms
peak_algorithms()
#> # A tibble: 3 × 5
#>   name         pack_name description                    technique default_params
#>   <chr>        <chr>     <chr>                          <chr>     <named list>  
#> 1 prominence   measure   Finds local maxima by promine… NA        <named list>  
#> 2 derivative   measure   Finds peaks by zero-crossings… NA        <named list>  
#> 3 local_maxima measure   Finds all local maxima above … NA        <named list>  

# List only algorithms from a specific pack
peak_algorithms(packs = "measure")
#> # A tibble: 3 × 5
#>   name         pack_name description                    technique default_params
#>   <chr>        <chr>     <chr>                          <chr>     <named list>  
#> 1 prominence   measure   Finds local maxima by promine… NA        <named list>  
#> 2 derivative   measure   Finds peaks by zero-crossings… NA        <named list>  
#> 3 local_maxima measure   Finds all local maxima above … NA        <named list>  
```
