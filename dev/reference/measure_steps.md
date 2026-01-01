# List Available Steps

Returns a tibble of all registered recipe steps from measure and any
loaded technique packs. Results can be filtered by pack, category, or
technique.

## Usage

``` r
measure_steps(packs = NULL, categories = NULL, techniques = NULL)
```

## Arguments

- packs:

  Character vector of pack names to include. If `NULL`, includes all
  packs.

- categories:

  Character vector of step categories to include. If `NULL`, includes
  all categories.

- techniques:

  Character vector of techniques to include. If `NULL`, includes all
  techniques.

## Value

A tibble with columns:

- `step_name`: Function name (e.g., "step_measure_baseline_als")

- `pack_name`: Source package name

- `category`: Step category (e.g., "baseline", "smoothing")

- `description`: Brief description

- `technique`: Technique (e.g., "general", "SEC/GPC")

## See also

[`measure_packs()`](https://jameshwade.github.io/measure/dev/reference/measure_packs.md),
[`register_measure_step()`](https://jameshwade.github.io/measure/dev/reference/register_measure_step.md)

## Examples

``` r
# List all steps
measure_steps()
#> # A tibble: 96 × 5
#>    step_name                    pack_name category  description        technique
#>    <chr>                        <chr>     <chr>     <chr>              <chr>    
#>  1 step_measure_input_long      measure   io        Convert long data… general  
#>  2 step_measure_input_wide      measure   io        Convert wide data… general  
#>  3 step_measure_output_long     measure   io        Convert measure t… general  
#>  4 step_measure_output_wide     measure   io        Convert measure t… general  
#>  5 step_measure_savitzky_golay  measure   smoothing Savitzky-Golay sm… general  
#>  6 step_measure_smooth_ma       measure   smoothing Moving average sm… general  
#>  7 step_measure_smooth_median   measure   smoothing Median filter smo… general  
#>  8 step_measure_smooth_gaussian measure   smoothing Gaussian smoothing general  
#>  9 step_measure_smooth_wavelet  measure   smoothing Wavelet smoothing  general  
#> 10 step_measure_filter_fourier  measure   smoothing Fourier filtering  general  
#> # ℹ 86 more rows

# List only baseline correction steps
measure_steps(categories = "baseline")
#> # A tibble: 14 × 5
#>    step_name                     pack_name category description        technique
#>    <chr>                         <chr>     <chr>    <chr>              <chr>    
#>  1 step_measure_baseline_als     measure   baseline Asymmetric Least … general  
#>  2 step_measure_baseline_poly    measure   baseline Polynomial baseli… general  
#>  3 step_measure_baseline_rf      measure   baseline Robust fitting ba… general  
#>  4 step_measure_baseline_custom  measure   baseline Custom baseline f… general  
#>  5 step_measure_baseline_py      measure   baseline Python pybaseline… general  
#>  6 step_measure_baseline_rolling measure   baseline Rolling ball base… general  
#>  7 step_measure_baseline_airpls  measure   baseline AirPLS baseline    general  
#>  8 step_measure_baseline_snip    measure   baseline SNIP baseline      general  
#>  9 step_measure_baseline_arpls   measure   baseline arPLS baseline     general  
#> 10 step_measure_baseline_tophat  measure   baseline Top-hat morpholog… general  
#> 11 step_measure_baseline_morph   measure   baseline Morphological bas… general  
#> 12 step_measure_baseline_minima  measure   baseline Local minima base… general  
#> 13 step_measure_baseline_auto    measure   baseline Automatic baselin… general  
#> 14 step_measure_baseline_gpc     measure   baseline GPC/SEC baseline … general  

# List steps from a specific technique pack
measure_steps(techniques = "SEC/GPC")
#> # A tibble: 0 × 5
#> # ℹ 5 variables: step_name <chr>, pack_name <chr>, category <chr>,
#> #   description <chr>, technique <chr>
```
