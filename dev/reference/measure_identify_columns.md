# Identify Column Types in Analytical Data

Automatically detects column types in a data frame based on naming
conventions common in analytical chemistry. This helps set up recipes
with appropriate roles for different column types.

## Usage

``` r
measure_identify_columns(data, patterns = measure_column_patterns)
```

## Arguments

- data:

  A data frame to analyze.

- patterns:

  Named list of regex patterns for column detection. Defaults to
  `measure_column_patterns`. Custom patterns can be provided as a named
  list where names become the detected type.

## Value

A tibble with columns:

- column:

  Column name

- type:

  Detected type (from pattern names, or "other" if no match)

- suggested_role:

  Suggested recipe role based on type

- n_values:

  Number of non-NA values

- class:

  R class of the column

## Details

Column type detection uses the following naming conventions:

|         |                |                |                          |
|---------|----------------|----------------|--------------------------|
| Prefix  | Type           | Suggested Role | Use Case                 |
| `wn_*`  | wavenumber     | predictor      | IR spectroscopy (cm^-1)  |
| `nm_*`  | wavelength     | predictor      | UV-Vis, NIR spectroscopy |
| `rt_*`  | retention_time | predictor      | Chromatography           |
| `mz_*`  | mz             | predictor      | Mass spectrometry        |
| `ppm_*` | ppm            | predictor      | NMR spectroscopy         |
| `ch_*`  | channel        | predictor      | Generic channel data     |
| `x_*`   | generic        | predictor      | Generic measurements     |

Columns not matching any pattern are classified as "other" and suggested
as either "outcome" (if numeric), "id" (if character/factor with unique
values), or "predictor".

## Examples

``` r
# Wide format spectral data
df <- data.frame(
  sample_id = 1:5,
  outcome = rnorm(5),
  wn_1000 = rnorm(5),
  wn_1001 = rnorm(5),
  wn_1002 = rnorm(5)
)
measure_identify_columns(df)
#> # A tibble: 5 × 5
#>   column    type       suggested_role n_values class  
#>   <chr>     <chr>      <chr>             <int> <chr>  
#> 1 sample_id other      predictor             5 integer
#> 2 outcome   other      outcome               5 numeric
#> 3 wn_1000   wavenumber predictor             5 numeric
#> 4 wn_1001   wavenumber predictor             5 numeric
#> 5 wn_1002   wavenumber predictor             5 numeric

# Chromatography data
df2 <- data.frame(
  id = letters[1:3],
  concentration = c(1.2, 2.3, 3.4),
  rt_0.5 = rnorm(3),
  rt_1.0 = rnorm(3),
  rt_1.5 = rnorm(3)
)
measure_identify_columns(df2)
#> # A tibble: 5 × 5
#>   column        type           suggested_role n_values class    
#>   <chr>         <chr>          <chr>             <int> <chr>    
#> 1 id            other          id                    3 character
#> 2 concentration other          outcome               3 numeric  
#> 3 rt_0.5        retention_time predictor             3 numeric  
#> 4 rt_1.0        retention_time predictor             3 numeric  
#> 5 rt_1.5        retention_time predictor             3 numeric  
```
