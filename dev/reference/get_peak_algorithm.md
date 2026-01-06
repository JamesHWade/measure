# Get a Peak Detection Algorithm

Retrieves a registered peak detection algorithm by name.

## Usage

``` r
get_peak_algorithm(name)
```

## Arguments

- name:

  Algorithm name.

## Value

A list with components:

- `name`: Algorithm name

- `algorithm_fn`: The algorithm function

- `pack_name`: Source package name

- `description`: Brief description

- `default_params`: List of default parameter values

- `param_info`: List of parameter descriptions

- `technique`: Technique name (or `NULL`)

Returns `NULL` if algorithm not found.

## See also

[`peak_algorithms()`](https://jameshwade.github.io/measure/dev/reference/peak_algorithms.md),
[`register_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/register_peak_algorithm.md)

## Examples

``` r
algo <- get_peak_algorithm("prominence")
if (!is.null(algo)) {
  print(algo$description)
}
#> [1] "Finds local maxima by prominence (height above surrounding signal)"
```
