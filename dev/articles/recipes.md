# What is a recipe?

``` r
library(tidymodels)
#> ── Attaching packages ────────────────────────────────────── tidymodels 1.4.1 ──
#> ✔ broom        1.0.11     ✔ recipes      1.3.1 
#> ✔ dials        1.4.2      ✔ rsample      1.3.1 
#> ✔ dplyr        1.1.4      ✔ tailor       0.1.0 
#> ✔ ggplot2      4.0.1      ✔ tidyr        1.3.2 
#> ✔ infer        1.1.0      ✔ tune         2.0.1 
#> ✔ modeldata    1.5.1      ✔ workflows    1.3.0 
#> ✔ parsnip      1.4.0      ✔ workflowsets 1.1.1 
#> ✔ purrr        1.2.0      ✔ yardstick    1.3.2
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard() masks scales::discard()
#> ✖ dplyr::filter()  masks stats::filter()
#> ✖ dplyr::lag()     masks stats::lag()
#> ✖ recipes::step()  masks stats::step()
library(measure)
```

``` r
data("credit_data")

set.seed(55)
train_test_split <- initial_split(credit_data)

credit_train <- training(train_test_split)
credit_test <- testing(train_test_split)
```

### Creating a Recipe

We specify a recipe providing formula and data arguments. Check out
[*Tidy Modeling with R*](https://www.tmwr.org/base-r.html) to learn more
about specifying formulas in R.

``` r
rec_obj <- recipe(Status ~ ., data = credit_train)
```

The `recipe` funtion returns a recipe object. The formula argument
determines the roles of each variables. `Status` is assigned the role of
`outcome`, and the 13 other variables are assigned to role of
`predictor`.

``` r
rec_obj
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> outcome:    1
#> predictor: 13
```

Diving a bit deeper, the recipe object is a list with 7 elements. Within
these elements, we can see more details are saved about our variables.
This includes the `type` and `source` stored in `rec_obj$var_info`.

``` r
cat(names(rec_obj), sep = "\n")
#> var_info
#> term_info
#> steps
#> template
#> levels
#> retained
#> requirements
#> ptype
#> strings_as_factors
rec_obj$var_info
#> # A tibble: 14 × 4
#>    variable  type      role      source  
#>    <chr>     <list>    <chr>     <chr>   
#>  1 Seniority <chr [2]> predictor original
#>  2 Home      <chr [3]> predictor original
#>  3 Time      <chr [2]> predictor original
#>  4 Age       <chr [2]> predictor original
#>  5 Marital   <chr [3]> predictor original
#>  6 Records   <chr [3]> predictor original
#>  7 Job       <chr [3]> predictor original
#>  8 Expenses  <chr [2]> predictor original
#>  9 Income    <chr [2]> predictor original
#> 10 Assets    <chr [2]> predictor original
#> 11 Debt      <chr [2]> predictor original
#> 12 Amount    <chr [2]> predictor original
#> 13 Price     <chr [2]> predictor original
#> 14 Status    <chr [3]> outcome   original
```

### Adding a Step

The recipe does not yet contain any steps.

``` r
rec_obj$steps
#> NULL

rec_obj_add_step <- rec_obj %>%
  step_impute_knn(all_predictors())

rec_obj_add_step$steps
#> [[1]]
#> • K-nearest neighbor imputation for: all_predictors()
```
