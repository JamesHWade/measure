# vctrs methods for measure classes

These methods enable nice printing of measure objects in tibbles.

## Usage

``` r
# S3 method for class 'measure_tbl'
print(x, ..., n = NULL, width = NULL)

# S3 method for class 'measure_tbl'
vec_ptype_abbr(x, ...)

# S3 method for class 'measure_list'
print(x, ...)

# S3 method for class 'measure_list'
vec_ptype_abbr(x, ...)

# S3 method for class 'measure_list'
vec_ptype_full(x, ...)

# S3 method for class 'measure_list'
format(x, ...)

# S3 method for class 'measure_list'
obj_print_data(x, ...)

# S3 method for class 'measure_list'
x[i, ...]

# S3 method for class 'measure_list'
x[[i, ...]]

# S3 method for class 'measure_list'
c(...)

# S3 method for class 'measure_nd_tbl'
print(x, ..., n = NULL, width = NULL)

# S3 method for class 'measure_nd_tbl'
vec_ptype_abbr(x, ...)

# S3 method for class 'measure_nd_list'
print(x, ...)

# S3 method for class 'measure_nd_list'
vec_ptype_abbr(x, ...)

# S3 method for class 'measure_nd_list'
vec_ptype_full(x, ...)

# S3 method for class 'measure_nd_list'
format(x, ...)

# S3 method for class 'measure_nd_list'
obj_print_data(x, ...)

# S3 method for class 'measure_nd_list'
x[i, ...]

# S3 method for class 'measure_nd_list'
x[[i, ...]]

# S3 method for class 'measure_nd_list'
c(...)
```

## Arguments

- x:

  A measure_list or measure_tbl object.

- ...:

  Additional arguments (unused).
