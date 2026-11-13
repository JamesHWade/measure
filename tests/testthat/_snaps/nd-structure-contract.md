# nD constructors reject noncanonical location suffixes

    Code
      new_measure_nd_tbl(location_1 = 1:2, location_3 = 1:2, value = 1:2)
    Condition
      Error in `new_measure_nd_tbl()`:
      ! Location argument suffixes must be unique and exactly 1 through 2.

---

    Code
      new_measure_nd_tbl(location_0 = 1:2, location_1 = 1:2, value = 1:2)
    Condition
      Error in `new_measure_nd_tbl()`:
      ! Location argument suffixes must be unique and exactly 1 through 2.

---

    Code
      new_measure_nd_tbl(location_1 = 1:2,
      location_999999999999999999999999999999999999 = 1:2, value = 1:2)
    Condition
      Error in `new_measure_nd_tbl()`:
      ! Location argument suffixes must be unique and exactly 1 through 2.

---

    Code
      do.call(new_measure_nd_tbl, duplicate_names)
    Condition
      Error:
      ! Location argument suffixes must be unique and exactly 1 through 2.

# nD constructors require plain numeric vectors and dimension metadata

    Code
      new_measure_nd_tbl(location_1 = matrix(1:2), location_2 = 1:2, value = 1:2)
    Condition
      Error in `.nd_check_numeric_vector()`:
      ! `location_1` must be numeric, specifically a plain one-dimensional integer or double vector, not an integer matrix.

---

    Code
      new_measure_nd_tbl(location_1 = as.Date(c("2020-01-01", "2020-01-02")),
      location_2 = 1:2, value = 1:2)
    Condition
      Error in `.nd_check_numeric_vector()`:
      ! `location_1` must be numeric, specifically a plain one-dimensional integer or double vector, not a <Date> object.

---

    Code
      new_measure_nd_tbl(location_1 = 1:2, location_2 = 1:2, value = 1:2, dim_names = c(
        "time", "time"))
    Condition
      Error in `.nd_check_dim_metadata()`:
      ! Known values in `dim_names` must be unique; missing values are allowed.

---

    Code
      new_measure_nd_tbl(location_1 = 1:2, location_2 = 1:2, value = 1:2, dim_names = c(
        "time", " "))
    Condition
      Error in `.nd_check_dim_metadata()`:
      ! Known values in `dim_names` must be nonblank; use `NA_character_` for unknown values.

---

    Code
      new_measure_nd_tbl(location_1 = 1:2, location_2 = 1:2, value = 1:2, dim_units = 1:
        2)
    Condition
      Error in `.nd_check_dim_metadata()`:
      ! `dim_units` must be NULL or an unclassed one-dimensional character vector.

---

    Code
      new_measure_nd_tbl(location_1 = 1:2, location_2 = 1:2, value = 1:2, dim_units = matrix(
        c("min", "nm"), nrow = 1))
    Condition
      Error in `.nd_check_dim_metadata()`:
      ! `dim_units` must be NULL or an unclassed one-dimensional character vector.

# nD mutable structures are checked at list and grid entry points

    Code
      .nd_validate_tbl(bad_columns)
    Condition
      Error in `.nd_validate_tbl()`:
      ! Columns in `x` must be exactly "location_1", "location_2", and "value" in canonical order.

---

    Code
      measure_grid_info(bad_columns)
    Condition
      Error in `.nd_validate_tbl()`:
      ! Columns in `x` must be exactly "location_1", "location_2", and "value" in canonical order.

---

    Code
      new_measure_nd_list(list(bad_order))
    Condition
      Error in `.nd_validate_tbl()`:
      ! `each measure_nd_tbl element` must have canonical `dim_order` equal to 1 and 2.

---

    Code
      new_measure_nd_list(list(bad_value))
    Condition
      Error in `.nd_check_numeric_vector()`:
      ! `each measure_nd_tbl element$value` must be numeric, specifically a plain one-dimensional integer or double vector, not a <factor> object.

---

    Code
      measure_grid_info(bad_names)
    Condition
      Error in `.nd_check_dim_metadata()`:
      ! Known values in `dim_names` must be unique; missing values are allowed.

---

    Code
      measure_grid_info(bad_units)
    Condition
      Error in `.nd_check_dim_metadata()`:
      ! `dim_units` must be NULL or an unclassed one-dimensional character vector.

---

    Code
      measure_grid_info(bad_list)
    Condition
      Error in `.nd_validate_tbl()`:
      ! Columns in `each measure_nd_tbl element` must be exactly "location_1", "location_2", and "value" in canonical order.

---

    Code
      .nd_validate_tbl(bad_ndim)
    Condition
      Error in `.nd_validate_tbl()`:
      ! The `ndim` attribute in `x` must equal the number of location columns (2).

# nD semantic metadata is unclassed character data

    Code
      new_measure_nd_tbl(location_1 = 1:2, location_2 = 3:4, value = 5:6, dim_names = I(
        c("time", "wavelength")))
    Condition
      Error in `.nd_check_dim_metadata()`:
      ! `dim_names` must be NULL or an unclassed one-dimensional character vector.

# nD grid inspection rejects forged mixed-dimension collections

    Code
      measure_grid_info(mixed)
    Condition
      Error in `.nd_validate_list()`:
      ! All elements of measure_nd_list must have the same dimensionality. Found: 2 and 3.

---

    Code
      measure_is_regular(mixed)
    Condition
      Error in `.nd_validate_list()`:
      ! All elements of measure_nd_list must have the same dimensionality. Found: 2 and 3.
