# nD list subsetting keeps strict vctrs index semantics

    Code
      measurements[1.5]
    Condition
      Error in `vctrs::vec_slice()`:
      ! Can't subset elements with `i`.
      x Can't convert from `i` <double> to <integer> due to loss of precision.

---

    Code
      measurements[c(TRUE, FALSE, TRUE)]
    Condition
      Error in `vctrs::vec_slice()`:
      ! Can't subset elements with `i`.
      x Logical subscript `i` must be size 1 or 2, not 3.

---

    Code
      measurements[3L]
    Condition
      Error in `vctrs::vec_slice()`:
      ! Can't subset elements past the end.
      i Location 3 doesn't exist.
      i There are only 2 elements.

---

    Code
      measurements["missing"]
    Condition
      Error in `vctrs::vec_slice()`:
      ! Can't subset elements that don't exist.
      x Element `missing` doesn't exist.

---

    Code
      measurements[NA_integer_]
    Condition
      Error in `FUN()`:
      ! Each element of measure_nd_list must be a data frame, not NULL.

# nD list subsetting rejects extra and missing dimensions

    Code
      measurements[, 1]
    Condition
      Error in `measurements[, 1]`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

---

    Code
      measurements[, ]
    Condition
      Error in `measurements[, ]`:
      ! Additional or missing subscripts are not supported for <measure_nd_list>.

---

    Code
      measurements[, , ]
    Condition
      Error in `measurements[, , ]`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = <empty>
      * ..2 = <empty>
      i Did you forget to name an argument?

---

    Code
      measurements[, 1, drop = FALSE]
    Condition
      Error in `measurements[, 1, drop = FALSE]`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

---

    Code
      measurements[foo = 1]
    Condition
      Error in `measurements[foo = 1]`:
      ! `...` must be empty.
      x Problematic argument:
      * foo = 1

# empty nD lists require a valid dimensional prototype

    Code
      measure_ndim(missing_ptype)
    Condition
      Error in `.measure_nd_list_ptype()`:
      ! An empty <measure_nd_list> must have a valid ptype.

---

    Code
      measure_ndim(malformed)
    Condition
      Error in `.nd_validate_tbl()`:
      ! The `ndim` attribute in `x ptype` must equal the number of location columns (2).

---

    Code
      measure_ndim(malformed_empty)
    Condition
      Error in `.measure_nd_list_ptype()`:
      ! The ptype of `x` must have zero rows.

---

    Code
      source[0]
    Condition
      Error in `.measure_nd_list_ptype()`:
      ! The ptype of `x` must have zero rows.
