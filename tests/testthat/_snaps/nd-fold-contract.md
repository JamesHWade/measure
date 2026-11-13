# fold validates metadata fields before rebuilding an nD object

    Code
      measure_fold(with_info(alter_info(valid_info, coordinates = list(dim_1 = 1:4))))
    Condition
      Error in `.nd_validate_fold_info()`:
      ! Fold metadata coordinates must contain exactly one vector per dimension.

# partial member metadata never falls back to a shared grid

    Code
      measure_fold(unfolded)
    Condition
      Error in `measure_fold()`:
      ! Cannot fold a list with partial member metadata.
      i Every member must carry its own fold_info attribute.

# legacy list-level shared metadata is accepted only when valid

    Code
      measure_fold(legacy)
    Condition
      Error in `.nd_validate_fold_order()`:
      ! `fold_info$order` must be a nonempty integral complete permutation of 1:2.

# fold metadata rejects Unicode-only unit labels

    Code
      measure_fold(unfolded)
    Condition
      Error in `.nd_validate_fold_info()`:
      ! Fold metadata dim_units cannot contain blank values.

# fold never flattens a malformed matrix-valued signal

    Code
      measure_fold(unfolded)
    Condition
      Error in `.nd_check_numeric_vector()`:
      ! `x$value` must be numeric, specifically a plain one-dimensional integer or double vector, not an integer matrix.

# fold rejects dimensional list containers before reconstruction

    Code
      measure_fold(bad_list)
    Condition
      Error in `measure_fold()`:
      ! Fold input must be a one-dimensional list of measurements.

---

    Code
      measure_fold(bad_info)
    Condition
      Error in `.nd_validate_fold_info()`:
      ! Fold metadata must be a one-dimensional named list containing ndim, dim_names, dim_units, order, and coordinates.

---

    Code
      measure_fold(bad_coordinates)
    Condition
      Error in `.nd_validate_fold_info()`:
      ! Fold metadata coordinates must contain exactly one vector per dimension.
