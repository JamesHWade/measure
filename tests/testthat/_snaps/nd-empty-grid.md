# empty grid inspection validates its structural prototype

    Code
      measure_grid_info(missing)
    Condition
      Error in `.measure_nd_list_ptype()`:
      ! An empty <measure_nd_list> must have a valid ptype.

---

    Code
      measure_grid_info(populated)
    Condition
      Error in `.measure_nd_list_ptype()`:
      ! The ptype of `x` must have zero rows.

---

    Code
      measure_grid_info(malformed)
    Condition
      Error in `.nd_validate_tbl()`:
      ! The `ndim` attribute in `x ptype` must equal the number of location columns (3).
