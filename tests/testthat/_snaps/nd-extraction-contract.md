# nD list slices preserve a shared schema or report mixed results

    Code
      measure_slice(dads, wavelength = 254)
    Condition
      Warning:
      Slice returned no data points.
      Error in `.nd_assemble_list_results()`:
      ! Slicing <measure_nd_list> members produced mixed scalar, 1D, and nD results.
      i Use `drop = FALSE` to retain all axes, or map the slice over members separately.

# nD list projections retain homogeneous result types and schemas

    Code
      measure_project(new_measure_nd_list(list(cube_a, cube_b)), along = 1)
    Condition
      Error in `.nd_assemble_list_results()`:
      ! Extraction results across <measure_nd_list> members retained different axis positions, names, or units.
      i Map the operation over members separately.

# nD list projections validate retained 1D axis schemas

    Code
      measure_project(new_measure_nd_list(different_traces), along = 2)
    Condition
      Error in `.nd_assemble_list_results()`:
      ! Extraction results across <measure_nd_list> members retained different axis positions, names, or units.
      i Map the operation over members separately.

# reserved dimension tokens match the entire selector

    Code
      do.call(measure_slice, c(list(x), bad_reserved))
    Condition
      Error in `FUN()`:
      ! Unknown dimension reference: "dim_1 ". Use dim_1, dim_2, an integer dimension string, or a known dimension name.

---

    Code
      do.call(measure_slice, c(list(x), bad_numeric))
    Condition
      Error in `FUN()`:
      ! Unknown dimension reference: "1 ". Use dim_1, dim_2, an integer dimension string, or a known dimension name.

# list slices do not flatten repeated observations into scalar results

    Code
      measure_slice(pair, dim_2 = 254)
    Condition
      Error in `.nd_assemble_list_results()`:
      ! Dropping all axes left multiple observations in a list member.
      i Use `drop = FALSE` or map the slice over members separately.

