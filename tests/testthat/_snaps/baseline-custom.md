# tunable declarations require names

    Code
      step_measure_baseline_custom(rec, .fn = ~ .x$value, degree = 2L, tunable = list(
        list(pkg = "dials", fun = "degree_int")))
    Condition
      Error in `step_measure_baseline_custom()`:
      ! `tunable` must have non-empty names for each declaration.

# tunable declarations require unique names

    Code
      step_measure_baseline_custom(rec, .fn = ~ .x$value, degree = 2L, tunable = duplicate_specs)
    Condition
      Error in `step_measure_baseline_custom()`:
      ! Duplicate parameter names in `tunable`: "degree".

# declared tunable arguments must be provided exactly once

    Code
      do.call(step_measure_baseline_custom, c(list(recipe = rec, .fn = ~ .x$value),
      duplicate_args, list(tunable = list(degree = spec))))
    Condition
      Error:
      ! Tunable callback arguments must be supplied once: "degree".

# tunable declarations must match callback argument names

    Code
      step_measure_baseline_custom(rec, .fn = ~ .x$value, degree = 2L, tunable = list(
        span = list(pkg = "dials", fun = "degree_int")))
    Condition
      Error in `step_measure_baseline_custom()`:
      ! Each `tunable` name must match a named argument in `...`: "span".

# tunable declarations require package and function strings

    Code
      step_measure_baseline_custom(rec, .fn = ~ .x$value, degree = 2L, tunable = list(
        degree = list(pkg = "dials")))
    Condition
      Error in `step_measure_baseline_custom()`:
      ! Each `tunable` declaration must include non-empty scalar character values for pkg and fun.
      x Invalid declarations: "degree".

---

    Code
      step_measure_baseline_custom(rec, .fn = ~ .x$value, degree = 2L, tunable = list(
        degree = list(fun = "degree_int")))
    Condition
      Error in `step_measure_baseline_custom()`:
      ! Each `tunable` declaration must include non-empty scalar character values for pkg and fun.
      x Invalid declarations: "degree".

