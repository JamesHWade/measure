# bake() check_new_data() reports missing columns cleanly

    Code
      recipes::bake(prepped$steps[[1]], new_data = df["id"])
    Condition
      Error in `step_measure_snv()`:
      ! The following required column is missing from `new_data`: .measures.

