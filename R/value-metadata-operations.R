# Internal helpers for carrying value metadata through recipe operations.

# `new_measure_list()` rebuilds the measure column after each operation. Keep
# metadata at the column level and derive it from the original bake input so
# that a transformation cannot accidentally preserve stale or undeclared
# metadata from an intermediate result.
.measure_operation_result <- function(
  input,
  result,
  quantity = NULL,
  unit = NULL
) {
  result <- new_measure_list(result)
  metadata <- measure_value_metadata(input)

  if (is.null(metadata)) {
    return(result)
  }

  if (is.null(quantity)) {
    quantity <- metadata$quantity
    unit <- metadata$unit
  }

  set_measure_value_metadata(
    result,
    quantity = quantity,
    unit = unit
  )
}

.abort_tagged_normalization <- function(reason) {
  cli::cli_abort(c(
    "Cannot normalize tagged measurement values: {reason}.",
    "i" = "Resolve the singular or missing normalizer before baking typed data."
  ))
}
