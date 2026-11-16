# Compile the fixed declaration into public API inputs and observe actual R data.
# Numerical truth lives in committed binary64 words, never JSON floating numbers.

decode_atom <- function(x, label) {
  if (!is.list(x) || is.null(names(x)) || length(x) != 1L) {
    stop(label, " must be one double atom.", call. = FALSE)
  }
  if (identical(names(x), "hex")) {
    value <- read_big_endian_doubles(hex_to_raw(x$hex, label), 1L, label)
    if (!is.finite(value)) {
      stop(label, " hex must encode a finite value.", call. = FALSE)
    }
    return(value)
  }
  if (!identical(names(x), "state")) {
    stop(label, " has an unknown atom field.", call. = FALSE)
  }
  switch(
    x$state,
    na = NA_real_,
    nan = NaN,
    positive_infinity = Inf,
    negative_infinity = -Inf,
    stop(label, " has an unknown numeric state.", call. = FALSE)
  )
}

decode_vector <- function(x, here, label) {
  storage <- json_field_string(x, "storage", label)
  if (!is.null(x$corpus)) {
    if (
      !identical(names(x), c("storage", "corpus")) &&
        !setequal(names(x), c("storage", "corpus"))
    ) {
      stop(label, " has unexpected corpus fields.", call. = FALSE)
    }
    if (
      !identical(storage, "double") ||
        !identical(x$corpus, "finite-stress.bin")
    ) {
      stop(label, " has an unsupported corpus reference.", call. = FALSE)
    }
    bytes <- read_raw_file(
      file.path(here, "finite-stress.bin"),
      max_bytes = 65664
    )
    if (length(bytes) != 65664L) {
      stop(
        "The fixed finite corpus must contain 8208 binary64 values.",
        call. = FALSE
      )
    }
    connection <- rawConnection(bytes, "rb")
    on.exit(close(connection), add = TRUE)
    values <- readBin(
      connection,
      double(),
      n = 8208L,
      size = 8L,
      endian = "big"
    )
    if (!all(is.finite(values))) {
      stop(
        "The declared finite corpus contains a nonfinite value.",
        call. = FALSE
      )
    }
    return(values)
  }
  if (!setequal(names(x), c("storage", "data")) || !is.list(x$data)) {
    stop(label, " requires exactly storage and data.", call. = FALSE)
  }
  if (identical(storage, "double")) {
    return(vapply(
      x$data,
      decode_atom,
      double(1),
      label = label,
      USE.NAMES = FALSE
    ))
  }
  if (identical(storage, "integer")) {
    return(vapply(
      x$data,
      function(value) {
        if (is.null(value)) {
          return(NA_integer_)
        }
        if (
          !is.numeric(value) ||
            length(value) != 1L ||
            !is.finite(value) ||
            value != trunc(value) ||
            value < -2147483647 ||
            value > 2147483647
        ) {
          stop(label, " contains an invalid integer.", call. = FALSE)
        }
        as.integer(value)
      },
      integer(1),
      USE.NAMES = FALSE
    ))
  }
  stop(label, " declares unsupported storage.", call. = FALSE)
}

decode_ordinals <- function(x) {
  if (!is.list(x)) {
    stop("Source rows must be a JSON array.", call. = FALSE)
  }
  vapply(
    x,
    function(value) {
      if (is.null(value)) {
        return(NA_integer_)
      }
      if (
        !is.numeric(value) ||
          length(value) != 1L ||
          !is.finite(value) ||
          value != trunc(value) ||
          value <= 0 ||
          value > 2147483647
      ) {
        stop("Invalid source record ordinal.", call. = FALSE)
      }
      as.integer(value)
    },
    integer(1),
    USE.NAMES = FALSE
  )
}

decode_mask <- function(x) {
  if (!is.list(x)) {
    stop("Saturation masks must be JSON arrays.", call. = FALSE)
  }
  vapply(
    x,
    function(value) {
      if (is.null(value)) {
        return(NA)
      }
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        stop("A saturation entry must be boolean or null.", call. = FALSE)
      }
      value
    },
    logical(1),
    USE.NAMES = FALSE
  )
}

fixture_text <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("Fixture text must be one string or null.", call. = FALSE)
  }
  enc2utf8(value)
}

fixture_integer <- function(value) {
  if (is.null(value)) {
    return(NA_integer_)
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value != trunc(value) ||
      abs(value) > 2147483647
  ) {
    stop("Fixture integer metadata is invalid.", call. = FALSE)
  }
  as.integer(value)
}

metadata_frame <- function(
  rows,
  fields,
  integers = character(),
  doubles = character()
) {
  if (!is.list(rows)) {
    stop("Fixture rows must be an array.", call. = FALSE)
  }
  columns <- lapply(fields, function(field) {
    if (field %in% integers) {
      vapply(
        rows,
        function(row) fixture_integer(row[[field]]),
        integer(1),
        USE.NAMES = FALSE
      )
    } else if (field %in% doubles) {
      vapply(
        rows,
        function(row) {
          if (is.null(row[[field]])) {
            NA_real_
          } else {
            decode_atom(row[[field]], field)
          }
        },
        double(1),
        USE.NAMES = FALSE
      )
    } else {
      vapply(
        rows,
        function(row) fixture_text(row[[field]]),
        character(1),
        USE.NAMES = FALSE
      )
    }
  })
  names(columns) <- fields
  tibble::as_tibble(columns, .name_repair = "minimal")
}

build_measure <- function(x, here, nd = FALSE) {
  value <- decode_vector(x$value, here, "value")
  if (!nd) {
    return(measure::new_measure_tbl(
      decode_vector(x$location, here, "location"),
      value
    ))
  }
  measure::new_measure_nd_tbl(
    location_1 = decode_vector(x$location_1, here, "location_1"),
    location_2 = decode_vector(x$location_2, here, "location_2"),
    value = value,
    dim_names = json_array_strings_and_null(x$dim_names, "dim_names"),
    dim_units = json_array_strings_and_null(x$dim_units, "dim_units")
  )
}

build_record <- function(x, here) {
  fields <- list(
    samples = "sample_id",
    injections = c("injection_id", "sample_id"),
    channels = c("channel_id", "channel_name"),
    sources = c("source_id", "source_name", "source_sha256"),
    curves = c(
      "curve_id",
      "injection_id",
      "channel_id",
      "source_id",
      "axis_name",
      "axis_unit",
      "value_name",
      "value_unit"
    )
  )
  tables <- lapply(names(fields), function(name) {
    metadata_frame(x[[name]], fields[[name]])
  })
  names(tables) <- names(fields)
  curves <- x$curves
  tables$curves$.measures <- lapply(curves, function(row) {
    # Curves declare location/value directly; raw scans use a measures field.
    build_measure(list(location = row$location, value = row$value), here)
  })
  tables$curves$.source_rows <- lapply(curves, function(row) {
    decode_ordinals(row$source_rows)
  })
  do.call(measure::measure_record, tables)
}

build_acquisition <- function(x, fixtures, here) {
  template <- fixtures$record_templates[[x$record_template]]
  if (is.null(template)) {
    stop("Unknown record template.", call. = FALSE)
  }
  streams <- metadata_frame(
    x$streams,
    c(
      "stream_id",
      "injection_id",
      "channel_id",
      "kind",
      "source_id",
      "time_unit",
      "mz_unit",
      "value_name",
      "value_unit"
    )
  )
  scans <- metadata_frame(
    x$scans,
    c(
      "scan_id",
      "stream_id",
      "scan_index",
      "scan_status",
      "retention_time",
      "polarity",
      "ms_level",
      "spectrum_type",
      "source_scan_id"
    ),
    integers = c("scan_index", "ms_level"),
    doubles = "retention_time"
  )
  scans$.measures <- lapply(x$scans, function(row) {
    build_measure(row$measures, here)
  })
  scans$.source_rows <- lapply(x$scans, function(row) {
    decode_ordinals(row$source_rows)
  })
  scans$.saturated <- lapply(x$scans, function(row) decode_mask(row$saturated))
  arrays <- metadata_frame(x$arrays, c("array_id", "stream_id"))
  arrays$.measures <- lapply(x$arrays, function(row) {
    build_measure(row$measures, here, nd = TRUE)
  })
  arrays$.source_rows <- lapply(x$arrays, function(row) {
    decode_ordinals(row$source_rows)
  })
  arrays$.saturated <- lapply(x$arrays, function(row) {
    decode_mask(row$saturated)
  })
  measure::measure_acquisition(
    build_record(template, here),
    streams,
    scans,
    arrays
  )
}

observe_atom <- function(x) {
  if (length(x) != 1L || typeof(x) != "double") {
    stop("An observed double atom must be one double.", call. = FALSE)
  }
  if (is.nan(x)) {
    return(list(state = json_string("nan")))
  }
  if (is.na(x)) {
    return(list(state = json_string("na")))
  }
  if (is.infinite(x)) {
    return(list(
      state = json_string(
        if (x > 0) "positive_infinity" else "negative_infinity"
      )
    ))
  }
  list(hex = json_string(double_vector_hex(x)[[1L]]))
}

observe_vector <- function(x) {
  storage <- typeof(x)
  if (!storage %in% c("integer", "double")) {
    stop("Unexpected numeric type.", call. = FALSE)
  }
  data <- if (identical(storage, "integer")) {
    lapply(x, function(value) if (is.na(value)) NULL else json_scalar(value))
  } else {
    lapply(seq_along(x), function(index) observe_atom(x[[index]]))
  }
  list(storage = json_string(storage), data = data)
}

observe_ordinals <- function(x) {
  lapply(x, function(value) if (is.na(value)) NULL else json_scalar(value))
}

observe_metadata_rows <- function(
  table,
  fields,
  integers = character(),
  doubles = character()
) {
  lapply(seq_len(nrow(table)), function(index) {
    row <- lapply(fields, function(field) {
      value <- table[[field]][[index]]
      if (field %in% doubles) {
        return(observe_atom(value))
      }
      if (is.na(value)) {
        return(NULL)
      }
      if (field %in% integers) json_scalar(value) else json_string(value)
    })
    names(row) <- fields
    row
  })
}

observe_measure <- function(x, nd = FALSE) {
  if (!nd) {
    return(list(
      location = observe_vector(x$location),
      value = observe_vector(x$value)
    ))
  }
  list(
    location_1 = observe_vector(x$location_1),
    location_2 = observe_vector(x$location_2),
    value = observe_vector(x$value),
    dim_names = json_character_array(attr(x, "dim_names", exact = TRUE)),
    dim_units = json_character_array(attr(x, "dim_units", exact = TRUE))
  )
}

observe_record <- function(x) {
  fields <- list(
    samples = "sample_id",
    injections = c("injection_id", "sample_id"),
    channels = c("channel_id", "channel_name"),
    sources = c("source_id", "source_name", "source_sha256"),
    curves = c(
      "curve_id",
      "injection_id",
      "channel_id",
      "source_id",
      "axis_name",
      "axis_unit",
      "value_name",
      "value_unit"
    )
  )
  out <- lapply(names(fields), function(name) {
    observe_metadata_rows(x[[name]], fields[[name]])
  })
  names(out) <- names(fields)
  out$curves <- lapply(seq_len(nrow(x$curves)), function(index) {
    c(
      out$curves[[index]],
      observe_measure(x$curves$.measures[[index]]),
      list(source_rows = observe_ordinals(x$curves$.source_rows[[index]]))
    )
  })
  out
}

observe_acquisition <- function(x, id) {
  streams <- observe_metadata_rows(
    x$streams,
    c(
      "stream_id",
      "injection_id",
      "channel_id",
      "kind",
      "source_id",
      "time_unit",
      "mz_unit",
      "value_name",
      "value_unit"
    )
  )
  scans <- observe_metadata_rows(
    x$scans,
    c(
      "scan_id",
      "stream_id",
      "scan_index",
      "scan_status",
      "retention_time",
      "polarity",
      "ms_level",
      "spectrum_type",
      "source_scan_id"
    ),
    integers = c("scan_index", "ms_level"),
    doubles = "retention_time"
  )
  scans <- lapply(seq_along(scans), function(index) {
    c(
      scans[[index]],
      list(
        measures = observe_measure(x$scans$.measures[[index]]),
        source_rows = observe_ordinals(x$scans$.source_rows[[index]]),
        saturated = lapply(x$scans$.saturated[[index]], function(v) {
          if (is.na(v)) NULL else json_scalar(v)
        })
      )
    )
  })
  arrays <- observe_metadata_rows(x$arrays, c("array_id", "stream_id"))
  arrays <- lapply(seq_along(arrays), function(index) {
    c(
      arrays[[index]],
      list(
        measures = observe_measure(x$arrays$.measures[[index]], nd = TRUE),
        source_rows = observe_ordinals(x$arrays$.source_rows[[index]]),
        saturated = lapply(x$arrays$.saturated[[index]], function(v) {
          if (is.na(v)) NULL else json_scalar(v)
        })
      )
    )
  })
  list(
    id = json_string(id),
    record = observe_record(x$record),
    streams = streams,
    scans = scans,
    arrays = arrays
  )
}

trace_arguments <- function(x, acquisition, here) {
  args <- x[c("stream_id", "polarity", "ms_level", "mz_range")]
  if (!is.null(args$mz_range)) {
    args$mz_range <- decode_vector(args$mz_range, here, "mz_range")
  }
  c(list(x = acquisition), args)
}

observe_trace <- function(x) {
  rows <- observe_metadata_rows(
    x,
    c(
      "scan_id",
      "scan_index",
      "source_scan_id",
      "retention_time",
      "value",
      "status",
      "selected_points"
    ),
    integers = c("scan_index", "selected_points"),
    doubles = c("retention_time", "value")
  )
  rows <- lapply(seq_along(rows), function(index) {
    c(
      rows[[index]],
      list(
        saturation_unknown = json_scalar(x$saturation_unknown[[index]]),
        point_indices = lapply(x$.point_indices[[index]], json_scalar)
      )
    )
  })
  processing <- attr(x, "processing", exact = TRUE)
  processing <- lapply(names(processing), function(name) {
    value <- attr(x, "processing", exact = TRUE)[[name]]
    if (is.null(value)) {
      return(NULL)
    }
    if (identical(name, "mz_range")) {
      return(observe_vector(value))
    }
    if (is.na(value)) {
      return(NULL)
    }
    if (is.character(value)) json_string(value) else json_scalar(value)
  }) |>
    stats::setNames(names(attr(x, "processing", exact = TRUE)))
  list(rows = rows, processing = processing)
}
