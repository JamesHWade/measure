#!/usr/bin/env Rscript

PROFILE <- "measure-nd-interchange/1"
INPUT_NAMES <- c("fixtures.json", "declaration.json", "finite-stress.bin")
EXPECTED_OBJECT_COUNT <- 15L
EXPECTED_FORWARD_COUNT <- 30L
EXPECTED_REVERSE_COUNT <- 2L
EXPECTED_REORDERED_COUNT <- 2L
EXPECTED_INVALID_COUNT <- 49L
EXPECTED_CROSS_FORMAT_COUNT <- 2L
FINITE_STRESS_COUNT <- 8208L
MAX_HASH_BYTES <- 100 * 1024^2

json_scalar <- function(x) {
  jsonlite::unbox(x)
}

json_string <- function(x) {
  json_scalar(enc2utf8(as.character(x)))
}

json_character_array <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  lapply(seq_along(x), function(i) {
    value <- x[[i]]
    if (is.na(value)) NULL else json_string(value)
  })
}

json_string_array <- function(x) {
  lapply(seq_along(x), function(i) json_string(x[[i]]))
}

json_text <- function(x, pretty = TRUE) {
  as.character(jsonlite::toJSON(
    x,
    auto_unbox = FALSE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = pretty
  ))
}

write_json_file <- function(x, path) {
  text <- paste0(json_text(x), "\n")
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), connection)
  invisible(path)
}

canonical_json <- function(x) {
  json_text(x, pretty = FALSE)
}

normalize_runtime <- function(x) {
  jsonlite::fromJSON(
    json_text(x, pretty = FALSE),
    simplifyVector = FALSE,
    bigint_as_char = TRUE
  )
}

assert_json_equal <- function(actual, expected, label) {
  if (!identical(canonical_json(actual), canonical_json(expected))) {
    stop(label, " did not match the frozen fixture.", call. = FALSE)
  }
  invisible(TRUE)
}

utc_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC", usetz = FALSE)
}

path_type <- function(path) {
  info <- fs::file_info(path, fail = FALSE, follow = FALSE)
  if (nrow(info) != 1L || is.na(info$type[[1L]])) {
    return(NA_character_)
  }
  as.character(info$type[[1L]])
}

is_path_entry <- function(path) {
  type <- path_type(path)
  if (!is.na(type)) {
    return(TRUE)
  }
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

assert_regular_file <- function(path, label = path, max_bytes = NULL) {
  type <- path_type(path)
  if (!identical(type, "file")) {
    stop(label, " must be a regular nonsymlink file.", call. = FALSE)
  }
  size <- as.numeric(fs::file_info(path, follow = FALSE)$size[[1L]])
  if (length(size) != 1L || is.na(size) || size < 0) {
    stop(label, " has an invalid file size.", call. = FALSE)
  }
  if (!is.null(max_bytes) && size > max_bytes) {
    stop(label, " exceeds its bounded byte limit.", call. = FALSE)
  }
  invisible(size)
}

assert_directory <- function(path, label = path) {
  if (!identical(path_type(path), "directory")) {
    stop(label, " must be a directory.", call. = FALSE)
  }
  invisible(path)
}

read_raw_file <- function(path, label = path, max_bytes = NULL) {
  size <- assert_regular_file(path, label, max_bytes)
  if (size > .Machine$integer.max) {
    stop(label, " is too large for this qualification tool.", call. = FALSE)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes <- readBin(connection, what = "raw", n = as.integer(size))
  if (length(bytes) != size) {
    stop(label, " changed while it was being read.", call. = FALSE)
  }
  bytes
}

sha256_file <- function(path, label = path) {
  assert_regular_file(path, label, max_bytes = MAX_HASH_BYTES)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

read_json_file <- function(path, label = path, max_bytes = 2 * 1024^2) {
  bytes <- read_raw_file(path, label, max_bytes)
  if (length(bytes) == 0L) {
    stop(label, " is empty JSON.", call. = FALSE)
  }
  text <- rawToChar(bytes)
  document <- jsonlite::fromJSON(
    text,
    simplifyVector = FALSE,
    bigint_as_char = TRUE
  )
  validate_strict_json(document, label)
  document
}

validate_strict_json <- function(value, label) {
  visit <- function(node, location) {
    if (is.list(node)) {
      field_names <- names(node)
      if (!is.null(field_names) && anyDuplicated(field_names) > 0L) {
        stop(location, " contains duplicate JSON object fields.", call. = FALSE)
      }
      for (index in seq_along(node)) {
        child_location <- if (is.null(field_names)) {
          paste0(location, "[", index, "]")
        } else {
          paste0(location, ".", field_names[[index]])
        }
        visit(node[[index]], child_location)
      }
    } else if (is.numeric(node) && !all(is.finite(node))) {
      stop(location, " contains a nonfinite JSON number.", call. = FALSE)
    }
    invisible(NULL)
  }
  visit(value, label)
  invisible(value)
}

json_array_character <- function(x, label) {
  if (is.null(x) || !is.list(x)) {
    stop(label, " must be a JSON array.", call. = FALSE)
  }
  if (length(x) == 0L) {
    return(character())
  }
  vapply(
    x,
    function(value) {
      if (is.null(value) || length(value) != 1L) {
        stop(label, " contains a non-scalar value.", call. = FALSE)
      }
      as.character(value)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

json_array_strings_and_null <- function(x, label) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.list(x)) {
    stop(label, " must be a JSON array or null.", call. = FALSE)
  }
  if (length(x) == 0L) {
    return(character())
  }
  vapply(
    x,
    function(value) {
      if (is.null(value)) {
        NA_character_
      } else if (length(value) == 1L && is.character(value)) {
        enc2utf8(value)
      } else {
        stop(
          label,
          " must contain only strings or null entries.",
          call. = FALSE
        )
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

json_field_string <- function(x, name, label = name) {
  value <- x[[name]]
  if (is.null(value) || length(value) != 1L || !is.character(value)) {
    stop(label, " must be a string.", call. = FALSE)
  }
  enc2utf8(value)
}

json_field_integer <- function(x, name, label = name) {
  value <- x[[name]]
  if (
    is.null(value) ||
      length(value) != 1L ||
      !is.numeric(value) ||
      !is.finite(value) ||
      value != trunc(value) ||
      value < -.Machine$integer.max ||
      value > .Machine$integer.max
  ) {
    stop(
      label,
      " must be a whole number in the supported range.",
      call. = FALSE
    )
  }
  as.integer(value)
}

json_field_true <- function(x, name, label = name) {
  value <- x[[name]]
  if (length(value) != 1L || !is.logical(value) || is.na(value) || !value) {
    stop(label, " must be true.", call. = FALSE)
  }
  invisible(TRUE)
}

nullable_character_vector <- function(x, label) {
  values <- json_array_strings_and_null(x, label)
  if (is.null(values)) {
    return(NULL)
  }
  values
}

raw_hex <- function(bytes) {
  paste0(sprintf("%02x", as.integer(bytes)), collapse = "")
}

hex_to_raw <- function(hex, label) {
  if (
    length(hex) != 1L ||
      is.na(hex) ||
      !grepl("^[0-9a-f]{16}$", hex, perl = TRUE)
  ) {
    stop(label, " must be 16 lowercase hexadecimal digits.", call. = FALSE)
  }
  pairs <- substring(hex, seq.int(1L, 15L, by = 2L), seq.int(2L, 16L, by = 2L))
  bytes <- strtoi(pairs, base = 16L)
  if (anyNA(bytes)) {
    stop(label, " contains invalid hexadecimal bytes.", call. = FALSE)
  }
  as.raw(bytes)
}

read_big_endian_doubles <- function(bytes, count, label) {
  if (length(bytes) != count * 8L) {
    stop(
      label,
      " does not contain the declared number of binary64 values.",
      call. = FALSE
    )
  }
  connection <- rawConnection(bytes, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(
    connection,
    what = double(),
    n = count,
    size = 8L,
    endian = "big"
  )
}

double_vector_hex <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }
  connection <- rawConnection(raw(), open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(as.double(x), connection, size = 8L, endian = "big")
  bytes <- rawConnectionValue(connection)
  vapply(
    seq_along(x),
    function(index) {
      start <- (index - 1L) * 8L + 1L
      raw_hex(bytes[start:(start + 7L)])
    },
    character(1),
    USE.NAMES = FALSE
  )
}

number_state <- function(value, storage) {
  if (identical(storage, "integer")) {
    if (is.na(value)) {
      return("na")
    }
    return("number")
  }
  if (is.nan(value)) {
    return("nan")
  }
  if (is.na(value)) {
    return("na")
  }
  if (is.infinite(value)) {
    if (value > 0) "+inf" else "-inf"
  } else {
    "number"
  }
}

observed_numeric_tokens <- function(x, storage) {
  if (identical(storage, "integer")) {
    return(lapply(seq_along(x), function(index) {
      value <- x[[index]]
      if (is.na(value)) {
        return(list(state = json_string("na")))
      }
      list(
        state = json_string("number"),
        number = json_scalar(as.integer(value))
      )
    }))
  }
  if (!identical(storage, "double")) {
    stop("Unsupported observed numeric storage.", call. = FALSE)
  }
  bits <- double_vector_hex(x)
  lapply(seq_along(x), function(index) {
    value <- x[[index]]
    state <- number_state(value, "double")
    if (identical(state, "number")) {
      list(
        state = json_string("number"),
        hex = json_string(bits[[index]])
      )
    } else {
      list(state = json_string(state))
    }
  })
}

decode_numeric_tokens <- function(tokens, storage, field) {
  if (!is.list(tokens)) {
    stop(field, " tokens must be a JSON array.", call. = FALSE)
  }
  if (!storage %in% c("integer", "double")) {
    stop(field, " declares an unsupported storage type.", call. = FALSE)
  }
  if (identical(storage, "integer")) {
    values <- integer(length(tokens))
    for (index in seq_along(tokens)) {
      token <- tokens[[index]]
      state <- json_field_string(token, "state", field)
      if (identical(state, "na")) {
        values[[index]] <- NA_integer_
      } else if (identical(state, "number")) {
        number <- token$number
        if (
          is.null(number) ||
            length(number) != 1L ||
            !is.numeric(number) ||
            !is.finite(number) ||
            number != trunc(number) ||
            number < -2147483647 ||
            number > 2147483647
        ) {
          stop(field, " has an invalid integer number token.", call. = FALSE)
        }
        values[[index]] <- as.integer(number)
      } else {
        stop(field, " has an unsupported integer state.", call. = FALSE)
      }
    }
    return(values)
  }
  values <- double(length(tokens))
  for (index in seq_along(tokens)) {
    token <- tokens[[index]]
    state <- json_field_string(token, "state", field)
    if (identical(state, "na")) {
      values[[index]] <- NA_real_
    } else if (identical(state, "nan")) {
      values[[index]] <- NaN
    } else if (identical(state, "+inf")) {
      values[[index]] <- Inf
    } else if (identical(state, "-inf")) {
      values[[index]] <- -Inf
    } else if (identical(state, "number")) {
      values[[index]] <- read_big_endian_doubles(
        hex_to_raw(json_field_string(token, "hex", field), field),
        1L,
        field
      )
    } else {
      stop(field, " has an unsupported double state.", call. = FALSE)
    }
  }
  values
}

decode_fixture_column <- function(column, binary_values, field) {
  name <- json_field_string(column, "name", paste0(field, " name"))
  storage <- json_field_string(column, "storage", paste0(field, " storage"))
  encoding <- json_field_string(column, "encoding", paste0(field, " encoding"))
  if (!storage %in% c("integer", "double")) {
    stop(field, " has an invalid storage declaration.", call. = FALSE)
  }

  if (identical(encoding, "values")) {
    return(decode_numeric_tokens(column$data, storage, field))
  }
  if (identical(encoding, "sequence")) {
    if (!identical(storage, "integer")) {
      stop(field, " sequence encoding must use integer storage.", call. = FALSE)
    }
    start <- json_field_integer(column, "start", paste0(field, " start"))
    n_values <- json_field_integer(column, "length", paste0(field, " length"))
    if (n_values < 0L) {
      stop(field, " sequence length cannot be negative.", call. = FALSE)
    }
    if (n_values == 0L) {
      return(integer())
    }
    end <- as.double(start) + as.double(n_values) - 1
    if (end > 2147483647 || end < -2147483647) {
      stop(
        field,
        " sequence exceeds the supported integer range.",
        call. = FALSE
      )
    }
    return(seq.int(start, length.out = n_values))
  }
  if (identical(encoding, "cycle")) {
    if (!identical(storage, "double")) {
      stop(field, " cycle encoding must use double storage.", call. = FALSE)
    }
    n_values <- json_field_integer(column, "length", paste0(field, " length"))
    if (n_values < 0L) {
      stop(field, " cycle length cannot be negative.", call. = FALSE)
    }
    source <- decode_numeric_tokens(column$data, "double", field)
    if (n_values == 0L) {
      return(double())
    }
    if (base::length(source) == 0L) {
      stop(field, " cannot cycle an empty token array.", call. = FALSE)
    }
    return(source[rep(seq_along(source), length.out = n_values)])
  }
  if (identical(encoding, "binary64")) {
    if (!identical(storage, "double")) {
      stop(field, " binary64 encoding must use double storage.", call. = FALSE)
    }
    file <- json_field_string(column, "file", paste0(field, " file"))
    endian <- json_field_string(column, "endian", paste0(field, " endian"))
    n_values <- json_field_integer(column, "length", paste0(field, " length"))
    if (
      !identical(file, "finite-stress.bin") ||
        !identical(endian, "big") ||
        n_values != FINITE_STRESS_COUNT ||
        is.null(binary_values) ||
        base::length(binary_values) != FINITE_STRESS_COUNT
    ) {
      stop(
        field,
        " does not match the frozen finite-stress corpus.",
        call. = FALSE
      )
    }
    return(binary_values)
  }
  stop(field, " uses an unsupported fixture encoding.", call. = FALSE)
}

decode_metadata <- function(x, label) {
  if (is.null(x)) {
    return(NULL)
  }
  nullable_character_vector(x, label)
}

decode_fixture_table <- function(table, ndim, binary_values, label) {
  columns <- table$columns
  if (is.null(columns) || !is.list(columns) || length(columns) != ndim + 1L) {
    stop(label, " has an invalid column array.", call. = FALSE)
  }
  expected_names <- c(paste0("location_", seq_len(ndim)), "value")
  column_names <- vapply(
    columns,
    function(column) {
      json_field_string(column, "name", paste0(label, " column name"))
    },
    character(1),
    USE.NAMES = FALSE
  )
  if (!identical(column_names, expected_names)) {
    stop(label, " columns are not in canonical order.", call. = FALSE)
  }
  values <- lapply(seq_along(columns), function(index) {
    decode_fixture_column(
      columns[[index]],
      binary_values,
      paste0(label, ".", column_names[[index]])
    )
  })
  names(values) <- expected_names
  dim_names <- decode_metadata(table$dim_names, paste0(label, " dim_names"))
  dim_units <- decode_metadata(table$dim_units, paste0(label, " dim_units"))
  args <- values[seq_len(ndim)]
  args$value <- values[[ndim + 1L]]
  args$dim_names <- dim_names
  args$dim_units <- dim_units
  do.call(measure::new_measure_nd_tbl, args)
}

fixture_expected_tokens <- function(column, binary_hex, field) {
  storage <- json_field_string(column, "storage", paste0(field, " storage"))
  encoding <- json_field_string(column, "encoding", paste0(field, " encoding"))
  if (identical(encoding, "values")) {
    return(lapply(column$data, function(token) {
      state <- json_field_string(token, "state", field)
      if (identical(state, "number")) {
        if (identical(storage, "integer")) {
          list(
            state = json_string("number"),
            number = json_scalar(as.integer(token$number))
          )
        } else {
          list(
            state = json_string("number"),
            hex = json_string(json_field_string(token, "hex", field))
          )
        }
      } else {
        list(state = json_string(state))
      }
    }))
  }
  if (identical(encoding, "sequence")) {
    start <- json_field_integer(column, "start", paste0(field, " start"))
    n_values <- json_field_integer(column, "length", paste0(field, " length"))
    if (n_values == 0L) {
      return(list())
    }
    return(lapply(seq_len(n_values), function(index) {
      list(
        state = json_string("number"),
        number = json_scalar(as.integer(start + index - 1L))
      )
    }))
  }
  if (identical(encoding, "cycle")) {
    n_values <- json_field_integer(column, "length", paste0(field, " length"))
    source <- fixture_expected_tokens(
      list(
        storage = column$storage,
        encoding = "values",
        data = column$data
      ),
      binary_hex,
      field
    )
    if (n_values == 0L) {
      return(list())
    }
    if (base::length(source) == 0L) {
      stop(field, " has an empty cycle source.", call. = FALSE)
    }
    return(source[rep(seq_along(source), length.out = n_values)])
  }
  if (identical(encoding, "binary64")) {
    n_values <- json_field_integer(column, "length", paste0(field, " length"))
    if (
      n_values != FINITE_STRESS_COUNT ||
        base::length(binary_hex) != n_values
    ) {
      stop(
        field,
        " does not match the frozen finite-stress bytes.",
        call. = FALSE
      )
    }
    return(lapply(binary_hex, function(hex) {
      list(state = json_string("number"), hex = json_string(hex))
    }))
  }
  stop(field, " uses an unsupported fixture encoding.", call. = FALSE)
}

fixture_expected_table <- function(table, ndim, binary_hex, label) {
  expected_names <- c(paste0("location_", seq_len(ndim)), "value")
  columns <- lapply(seq_along(table$columns), function(index) {
    column <- table$columns[[index]]
    name <- json_field_string(column, "name", paste0(label, " column name"))
    storage <- json_field_string(column, "storage", paste0(label, " storage"))
    if (!identical(name, expected_names[[index]])) {
      stop(label, " has noncanonical columns.", call. = FALSE)
    }
    list(
      name = json_string(name),
      storage = json_string(storage),
      data = fixture_expected_tokens(
        column,
        binary_hex,
        paste0(label, ".", name)
      )
    )
  })
  list(
    columns = columns,
    dim_names = json_character_array(
      nullable_character_vector(table$dim_names, paste0(label, " dim_names"))
    ),
    dim_units = json_character_array(
      nullable_character_vector(table$dim_units, paste0(label, " dim_units"))
    )
  )
}

fixture_expected_object <- function(fixture, binary_hex) {
  kind <- json_field_string(fixture, "kind", "fixture kind")
  ndim <- json_field_integer(fixture, "ndim", "fixture ndim")
  object_id <- json_field_string(fixture, "id", "fixture id")
  members <- lapply(seq_along(fixture$members), function(index) {
    fixture_expected_table(
      fixture$members[[index]],
      ndim,
      binary_hex,
      paste0(object_id, ".members[", index, "]")
    )
  })
  if (identical(kind, "tbl") && length(members) != 1L) {
    stop(object_id, " table fixture must have one member.", call. = FALSE)
  }
  ptype <- if (is.null(fixture$ptype)) {
    NULL
  } else {
    fixture_expected_table(
      fixture$ptype,
      ndim,
      binary_hex,
      paste0(object_id, ".ptype")
    )
  }
  list(
    kind = json_string(kind),
    ndim = json_scalar(ndim),
    list_names = json_character_array(
      nullable_character_vector(fixture$list_names, paste0(object_id, " names"))
    ),
    ptype = ptype,
    members = members
  )
}

fixture_object_names <- function(fixture, label) {
  nullable_character_vector(fixture$list_names, paste0(label, " list_names"))
}

build_fixture_object <- function(fixture, binary_values) {
  kind <- json_field_string(fixture, "kind", "fixture kind")
  ndim <- json_field_integer(fixture, "ndim", "fixture ndim")
  id <- json_field_string(fixture, "id", "fixture id")
  members <- lapply(seq_along(fixture$members), function(index) {
    decode_fixture_table(
      fixture$members[[index]],
      ndim,
      binary_values,
      paste0(id, ".members[", index, "]")
    )
  })
  if (identical(kind, "tbl")) {
    if (length(members) != 1L) {
      stop(id, " table fixture must contain exactly one member.", call. = FALSE)
    }
    return(members[[1L]])
  }
  if (!identical(kind, "list")) {
    stop(id, " has an unsupported fixture root kind.", call. = FALSE)
  }

  construction <- json_field_string(
    fixture,
    "construction",
    paste0(id, " construction")
  )
  ptype <- if (is.null(fixture$ptype)) {
    NULL
  } else {
    decode_fixture_table(
      fixture$ptype,
      ndim,
      binary_values,
      paste0(id, ".ptype")
    )
  }
  object <- switch(
    construction,
    "default-empty" = measure::new_measure_nd_list(),
    "public-empty-subset" = {
      if (length(members) != 0L || is.null(ptype)) {
        stop(
          id,
          " public empty subset requires its declared prototype.",
          call. = FALSE
        )
      }
      measure::new_measure_nd_list(list(ptype))[0]
    },
    "list-of" = {
      if (length(members) == 0L) {
        if (is.null(ptype)) {
          stop(
            id,
            " typed empty list lacks its declared prototype.",
            call. = FALSE
          )
        }
        vctrs::new_list_of(
          list(),
          ptype = ptype,
          class = c("measure_nd_list", "measure_list")
        )
      } else {
        validated <- measure::new_measure_nd_list(unname(members))
        if (is.null(ptype)) {
          stop(
            id,
            " nonempty list lacks its declared prototype.",
            call. = FALSE
          )
        }
        vctrs::new_list_of(
          unname(vctrs::vec_data(validated)),
          ptype = ptype,
          class = c("measure_nd_list", "measure_list")
        )
      }
    },
    stop(id, " uses an unsupported list construction.", call. = FALSE)
  )
  names_value <- fixture_object_names(fixture, id)
  attr(object, "names") <- names_value
  object
}

observe_table <- function(x) {
  column_names <- names(x)
  if (is.null(column_names)) {
    stop("An observed nD table has no column names.", call. = FALSE)
  }
  columns <- lapply(seq_along(column_names), function(index) {
    name <- column_names[[index]]
    values <- x[[index]]
    storage <- typeof(values)
    if (!storage %in% c("integer", "double")) {
      stop(
        "Observed column ",
        name,
        " is not integer or double.",
        call. = FALSE
      )
    }
    list(
      name = json_string(name),
      storage = json_string(storage),
      data = observed_numeric_tokens(values, storage)
    )
  })
  list(
    columns = columns,
    dim_names = json_character_array(attr(x, "dim_names", exact = TRUE)),
    dim_units = json_character_array(attr(x, "dim_units", exact = TRUE))
  )
}

observe_object <- function(x) {
  if (inherits(x, "measure_nd_tbl")) {
    kind <- "tbl"
    members <- list(observe_table(x))
    list_names <- NULL
    ptype <- NULL
  } else if (inherits(x, "measure_nd_list")) {
    kind <- "list"
    list_names <- json_character_array(names(x))
    members <- lapply(seq_along(x), function(index) observe_table(x[[index]]))
    ptype_value <- attr(x, "ptype", exact = TRUE)
    if (is.null(ptype_value)) {
      stop("An observed nD list has no exact ptype attribute.", call. = FALSE)
    }
    ptype <- observe_table(ptype_value)
  } else {
    stop(
      "Unsupported installed measure object class: ",
      paste(class(x), collapse = "/"),
      call. = FALSE
    )
  }
  list(
    kind = json_string(kind),
    ndim = json_scalar(as.integer(measure::measure_ndim(x))),
    list_names = list_names,
    ptype = ptype,
    members = members
  )
}

fixture_map <- function(fixtures) {
  ids <- vapply(
    fixtures,
    function(x) {
      json_field_string(x, "id", "fixture id")
    },
    character(1),
    USE.NAMES = FALSE
  )
  if (anyDuplicated(ids) > 0L) {
    stop("Fixture IDs are duplicated.", call. = FALSE)
  }
  stats::setNames(fixtures, ids)
}

validate_declaration <- function(declaration, fixtures) {
  profile <- json_field_string(declaration, "profile", "declaration profile")
  if (!identical(profile, PROFILE)) {
    stop("The declaration has the wrong qualification profile.", call. = FALSE)
  }
  object_ids <- json_array_character(declaration$object_ids, "object_ids")
  fixture_ids <- names(fixtures)
  if (
    length(object_ids) != EXPECTED_OBJECT_COUNT ||
      !identical(object_ids, fixture_ids)
  ) {
    stop("The declaration and fixture object IDs do not match.", call. = FALSE)
  }
  forward <- declaration$forward_bundles
  reverse <- declaration$reverse_bundles
  reordered <- declaration$reordered_bundles
  invalid <- json_array_character(
    declaration$invalid_bundles,
    "invalid_bundles"
  )
  cross <- json_array_character(
    declaration$cross_format_rejections,
    "cross_format_rejections"
  )
  if (
    !is.list(forward) ||
      length(forward) != EXPECTED_FORWARD_COUNT ||
      !is.list(reverse) ||
      length(reverse) != EXPECTED_REVERSE_COUNT ||
      !is.list(reordered) ||
      length(reordered) != EXPECTED_REORDERED_COUNT ||
      length(invalid) != EXPECTED_INVALID_COUNT ||
      length(cross) != EXPECTED_CROSS_FORMAT_COUNT
  ) {
    stop("The declaration counts differ from the frozen wire.", call. = FALSE)
  }
  forward_ids <- vapply(forward, json_field_string, character(1), "id")
  reverse_ids <- vapply(reverse, json_field_string, character(1), "id")
  reordered_ids <- vapply(reordered, json_field_string, character(1), "id")
  all_ids <- c(forward_ids, reverse_ids, reordered_ids, invalid, cross)
  if (anyDuplicated(all_ids) > 0L) {
    stop("The declaration contains duplicate case IDs.", call. = FALSE)
  }
  list(
    forward = forward,
    reverse = reverse,
    reordered = reordered,
    invalid = invalid,
    cross_format = cross
  )
}

resolve_script <- function() {
  all_arguments <- commandArgs(trailingOnly = FALSE)
  script_arguments <- all_arguments[startsWith(all_arguments, "--file=")]
  if (length(script_arguments) != 1L) {
    stop("Could not determine the qualification script path.", call. = FALSE)
  }
  path <- sub("^--file=", "", script_arguments[[1L]])
  path <- gsub("~+~", " ", path, fixed = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

package_root_from_script <- function(script_path) {
  normalizePath(
    file.path(dirname(script_path), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
}

tool_checkout_head <- function(package_root) {
  git_entry <- file.path(package_root, ".git")
  description <- file.path(package_root, "DESCRIPTION")
  if (
    !is_path_entry(git_entry) ||
      !identical(path_type(description), "file")
  ) {
    return(NULL)
  }
  fields <- tryCatch(read.dcf(description), error = function(error) NULL)
  if (
    is.null(fields) ||
      nrow(fields) != 1L ||
      !identical(unname(fields[1L, "Package"]), "measure")
  ) {
    return(NULL)
  }
  output <- tryCatch(
    suppressWarnings(system2(
      "git",
      c("-C", shQuote(package_root), "rev-parse", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(error) character()
  )
  if (
    length(output) != 1L ||
      !grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", output[[1L]])
  ) {
    return(NULL)
  }
  output[[1L]]
}

installed_measure_package <- function() {
  namespace <- loadNamespace("measure")
  namespace_path <- normalizePath(
    getNamespaceInfo(namespace, "path"),
    winslash = "/",
    mustWork = TRUE
  )
  installed_path <- normalizePath(
    find.package("measure"),
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(namespace_path, installed_path)) {
    stop(
      "The loaded measure namespace does not match find.package('measure').",
      call. = FALSE
    )
  }
  if (
    !identical(
      path_type(file.path(namespace_path, "Meta", "package.rds")),
      "file"
    )
  ) {
    stop(
      "The measure namespace has no installed Meta/package.rds; ",
      "source-loaded packages are not accepted.",
      call. = FALSE
    )
  }
  description_path <- file.path(namespace_path, "DESCRIPTION")
  if (!identical(path_type(description_path), "file")) {
    stop("The installed measure DESCRIPTION is missing.", call. = FALSE)
  }
  description <- read.dcf(description_path)
  field_or_null <- function(name) {
    if (!name %in% colnames(description)) {
      return(NULL)
    }
    value <- description[1L, name]
    if (length(value) != 1L || is.na(value) || !nzchar(value)) NULL else value
  }
  list(
    path = namespace_path,
    namespace_path = namespace_path,
    version = as.character(utils::packageVersion("measure")),
    built = field_or_null("Built"),
    remote_sha = field_or_null("RemoteSha")
  )
}

dependency_versions <- function() {
  packages <- c("measure", "vctrs", "jsonlite", "digest", "fs")
  versions <- lapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Required package ", package, " is not installed.", call. = FALSE)
    }
    json_string(as.character(utils::packageVersion(package)))
  })
  names(versions) <- packages
  versions
}

capture_runtime <- function(installed, head) {
  info <- capture.output(utils::sessionInfo())
  if (length(info) == 0L || !identical(info[[1L]], R.version.string)) {
    stop(
      "sessionInfo() did not report the active R version first.",
      call. = FALSE
    )
  }
  list(
    profile = json_string(PROFILE),
    R = json_string(R.version.string),
    installed_package = list(
      path = json_string(installed$path),
      namespace_path = json_string(installed$namespace_path),
      version = json_string(installed$version),
      built = if (is.null(installed$built)) {
        NULL
      } else {
        json_string(installed$built)
      },
      remote_sha = if (is.null(installed$remote_sha)) {
        NULL
      } else {
        json_string(installed$remote_sha)
      }
    ),
    tool_checkout_head = if (is.null(head)) NULL else json_string(head),
    created_utc = json_string(utc_now()),
    session_info = json_string_array(info),
    dependency_versions = dependency_versions()
  )
}

input_manifest <- function(script_directory) {
  manifest_path <- file.path(script_directory, "input-manifest.json")
  manifest <- read_json_file(
    manifest_path,
    "input-manifest.json",
    max_bytes = 128 * 1024
  )
  if (
    !identical(json_field_string(manifest, "profile", "input profile"), PROFILE)
  ) {
    stop("The frozen input manifest has the wrong profile.", call. = FALSE)
  }
  files <- manifest$files
  if (
    is.null(files) || !is.list(files) || !setequal(names(files), INPUT_NAMES)
  ) {
    stop(
      "The frozen input manifest has unexpected file entries.",
      call. = FALSE
    )
  }
  records <- stats::setNames(vector("list", length(INPUT_NAMES)), INPUT_NAMES)
  for (name in INPUT_NAMES) {
    record <- files[[name]]
    sha <- json_field_string(record, "sha256", paste0(name, " SHA256"))
    if (!grepl("^[0-9a-f]{64}$", sha, perl = TRUE)) {
      stop(name, " has invalid frozen SHA256 syntax.", call. = FALSE)
    }
    size <- json_field_integer(record, "size_bytes", paste0(name, " size"))
    if (size < 0L) {
      stop(name, " has a negative frozen size.", call. = FALSE)
    }
    records[[name]] <- list(sha256 = sha, size_bytes = size)
  }
  records
}

verify_fixed_inputs <- function(script_directory) {
  records <- input_manifest(script_directory)
  paths <- stats::setNames(
    file.path(script_directory, INPUT_NAMES),
    INPUT_NAMES
  )
  hashes <- stats::setNames(character(length(INPUT_NAMES)), INPUT_NAMES)
  for (name in INPUT_NAMES) {
    expected <- records[[name]]
    size <- assert_regular_file(
      paths[[name]],
      name,
      max_bytes = expected$size_bytes
    )
    if (size != expected$size_bytes) {
      stop(name, " does not match its frozen byte size.", call. = FALSE)
    }
    hashes[[name]] <- sha256_file(paths[[name]], name)
    if (!identical(hashes[[name]], expected$sha256)) {
      stop(name, " does not match its frozen SHA256.", call. = FALSE)
    }
  }
  list(paths = paths, hashes = hashes)
}

read_input_fixtures <- function(inputs) {
  fixture_document <- read_json_file(
    inputs$paths[["fixtures.json"]],
    "fixtures.json",
    max_bytes = 2 * 1024^2
  )
  declaration <- read_json_file(
    inputs$paths[["declaration.json"]],
    "declaration.json",
    max_bytes = 512 * 1024
  )
  if (
    !identical(
      json_field_string(fixture_document, "profile", "fixture profile"),
      PROFILE
    )
  ) {
    stop(
      "The frozen fixtures have the wrong qualification profile.",
      call. = FALSE
    )
  }
  fixture_list <- fixture_document$objects
  if (!is.list(fixture_list) || length(fixture_list) != EXPECTED_OBJECT_COUNT) {
    stop("The frozen fixture object count is not 15.", call. = FALSE)
  }
  fixtures <- fixture_map(fixture_list)
  declared <- validate_declaration(declaration, fixtures)
  corpus <- fixture_document$finite_corpus
  if (
    json_field_integer(corpus, "count", "finite corpus count") !=
      FINITE_STRESS_COUNT ||
      json_field_integer(
        corpus,
        "fixed_controls",
        "finite corpus fixed count"
      ) !=
        16L ||
      json_field_integer(
        corpus,
        "random_finite_values",
        "finite corpus random count"
      ) !=
        8192L
  ) {
    stop(
      "The finite-stress corpus counts differ from the frozen wire.",
      call. = FALSE
    )
  }
  raw <- read_raw_file(
    inputs$paths[["finite-stress.bin"]],
    "finite-stress.bin",
    max_bytes = 8L * FINITE_STRESS_COUNT
  )
  finite_values <- read_big_endian_doubles(
    raw,
    FINITE_STRESS_COUNT,
    "finite-stress.bin"
  )
  if (
    length(finite_values) != FINITE_STRESS_COUNT ||
      !all(is.finite(finite_values))
  ) {
    stop(
      "The finite-stress corpus must contain 8,208 finite doubles.",
      call. = FALSE
    )
  }
  list(
    fixture_document = fixture_document,
    fixtures = fixtures,
    declaration = declaration,
    cases = declared,
    finite_values = finite_values,
    finite_hex = {
      if (length(raw) == 0L) {
        character()
      } else {
        vapply(
          seq_len(FINITE_STRESS_COUNT),
          function(index) {
            start <- (index - 1L) * 8L + 1L
            raw_hex(raw[start:(start + 7L)])
          },
          character(1),
          USE.NAMES = FALSE
        )
      }
    }
  )
}

input_hash_json <- function(hashes) {
  output <- lapply(INPUT_NAMES, function(name) json_string(hashes[[name]]))
  names(output) <- INPUT_NAMES
  output
}

reserve_output_root <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Output root must be one nonempty path.", call. = FALSE)
  }
  path <- path.expand(path)
  parent <- dirname(path)
  leaf <- basename(path)
  if (!nzchar(leaf) || identical(leaf, ".") || identical(leaf, "..")) {
    stop("Output root must name a new directory.", call. = FALSE)
  }
  assert_directory(parent, "Output parent")
  normalized_parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  output <- file.path(normalized_parent, leaf)
  if (is_path_entry(output)) {
    stop(
      "Output root already exists, including as a dangling symlink; choose a new path.",
      call. = FALSE
    )
  }
  if (!dir.create(output, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not reserve the new output root.", call. = FALSE)
  }
  normalizePath(output, winslash = "/", mustWork = TRUE)
}

new_run_state <- function(phase, output) {
  run <- new.env(parent = emptyenv())
  run$phase <- phase
  run$output <- output
  run$stage <- "initialized"
  run$current_case <- NULL
  run$completed <- character()
  run$failure <- NULL
  run$status_path <- file.path(
    output,
    if (identical(phase, "produce")) {
      "run-status.json"
    } else {
      "rejection-status.json"
    }
  )
  run$progress_path <- file.path(
    output,
    if (identical(phase, "produce")) {
      "progress.json"
    } else {
      "rejection-progress.json"
    }
  )
  run
}

run_status_json <- function(run, complete) {
  list(
    phase = json_string(run$phase),
    complete = json_scalar(complete),
    stage = json_string(run$stage),
    error = if (is.null(run$failure)) NULL else json_string(run$failure),
    updated_utc = json_string(utc_now())
  )
}

run_progress_json <- function(run, complete) {
  list(
    phase = json_string(run$phase),
    complete = json_scalar(complete),
    stage = json_string(run$stage),
    current_case = if (is.null(run$current_case)) {
      NULL
    } else {
      json_string(run$current_case)
    },
    completed_bundles = json_string_array(run$completed),
    error = if (is.null(run$failure)) NULL else json_string(run$failure),
    updated_utc = json_string(utc_now())
  )
}

write_run_state <- function(run, complete) {
  write_json_file(run_status_json(run, complete), run$status_path)
  write_json_file(run_progress_json(run, complete), run$progress_path)
  invisible(NULL)
}

record_case_complete <- function(run, id) {
  run$completed <- c(run$completed, id)
  run$current_case <- NULL
  write_run_state(run, FALSE)
  invisible(NULL)
}

start_case <- function(run, stage, id) {
  run$stage <- stage
  run$current_case <- id
  write_run_state(run, FALSE)
  invisible(NULL)
}

finish_run <- function(run) {
  run$stage <- "complete"
  run$current_case <- NULL
  write_run_state(run, TRUE)
  invisible(NULL)
}

fail_run <- function(run, error) {
  message <- conditionMessage(error)
  if (length(message) != 1L || !nzchar(trimws(message))) {
    message <- paste0("Qualification failed during ", run$stage, ".")
  }
  run$failure <- message
  run$current_case <- NULL
  try(write_run_state(run, FALSE), silent = TRUE)
  message
}

run_with_receipts <- function(run, expression) {
  write_run_state(run, FALSE)
  tryCatch(
    {
      force(expression)
      finish_run(run)
      TRUE
    },
    error = function(error) {
      message <- fail_run(run, error)
      cat("Qualification failed: ", message, "\n", sep = "", file = stderr())
      FALSE
    }
  )
}

read_case_field <- function(case, name, label = name) {
  json_field_string(case, name, label)
}

write_forward_bundle <- function(object, destination, delimiter) {
  delim <- if (identical(delimiter, "csv")) "," else "\t"
  measure::write_measure_nd(object, destination, delim = delim)
  invisible(destination)
}

capture_original_bundle_hashes <- function(output, forward_cases) {
  hashes <- stats::setNames(
    vector("list", length(forward_cases)),
    character(length(forward_cases))
  )
  for (index in seq_along(forward_cases)) {
    case <- forward_cases[[index]]
    id <- read_case_field(case, "id", "forward bundle id")
    delimiter <- read_case_field(case, "delimiter", "forward bundle delimiter")
    extension <- if (identical(delimiter, "csv")) "csv" else "tsv"
    bundle_path <- file.path(output, "bundles", id)
    assert_directory(bundle_path, paste0("forward bundle ", id))
    names_expected <- c(
      "manifest.dcf",
      paste0(c("members", "axes", "points"), ".", extension)
    )
    assert_bundle_inventory(
      bundle_path,
      names_expected,
      paste0("Forward bundle ", id)
    )
    file_hashes <- stats::setNames(
      character(length(names_expected)),
      names_expected
    )
    for (name in names_expected) {
      path <- file.path(bundle_path, name)
      file_hashes[[name]] <- sha256_file(path, paste(id, name))
    }
    hashes[[index]] <- file_hashes
    names(hashes)[[index]] <- id
  }
  hashes
}

assert_original_bundle_hashes <- function(output, hashes) {
  for (id in names(hashes)) {
    bundle_path <- file.path(output, "bundles", id)
    assert_bundle_inventory(
      bundle_path,
      names(hashes[[id]]),
      paste0("Forward bundle ", id)
    )
    for (name in names(hashes[[id]])) {
      actual <- sha256_file(
        file.path(bundle_path, name),
        paste(id, name)
      )
      if (!identical(actual, hashes[[id]][[name]])) {
        stop(
          "Original bundle file changed during rejection: ",
          id,
          "/",
          name,
          call. = FALSE
        )
      }
    }
  }
  invisible(TRUE)
}

produce_phase <- function(script_directory, output_argument) {
  output <- reserve_output_root(output_argument)
  run <- new_run_state("produce", output)
  succeeded <- run_with_receipts(run, {
    run$stage <- "frozen-input verification"
    write_run_state(run, FALSE)
    inputs <- verify_fixed_inputs(script_directory)
    data <- read_input_fixtures(inputs)

    run$stage <- "installed namespace verification"
    write_run_state(run, FALSE)
    installed <- installed_measure_package()
    head <- tool_checkout_head(package_root_from_script(resolve_script()))
    runtime <- capture_runtime(installed, head)
    bundle_parent <- file.path(output, "bundles")
    if (!dir.create(bundle_parent, recursive = FALSE, showWarnings = FALSE)) {
      stop("Could not create the forward bundle parent.", call. = FALSE)
    }

    observations <- vector("list", length(data$cases$forward))
    for (index in seq_along(data$cases$forward)) {
      case <- data$cases$forward[[index]]
      id <- read_case_field(case, "id", "forward bundle id")
      object_id <- read_case_field(case, "object_id", "forward object id")
      delimiter <- read_case_field(case, "delimiter", "forward delimiter")
      if (!delimiter %in% c("csv", "tsv")) {
        stop("Forward bundle ", id, " has an invalid delimiter.", call. = FALSE)
      }
      fixture <- data$fixtures[[object_id]]
      if (is.null(fixture)) {
        stop(
          "Forward bundle ",
          id,
          " references an unknown object.",
          call. = FALSE
        )
      }
      start_case(run, "forward write and read", id)
      object <- build_fixture_object(fixture, data$finite_values)
      expected <- fixture_expected_object(fixture, data$finite_hex)
      original_observation <- observe_object(object)
      assert_json_equal(
        original_observation,
        expected,
        paste0("Original object ", object_id)
      )
      destination <- file.path(bundle_parent, id)
      if (is_path_entry(destination)) {
        stop("Forward bundle path already exists: ", id, call. = FALSE)
      }
      write_forward_bundle(object, destination, delimiter)
      restored <- measure::read_measure_nd(destination)
      observed <- observe_object(restored)
      assert_json_equal(
        observed,
        expected,
        paste0("Read-back object ", object_id, " from ", id)
      )
      assert_json_equal(
        observed,
        original_observation,
        paste0("Original/read-back object ", object_id, " from ", id)
      )
      observations[[index]] <- list(
        id = json_string(id),
        object_id = json_string(object_id),
        delimiter = json_string(delimiter),
        path = json_string(file.path("bundles", id)),
        observed = observed
      )
      record_case_complete(run, id)
    }

    start_case(run, "scalar cross-format control", "scalar-control")
    scalar_path <- file.path(output, "scalar-control")
    if (is_path_entry(scalar_path)) {
      stop("Scalar control path already exists.", call. = FALSE)
    }
    measure::write_measure_record(measure::measure_record(), scalar_path)
    record_case_complete(run, "scalar-control")

    run$stage <- "produce receipts"
    run$current_case <- NULL
    observation_document <- list(
      profile = json_string(PROFILE),
      input_files_sha256 = input_hash_json(inputs$hashes),
      installed_package = runtime$installed_package,
      R = runtime$R,
      tool_checkout_head = runtime$tool_checkout_head,
      bundles = observations
    )
    write_json_file(
      observation_document,
      file.path(output, "observations.json")
    )
    write_json_file(runtime, file.path(output, "runtime.json"))
    write_run_state(run, FALSE)
    TRUE
  })
  if (!succeeded) {
    quit(save = "no", status = 1L, runLast = FALSE)
  }
  invisible(output)
}

read_fixed_json_from_output <- function(output, name, max_bytes = 2 * 1024^2) {
  path <- file.path(output, name)
  read_json_file(path, name, max_bytes)
}

validate_hash_record <- function(record, label, allowed_names) {
  if (is.null(record) || !is.list(record) || is.null(names(record))) {
    stop(label, " must be a JSON object of file hashes.", call. = FALSE)
  }
  filenames <- names(record)
  if (anyDuplicated(filenames) > 0L || !all(filenames %in% allowed_names)) {
    stop(label, " contains a duplicate or unsupported basename.", call. = FALSE)
  }
  hashes <- stats::setNames(character(length(filenames)), filenames)
  for (name in filenames) {
    value <- record[[name]]
    if (
      length(value) != 1L ||
        !is.character(value) ||
        !grepl("^[0-9a-f]{64}$", value, perl = TRUE)
    ) {
      stop(label, " has an invalid SHA256 for ", name, ".", call. = FALSE)
    }
    hashes[[name]] <- value
  }
  hashes
}

allowed_bundle_files <- function() {
  c(
    "manifest.dcf",
    "members.csv",
    "axes.csv",
    "points.csv",
    "members.tsv",
    "axes.tsv",
    "points.tsv"
  )
}

assert_bundle_inventory <- function(root, expected_names, label) {
  assert_directory(root, label)
  allowed <- allowed_bundle_files()
  present <- character()
  for (name in allowed) {
    path <- file.path(root, name)
    type <- path_type(path)
    if (is.na(type)) {
      if (is_path_entry(path)) {
        stop(label, "/", name, " is not a regular file.", call. = FALSE)
      }
      next
    }
    if (!identical(type, "file")) {
      stop(
        label,
        "/",
        name,
        " is not a regular nonsymlink file.",
        call. = FALSE
      )
    }
    present <- c(present, name)
  }
  if (
    !setequal(present, expected_names) ||
      length(present) != length(expected_names)
  ) {
    stop(
      label,
      " fixed-file inventory differs from its hash receipt.",
      call. = FALSE
    )
  }
  invisible(present)
}

scalar_control_files <- function() {
  c(
    "manifest.dcf",
    "sources.csv",
    "samples.csv",
    "injections.csv",
    "channels.csv",
    "curves.csv",
    "points.csv"
  )
}

assert_scalar_control_inventory <- function(
  root,
  expected_names = scalar_control_files()
) {
  label <- "Scalar cross-format control"
  assert_directory(root, label)
  allowed <- c(
    "manifest.dcf",
    "sources.csv",
    "samples.csv",
    "injections.csv",
    "channels.csv",
    "curves.csv",
    "points.csv",
    "sources.tsv",
    "samples.tsv",
    "injections.tsv",
    "channels.tsv",
    "curves.tsv",
    "points.tsv"
  )
  present <- character()
  for (name in allowed) {
    path <- file.path(root, name)
    type <- path_type(path)
    if (is.na(type)) {
      if (is_path_entry(path)) {
        stop(label, "/", name, " is not a regular file.", call. = FALSE)
      }
      next
    }
    if (!identical(type, "file")) {
      stop(
        label,
        "/",
        name,
        " is not a regular nonsymlink file.",
        call. = FALSE
      )
    }
    present <- c(present, name)
  }
  if (
    !setequal(present, expected_names) ||
      length(present) != length(expected_names)
  ) {
    stop(
      label,
      " fixed-file inventory is incomplete or unexpected.",
      call. = FALSE
    )
  }
  invisible(present)
}

capture_scalar_control_hashes <- function(root) {
  assert_scalar_control_inventory(root)
  files <- scalar_control_files()
  hashes <- stats::setNames(character(length(files)), files)
  for (name in files) {
    hashes[[name]] <- sha256_file(
      file.path(root, name),
      paste("Scalar cross-format control", name)
    )
  }
  hashes
}

assert_scalar_control_hashes <- function(root, expected) {
  assert_scalar_control_inventory(root, names(expected))
  for (name in names(expected)) {
    actual <- sha256_file(
      file.path(root, name),
      paste("Scalar cross-format control", name)
    )
    if (!identical(actual, expected[[name]])) {
      stop(
        "Scalar cross-format control changed during rejection.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

verify_hashed_bundle <- function(output, relative_path, hashes, label) {
  if (
    length(relative_path) != 1L ||
      !is.character(relative_path) ||
      grepl("(^/|\\\\|(^|/)\\.\\.?(/|$))", relative_path, perl = TRUE)
  ) {
    stop(label, " has an unsafe relative path.", call. = FALSE)
  }
  root <- file.path(output, relative_path)
  assert_bundle_inventory(root, names(hashes), label)
  for (name in names(hashes)) {
    actual <- sha256_file(file.path(root, name), paste(label, name))
    if (!identical(actual, hashes[[name]])) {
      stop(
        label,
        "/",
        name,
        " does not match its mutation receipt.",
        call. = FALSE
      )
    }
  }
  list(root = root, hashes = hashes, relative_path = relative_path)
}

validate_source_hashes <- function(output, source_hashes, include_mutations) {
  source_names <- c(
    "observations.json",
    "runtime.json",
    "run-status.json",
    "independent-python-receipt.json"
  )
  if (include_mutations) {
    source_names <- c(source_names, "mutations.json")
  }
  expected <- stats::setNames(character(length(source_names)), source_names)
  for (name in source_names) {
    expected[[name]] <- sha256_file(file.path(output, name), name)
  }
  if (
    !is.list(source_hashes) ||
      is.null(names(source_hashes)) ||
      !identical(sort(names(source_hashes)), sort(source_names))
  ) {
    stop(
      "Mutation source artifact names do not match the frozen wire.",
      call. = FALSE
    )
  }
  actual <- stats::setNames(character(length(source_names)), source_names)
  for (name in source_names) {
    value <- source_hashes[[name]]
    if (
      length(value) != 1L ||
        !is.character(value) ||
        !identical(value, expected[[name]])
    ) {
      stop(
        "Mutation receipt does not bind source artifact ",
        name,
        ".",
        call. = FALSE
      )
    }
    actual[[name]] <- value
  }
  expected
}

validate_mutation_receipt <- function(output, data, inputs, produce_runtime) {
  mutation_status <- read_fixed_json_from_output(
    output,
    "mutation-status.json",
    max_bytes = 128 * 1024
  )
  json_field_true(mutation_status, "complete", "mutation status complete")
  mutation_document <- read_fixed_json_from_output(
    output,
    "mutations.json",
    max_bytes = 2 * 1024^2
  )
  if (
    !identical(
      json_field_string(mutation_document, "profile", "mutation profile"),
      PROFILE
    )
  ) {
    stop("Mutation receipt has the wrong profile.", call. = FALSE)
  }
  input_hashes <- mutation_document$input_files_sha256
  if (
    !is.list(input_hashes) ||
      !identical(sort(names(input_hashes)), sort(INPUT_NAMES))
  ) {
    stop(
      "Mutation receipt input file names differ from the frozen wire.",
      call. = FALSE
    )
  }
  for (name in INPUT_NAMES) {
    if (!identical(input_hashes[[name]], inputs$hashes[[name]])) {
      stop(
        "Mutation receipt does not bind frozen input ",
        name,
        ".",
        call. = FALSE
      )
    }
  }
  source_hashes <- mutation_document$source_artifacts_sha256
  validate_source_hashes(output, source_hashes, include_mutations = FALSE)

  invalid <- mutation_document$invalid_bundles
  if (!is.list(invalid) || length(invalid) != EXPECTED_INVALID_COUNT) {
    stop(
      "Mutation receipt must contain exactly 49 invalid bundles.",
      call. = FALSE
    )
  }
  invalid_ids <- vapply(invalid, json_field_string, character(1), "id")
  if (!identical(invalid_ids, data$cases$invalid)) {
    stop(
      "Mutation receipt invalid IDs differ from the declaration.",
      call. = FALSE
    )
  }
  invalid_records <- vector("list", length(invalid))
  allowed_files <- c(
    "manifest.dcf",
    "members.csv",
    "axes.csv",
    "points.csv",
    "members.tsv",
    "axes.tsv",
    "points.tsv"
  )
  for (index in seq_along(invalid)) {
    record <- invalid[[index]]
    id <- invalid_ids[[index]]
    expected_path <- file.path("mutations", id)
    path <- json_field_string(record, "path", paste(id, "path"))
    if (!identical(path, expected_path)) {
      stop(
        "Mutation path for ",
        id,
        " differs from the frozen wire.",
        call. = FALSE
      )
    }
    hashes <- validate_hash_record(
      record$files_sha256,
      paste(id, "file hashes"),
      allowed_files
    )
    invalid_records[[index]] <- verify_hashed_bundle(
      output,
      path,
      hashes,
      paste0("Invalid bundle ", id)
    )
  }

  expected_valid <- c(data$cases$reverse, data$cases$reordered)
  valid <- mutation_document$valid_bundles
  if (
    !is.list(valid) ||
      length(valid) != EXPECTED_REVERSE_COUNT + EXPECTED_REORDERED_COUNT
  ) {
    stop(
      "Mutation receipt must contain exactly four positive controls.",
      call. = FALSE
    )
  }
  expected_ids <- vapply(expected_valid, json_field_string, character(1), "id")
  valid_ids <- vapply(valid, json_field_string, character(1), "id")
  if (!identical(valid_ids, expected_ids)) {
    stop(
      "Mutation receipt positive control IDs differ from the declaration.",
      call. = FALSE
    )
  }
  valid_records <- vector("list", length(valid))
  for (index in seq_along(valid)) {
    record <- valid[[index]]
    id <- valid_ids[[index]]
    expected_case <- expected_valid[[index]]
    object_id <- json_field_string(
      expected_case,
      "object_id",
      paste(id, "object_id")
    )
    kind <- if (index <= EXPECTED_REVERSE_COUNT) "reverse" else "reordered"
    expected_relative <- file.path(
      if (identical(kind, "reverse")) "reverse" else "reordered",
      id
    )
    path <- json_field_string(record, "path", paste(id, "path"))
    record_kind <- json_field_string(record, "kind", paste(id, "kind"))
    record_object_id <- json_field_string(
      record,
      "object_id",
      paste(id, "object_id")
    )
    if (
      !identical(path, expected_relative) ||
        !identical(record_kind, kind) ||
        !identical(record_object_id, object_id)
    ) {
      stop(
        "Mutation positive control metadata differ for ",
        id,
        ".",
        call. = FALSE
      )
    }
    hashes <- validate_hash_record(
      record$files_sha256,
      paste(id, "file hashes"),
      allowed_files
    )
    valid_record <- verify_hashed_bundle(
      output,
      path,
      hashes,
      paste0("Positive bundle ", id)
    )
    valid_record$record <- record
    valid_records[[index]] <- valid_record
  }

  observations <- read_fixed_json_from_output(
    output,
    "observations.json",
    max_bytes = 16 * 1024^2
  )
  produced_runtime <- read_fixed_json_from_output(output, "runtime.json")
  produced_status <- read_fixed_json_from_output(
    output,
    "run-status.json",
    max_bytes = 128 * 1024
  )
  json_field_true(produced_status, "complete", "producer status complete")
  if (
    !identical(
      json_field_string(observations, "profile", "observations profile"),
      PROFILE
    ) ||
      !identical(
        json_field_string(produced_runtime, "profile", "runtime profile"),
        PROFILE
      )
  ) {
    stop("Producer artifacts have the wrong profile.", call. = FALSE)
  }
  for (name in INPUT_NAMES) {
    if (
      !identical(observations$input_files_sha256[[name]], inputs$hashes[[name]])
    ) {
      stop(
        "Producer observations do not bind frozen input ",
        name,
        ".",
        call. = FALSE
      )
    }
  }
  if (
    !identical(
      canonical_json(observations$installed_package),
      canonical_json(produced_runtime$installed_package)
    )
  ) {
    stop(
      "Producer observations and runtime disagree about the installed package.",
      call. = FALSE
    )
  }
  if (
    !identical(
      json_field_string(produced_runtime, "R", "producer runtime R"),
      R.version.string
    ) ||
      !identical(
        json_field_string(observations, "R", "producer observation R"),
        R.version.string
      )
  ) {
    stop(
      "The installed R version differs from the produce phase.",
      call. = FALSE
    )
  }
  if (
    !identical(
      canonical_json(produced_runtime$installed_package),
      canonical_json(produce_runtime$installed_package)
    ) ||
      !identical(
        canonical_json(produced_runtime$dependency_versions),
        canonical_json(produce_runtime$dependency_versions)
      )
  ) {
    stop(
      "The active runtime differs from the installed produce environment.",
      call. = FALSE
    )
  }
  list(
    mutation_document = mutation_document,
    invalid_records = invalid_records,
    valid_records = valid_records,
    valid_cases = expected_valid,
    produced_runtime = produced_runtime
  )
}

positive_control_rejections <- function(
  output,
  scalar_root,
  data,
  mutation_data,
  run
) {
  valid_results <- vector("list", length(mutation_data$valid_records))
  for (index in seq_along(mutation_data$valid_records)) {
    record <- mutation_data$valid_records[[index]]
    case <- mutation_data$valid_cases[[index]]
    id <- json_field_string(case, "id", "positive bundle id")
    object_id <- json_field_string(case, "object_id", "positive object id")
    kind <- json_field_string(record$record, "kind", paste(id, "kind"))
    start_case(run, "positive control reads", id)
    object <- measure::read_measure_nd(record$root)
    observed <- observe_object(object)
    expected <- fixture_expected_object(
      data$fixtures[[object_id]],
      data$finite_hex
    )
    assert_json_equal(observed, expected, paste0("Positive control ", id))
    valid_results[[index]] <- list(
      id = json_string(id),
      object_id = json_string(object_id),
      kind = json_string(kind),
      observed = observed
    )
    record_case_complete(run, id)
  }
  list(results = valid_results)
}

expected_error <- function(expression, label) {
  outcome <- tryCatch(
    list(ok = TRUE, value = force(expression)),
    error = function(condition) list(ok = FALSE, error = condition)
  )
  if (isTRUE(outcome$ok)) {
    stop(label, " unexpectedly succeeded.", call. = FALSE)
  }
  message <- conditionMessage(outcome$error)
  if (length(message) != 1L || !nzchar(trimws(message))) {
    stop(label, " returned an empty rejection diagnostic.", call. = FALSE)
  }
  message
}

cross_format_results <- function(output, scalar_root, data, run) {
  results <- vector("list", length(data$cases$cross_format))
  for (index in seq_along(data$cases$cross_format)) {
    id <- data$cases$cross_format[[index]]
    start_case(run, "cross-format rejection", id)
    message <- if (identical(id, "scalar-reader-on-nd")) {
      expected_error(
        measure::read_measure_record(file.path(
          output,
          "bundles",
          "dad-table-csv"
        )),
        id
      )
    } else if (identical(id, "nd-reader-on-scalar")) {
      expected_error(
        measure::read_measure_nd(scalar_root),
        id
      )
    } else {
      stop("Undeclared cross-format case: ", id, call. = FALSE)
    }
    results[[index]] <- list(
      id = json_string(id),
      error = json_scalar(TRUE),
      message = json_string(message)
    )
    record_case_complete(run, id)
  }
  results
}

validate_rejection_output_absent <- function(output) {
  names <- c(
    "rejection-status.json",
    "rejection-progress.json",
    "rejections.json",
    "independent-rejection-receipt.json"
  )
  existing <- names[vapply(
    names,
    function(name) is_path_entry(file.path(output, name)),
    logical(1)
  )]
  if (length(existing) > 0L) {
    stop(
      "Rejection phase output already exists: ",
      paste(existing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_existing_run <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Output root must be one nonempty path.", call. = FALSE)
  }
  path <- path.expand(path)
  assert_directory(path, "Output root")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

reject_phase <- function(script_directory, output_argument) {
  output <- validate_existing_run(output_argument)
  validate_rejection_output_absent(output)
  run <- new_run_state("reject", output)
  succeeded <- run_with_receipts(run, {
    run$stage <- "frozen-input and runtime verification"
    write_run_state(run, FALSE)
    inputs <- verify_fixed_inputs(script_directory)
    data <- read_input_fixtures(inputs)
    installed <- installed_measure_package()
    head <- tool_checkout_head(package_root_from_script(resolve_script()))
    runtime <- capture_runtime(installed, head)
    runtime_plain <- normalize_runtime(runtime)

    produce_runtime <- read_fixed_json_from_output(output, "runtime.json")
    observations <- read_fixed_json_from_output(
      output,
      "observations.json",
      max_bytes = 16 * 1024^2
    )
    if (
      !identical(
        json_field_string(produce_runtime, "R", "produce R"),
        R.version.string
      ) ||
        !identical(
          json_field_string(observations, "R", "produce observation R"),
          R.version.string
        )
    ) {
      stop(
        "The active R runtime differs from the produce phase.",
        call. = FALSE
      )
    }
    mutation_path <- file.path(output, "mutations.json")
    mutation_file_sha <- sha256_file(mutation_path, "mutations.json")
    mutation_data <- validate_mutation_receipt(
      output,
      data,
      inputs,
      produce_runtime
    )
    if (
      !identical(
        sha256_file(mutation_path, "mutations.json"),
        mutation_file_sha
      )
    ) {
      stop(
        "mutations.json changed while its contents were being validated.",
        call. = FALSE
      )
    }
    for (name in c("R", "installed_package", "dependency_versions")) {
      if (
        !identical(
          canonical_json(runtime_plain[[name]]),
          canonical_json(produce_runtime[[name]])
        )
      ) {
        stop(
          "Reject runtime field ",
          name,
          " differs from produce.",
          call. = FALSE
        )
      }
    }
    scalar_root <- file.path(output, "scalar-control")
    scalar_hashes <- capture_scalar_control_hashes(scalar_root)
    original_bundle_hashes <- capture_original_bundle_hashes(
      output,
      data$cases$forward
    )

    run$stage <- "positive control reads"
    write_run_state(run, FALSE)
    valid <- positive_control_rejections(
      output,
      scalar_root,
      data,
      mutation_data,
      run
    )
    start_case(run, "scalar control read", "scalar-control")
    scalar_record <- measure::read_measure_record(scalar_root)
    if (!inherits(scalar_record, "measure_record")) {
      stop(
        "The scalar control did not restore a public measure_record.",
        call. = FALSE
      )
    }
    record_case_complete(run, "scalar-control")

    invalid_results <- vector("list", length(mutation_data$invalid_records))
    for (index in seq_along(mutation_data$invalid_records)) {
      record <- mutation_data$invalid_records[[index]]
      id <- data$cases$invalid[[index]]
      start_case(run, "declared corrupt bundle rejections", id)
      message <- expected_error(
        measure::read_measure_nd(record$root),
        paste0("Corrupt bundle ", id)
      )
      invalid_results[[index]] <- list(
        id = json_string(id),
        error = json_scalar(TRUE),
        message = json_string(message)
      )
      record_case_complete(run, id)
    }

    cross_results <- cross_format_results(output, scalar_root, data, run)
    run$stage <- "source and mutation recheck"
    run$current_case <- NULL
    write_run_state(run, FALSE)
    assert_original_bundle_hashes(output, original_bundle_hashes)
    assert_scalar_control_hashes(scalar_root, scalar_hashes)
    for (index in seq_along(mutation_data$invalid_records)) {
      record <- mutation_data$invalid_records[[index]]
      verify_hashed_bundle(
        output,
        record$relative_path,
        record$hashes,
        paste0("Corrupt bundle ", data$cases$invalid[[index]])
      )
    }
    for (record in mutation_data$valid_records) {
      verify_hashed_bundle(
        output,
        record$relative_path,
        record$hashes,
        paste0("Positive bundle ", record$relative_path)
      )
    }
    rejection_source_hashes <- mutation_data$mutation_document$source_artifacts_sha256
    rejection_source_hashes[["mutations.json"]] <- mutation_file_sha
    if (
      !identical(
        sha256_file(mutation_path, "mutations.json"),
        mutation_file_sha
      )
    ) {
      stop("mutations.json changed during rejection.", call. = FALSE)
    }
    source_sha <- validate_source_hashes(
      output,
      rejection_source_hashes,
      include_mutations = TRUE
    )

    rejections <- list(
      profile = json_string(PROFILE),
      input_files_sha256 = input_hash_json(inputs$hashes),
      source_artifacts_sha256 = lapply(
        names(source_sha),
        function(name) json_string(source_sha[[name]])
      ),
      runtime = runtime,
      valid_bundles = valid$results,
      invalid_bundles = invalid_results,
      cross_format_rejections = cross_results
    )
    names(rejections$source_artifacts_sha256) <- names(source_sha)
    run$stage <- "rejection receipt"
    run$current_case <- NULL
    write_run_state(run, FALSE)
    write_json_file(rejections, file.path(output, "rejections.json"))
    TRUE
  })
  if (!succeeded) {
    quit(save = "no", status = 1L, runLast = FALSE)
  }
  invisible(output)
}

main <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "jsonlite is required before an output root can be reserved.",
      call. = FALSE
    )
  }
  trailing <- commandArgs(trailingOnly = TRUE)
  if (
    length(trailing) != 2L ||
      !trailing[[1L]] %in% c("produce", "reject") ||
      !nzchar(trailing[[2L]])
  ) {
    stop(
      paste(
        "Usage: Rscript tools/nd-interchange/qualify.R",
        "produce NEW_OUTPUT_ROOT",
        "or: Rscript tools/nd-interchange/qualify.R reject OUTPUT_ROOT"
      ),
      call. = FALSE
    )
  }
  script_path <- resolve_script()
  script_directory <- dirname(script_path)
  if (identical(trailing[[1L]], "produce")) {
    produce_phase(script_directory, trailing[[2L]])
  } else {
    reject_phase(script_directory, trailing[[2L]])
  }
  invisible(NULL)
}

main()
