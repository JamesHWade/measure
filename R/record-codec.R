# Private parsing and formatting helpers for analytical record interchange.

.record_parse_double <- function(x, field, allow_missing = FALSE) {
  if (!is.character(x) || !is.null(dim(x))) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  if (
    !is.character(field) ||
      length(field) != 1L ||
      is.na(field) ||
      !nzchar(field)
  ) {
    stop("`field` must be one non-empty string.", call. = FALSE)
  }
  if (
    !is.logical(allow_missing) ||
      length(allow_missing) != 1L ||
      is.na(allow_missing)
  ) {
    stop("`allow_missing` must be TRUE or FALSE.", call. = FALSE)
  }

  missing <- if (allow_missing) x == "" | x == "NA" else rep(FALSE, length(x))
  missing[is.na(missing)] <- FALSE
  present <- which(!missing)
  tokens <- x[present]

  grammar <- "\\A[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?\\z"
  valid <- !is.na(tokens) & grepl(grammar, tokens, perl = TRUE, useBytes = TRUE)
  if (!all(valid)) {
    row <- present[which(!valid)[1L]]
    stop(
      "Invalid numeric token in field `",
      field,
      "` at row ",
      row,
      ".",
      call. = FALSE
    )
  }

  normalized <- vapply(tokens, .record_codec_json_number, character(1))
  json_grammar <- "\\A-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?(?:0|[1-9][0-9]*))?\\z"
  json_valid <- grepl(json_grammar, normalized, perl = TRUE, useBytes = TRUE)
  if (!all(json_valid)) {
    row <- present[which(!json_valid)[1L]]
    stop(
      "Invalid numeric token in field `",
      field,
      "` at row ",
      row,
      ".",
      call. = FALSE
    )
  }

  out <- rep(NA_real_, length(x))
  if (length(normalized) == 0L) {
    return(out)
  }

  json <- paste0("[", paste(normalized, collapse = ","), "]")
  parsed <- .record_codec_parse_json(json)
  values <- as.double(unlist(parsed, recursive = FALSE, use.names = FALSE))

  bad_range <- !is.finite(values)
  if (any(bad_range)) {
    row <- present[which(bad_range)[1L]]
    stop(
      "Numeric value in field `",
      field,
      "` is outside the finite double range at row ",
      row,
      ".",
      call. = FALSE
    )
  }

  underflow <- values == 0 &
    vapply(tokens, .record_codec_nonzero_significand, logical(1))
  if (any(underflow)) {
    row <- present[which(underflow)[1L]]
    stop(
      "Non-zero numeric value in field `",
      field,
      "` underflows to zero at row ",
      row,
      ".",
      call. = FALSE
    )
  }

  out[present] <- values
  as.double(out)
}

.record_codec_json_number <- function(x) {
  parts <- regmatches(
    x,
    regexec(
      "\\A([+-]?)([0-9]*)(?:\\.([0-9]*))?(?:[eE]([+-]?)([0-9]+))?\\z",
      x,
      perl = TRUE,
      useBytes = TRUE
    )
  )[[1L]]

  sign <- parts[2L]
  integer <- parts[3L]
  fraction <- parts[4L]
  exponent_sign <- parts[5L]
  exponent <- parts[6L]

  if (identical(sign, "+")) {
    sign <- ""
  }
  integer <- sub("^0+", "", integer)
  if (!nzchar(integer)) {
    integer <- "0"
  }

  mantissa <- paste0(sign, integer)
  if (!identical(fraction, "")) {
    mantissa <- paste0(mantissa, ".", fraction)
  } else if (grepl("\\.", x, fixed = FALSE)) {
    mantissa <- paste0(mantissa, ".0")
  }

  if (!identical(exponent, "")) {
    exponent <- sub("^0+", "", exponent)
    if (!nzchar(exponent)) {
      exponent <- "0"
    }
    if (identical(exponent_sign, "+")) {
      exponent_sign <- ""
    }
    mantissa <- paste0(mantissa, "e", exponent_sign, exponent)
  }

  mantissa
}

.record_codec_nonzero_significand <- function(x) {
  significand <- sub("[eE].*$", "", x)
  grepl("[1-9]", significand, perl = TRUE, useBytes = TRUE)
}

.record_codec_parse_json <- function(json) {
  decimal <- Sys.localeconv()[["decimal_point"]]
  if (!identical(decimal, ".")) {
    old_locale <- Sys.getlocale("LC_NUMERIC")
    locale <- suppressWarnings(Sys.setlocale("LC_NUMERIC", "C"))
    on.exit(
      suppressWarnings(Sys.setlocale("LC_NUMERIC", old_locale)),
      add = TRUE
    )
    if (
      is.na(locale) ||
        !nzchar(locale) ||
        Sys.localeconv()[["decimal_point"]] != "."
    ) {
      stop(
        "Could not select a dot-decimal locale for strict numeric parsing.",
        call. = FALSE
      )
    }
  }
  suppressWarnings(jsonlite::fromJSON(json, simplifyVector = FALSE))
}

.record_format_double <- function(x) {
  if (typeof(x) != "double" || !is.null(dim(x))) {
    stop("`x` must be a double vector.", call. = FALSE)
  }
  invalid <- is.nan(x) | is.infinite(x)
  if (any(invalid)) {
    stop("Only finite doubles and NA values can be formatted.", call. = FALSE)
  }

  x[x == 0] <- 0
  out <- rep("", length(x))
  present <- which(!is.na(x))
  if (length(present) > 0L) {
    formatted <- sprintf("%.17g", x[present])
    decimal <- Sys.localeconv()[["decimal_point"]]
    if (!is.null(decimal) && !identical(decimal, ".") && nzchar(decimal)) {
      formatted <- gsub(decimal, ".", formatted, fixed = TRUE)
    }
    out[present] <- formatted

    parsed <- .record_parse_double(out[present], "formatted value")
    expected <- x[present]
    expected[expected == 0] <- 0
    if (!identical(parsed, expected)) {
      stop(
        "Formatted double values did not survive an exact codec roundtrip.",
        call. = FALSE
      )
    }
  }
  out
}

.record_read_raw <- function(file, max_bytes) {
  if (
    !is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)
  ) {
    stop("`file` must be one non-empty local file path.", call. = FALSE)
  }
  if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://", file, perl = TRUE)) {
    stop("`file` must be a local regular file path.", call. = FALSE)
  }
  if (
    !is.numeric(max_bytes) ||
      is.complex(max_bytes) ||
      length(max_bytes) != 1L ||
      is.na(max_bytes) ||
      !is.finite(max_bytes) ||
      max_bytes <= 0 ||
      max_bytes != floor(max_bytes) ||
      max_bytes > .Machine$integer.max - 1
  ) {
    stop(
      "`max_bytes` must be a positive whole number safely representable for bounded reads.",
      call. = FALSE
    )
  }
  if (!utils::file_test("-f", file)) {
    stop("`file` must name an existing local regular file.", call. = FALSE)
  }

  limit <- as.integer(max_bytes)
  con <- file(file, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = limit + 1L)
  if (length(bytes) > limit) {
    stop("File exceeds `max_bytes` while reading.", call. = FALSE)
  }
  bytes
}

.record_parse_delim <- function(raw, delim) {
  if (!is.raw(raw)) {
    stop("`raw` must be a raw vector.", call. = FALSE)
  }
  if (
    !is.character(delim) ||
      length(delim) != 1L ||
      is.na(delim) ||
      !(delim %in% c(",", "\t"))
  ) {
    stop("`delim` must be either a comma or a tab.", call. = FALSE)
  }
  if (any(raw == as.raw(0))) {
    stop("Delimited input must not contain NUL bytes.", call. = FALSE)
  }

  if (length(raw) >= 3L && identical(raw[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    raw <- raw[-(1:3)]
  }
  text <- rawToChar(raw)
  Encoding(text) <- "UTF-8"
  iconv_text <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  if (length(iconv_text) != 1L || is.na(iconv_text) || !validUTF8(text)) {
    stop("Delimited input must be valid UTF-8.", call. = FALSE)
  }

  if (length(raw) == 0L) {
    stop("Delimited input is empty and has no header.", call. = FALSE)
  }

  bytes <- as.integer(raw)
  delimiter <- as.integer(charToRaw(delim))
  quote <- 34L
  lf <- 10L
  cr <- 13L
  specials <- which(bytes %in% c(delimiter, quote, lf, cr))
  input_length <- length(bytes)
  parser <- new.env(parent = emptyenv())
  parser$rows <- list()
  parser$current <- character()
  parser$field_parts <- list()
  parser$ended_record <- FALSE
  mode <- "start"
  field_start <- 1L
  chunk_start <- NA_integer_
  last_event <- 0L
  event_index <- 1L

  finish_field <- function(value) {
    Encoding(value) <- "UTF-8"
    parser$current <- c(parser$current, value)
  }

  finish_quoted_field <- function() {
    if (length(parser$field_parts) == 0L) {
      finish_field("")
    } else {
      finish_field(rawToChar(do.call(c, parser$field_parts)))
    }
    parser$field_parts <- list()
  }

  finish_row <- function() {
    parser$rows[[length(parser$rows) + 1L]] <- parser$current
    parser$current <- character()
    parser$field_parts <- list()
    parser$ended_record <- TRUE
  }

  append_chunk <- function(first, last) {
    if (first <= last) {
      parser$field_parts[[length(parser$field_parts) + 1L]] <- raw[first:last]
    }
  }

  while (event_index <= length(specials)) {
    position <- specials[event_index]
    byte <- bytes[position]

    if (mode == "quoted") {
      if (byte == quote) {
        append_chunk(chunk_start, position - 1L)
        if (position < input_length && bytes[position + 1L] == quote) {
          parser$field_parts[[length(parser$field_parts) + 1L]] <- as.raw(quote)
          chunk_start <- position + 2L
          event_index <- event_index + 2L
          last_event <- position + 1L
          parser$ended_record <- FALSE
          next
        }
        mode <- "after_quote"
        chunk_start <- NA_integer_
      }
      last_event <- position
      parser$ended_record <- FALSE
      event_index <- event_index + 1L
      next
    }

    if (mode == "after_quote" && position > last_event + 1L) {
      stop(
        "Unexpected bytes after a closing quote in delimited input.",
        call. = FALSE
      )
    }

    is_newline <- byte == lf || byte == cr
    if (
      byte == cr && (position == input_length || bytes[position + 1L] != lf)
    ) {
      stop(
        "A carriage return outside a quoted field must be followed by line feed.",
        call. = FALSE
      )
    }
    newline_width <- if (byte == cr) 2L else 1L

    if (mode == "start") {
      if (byte == quote) {
        if (position != field_start) {
          stop("Unexpected quote inside an unquoted field.", call. = FALSE)
        }
        mode <- "quoted"
        chunk_start <- position + 1L
      } else if (byte == delimiter) {
        if (position > field_start) {
          finish_field(rawToChar(raw[field_start:(position - 1L)]))
        } else {
          finish_field("")
        }
        field_start <- position + 1L
      } else if (is_newline) {
        if (position > field_start) {
          finish_field(rawToChar(raw[field_start:(position - 1L)]))
        } else {
          finish_field("")
        }
        finish_row()
        field_start <- position + newline_width
      }
    } else if (mode == "after_quote") {
      if (byte == delimiter) {
        finish_quoted_field()
        mode <- "start"
        field_start <- position + 1L
      } else if (is_newline) {
        finish_quoted_field()
        finish_row()
        mode <- "start"
        field_start <- position + newline_width
      } else {
        stop(
          "Unexpected bytes after a closing quote in delimited input.",
          call. = FALSE
        )
      }
    }

    if (is_newline && byte == cr) {
      event_index <- event_index + 1L
      if (
        event_index <= length(specials) &&
          specials[event_index] == position + 1L
      ) {
        event_index <- event_index + 1L
      }
    } else {
      event_index <- event_index + 1L
    }
    last_event <- position
    parser$ended_record <- is_newline
  }

  if (mode == "quoted") {
    stop("Unterminated quoted field in delimited input.", call. = FALSE)
  }
  if (mode == "after_quote") {
    if (input_length > last_event) {
      stop(
        "Unexpected bytes after a closing quote in delimited input.",
        call. = FALSE
      )
    }
    finish_quoted_field()
    finish_row()
  } else if (field_start <= input_length || !parser$ended_record) {
    if (field_start <= input_length) {
      finish_field(rawToChar(raw[field_start:input_length]))
    } else {
      finish_field("")
    }
    finish_row()
  }

  if (length(parser$rows) == 0L) {
    stop("Delimited input has no header record.", call. = FALSE)
  }
  header <- parser$rows[[1L]]
  if (anyNA(header) || !all(nzchar(header))) {
    stop("Delimited input headers must be non-empty strings.", call. = FALSE)
  }
  if (anyDuplicated(header)) {
    stop("Delimited input has duplicate headers.", call. = FALSE)
  }

  data_rows <- parser$rows[-1L]
  if (length(data_rows) > 0L) {
    bad_counts <- which(lengths(data_rows) != length(header))
    if (length(bad_counts) > 0L) {
      stop(
        "Delimited record ",
        bad_counts[1L] + 1L,
        " has a different number of fields than the header.",
        call. = FALSE
      )
    }
  }

  columns <- stats::setNames(
    lapply(seq_along(header), function(column) {
      if (length(data_rows) == 0L) {
        character()
      } else {
        vapply(data_rows, `[[`, character(1), column)
      }
    }),
    header
  )
  tibble::as_tibble(columns, .name_repair = "minimal")
}
