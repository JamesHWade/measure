# Typed numeric tokens for native nD interchange.

.nd_io_format_numeric <- function(x) {
  if (
    !typeof(x) %in% c("integer", "double") ||
      !is.null(attributes(x))
  ) {
    stop(
      "`x` must be an attribute-free integer or double vector.",
      call. = FALSE
    )
  }

  if (typeof(x) == "integer") {
    out <- rep(NA_character_, length(x))
    missing <- is.na(x)
    present <- which(!missing)
    out[present] <- as.character(x[present])
    out[missing] <- "NA"
    return(out)
  }

  out <- rep(NA_character_, length(x))
  nan <- is.nan(x)
  missing <- is.na(x) & !nan
  positive_infinity <- is.infinite(x) & x > 0
  negative_infinity <- is.infinite(x) & x < 0
  finite <- is.finite(x)

  if (any(finite)) {
    out[finite] <- .record_format_double(x[finite])
  }
  out[.nd_io_is_negative_zero(x)] <- "-0"
  out[missing] <- "NA"
  out[nan] <- "NaN"
  out[positive_infinity] <- "Inf"
  out[negative_infinity] <- "-Inf"
  out
}

.nd_io_parse_numeric <- function(x, type, field) {
  .nd_io_validate_codec_vector(x)
  .nd_io_validate_codec_type(type)
  .nd_io_validate_codec_field(field)

  if (identical(type, "integer")) {
    return(.nd_io_parse_integer(x, field))
  }

  .nd_io_parse_double(x, field)
}

.nd_io_validate_codec_vector <- function(x) {
  if (typeof(x) != "character" || !is.null(attributes(x))) {
    stop(
      "`x` must be an attribute-free character vector.",
      call. = FALSE
    )
  }
  invisible(x)
}

.nd_io_validate_codec_type <- function(type) {
  if (
    typeof(type) != "character" ||
      length(type) != 1L ||
      !is.null(attributes(type)) ||
      is.na(type) ||
      !(type %in% c("integer", "double"))
  ) {
    stop(
      "`type` must be the plain scalar `integer` or `double`.",
      call. = FALSE
    )
  }
  invisible(type)
}

.nd_io_validate_codec_field <- function(field) {
  if (
    typeof(field) != "character" ||
      length(field) != 1L ||
      !is.null(attributes(field)) ||
      is.na(field) ||
      !nzchar(field)
  ) {
    stop("`field` must be one non-empty plain character scalar.", call. = FALSE)
  }
  invisible(field)
}

.nd_io_parse_integer <- function(x, field) {
  missing <- !is.na(x) & x == "NA"
  present <- which(!missing)
  tokens <- x[present]
  grammar <- "\\A(?:0|-[1-9][0-9]*|[1-9][0-9]*)\\z"
  valid <- !is.na(tokens) & grepl(grammar, tokens, perl = TRUE, useBytes = TRUE)

  if (!all(valid)) {
    row <- present[which(!valid)[1L]]
    stop(
      "Invalid integer token in field `",
      field,
      "` at row ",
      row,
      ".",
      call. = FALSE
    )
  }

  values <- suppressWarnings(as.double(tokens))
  in_range <- is.finite(values) &
    values >= -2147483647 &
    values <= .Machine$integer.max
  if (!all(in_range)) {
    row <- present[which(!in_range)[1L]]
    stop(
      "Integer in field `",
      field,
      "` is outside the supported range at row ",
      row,
      ".",
      call. = FALSE
    )
  }

  out <- rep(NA_integer_, length(x))
  out[present] <- as.integer(values)
  out
}

.nd_io_parse_double <- function(x, field) {
  out <- rep(NA_real_, length(x))
  missing <- which(!is.na(x) & x == "NA")
  nan <- which(!is.na(x) & x == "NaN")
  positive_infinity <- which(!is.na(x) & x == "Inf")
  negative_infinity <- which(!is.na(x) & x == "-Inf")
  negative_zero <- which(!is.na(x) & x == "-0")

  special <- rep(FALSE, length(x))
  special[c(
    missing,
    nan,
    positive_infinity,
    negative_infinity,
    negative_zero
  )] <-
    TRUE

  ordinary <- which(!special)
  finite_tokens <- x
  finite_tokens[special] <- "0"
  values <- .record_parse_double(finite_tokens, field)

  if (length(ordinary) > 0L) {
    ordinary_values <- values[ordinary]
    tokens <- x[ordinary]
    negative_finite_zero <- startsWith(tokens, "-") & ordinary_values == 0
    ordinary_values[negative_finite_zero] <- .nd_io_negative_zero_values(
      sum(negative_finite_zero)
    )
    out[ordinary] <- ordinary_values
  }

  out[nan] <- NaN
  out[positive_infinity] <- Inf
  out[negative_infinity] <- -Inf
  out[negative_zero] <- .nd_io_negative_zero_values(length(negative_zero))
  out
}

.nd_io_is_negative_zero <- function(x) {
  zeros <- which(is.finite(x) & x == 0)
  negative <- rep(FALSE, length(x))
  if (length(zeros) == 0L) {
    return(negative)
  }

  connection <- rawConnection(raw(), open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(as.double(x[zeros]), connection, size = 8L, endian = "big")
  bytes <- rawConnectionValue(connection)
  sign_bytes <- bytes[seq.int(1L, length(bytes), by = 8L)]
  negative[zeros] <- sign_bytes == as.raw(128L)
  negative
}

.nd_io_negative_zero_values <- function(n) {
  zero_bytes <- c(128L, rep(0L, 7L))
  readBin(
    as.raw(rep(zero_bytes, n)),
    what = double(),
    n = n,
    size = 8L,
    endian = "big"
  )
}
