# Decode one continuous infrared spectrum using the declared AFFN data layout.
jcamp_decode <- function(block) {
  headers <- block$headers
  get <- function(key, required = TRUE) {
    value <- unname(headers[names(headers) == key])
    if (!length(value) && !required) {
      return(NULL)
    }
    if (length(value) != 1L || !nzchar(value)) {
      cli::cli_abort(
        "Expected one nonblank JCAMP declaration for {.val {key}}."
      )
    }
    value
  }
  number <- function(key) .record_parse_double(get(key), key)
  mode <- block$mode
  title <- get("TITLE")
  version <- get("JCAMPDX")
  if (!version %in% c("4.24", "5.00", "5.01")) {
    cli::cli_abort("Unsupported JCAMP-DX version {.val {version}}.")
  }
  if (toupper(get("DATATYPE")) != "INFRARED SPECTRUM") {
    cli::cli_abort("Expected a single INFRARED SPECTRUM.")
  }
  data_class <- get("DATACLASS", FALSE)
  if (!is.null(data_class) && toupper(data_class) != mode) {
    cli::cli_abort(
      "Unsupported DATA CLASS {.val {data_class}} for {.val {mode}}."
    )
  }
  form <- toupper(gsub("[[:space:]]", "", get(mode)))
  expected <- if (mode == "XYDATA") "(X++(Y..Y))" else "(XY..XY)"
  if (form != expected) {
    cli::cli_abort(
      "Expected uncompressed {.val {expected}} data in {.val {mode}}."
    )
  }

  n <- number("NPOINTS")
  if (n < 1 || n > 1000000L || n != trunc(n)) {
    cli::cli_abort("NPOINTS must be a whole number between 1 and 1,000,000.")
  }
  n <- as.integer(n)
  xf <- number("XFACTOR")
  yf <- number("YFACTOR")
  if (xf == 0 || yf == 0) {
    cli::cli_abort("JCAMP scale factors must be nonzero.")
  }
  first <- get("FIRSTX", mode == "XYDATA")
  last <- get("LASTX", mode == "XYDATA")
  if (xor(is.null(first), is.null(last))) {
    cli::cli_abort("Declare FIRSTX and LASTX together.")
  }
  if (!is.null(first)) {
    first <- .record_parse_double(first, "FIRSTX")
    last <- .record_parse_double(last, "LASTX")
  }
  if (length(block$lines) > n) {
    cli::cli_abort("Data line count exceeds NPOINTS.")
  }

  if (mode == "XYDATA") {
    rows <- strsplit(block$lines, "[[:space:]]+", perl = TRUE)
    sizes <- lengths(rows)
    if (any(sizes < 2L) || sum(sizes - 1L) != n) {
      cli::cli_abort(
        "XYDATA needs an X anchor per line and NPOINTS observations in total."
      )
    }
    tokens <- unlist(rows, use.names = FALSE)
    anchor_index <- cumsum(c(1L, utils::head(sizes, -1L)))
    anchors <- jcamp_scaled(tokens[anchor_index], xf, "XYDATA anchor")
    values <- tokens[-anchor_index]
    if (n == 1L && first != last) {
      cli::cli_abort("A one-point XYDATA grid must have equal endpoints.")
    }
    x <- seq(first, last, length.out = n)
    starts <- anchor_index - seq_along(anchor_index) + 1L
    # Historical instruments rounded line anchors. Each must still identify
    # the same grid point unambiguously. DELTAX is only nominal in JCAMP-DX.
    interval <- if (n == 1L) 0 else abs(x[2L] - x[1L])
    half_step <- if (is.infinite(interval)) {
      abs(x[2L] / 2 - x[1L] / 2)
    } else {
      interval / 2
    }
    error <- abs(anchors - x[starts])
    if (any(error != 0 & error >= half_step)) {
      cli::cli_abort(
        "XYDATA line anchor is inconsistent with the declared grid."
      )
    }
  } else {
    pairs <- gsub("[[:blank:]]*,[[:blank:]]*", ",", block$lines, perl = TRUE)
    pairs <- unlist(
      strsplit(pairs, "[;[:blank:]]+", perl = TRUE),
      use.names = FALSE
    )
    if (length(pairs) != n) {
      cli::cli_abort("XYPOINTS point count disagrees with NPOINTS.")
    }
    comma <- regexpr(",", pairs, fixed = TRUE)
    if (any(comma <= 1L)) {
      cli::cli_abort("Expected an X,Y pair in XYPOINTS.")
    }
    x <- jcamp_scaled(substr(pairs, 1L, comma - 1L), xf, "XYPOINTS X")
    values <- substring(pairs, comma + 1L)
    if (!is.null(first)) {
      endpoints <- c(x[1L], x[n])
      declared <- c(first, last)
      gaps <- if (n == 1L) {
        c(Inf, Inf)
      } else {
        abs(c(x[2L] - x[1L], x[n] - x[n - 1L]))
      }
      error <- abs(endpoints - declared)
      rounding <- if (abs(xf) == 1) {
        0
      } else {
        2 * .Machine$double.eps * pmax(abs(declared), abs(endpoints))
      }
      if (any(error > rounding | (error != 0 & error >= gaps / 2))) {
        cli::cli_abort("XYPOINTS coordinates disagree with FIRSTX or LASTX.")
      }
    }
  }
  if (n > 1L && !(all(diff(x) > 0) || all(diff(x) < 0))) {
    cli::cli_abort("Coordinates must be strictly ascending or descending.")
  }
  y <- rep(NA_real_, n)
  observed <- values != "?"
  y[observed] <- jcamp_scaled(values[observed], yf, "signal")
  list(
    x = x,
    y = y,
    x_unit = get("XUNITS"),
    y_unit = get("YUNITS"),
    x_label = get("XLABEL", FALSE),
    y_label = get("YLABEL", FALSE),
    title = title
  )
}

jcamp_scaled <- function(tokens, factor, field) {
  values <- .record_parse_double(tokens, field)
  scaled <- values * factor
  if (!all(is.finite(scaled))) {
    cli::cli_abort("Scaled {.field {field}} exceeds the finite range.")
  }
  if (any(scaled == 0 & values != 0)) {
    cli::cli_abort("Scaling causes underflow in {.field {field}}.")
  }
  scaled
}
