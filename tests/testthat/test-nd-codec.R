nd_test_read_double_hex <- function(hex) {
  bytes <- substring(
    hex,
    seq.int(1L, nchar(hex), by = 2L),
    seq.int(2L, nchar(hex), by = 2L)
  )
  readBin(
    as.raw(strtoi(bytes, base = 16L)),
    what = double(),
    n = 1L,
    size = 8L,
    endian = "big"
  )
}

nd_test_write_double_bytes <- function(x) {
  connection <- rawConnection(raw(), open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(x, connection, size = 8L, endian = "big")
  rawConnectionValue(connection)
}

nd_test_state_tags <- function(x) {
  tags <- rep("finite", length(x))
  nan <- is.nan(x)
  tags[is.na(x) & !nan] <- "NA"
  tags[nan] <- "NaN"
  tags[is.infinite(x) & x > 0] <- "Inf"
  tags[is.infinite(x) & x < 0] <- "-Inf"
  tags
}

test_that("nD point tokens retain storage type and special double states", {
  point_index <- c(-2147483647L, -1L, 0L, 2147483647L, NA_integer_)
  expect_identical(
    .nd_io_format_numeric(point_index),
    c("-2147483647", "-1", "0", "2147483647", "NA")
  )

  response <- c(
    NA_real_,
    NaN,
    Inf,
    -Inf,
    nd_test_read_double_hex("8000000000000000"),
    nd_test_read_double_hex("0000000000000000"),
    nd_test_read_double_hex("0000000000000001"),
    nd_test_read_double_hex("000fffffffffffff"),
    nd_test_read_double_hex("0010000000000000"),
    nd_test_read_double_hex("3fb999999999999a"),
    nd_test_read_double_hex("7fefffffffffffff")
  )
  encoded <- .nd_io_format_numeric(response)

  expect_identical(
    encoded[seq_len(6L)],
    c("NA", "NaN", "Inf", "-Inf", "-0", "0")
  )
  expect_identical(
    encoded[7:11],
    c(
      "4.9406564584124654e-324",
      "2.2250738585072009e-308",
      "2.2250738585072014e-308",
      "0.10000000000000001",
      "1.7976931348623157e+308"
    )
  )

  decoded <- .nd_io_parse_numeric(encoded, "double", "detector_response")
  expect_identical(typeof(decoded), "double")
  expect_null(attributes(decoded))
  expect_identical(nd_test_state_tags(decoded), nd_test_state_tags(response))
  finite <- is.finite(response)
  expect_identical(
    nd_test_write_double_bytes(decoded[finite]),
    nd_test_write_double_bytes(response[finite])
  )
})

test_that("integer nD tokens parse canonical boundary values exactly", {
  parsed <- .nd_io_parse_numeric(
    c("-2147483647", "-1", "0", "2147483647", "NA"),
    "integer",
    "point_index"
  )

  expect_identical(
    parsed,
    c(-2147483647L, -1L, 0L, 2147483647L, NA_integer_)
  )
  expect_identical(typeof(parsed), "integer")
  expect_null(attributes(parsed))
  expect_identical(.nd_io_format_numeric(integer()), character())
  expect_identical(
    .nd_io_parse_numeric(character(), "integer", "point_index"),
    integer()
  )
})

test_that("negative finite zero spellings preserve their IEEE sign", {
  tokens <- c("-0", "-0.0", "-00e5", "-0.000E-99", "0", "0.0")
  parsed <- .nd_io_parse_numeric(tokens, "double", "location_1")
  expected <- c(
    nd_test_read_double_hex("8000000000000000"),
    nd_test_read_double_hex("8000000000000000"),
    nd_test_read_double_hex("8000000000000000"),
    nd_test_read_double_hex("8000000000000000"),
    nd_test_read_double_hex("0000000000000000"),
    nd_test_read_double_hex("0000000000000000")
  )

  expect_identical(
    nd_test_write_double_bytes(parsed),
    nd_test_write_double_bytes(expected)
  )
  expect_identical(.nd_io_format_numeric(parsed)[1L], "-0")
  expect_identical(.nd_io_format_numeric(parsed)[5L], "0")
  expect_identical(
    .nd_io_parse_numeric(character(), "double", "value"),
    double()
  )
})

test_that("nD integer and double token grammars reject malformed values", {
  bad_integer <- c(
    "-2147483648",
    "2147483648",
    "999999999999999999999999",
    "-0",
    "+1",
    "01",
    "-01",
    "1.0",
    "1e0",
    "",
    "NaN",
    "Inf",
    "-Inf",
    " "
  )
  for (token in bad_integer) {
    expect_error(
      .nd_io_parse_numeric(token, "integer", "point_index"),
      "point_index"
    )
  }

  bad_double <- c(
    "1e309",
    "-1e309",
    "1e-9999",
    "-1e-9999",
    " 1",
    "1 ",
    "+Inf",
    "Infinity",
    "nan",
    "",
    "1,5"
  )
  for (token in bad_double) {
    expect_error(
      .nd_io_parse_numeric(token, "double", "location_2"),
      "location_2"
    )
  }

  expect_snapshot(
    error = TRUE,
    .nd_io_parse_numeric("01", "integer", "point_index")
  )
})

test_that("double parse errors retain row positions after special tokens", {
  expect_error(
    .nd_io_parse_numeric(c("NA", "Inf", "bad"), "double", "value"),
    "at row 3"
  )
  expect_error(
    .nd_io_parse_numeric(c("NA", "Inf", "1e309"), "double", "value"),
    "at row 3"
  )
  expect_error(
    .nd_io_parse_numeric(c("NaN", "-Inf", "1e-9999"), "double", "value"),
    "at row 3"
  )
  expect_error(
    .nd_io_parse_numeric(
      c("NA", "Inf", NA_character_),
      "double",
      "value"
    ),
    "at row 3"
  )
})

test_that("nD codec validates plain vectors and exact scalar labels", {
  expect_error(.nd_io_format_numeric(TRUE), "integer or double")
  expect_error(.nd_io_format_numeric(1 + 0i), "integer or double")
  expect_error(.nd_io_format_numeric(matrix(1, nrow = 1L)), "attribute-free")
  expect_error(
    .nd_io_format_numeric(structure(1L, names = "row")),
    "attribute-free"
  )
  expect_error(
    .nd_io_format_numeric(structure(1, unit = "m")),
    "attribute-free"
  )

  expect_error(.nd_io_parse_numeric(1, "integer", "point_index"), "character")
  expect_error(
    .nd_io_parse_numeric(matrix("1"), "integer", "point_index"),
    "attribute-free"
  )
  expect_error(
    .nd_io_parse_numeric(
      structure("1", names = "row"),
      "integer",
      "point_index"
    ),
    "attribute-free"
  )
  expect_error(.nd_io_parse_numeric("1", "integer", ""), "field")
  expect_error(.nd_io_parse_numeric("1", NA_character_, "point_index"), "type")
  expect_error(
    .nd_io_parse_numeric("1", c("integer", "double"), "point_index"),
    "type"
  )
  expect_error(
    .nd_io_parse_numeric("1", structure("integer", foo = 1), "point_index"),
    "type"
  )
  expect_error(.nd_io_parse_numeric("1", "numeric", "point_index"), "type")
  expect_error(.nd_io_parse_numeric("1", "double", c("x", "y")), "field")
  expect_error(
    .nd_io_parse_numeric("1", "double", structure("x", foo = 1)),
    "field"
  )
})
