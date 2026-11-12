test_that("the numeric codec accepts decimal tokens and normalizes them as JSON numbers", {
  tokens <- c("+000", ".5", "1.", "00012.3400", "-000.0", "1e+0003", "0e-999")

  expect_identical(
    .record_parse_double(tokens, "signal"),
    c(0, 0.5, 1, 12.34, 0, 1000, 0)
  )
  expect_identical(
    .record_parse_double(
      c("", "NA", "-2.5", "0"),
      "value",
      allow_missing = TRUE
    ),
    c(NA_real_, NA_real_, -2.5, 0)
  )
})

test_that("numeric missing tokens retain their positions and strict parsing reports rows", {
  expect_identical(
    .record_parse_double(c("1", "NA", "", "4"), "value", allow_missing = TRUE),
    c(1, NA_real_, NA_real_, 4)
  )
  expect_error(.record_parse_double(c("1", " "), "location"), "row 2")
  expect_error(.record_parse_double(c("1", NA_character_), "location"), "row 2")
  expect_error(.record_parse_double("NA", "location"), "row 1")
})

test_that("numeric parsing rejects malformed, non-finite, overflow, and non-zero underflow tokens", {
  invalid <- c(
    "",
    " ",
    " 1",
    "1 ",
    "1\n",
    "1\r",
    "1x",
    "0x10",
    "1,5",
    ".",
    "+",
    "-",
    "1e",
    "1e+",
    "NaN",
    "Inf",
    "-Inf",
    "Infinity",
    "1e309",
    "1e-324"
  )

  for (token in invalid) {
    expect_error(.record_parse_double(token, "signal"), regexp = "row 1")
  }
})

test_that("formatted doubles roundtrip exactly across difficult binary64 values", {
  values <- c(
    0,
    -0,
    1 + .Machine$double.eps,
    pi,
    .Machine$double.xmin,
    .Machine$double.xmin * .Machine$double.eps,
    .Machine$double.xmax,
    -1.2345678901234567e-200,
    9.876543210987654e+200,
    NA_real_
  )
  formatted <- .record_format_double(values)
  expected <- values
  expected[expected == 0] <- 0

  expect_identical(formatted[1:2], c("0", "0"))
  expect_identical(formatted[10], "")
  expect_identical(
    .record_parse_double(formatted, "value", allow_missing = TRUE),
    expected
  )
})

test_that("a seeded binary64 corpus roundtrips without tolerance", {
  set.seed(20261111)
  exponent <- sample(-1074:1023, 5000, replace = TRUE)
  mantissa <- runif(length(exponent), min = -1, max = 1)
  values <- mantissa * 2^exponent
  values <- values[is.finite(values)]
  expected <- values
  expected[expected == 0] <- 0

  expect_identical(
    .record_parse_double(.record_format_double(values), "seeded value"),
    expected
  )
})

test_that("numeric codec behavior is independent of display options", {
  old <- options(digits = 3, scipen = 1000)
  on.exit(options(old), add = TRUE)
  values <- c(1 + .Machine$double.eps, pi, .Machine$double.xmax)

  expect_identical(
    .record_parse_double(.record_format_double(values), "value"),
    values
  )
})

test_that("numeric codec uses a dot under an available alternate numeric locale", {
  original <- Sys.getlocale("LC_NUMERIC")
  on.exit(suppressWarnings(Sys.setlocale("LC_NUMERIC", original)), add = TRUE)

  candidates <- c("fr_FR.UTF-8", "fr_FR.utf8", "de_DE.UTF-8", "de_DE.utf8")
  changed <- FALSE
  for (candidate in candidates) {
    locale <- suppressWarnings(Sys.setlocale("LC_NUMERIC", candidate))
    if (
      !is.na(locale) &&
        nzchar(locale) &&
        Sys.localeconv()[["decimal_point"]] != "."
    ) {
      changed <- TRUE
      break
    }
  }
  if (!changed) {
    skip(
      "No installed alternate locale with a non-dot decimal mark is available."
    )
  }

  values <- c(
    0.125,
    1 + .Machine$double.eps,
    pi,
    .Machine$double.xmin,
    .Machine$double.xmax
  )
  formatted <- .record_format_double(values)
  expect_false(any(grepl(",", formatted, fixed = TRUE)))
  expect_identical(.record_parse_double(formatted, "value"), values)
})

test_that("double formatting rejects non-finite or non-double inputs", {
  expect_error(.record_format_double(c(1, Inf)), "finite doubles")
  expect_error(.record_format_double(c(1, NaN)), "finite doubles")
  expect_error(.record_format_double(1L), "double vector")
})

test_that("bounded raw reading enforces limits while reading", {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  writeBin(as.raw(c(1, 2, 3)), path)
  original <- readBin(path, what = "raw", n = 3)

  expect_identical(.record_read_raw(path, 3), as.raw(c(1, 2, 3)))
  expect_error(.record_read_raw(path, 2), "exceeds `max_bytes`")
  expect_identical(readBin(path, what = "raw", n = 3), original)
  expect_error(.record_read_raw(path, 0), "positive whole number")
  expect_error(.record_read_raw(path, 1.5), "positive whole number")
  expect_error(.record_read_raw(path, Inf), "positive whole number")
  expect_error(
    .record_read_raw(path, .Machine$integer.max),
    "positive whole number"
  )
  expect_error(.record_read_raw(c(path, path), 3), "one non-empty")
  expect_error(
    .record_read_raw("https://example.invalid/data.csv", 3),
    "local regular file"
  )
  expect_error(.record_read_raw(dirname(path), 3), "local regular file")
})

test_that("delimited parsing preserves UTF-8, blanks, literal NA, and quoted content", {
  raw <- charToRaw(enc2utf8(paste0(
    "\ufeffid,label,value,note\r\n",
    "0007,NA,,\"quoted, text\"\r\n",
    "-MS-α,  kept  ,1,\"line one\nline two\"\r\n",
    "#literal,plain,2,\"escaped \"\"quote\"\"\""
  )))
  parsed <- .record_parse_delim(raw, ",")

  expect_s3_class(parsed, "tbl_df")
  expect_true(all(vapply(parsed, is.character, logical(1))))
  expect_identical(names(parsed), c("id", "label", "value", "note"))
  expect_identical(parsed$id, c("0007", "-MS-α", "#literal"))
  expect_identical(parsed$label, c("NA", "  kept  ", "plain"))
  expect_identical(parsed$value, c("", "1", "2"))
  expect_identical(
    parsed$note,
    c("quoted, text", "line one\nline two", "escaped \"quote\"")
  )
})

test_that("delimited parsing supports TSV and header-only files", {
  parsed <- .record_parse_delim(charToRaw("left\tright\nA\tB\n"), "\t")
  header_only <- .record_parse_delim(charToRaw("left,right"), ",")

  expect_identical(parsed$left, "A")
  expect_identical(parsed$right, "B")
  expect_identical(nrow(header_only), 0L)
  expect_identical(header_only$left, character())
})

test_that("delimited parsing rejects malformed CSV and unsafe byte sequences", {
  expect_error(.record_parse_delim(raw(), ","), "empty")
  expect_error(
    .record_parse_delim(charToRaw("a,a\n1,2"), ","),
    "duplicate headers"
  )
  expect_error(
    .record_parse_delim(charToRaw("a,b\nx"), ","),
    "different number"
  )
  expect_error(
    .record_parse_delim(charToRaw("a,b\nx,\"y"), ","),
    "Unterminated"
  )
  expect_error(
    .record_parse_delim(charToRaw("a,b\nx\"y,z"), ","),
    "Unexpected quote"
  )
  expect_error(
    .record_parse_delim(charToRaw("a,b\n\"x\"z,q"), ","),
    "after a closing quote"
  )
  expect_error(.record_parse_delim(as.raw(c(0x61, 0x00, 0x62)), ","), "NUL")
  expect_error(.record_parse_delim(as.raw(c(0xc3, 0x28)), ","), "valid UTF-8")
  expect_error(
    .record_parse_delim(as.raw(c(0xf4, 0x90, 0x80, 0x80)), ","),
    "valid UTF-8"
  )
  expect_error(
    .record_parse_delim(charToRaw("a;b\n1;2"), ";"),
    "comma or a tab"
  )
})
