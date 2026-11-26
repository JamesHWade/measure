jcamp_decode_test_block <- function(
  mode,
  data,
  n_points,
  first_x = NULL,
  last_x = NULL,
  x_factor = "1",
  y_factor = "1",
  version = "4.24",
  data_class = NULL,
  form = NULL
) {
  headers <- c(
    TITLE = "Test spectrum",
    JCAMPDX = version,
    DATATYPE = "INFRARED SPECTRUM",
    XUNITS = "1/CM",
    YUNITS = "ABSORBANCE",
    XFACTOR = x_factor,
    YFACTOR = y_factor,
    NPOINTS = as.character(n_points)
  )
  if (!is.null(first_x)) {
    headers <- c(headers, FIRSTX = as.character(first_x))
  }
  if (!is.null(last_x)) {
    headers <- c(headers, LASTX = as.character(last_x))
  }
  if (!is.null(data_class)) {
    headers <- c(headers, DATACLASS = data_class)
  }
  if (is.null(form)) {
    form <- if (mode == "XYDATA") "(X++(Y..Y))" else "(XY..XY)"
  }
  headers <- c(headers, stats::setNames(form, mode), END = "")
  list(headers = headers, lines = data, mode = mode)
}

test_that("XYPOINTS decodes scaled irregular points and missing signals", {
  block <- jcamp_decode_test_block(
    "XYPOINTS",
    "1,2; 2,? 3,4 4,6",
    4,
    first_x = 2,
    last_x = 8,
    x_factor = "2",
    y_factor = "0.5",
    version = "5.01"
  )

  decoded <- jcamp_decode(block)

  expect_identical(decoded$x, c(2, 4, 6, 8))
  expect_identical(decoded$y, c(1, NA_real_, 2, 3))
  expect_identical(decoded$x_unit, "1/CM")
  expect_identical(decoded$y_unit, "ABSORBANCE")
  expect_identical(decoded$title, "Test spectrum")
})

test_that("XYPOINTS accepts descending and irregular explicit X values", {
  block <- jcamp_decode_test_block(
    "XYPOINTS",
    "4,0.1; 2.5,0.2 1,0.3",
    3,
    first_x = 0.4,
    last_x = 0.1,
    x_factor = "0.1",
    y_factor = "10"
  )

  decoded <- jcamp_decode(block)

  expect_equal(decoded$x, c(0.4, 0.25, 0.1))
  expect_equal(decoded$y, c(1, 2, 3))
})

test_that("XYPOINTS endpoint declarations cannot hide a coordinate mismatch", {
  block <- jcamp_decode_test_block(
    "XYPOINTS",
    "10000000000000000,1; 10000000000000010,2",
    2,
    first_x = "10000000000000004",
    last_x = "10000000000000010"
  )

  expect_error(jcamp_decode(block), "coordinates disagree|FIRSTX")
})

test_that("XYDATA uses actual endpoints and accepts rounded line anchors", {
  block <- jcamp_decode_test_block(
    "XYDATA",
    c("5 2 4", "3.005 6 8"),
    4,
    first_x = 10,
    last_x = 4,
    x_factor = "2",
    y_factor = "0.1",
    version = "5.00"
  )

  decoded <- jcamp_decode(block)

  expect_identical(decoded$x, c(10, 8, 6, 4))
  expect_equal(decoded$y, c(0.2, 0.4, 0.6, 0.8))
})

test_that("XYDATA supports one point and missing signal values", {
  block <- jcamp_decode_test_block(
    "XYDATA",
    "8 ?",
    1,
    first_x = 4,
    last_x = 4,
    x_factor = "0.5",
    y_factor = "2"
  )

  decoded <- jcamp_decode(block)

  expect_identical(decoded$x, 4)
  expect_identical(decoded$y, NA_real_)
})

test_that("XYDATA rejects a line anchor at the wrong grid position", {
  block <- jcamp_decode_test_block(
    "XYDATA",
    c("0 1 2", "3 3 4 5"),
    5,
    first_x = 0,
    last_x = 4
  )

  expect_error(jcamp_decode(block), "anchor")
})

test_that("malformed and unsupported data forms fail closed", {
  compressed <- jcamp_decode_test_block(
    "XYDATA",
    "1 1 2",
    2,
    first_x = 1,
    last_x = 2,
    form = "(X++(SQZ..SQZ))"
  )
  peaks <- jcamp_decode_test_block(
    "XYPOINTS",
    "1,1; 2,2",
    2,
    first_x = 1,
    last_x = 2,
    data_class = "PEAK TABLE"
  )
  bad_token <- jcamp_decode_test_block(
    "XYDATA",
    "1 nope 2",
    2,
    first_x = 1,
    last_x = 2
  )
  bad_count <- jcamp_decode_test_block(
    "XYPOINTS",
    "1,1",
    2,
    first_x = 1,
    last_x = 2
  )
  nonmonotone <- jcamp_decode_test_block(
    "XYPOINTS",
    "1,1; 3,2 2,3",
    3,
    first_x = 1,
    last_x = 2
  )

  expect_error(jcamp_decode(compressed), "compressed")
  expect_error(jcamp_decode(peaks), "unsupported|PEAK")
  expect_error(jcamp_decode(bad_token), "Invalid numeric")
  expect_error(jcamp_decode(bad_count), "point count|NPOINTS")
  expect_error(jcamp_decode(nonmonotone), "ascending|descending")
})

test_that("declared and scaled point limits reject oversized or underflowing values", {
  too_many <- jcamp_decode_test_block(
    "XYPOINTS",
    "1,1",
    1000001,
    first_x = 1,
    last_x = 1
  )
  underflow <- jcamp_decode_test_block(
    "XYPOINTS",
    "1,1e-300",
    1,
    first_x = 1,
    last_x = 1,
    y_factor = "1e-100"
  )
  finite_max <- jcamp_decode_test_block(
    "XYPOINTS",
    "1,1",
    1,
    first_x = 1,
    last_x = 1,
    y_factor = "1.7976931348623157e+308"
  )
  overflow <- jcamp_decode_test_block(
    "XYPOINTS",
    "1,2",
    1,
    first_x = 1,
    last_x = 1,
    y_factor = "1.7976931348623157e+308"
  )

  expect_error(jcamp_decode(too_many), "NPOINTS|1000000")
  expect_error(jcamp_decode(underflow), "underflow")
  expect_identical(jcamp_decode(finite_max)$y, .Machine$double.xmax)
  expect_error(jcamp_decode(overflow), "finite range|overflow")
})

test_that("the scanned reader rejects a second JCAMP block", {
  lines <- c(
    "##TITLE=First",
    "##JCAMP-DX=4.24",
    "##DATA TYPE=INFRARED SPECTRUM",
    "##XUNITS=1/CM",
    "##YUNITS=ABSORBANCE",
    "##XFACTOR=1",
    "##YFACTOR=1",
    "##NPOINTS=1",
    "##XYPOINTS=(XY..XY)",
    "1,1",
    "##END=",
    "##TITLE=Second"
  )

  expect_error(jcamp_block(lines), "one JCAMP-DX block")
})
