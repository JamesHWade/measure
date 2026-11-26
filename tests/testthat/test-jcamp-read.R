test_that("JCAMP import and exchange preserve an irregular spectrum and its attribution", {
  path <- withr::local_tempfile(fileext = ".jdx")
  lines <- c(
    "##TITLE=Film B",
    "##JCAMP-DX=4.24",
    "##DATA TYPE=INFRARED SPECTRUM",
    "##ORIGIN=Lab B",
    "##OWNER=Spectroscopy group",
    "Instrument facility",
    "##$NOTE=first",
    "##$NOTE=second",
    "##XLABEL=Wavenumber",
    "##YLABEL=Detector response",
    "##XUNITS=1/CM",
    "##YUNITS=ABSORBANCE",
    "##XFACTOR=1",
    "##YFACTOR=0.01",
    "##FIRSTX=1800",
    "##LASTX=1670",
    "##NPOINTS=4",
    "##XYPOINTS=(XY..XY)",
    "1800,1; 1740,20 $$ strongest observed band",
    "1725,?; 1670,3",
    "##END="
  )
  bytes <- charToRaw(paste(lines, collapse = "\r\n"))
  writeBin(bytes, path)
  h <- measure_jcamp_headers(path)
  expect_identical(h$value[h$label == "$NOTE"], c("first", "second"))
  owner <- h$value[h$label == "OWNER"]
  expect_identical(owner, "Spectroscopy group\nInstrument facility")

  read <- function(file) {
    measure_read_jcamp(file, "film-B", "scan-1", "ir", "ir-1", "instrument-1")
  }
  record <- read(path)
  expect_identical(validate_measure_record(record), record)
  expect_identical(
    record$sources$source_sha256,
    digest::digest(bytes, "sha256", serialize = FALSE)
  )
  expect_identical(record$sources$source_name, basename(path))
  expect_identical(record$curves$.source_rows[[1L]], 1:4)
  expect_identical(
    record$curves$.measures[[1L]]$location,
    c(1800, 1740, 1725, 1670)
  )
  expect_equal(record$curves$.measures[[1L]]$value, c(0.01, 0.2, NA, 0.03))
  expect_identical(record$curves$axis_name, "Wavenumber")
  expect_identical(record$curves$value_name, "Detector response")
  expect_identical(record$curves$axis_unit, "1/CM")
  expect_identical(record$curves$value_unit, "ABSORBANCE")

  out <- withr::local_tempfile(fileext = ".jdx")
  measure_write_jcamp(record, out, "ir-1", "Film B", "Lab B", owner)
  expect_identical(read(out)$curves$.measures, record$curves$.measures)
  headers <- measure_jcamp_headers(out)
  expect_identical(headers$value[headers$label == "OWNER"], owner)
})

test_that("header inspection is bounded and detects structural ambiguity", {
  path <- withr::local_tempfile()
  writeLines(c("##TITLE=T", "##XYDATA=(X++(Y..Y))", "123 bad", "##END="), path)
  expect_named(measure_jcamp_headers(path), c("label", "value"))
  expect_error(measure_jcamp_headers(path, max_bytes = 4L), "max_bytes")
  expect_error(
    measure_read_jcamp(path, "x", c("a", "b"), "ir", "c", "src"),
    "injection_id"
  )
  writeLines(
    c("##TITLE=T", "##XYPOINTS=(XY..XY)", "1,2", "##END=", "2,3"),
    path
  )
  expect_error(measure_jcamp_headers(path), "one JCAMP-DX block")
  writeBin(as.raw(c(35, 35, 0, 65)), path)
  expect_error(measure_jcamp_headers(path), "NUL")
})

test_that("private declarations cannot replace scale factors or move line anchors", {
  path <- withr::local_tempfile()
  src <- readLines(system.file("extdata", "ir-film.jdx", package = "measure"))
  writeLines(sub("##YFACTOR=", "##$YFACTOR=", src, fixed = TRUE), path)
  read <- function() measure_read_jcamp(path, "s", "i", "ir", "c", "file")
  expect_error(read(), "YFACTOR")

  shift <- sub("1784 ", "1782 ", src, fixed = TRUE)
  writeLines(shift, path)
  expect_error(read(), "anchor")
})
