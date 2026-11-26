# Run from the repository root with the candidate measure package installed.
library(measure)

stopifnot(as.character(utils::packageVersion("readJDX")) == "0.6.4")
fixtures <- jsonlite::read_json(
  "tools/jcamp/fixture.json",
  simplifyVector = TRUE
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) && length(args) != nrow(fixtures)) {
  stop(
    "Supply both local reference files in manifest order, or no arguments to download them."
  )
}

import <- function(path) {
  measure_read_jcamp(path, "ethanol", "scan", "ir", "curve", "source")
}
compare <- function(curve, external) {
  stopifnot(
    nrow(curve) == nrow(external),
    identical(is.na(curve$value), is.na(external$y))
  )
  dx <- max(abs(curve$location - external$x))
  dy <- max(abs(curve$value - external$y), na.rm = TRUE)
  stopifnot(dx <= 1e-11, dy <= 1e-14)
  c(points = nrow(curve), max_x_error = dx, max_y_error = dy)
}

for (i in seq_len(nrow(fixtures))) {
  ref <- fixtures[i, ]
  path <- if (length(args)) args[i] else tempfile(fileext = ".jdx")
  if (!length(args)) {
    utils::download.file(ref$url, path, mode = "wb", quiet = TRUE)
  }
  stopifnot(digest::digest(file = path, algo = "sha256") == ref$sha256)

  record <- import(path)
  curve <- record$curves$.measures[[1L]]
  stopifnot(
    nrow(curve) == ref$n,
    identical(record$sources$source_sha256, ref$sha256),
    record$curves$axis_unit == ref$xunit,
    record$curves$value_unit == ref$yunit
  )
  other <- readJDX::readJDX(path)[[ref$title]]
  cat(ref$title, "/", ref$xunit, "import\n")
  print(compare(curve, other))
  stopifnot(
    curve$location[ref$index] == ref$x,
    abs(curve$value[ref$index] - ref$y) < 1e-14
  )

  if (ref$exchange) {
    headers <- measure_jcamp_headers(path)
    field <- function(key) headers$value[headers$label == key]
    output <- tempfile(fileext = ".jdx")
    measure_write_jcamp(
      record,
      output,
      "curve",
      field("TITLE"),
      field("ORIGIN"),
      field("OWNER")
    )
    exported <- measure_jcamp_headers(output)
    for (key in c("TITLE", "ORIGIN", "OWNER", "XUNITS", "YUNITS")) {
      stopifnot(identical(exported$value[exported$label == key], field(key)))
    }
    stopifnot(identical(
      import(output)$curves$.measures,
      record$curves$.measures
    ))
    cat("Export as explicit XY pairs\n")
    print(compare(curve, readJDX::readJDX(output)[[ref$title]]))
  }
}

# Probe how the external reader handles an unknown response on an irregular grid.
probe <- import(system.file("extdata", "ir-film.jdx", package = "measure"))
keep <- c(1L, 8L, 38L, 66L, 81L)
probe$curves$.measures[[1L]] <- probe$curves$.measures[[1L]][keep, ]
probe$curves$.source_rows[[1L]] <- probe$curves$.source_rows[[1L]][keep]
out <- tempfile(fileext = ".jdx")
measure_write_jcamp(
  probe,
  out,
  "curve",
  "Simulated film",
  "measure examples",
  "measure contributors"
)
other <- readJDX::readJDX(out)[["Simulated film"]]
curve <- probe$curves$.measures[[1L]]
native <- import(out)$curves$.measures[[1L]]
stopifnot(
  identical(native, curve),
  "1670,?" %in% readLines(out),
  identical(which(is.na(other$y)), 4L),
  identical(which(is.na(other$x)), 4L)
)
cat("readJDX 0.6.4 returns NA for both coordinates of 1670,?; finite pairs:\n")
print(compare(curve[-4L, ], other[-4L, ]))
