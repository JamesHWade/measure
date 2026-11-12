#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
full_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grepv("^--file=", full_args)
if (length(script_arg) != 1L) {
  stop("Run this file with Rscript so its fixture directory can be resolved.")
}
script_path <- normalizePath(
  sub("^--file=", "", script_arg[[1]]),
  winslash = "/",
  mustWork = TRUE
)
script_dir <- dirname(script_path)
profile <- "measure-record-interchange-qualification/1"

is_symlink <- function(path) {
  target <- suppressWarnings(Sys.readlink(path))
  length(target) == 1L && !is.na(target) && nzchar(target)
}

sha256_file <- function(path) {
  size <- file.info(path)$size
  if (is.na(size)) {
    stop("Cannot read file size for ", path)
  }
  bytes <- readBin(path, what = "raw", n = size)
  if (length(bytes) != size) {
    stop("Short read while hashing ", path)
  }
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

declaration_path <- file.path(script_dir, "qualification-declaration.json")
input_manifest_path <- file.path(
  script_dir,
  "qualification-input-manifest.json"
)
declaration <- jsonlite::read_json(declaration_path)
input_manifest <- jsonlite::read_json(input_manifest_path)
declaration_sha256 <- sha256_file(declaration_path)
input_manifest_sha256 <- sha256_file(input_manifest_path)
if (!identical(declaration_sha256, input_manifest$declaration_sha256)) {
  stop("The frozen qualification declaration checksum does not match.")
}
for (source_name in names(input_manifest$source_files)) {
  source_path <- file.path(script_dir, "qualification-input", source_name)
  source_info <- input_manifest$source_files[[source_name]]
  if (
    file.info(source_path)$size != source_info$bytes ||
      !identical(sha256_file(source_path), source_info$sha256)
  ) {
    stop("Frozen input bytes do not match the manifest: ", source_name)
  }
}

scalar <- function(x) ifelse(is.null(x), NA_character_, x)

new_output_root <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Supply one nonblank output path.")
  }
  path <- path.expand(path)
  parent <- dirname(path)
  leaf <- basename(path)
  if (!dir.exists(parent)) {
    stop("The output parent directory must already exist: ", parent)
  }
  if (leaf %in% c("", ".", "..")) {
    stop("The output path must name a new directory.")
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  path <- file.path(parent, leaf)
  if (file.exists(path) || dir.exists(path) || is_symlink(path)) {
    stop("The output path already exists; choose a new directory: ", path)
  }
  if (!dir.create(path, showWarnings = FALSE)) {
    stop("Could not create the new output directory: ", path)
  }

  marker <- list(
    format = profile,
    declaration_sha256 = declaration_sha256,
    input_manifest_sha256 = input_manifest_sha256
  )
  jsonlite::write_json(
    marker,
    file.path(path, "qualification-root.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  path
}

existing_output_root <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Supply one nonblank output path.")
  }
  path <- path.expand(path)
  if (is_symlink(path) || !dir.exists(path)) {
    stop("The output path must be an existing qualification directory.")
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  marker_path <- file.path(path, "qualification-root.json")
  if (is_symlink(marker_path) || !file.exists(marker_path)) {
    stop("The output directory is not an owned qualification root.")
  }
  marker <- jsonlite::read_json(marker_path)
  if (
    !identical(marker$format, profile) ||
      !identical(marker$declaration_sha256, declaration_sha256) ||
      !identical(marker$input_manifest_sha256, input_manifest_sha256)
  ) {
    stop("The qualification-root marker does not match these frozen inputs.")
  }
  path
}

if (length(args) != 2L || !args[[1]] %in% c("produce", "reject")) {
  stop(
    "Usage: Rscript qualify.R produce OUTPUT\n",
    "   or: Rscript qualify.R reject OUTPUT"
  )
}

phase <- args[[1]]
out <- if (identical(phase, "produce")) {
  new_output_root(args[[2]])
} else {
  existing_output_root(args[[2]])
}

library(measure)

mapping <- stats::setNames(
  c(
    "sample_id",
    "injection_id",
    "channel_id",
    "curve_id",
    "location",
    "value",
    "channel_name",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  ),
  c(
    "sample_id",
    "injection_id",
    "channel_id",
    "curve_id",
    "location",
    "value",
    "channel_name",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )
)

assert_declared <- function(record, source_name) {
  validate_measure_record(record)
  stopifnot(
    identical(
      record$samples$sample_id,
      unlist(declaration$expected_sample_order)
    ),
    identical(
      record$injections$injection_id,
      unlist(declaration$expected_injection_order)
    ),
    identical(
      record$channels$channel_id,
      unlist(declaration$expected_channel_order)
    ),
    identical(record$curves$curve_id, unlist(declaration$expected_curve_order)),
    identical(record$sources$source_id, declaration$source_id),
    identical(record$sources$source_name, source_name),
    identical(
      record$sources$source_sha256,
      input_manifest$source_files[[source_name]]$sha256
    )
  )
  for (i in seq_along(declaration$curves)) {
    expected <- declaration$curves[[i]]
    observed <- record$curves[i, ]
    for (field in c(
      "curve_id",
      "injection_id",
      "channel_id",
      "axis_name",
      "axis_unit",
      "value_name",
      "value_unit"
    )) {
      stopifnot(identical(observed[[field]], scalar(expected[[field]])))
    }
    expected_value <- vapply(
      expected$value,
      function(x) if (is.null(x)) NA_real_ else as.double(x),
      double(1)
    )
    stopifnot(
      identical(
        observed$.measures[[1]]$location,
        as.double(unlist(expected$location))
      ),
      identical(observed$.measures[[1]]$value, expected_value),
      identical(
        observed$.source_rows[[1]],
        as.integer(unlist(expected$source_rows))
      )
    )
  }
  invisible(record)
}

if (identical(phase, "produce")) {
  receipts <- list()
  for (source in c("csv", "tsv")) {
    source_name <- paste0("roundtrip.", source)
    record <- read_measure_record_delim(
      file.path(script_dir, "qualification-input", source_name),
      columns = mapping,
      source_id = declaration$source_id,
      delim = if (source == "csv") "," else "\t"
    )
    assert_declared(record, source_name)
    for (target in c("csv", "tsv")) {
      path <- file.path(out, paste0("from-", source, "-to-", target))
      write_measure_record(
        record,
        path,
        delim = if (target == "csv") "," else "\t"
      )
      restored <- read_measure_record(path)
      assert_declared(restored, source_name)
      stopifnot(identical(record, restored))
      receipts[[length(receipts) + 1L]] <- list(
        source = source,
        target = target,
        samples = nrow(record$samples),
        injections = nrow(record$injections),
        channels = nrow(record$channels),
        curves = nrow(record$curves),
        points = sum(vapply(record$curves$.measures, nrow, integer(1))),
        exact_public_roundtrip = TRUE
      )
    }
  }

  # Frozen seed and corpus were declared before observing the interchange code.
  corpus_rng_kind <- c("Mersenne-Twister", "Inversion", "Rejection")
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(81107)
  stopifnot(identical(RNGkind(), corpus_rng_kind))
  corpus <- c(
    1 + .Machine$double.eps,
    pi,
    .Machine$double.xmin,
    .Machine$double.xmin * .Machine$double.eps,
    .Machine$double.xmax,
    -pi,
    1e-300,
    1e300,
    runif(10000, -1e6, 1e6),
    (runif(10000) * 2 - 1) * 2^sample(-1074:1023, 10000, replace = TRUE)
  )
  # This corpus has finite values; source declaration excludes signed-zero bits.
  stopifnot(length(corpus) == 20008L, all(is.finite(corpus)))
  wide <- measure_record(
    samples = data.frame(sample_id = "numeric-corpus"),
    injections = data.frame(
      injection_id = "injection-1",
      sample_id = "numeric-corpus"
    ),
    channels = data.frame(channel_id = "channel-1"),
    curves = tibble::tibble(
      curve_id = "corpus",
      injection_id = "injection-1",
      channel_id = "channel-1",
      .measures = list(new_measure_tbl(as.double(seq_along(corpus)), corpus))
    )
  )
  for (format in c("csv", "tsv")) {
    path <- file.path(out, paste0("numeric-corpus-", format))
    write_measure_record(wide, path, delim = if (format == "csv") "," else "\t")
    restored <- read_measure_record(path)
    stopifnot(identical(restored, wide))
  }

  binary_path <- file.path(out, "numeric-corpus.f64le")
  writeBin(
    wide$curves$.measures[[1]]$value,
    binary_path,
    size = 8L,
    endian = "little"
  )
  binary_size <- file.info(binary_path)$size
  if (binary_size != length(corpus) * 8) {
    stop("The numeric corpus binary oracle has an unexpected size.")
  }

  versions <- vapply(
    c("measure", "recipes", "jsonlite", "digest"),
    function(package) as.character(utils::packageVersion(package)),
    character(1)
  )
  tool_checkout_head <- tryCatch(
    suppressWarnings(system2(
      "git",
      c("-C", shQuote(script_dir), "rev-parse", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character()
  )
  if (
    length(tool_checkout_head) != 1L ||
      !grepl("^[0-9a-f]{40,64}$", tool_checkout_head[[1]])
  ) {
    tool_checkout_head <- NA_character_
  } else {
    tool_checkout_head <- unname(tool_checkout_head[[1]])
  }
  session_info_file <- "session-info.txt"
  writeLines(
    capture.output(sessionInfo()),
    file.path(out, session_info_file)
  )
  receipt <- list(
    profile = declaration$profile,
    fixture_cross_format_roundtrips = receipts,
    numeric_corpus = list(
      seed = 81107L,
      rng_kind = corpus_rng_kind,
      values = length(corpus),
      formats = c("csv", "tsv"),
      binary_file = basename(binary_path),
      binary_bytes = binary_size,
      binary_sha256 = sha256_file(binary_path)
    ),
    installed_measure = list(
      path = find.package("measure"),
      version = as.character(utils::packageVersion("measure"))
    ),
    qualification_tool_checkout_head = tool_checkout_head,
    R = as.character(getRversion()),
    session_info_file = session_info_file,
    versions = as.list(versions),
    scope = declaration$scope
  )
  jsonlite::write_json(
    receipt,
    file.path(out, "public-R-receipt.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  print(receipt)
} else {
  expected_cases <- c(
    "corrupt-numeric-byte",
    "missing-table",
    "unknown-version",
    "duplicate-index-rehashed",
    "fractional-source-row-rehashed",
    "reversed-physical-rows-rehashed",
    "partial-no-manifest",
    "symlink-member"
  )
  expected_outcomes <- c(
    rep("reject", 5L),
    "restore",
    "reject",
    "reject"
  )
  cases <- jsonlite::read_json(
    file.path(out, "mutations.json"),
    simplifyVector = FALSE
  )
  if (
    !identical(
      vapply(cases, `[[`, character(1), "directory"),
      expected_cases
    ) ||
      !identical(
        vapply(cases, `[[`, character(1), "expect"),
        expected_outcomes
      )
  ) {
    stop("Mutation cases do not match the fixed qualification profile.")
  }
  receipt_path <- file.path(out, "corruption-receipt.json")
  if (file.exists(receipt_path) || is_symlink(receipt_path)) {
    stop("Refusing to overwrite an existing corruption receipt.")
  }
  outcomes <- lapply(cases, function(case) {
    value <- tryCatch(
      read_measure_record(file.path(out, case$directory)),
      error = identity
    )
    if (case$expect == "reject") {
      stopifnot(inherits(value, "error"))
      list(
        case = case$directory,
        rejected = TRUE,
        message = conditionMessage(value)
      )
    } else {
      assert_declared(value, "roundtrip.csv")
      list(case = case$directory, reordered_points_restored = TRUE)
    }
  })
  jsonlite::write_json(outcomes, receipt_path, auto_unbox = TRUE, pretty = TRUE)
  print(outcomes)
}
