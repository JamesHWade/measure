# Runtime and progress helpers retained from the qualified native nD tools.
# This file is sourced by qualify.R; it does not load a source package.

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

input_hash_json <- function(hashes) {
  output <- lapply(INPUT_NAMES, function(name) json_string(hashes[[name]]))
  names(output) <- INPUT_NAMES
  output
}
