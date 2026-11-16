#!/usr/bin/env Rscript

PROFILE <- "measure-acquisition/1"
INPUT_NAMES <- c(
  "declaration.json",
  "fixtures.json",
  "wire-schema.json",
  "finite-stress.bin"
)
MAX_HASH_BYTES <- 100 * 1024^2
EXPECTED_OBJECTS <- 16L
EXPECTED_FORWARD <- 32L
EXPECTED_CONTROLS <- 4L
EXPECTED_INVALID <- 46L
EXPECTED_CROSS_FORMAT <- 4L
EXPECTED_TRACES <- 10L
EXPECTED_SEMANTIC_REJECTIONS <- 15L
FINITE_STRESS_COUNT <- 8208L
FINITE_STRESS_BYTES <- 65664L
FROZEN_COPY_NAMES <- c(INPUT_NAMES, "input-manifest.json")
PRODUCER_RECEIPTS <- c(
  "runtime.json",
  "observations.json",
  "trace-results.json",
  "semantic-rejections.json",
  "run-status.json",
  "progress.json"
)
MUTATION_RECEIPTS <- c(PRODUCER_RECEIPTS, "independent-python-receipt.json")

script_path <- NULL
HERE <- NULL
script_arguments <- commandArgs(trailingOnly = FALSE)
script_file_argument <- script_arguments[startsWith(
  script_arguments,
  "--file="
)]
if (length(script_file_argument) != 1L) {
  stop("Could not determine the qualification script path.", call. = FALSE)
}
raw_script_path <- sub("^--file=", "", script_file_argument[[1L]])
raw_script_path <- gsub("~+~", " ", raw_script_path, fixed = TRUE)
script_path <- normalizePath(raw_script_path, winslash = "/", mustWork = TRUE)
HERE <- dirname(script_path)
source(file.path(HERE, "support.R"))
script_path <- resolve_script()
HERE <- dirname(script_path)
source(file.path(HERE, "fixtures.R"))

.acq_abort <- function(message) {
  stop(message, call. = FALSE)
}

.acq_fields <- function(x, expected, label) {
  if (
    !is.list(x) ||
      is.null(names(x)) ||
      anyDuplicated(names(x)) > 0L ||
      length(names(x)) != length(expected) ||
      !setequal(names(x), expected)
  ) {
    .acq_abort(paste0(label, " has unexpected fields."))
  }
  invisible(x)
}

.acq_nonblank_string <- function(x, name, label) {
  value <- json_field_string(x, name, label)
  if (!nzchar(trimws(value))) {
    .acq_abort(paste0(label, " must be nonblank."))
  }
  value
}

.acq_safe_id <- function(id, label) {
  if (
    length(id) != 1L ||
      is.na(id) ||
      !grepl("^[a-z0-9]+(?:-[a-z0-9]+)*$", id, perl = TRUE)
  ) {
    .acq_abort(paste0(
      label,
      " must be a generated lowercase ASCII identifier."
    ))
  }
  id
}

.acq_require_function_exports <- function() {
  installed <- installed_measure_package()
  exports <- getNamespaceExports("measure")
  required <- c(
    "measure_acquisition",
    "is_measure_acquisition",
    "validate_measure_acquisition",
    "measure_ms_trace",
    "write_measure_acquisition",
    "read_measure_acquisition",
    "measure_record",
    "new_measure_tbl",
    "new_measure_nd_tbl",
    "write_measure_record",
    "read_measure_record",
    "write_measure_nd",
    "read_measure_nd"
  )
  missing <- setdiff(required, exports)
  if (length(missing) > 0L) {
    .acq_abort(paste0(
      "The installed measure namespace is missing required exports: ",
      paste(missing, collapse = ", "),
      "."
    ))
  }
  for (name in required) {
    if (!is.function(getExportedValue("measure", name))) {
      .acq_abort(paste0("Installed export ", name, " is not a function."))
    }
  }
  installed
}

.acq_validate_runtime <- function(runtime, label = "Runtime") {
  .acq_fields(
    runtime,
    c(
      "profile",
      "R",
      "installed_package",
      "tool_checkout_head",
      "created_utc",
      "session_info",
      "dependency_versions"
    ),
    label
  )
  if (
    !identical(
      json_field_string(runtime, "profile", paste0(label, " profile")),
      PROFILE
    )
  ) {
    .acq_abort(paste0(label, " profile is not supported."))
  }
  r_version <- .acq_nonblank_string(runtime, "R", paste0(label, " R version"))
  if (!startsWith(r_version, "R version ")) {
    .acq_abort(paste0(label, " has an invalid R version string."))
  }
  installed <- runtime$installed_package
  .acq_fields(
    installed,
    c("path", "namespace_path", "version", "built", "remote_sha"),
    paste0(label, " installed package")
  )
  path <- .acq_nonblank_string(
    installed,
    "path",
    paste0(label, " package path")
  )
  namespace_path <- .acq_nonblank_string(
    installed,
    "namespace_path",
    paste0(label, " namespace path")
  )
  if (!identical(path, namespace_path)) {
    .acq_abort(paste0(label, " package and namespace paths differ."))
  }
  .acq_nonblank_string(installed, "version", paste0(label, " package version"))
  for (field in c("built", "remote_sha")) {
    value <- installed[[field]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L)) {
      .acq_abort(paste0(
        label,
        " has an invalid installed package ",
        field,
        "."
      ))
    }
  }
  head <- runtime$tool_checkout_head
  if (
    !is.null(head) &&
      (!is.character(head) ||
        length(head) != 1L ||
        !grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", head))
  ) {
    .acq_abort(paste0(label, " has an invalid tool checkout HEAD."))
  }
  session <- runtime$session_info
  if (
    !is.list(session) ||
      length(session) == 0L ||
      !all(vapply(session, is.character, logical(1))) ||
      !identical(as.character(session[[1L]]), r_version)
  ) {
    .acq_abort(paste0(label, " has invalid session information."))
  }
  dependencies <- runtime$dependency_versions
  expected_dependencies <- c("measure", "vctrs", "jsonlite", "digest", "fs")
  .acq_fields(
    dependencies,
    expected_dependencies,
    paste0(label, " dependency versions")
  )
  for (name in expected_dependencies) {
    .acq_nonblank_string(
      dependencies,
      name,
      paste0(label, " dependency version ", name)
    )
  }
  if (!identical(dependencies$measure, installed$version)) {
    .acq_abort(paste0(
      label,
      " measure dependency version differs from its package version."
    ))
  }
  .acq_nonblank_string(runtime, "created_utc", paste0(label, " timestamp"))
  invisible(runtime)
}

.acq_runtime_consistent <- function(produce, reject) {
  .acq_validate_runtime(produce, "Producer runtime")
  .acq_validate_runtime(reject, "Rejection runtime")
  for (field in c(
    "profile",
    "R",
    "installed_package",
    "tool_checkout_head",
    "dependency_versions"
  )) {
    assert_json_equal(
      reject[[field]],
      produce[[field]],
      paste0("Rejection runtime ", field)
    )
  }
  invisible(TRUE)
}

.acq_read_inputs <- function(inputs) {
  fixture_document <- read_json_file(
    inputs$paths[["fixtures.json"]],
    "fixtures.json",
    max_bytes = 16 * 1024^2
  )
  declaration <- read_json_file(
    inputs$paths[["declaration.json"]],
    "declaration.json",
    max_bytes = 2 * 1024^2
  )
  wire <- read_json_file(
    inputs$paths[["wire-schema.json"]],
    "wire-schema.json",
    max_bytes = 128 * 1024
  )
  .acq_fields(
    fixture_document,
    c(
      "profile",
      "vector_grammar",
      "record_templates",
      "objects",
      "traces",
      "semantic_rejections"
    ),
    "Frozen fixtures"
  )
  .acq_fields(
    declaration,
    c(
      "profile",
      "contract_sha256",
      "wire_schema_sha256",
      "object_ids",
      "forward",
      "controls",
      "invalid",
      "cross_format",
      "semantic_rejection_ids",
      "trace_ids",
      "finite_stress",
      "payload_table_count",
      "declaration_before_qualification_execution",
      "scope"
    ),
    "Frozen declaration"
  )
  .acq_fields(
    wire,
    c(
      "format",
      "version",
      "tables",
      "metadata_order_field",
      "mask_tokens",
      "contract_sha256"
    ),
    "Frozen wire schema"
  )

  if (
    !identical(
      json_field_string(fixture_document, "profile", "fixture profile"),
      PROFILE
    ) ||
      !identical(
        json_field_string(declaration, "profile", "declaration profile"),
        PROFILE
      )
  ) {
    .acq_abort("Frozen fixture or declaration profile is incorrect.")
  }
  if (
    !identical(
      json_field_string(wire, "format", "wire format"),
      "measure-acquisition"
    ) ||
      json_field_integer(wire, "version", "wire version") != 1L ||
      !identical(
        json_field_string(wire, "metadata_order_field"),
        "row_index"
      ) ||
      json_field_integer(declaration, "payload_table_count") != 12L ||
      !identical(
        json_field_string(wire, "contract_sha256"),
        json_field_string(declaration, "contract_sha256")
      ) ||
      !identical(
        json_field_string(declaration, "wire_schema_sha256"),
        unname(inputs$hashes[["wire-schema.json"]])
      )
  ) {
    .acq_abort("Frozen wire schema and declaration bindings do not match.")
  }
  finite <- declaration$finite_stress
  .acq_fields(
    finite,
    c("file", "storage", "byte_order", "values", "bytes", "sha256"),
    "Finite stress declaration"
  )
  if (
    !identical(json_field_string(finite, "file"), "finite-stress.bin") ||
      !identical(json_field_string(finite, "storage"), "double") ||
      !identical(json_field_string(finite, "byte_order"), "big") ||
      json_field_integer(finite, "values") != FINITE_STRESS_COUNT ||
      json_field_integer(finite, "bytes") != FINITE_STRESS_BYTES ||
      !identical(
        json_field_string(finite, "sha256"),
        inputs$hashes[["finite-stress.bin"]]
      )
  ) {
    .acq_abort("Finite stress declaration differs from the frozen input bytes.")
  }
  json_field_true(declaration, "declaration_before_qualification_execution")
  .acq_nonblank_string(declaration, "scope", "Qualification scope")

  objects <- fixture_document$objects
  if (!is.list(objects) || length(objects) != EXPECTED_OBJECTS) {
    .acq_abort(
      "The frozen fixture object count differs from the qualification profile."
    )
  }
  object_ids <- vapply(
    objects,
    function(object) json_field_string(object, "id", "fixture object ID"),
    character(1),
    USE.NAMES = FALSE
  )
  declared_ids <- json_array_character(
    declaration$object_ids,
    "declaration object_ids"
  )
  if (anyDuplicated(object_ids) > 0L || !identical(object_ids, declared_ids)) {
    .acq_abort("Frozen fixture order differs from the declared object IDs.")
  }
  if (
    length(fixture_document$record_templates) == 0L ||
      is.null(names(fixture_document$record_templates))
  ) {
    .acq_abort("Frozen fixtures have no named record templates.")
  }

  .acq_validate_declaration(declaration, fixture_document, inputs$hashes)
  corpus_bytes <- read_raw_file(
    inputs$paths[["finite-stress.bin"]],
    "finite-stress.bin",
    max_bytes = FINITE_STRESS_BYTES
  )
  corpus_values <- read_big_endian_doubles(
    corpus_bytes,
    FINITE_STRESS_COUNT,
    "finite-stress.bin"
  )
  if (
    length(corpus_values) != FINITE_STRESS_COUNT ||
      !all(is.finite(corpus_values))
  ) {
    .acq_abort(
      "Finite stress input must contain 8,208 finite big-endian doubles."
    )
  }
  list(
    fixture_document = fixture_document,
    fixtures = stats::setNames(objects, object_ids),
    declaration = declaration,
    inputs = inputs,
    wire = wire,
    corpus_values = corpus_values
  )
}

.acq_validate_declaration <- function(
  declaration,
  fixture_document,
  input_hashes
) {
  forward <- declaration$forward
  controls <- declaration$controls
  invalid <- declaration$invalid
  cross_format <- declaration$cross_format
  traces <- fixture_document$traces
  semantic <- fixture_document$semantic_rejections
  if (
    length(forward) != EXPECTED_FORWARD ||
      length(controls) != EXPECTED_CONTROLS ||
      length(invalid) != EXPECTED_INVALID ||
      length(cross_format) != EXPECTED_CROSS_FORMAT ||
      length(traces) != EXPECTED_TRACES ||
      length(semantic) != EXPECTED_SEMANTIC_REJECTIONS
  ) {
    .acq_abort(
      "Frozen qualification case counts differ from the declared profile."
    )
  }

  object_ids <- vapply(
    declaration$object_ids,
    as.character,
    character(1),
    USE.NAMES = FALSE
  )
  forward_ids <- character(length(forward))
  for (index in seq_along(forward)) {
    case <- forward[[index]]
    .acq_fields(
      case,
      c("id", "object_id", "path", "delimiter"),
      "Forward descriptor"
    )
    id <- .acq_safe_id(json_field_string(case, "id"), "Forward case ID")
    object_id <- json_field_string(case, "object_id")
    if (!object_id %in% object_ids) {
      .acq_abort(paste0(
        "Forward descriptor ",
        id,
        " references an unknown object."
      ))
    }
    if (!json_field_string(case, "delimiter") %in% c("csv", "tsv")) {
      .acq_abort(paste0(
        "Forward descriptor ",
        id,
        " has an unsupported delimiter."
      ))
    }
    if (!identical(json_field_string(case, "path"), paste0("bundles/", id))) {
      .acq_abort(paste0("Forward descriptor ", id, " has a noncanonical path."))
    }
    forward_ids[[index]] <- id
  }
  if (anyDuplicated(forward_ids) > 0L) {
    .acq_abort("Forward descriptor IDs must be unique.")
  }
  for (case in controls) {
    fields <- c("id", "kind", "object_id", "path", "delimiter")
    if ("source_case_id" %in% names(case)) {
      fields <- c(fields, "source_case_id")
    }
    .acq_fields(case, fields, "Control descriptor")
    id <- .acq_safe_id(json_field_string(case, "id"), "Control case ID")
    if (!json_field_string(case, "object_id") %in% object_ids) {
      .acq_abort(paste0(
        "Control descriptor ",
        id,
        " references an unknown object."
      ))
    }
    if (!json_field_string(case, "kind") %in% c("reverse", "reorder")) {
      .acq_abort(paste0("Control descriptor ", id, " has an unsupported kind."))
    }
    if (!json_field_string(case, "delimiter") %in% c("csv", "tsv")) {
      .acq_abort(paste0(
        "Control descriptor ",
        id,
        " has an unsupported delimiter."
      ))
    }
    if (!identical(json_field_string(case, "path"), paste0("controls/", id))) {
      .acq_abort(paste0("Control descriptor ", id, " has a noncanonical path."))
    }
    if (
      "source_case_id" %in% names(case) && !case$source_case_id %in% forward_ids
    ) {
      .acq_abort(paste0(
        "Control descriptor ",
        id,
        " references an unknown source bundle."
      ))
    }
  }
  for (case in invalid) {
    .acq_fields(
      case,
      c("id", "source_case_id", "path", "mutation"),
      "Invalid descriptor"
    )
    id <- .acq_safe_id(json_field_string(case, "id"), "Invalid case ID")
    if (
      !identical(json_field_string(case, "path"), paste0("mutants/", id)) ||
        !identical(json_field_string(case, "mutation"), id) ||
        !json_field_string(case, "source_case_id") %in% forward_ids
    ) {
      .acq_abort(paste0(
        "Invalid descriptor ",
        id,
        " does not match a fixed source case."
      ))
    }
  }
  for (case in cross_format) {
    fields <- if ("source_kind" %in% names(case)) {
      c("id", "reader", "source_kind")
    } else {
      c("id", "reader", "source_case_id")
    }
    .acq_fields(case, fields, "Cross-format descriptor")
    id <- .acq_safe_id(json_field_string(case, "id"), "Cross-format case ID")
    reader <- json_field_string(case, "reader")
    if (!reader %in% c("acquisition", "scalar-record", "nd-object")) {
      .acq_abort(paste0(
        "Cross-format descriptor ",
        id,
        " has an unsupported reader."
      ))
    }
    if ("source_case_id" %in% names(case)) {
      source_case_id <- json_field_string(case, "source_case_id")
      if (!source_case_id %in% forward_ids || reader == "acquisition") {
        .acq_abort(paste0(
          "Cross-format descriptor ",
          id,
          " has an invalid acquisition source."
        ))
      }
    } else {
      source_kind <- json_field_string(case, "source_kind")
      if (
        !source_kind %in% c("scalar-record", "nd-object") ||
          reader != "acquisition"
      ) {
        .acq_abort(paste0(
          "Cross-format descriptor ",
          id,
          " has an invalid legacy source."
        ))
      }
    }
  }
  case_ids <- c(
    forward_ids,
    vapply(controls, function(case) case$id, character(1)),
    vapply(invalid, function(case) case$id, character(1)),
    vapply(cross_format, function(case) case$id, character(1))
  )
  if (anyDuplicated(case_ids) > 0L) {
    .acq_abort("Qualification descriptor IDs must be unique.")
  }

  trace_ids <- vapply(
    traces,
    function(trace) json_field_string(trace, "id", "Trace ID"),
    character(1),
    USE.NAMES = FALSE
  )
  declared_traces <- json_array_character(
    declaration$trace_ids,
    "declaration trace_ids"
  )
  semantic_ids <- vapply(
    semantic,
    function(trace) json_field_string(trace, "id", "Semantic rejection ID"),
    character(1),
    USE.NAMES = FALSE
  )
  declared_semantic <- json_array_character(
    declaration$semantic_rejection_ids,
    "declaration semantic_rejection_ids"
  )
  if (
    anyDuplicated(trace_ids) > 0L ||
      !identical(trace_ids, declared_traces) ||
      anyDuplicated(semantic_ids) > 0L ||
      !identical(semantic_ids, declared_semantic)
  ) {
    .acq_abort("Trace descriptors differ from the frozen declaration order.")
  }
  for (trace in c(traces, semantic)) {
    .acq_fields(
      trace,
      c(
        "id",
        "object_id",
        "stream_id",
        "polarity",
        "ms_level",
        "mz_range",
        "expected"
      ),
      "Trace descriptor"
    )
    if (
      !json_field_string(trace, "object_id") %in% object_ids ||
        !nzchar(json_field_string(trace, "stream_id"))
    ) {
      .acq_abort("Trace descriptor has an invalid object or stream selector.")
    }
  }
  invisible(TRUE)
}

.acq_assert_input_manifest <- function(here, inputs) {
  manifest <- read_json_file(
    file.path(here, "input-manifest.json"),
    "input-manifest.json",
    max_bytes = 128 * 1024
  )
  .acq_fields(manifest, c("profile", "files"), "Input manifest")
  if (!identical(json_field_string(manifest, "profile"), PROFILE)) {
    .acq_abort("Input manifest profile is incorrect.")
  }
  .acq_fields(manifest$files, INPUT_NAMES, "Input manifest files")
  for (name in INPUT_NAMES) {
    .acq_fields(
      manifest$files[[name]],
      c("sha256", "size_bytes"),
      paste0("Manifest ", name)
    )
    if (
      !identical(
        json_field_string(manifest$files[[name]], "sha256"),
        unname(inputs$hashes[[name]])
      )
    ) {
      .acq_abort(paste0("Input manifest hash differs for ", name, "."))
    }
  }
  list(
    manifest = manifest,
    sha256 = sha256_file(
      file.path(here, "input-manifest.json"),
      "input-manifest.json"
    )
  )
}

.acq_copy_frozen_inputs <- function(here, output, inputs, manifest_hash) {
  destination <- file.path(output, "frozen-inputs")
  if (!dir.create(destination, recursive = FALSE, showWarnings = FALSE)) {
    .acq_abort("Could not create the frozen-input copy directory.")
  }
  copy_bytes <- function(bytes, target) {
    connection <- file(target, open = "wb")
    on.exit(close(connection), add = TRUE)
    writeBin(bytes, connection)
    invisible(target)
  }
  for (name in FROZEN_COPY_NAMES) {
    source_path <- file.path(here, name)
    bytes <- read_raw_file(
      source_path,
      name,
      max_bytes = MAX_HASH_BYTES
    )
    target <- file.path(destination, name)
    copy_bytes(bytes, target)
    if (
      !identical(
        read_raw_file(target, paste0("copied ", name), MAX_HASH_BYTES),
        bytes
      )
    ) {
      .acq_abort(paste0("Copied frozen input bytes changed for ", name, "."))
    }
  }
  marker <- list(
    format = json_string(PROFILE),
    declaration_sha256 = json_string(inputs$hashes[["declaration.json"]]),
    input_manifest_sha256 = json_string(manifest_hash)
  )
  write_json_file(marker, file.path(output, "qualification-root.json"))
  invisible(destination)
}

.acq_verify_frozen_copies <- function(here, output, inputs, manifest_hash) {
  directory <- file.path(output, "frozen-inputs")
  assert_directory(directory, "Frozen-input copies")
  present <- list.files(directory, all.files = TRUE, no.. = TRUE)
  if (!identical(sort(present), sort(FROZEN_COPY_NAMES))) {
    .acq_abort(
      "Frozen-input copy directory must contain exactly the five source files."
    )
  }
  for (name in FROZEN_COPY_NAMES) {
    original <- read_raw_file(
      file.path(here, name),
      paste0("source ", name),
      max_bytes = MAX_HASH_BYTES
    )
    copied <- read_raw_file(
      file.path(directory, name),
      paste0("frozen copy ", name),
      max_bytes = MAX_HASH_BYTES
    )
    if (!identical(copied, original)) {
      .acq_abort(paste0(
        "Frozen-input copy differs from the original ",
        name,
        "."
      ))
    }
    if (
      name %in%
        INPUT_NAMES &&
        !identical(
          sha256_file(file.path(directory, name), paste0("frozen copy ", name)),
          inputs$hashes[[name]]
        )
    ) {
      .acq_abort(paste0("Frozen-input copy hash differs for ", name, "."))
    }
  }
  marker <- read_json_file(
    file.path(output, "qualification-root.json"),
    "qualification-root.json",
    max_bytes = 128 * 1024
  )
  .acq_fields(
    marker,
    c("format", "declaration_sha256", "input_manifest_sha256"),
    "Qualification marker"
  )
  if (
    !identical(json_field_string(marker, "format"), PROFILE) ||
      !identical(
        json_field_string(marker, "declaration_sha256"),
        inputs$hashes[["declaration.json"]]
      ) ||
      !identical(
        json_field_string(marker, "input_manifest_sha256"),
        manifest_hash
      )
  ) {
    .acq_abort("Qualification marker does not bind the frozen source bytes.")
  }
  invisible(TRUE)
}

.acq_observation_map <- function(observation_document, data) {
  .acq_fields(
    observation_document,
    c("profile", "objects"),
    "Producer observations"
  )
  if (!identical(json_field_string(observation_document, "profile"), PROFILE)) {
    .acq_abort("Producer observations have the wrong profile.")
  }
  observations <- observation_document$objects
  object_ids <- names(data$fixtures)
  if (!is.list(observations) || length(observations) != length(object_ids)) {
    .acq_abort(
      "Producer observation count differs from the frozen fixture count."
    )
  }
  observed_ids <- vapply(
    observations,
    function(object) json_field_string(object, "id", "Observed object ID"),
    character(1),
    USE.NAMES = FALSE
  )
  if (!identical(observed_ids, object_ids)) {
    .acq_abort("Producer observations differ from frozen object order.")
  }
  stats::setNames(observations, observed_ids)
}

.acq_table_stems <- function(wire) {
  prefixes <- names(wire$tables)
  if (
    is.null(prefixes) || length(prefixes) != 12L || anyDuplicated(prefixes) > 0L
  ) {
    .acq_abort("Wire schema has an invalid table prefix set.")
  }
  stems <- vapply(
    prefixes,
    function(prefix) {
      switch(
        prefix,
        ScanPoints = "scan_points",
        ArrayAxes = "array_axes",
        ArrayPoints = "array_points",
        tolower(prefix)
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
  stats::setNames(stems, prefixes)
}

.acq_fixed_member_candidates <- function(wire) {
  stems <- .acq_table_stems(wire)
  c(
    "manifest.dcf",
    as.vector(unlist(
      lapply(c("csv", "tsv"), function(extension) {
        paste0(stems, ".", extension)
      }),
      use.names = FALSE
    ))
  )
}

.acq_bundle_file_hashes <- function(path, wire) {
  assert_directory(path, paste0("Bundle ", path))
  candidates <- .acq_fixed_member_candidates(wire)
  present <- candidates[vapply(
    candidates,
    function(name) {
      is_path_entry(file.path(path, name))
    },
    logical(1)
  )]
  total_bytes <- 0
  hashes <- stats::setNames(vector("list", length(present)), present)
  for (name in present) {
    member <- file.path(path, name)
    if (!identical(path_type(member), "file")) {
      .acq_abort(paste0(
        "Selected bundle member is not a regular file: ",
        path,
        "/",
        name
      ))
    }
    size <- as.numeric(fs::file_info(member, follow = FALSE)$size[[1L]])
    if (is.na(size) || size < 0 || size > MAX_HASH_BYTES - total_bytes) {
      .acq_abort(paste0(
        "Selected bundle bytes exceed the cumulative hash limit: ",
        path
      ))
    }
    total_bytes <- total_bytes + size
    hashes[[name]] <- json_string(sha256_file(member, paste0(path, "/", name)))
  }
  hashes
}

.acq_assert_hash_map <- function(actual, expected, label) {
  if (
    !is.list(actual) ||
      is.null(names(actual)) ||
      anyDuplicated(names(actual)) > 0L ||
      !setequal(names(actual), names(expected)) ||
      length(actual) != length(expected)
  ) {
    .acq_abort(paste0(label, " does not have the expected fixed hash keys."))
  }
  for (name in names(expected)) {
    if (
      !identical(as.character(actual[[name]]), as.character(expected[[name]]))
    ) {
      .acq_abort(paste0(label, " hash differs for ", name, "."))
    }
  }
  invisible(TRUE)
}

.acq_assert_artifact_hashes <- function(root, actual, expected_names, label) {
  expected <- stats::setNames(
    vector("list", length(expected_names)),
    expected_names
  )
  for (name in expected_names) {
    expected[[name]] <- json_string(sha256_file(
      file.path(root, name),
      paste0(label, " ", name)
    ))
  }
  .acq_assert_hash_map(actual, expected, label)
}

.acq_read_receipt <- function(root, filename, max_bytes = 4 * 1024^2) {
  read_json_file(file.path(root, filename), filename, max_bytes = max_bytes)
}

.acq_check_phase_status <- function(
  root,
  phase,
  status_name,
  progress_name,
  expected_cases = character()
) {
  status <- .acq_read_receipt(root, status_name, 128 * 1024)
  .acq_fields(
    status,
    c("phase", "complete", "stage", "error", "updated_utc"),
    status_name
  )
  if (
    !identical(json_field_string(status, "phase"), phase) ||
      !isTRUE(status$complete) ||
      !identical(json_field_string(status, "stage"), "complete") ||
      !is.null(status$error)
  ) {
    .acq_abort(paste0(status_name, " is incomplete."))
  }
  .acq_nonblank_string(status, "updated_utc", paste0(status_name, " timestamp"))

  progress <- .acq_read_receipt(root, progress_name, 128 * 1024)
  .acq_fields(
    progress,
    c(
      "phase",
      "complete",
      "stage",
      "current_case",
      "completed_bundles",
      "error",
      "updated_utc"
    ),
    progress_name
  )
  if (
    !identical(json_field_string(progress, "phase"), phase) ||
      !isTRUE(progress$complete) ||
      !identical(json_field_string(progress, "stage"), "complete") ||
      !is.null(progress$current_case) ||
      !is.null(progress$error) ||
      !is.list(progress$completed_bundles)
  ) {
    .acq_abort(paste0(progress_name, " is incomplete."))
  }
  .acq_nonblank_string(
    progress,
    "updated_utc",
    paste0(progress_name, " timestamp")
  )
  completed <- json_array_character(
    progress$completed_bundles,
    paste0(progress_name, " completed_bundles")
  )
  if (length(completed) != length(unique(completed))) {
    .acq_abort(paste0(progress_name, " contains duplicate completed cases."))
  }
  if (!identical(as.character(expected_cases), completed)) {
    .acq_abort(paste0(
      progress_name,
      " completed cases differ from the fixed phase plan."
    ))
  }
  invisible(list(status = status, progress = progress))
}

.acq_valid_delimiter <- function(value) {
  if (identical(value, "csv")) {
    ","
  } else if (identical(value, "tsv")) {
    "\t"
  } else {
    .acq_abort("Bundle delimiter must be csv or tsv.")
  }
}

.acq_output_path <- function(root, descriptor, prefix) {
  id <- .acq_safe_id(json_field_string(descriptor, "id"), "Descriptor ID")
  relative <- json_field_string(descriptor, "path")
  if (!identical(relative, paste0(prefix, "/", id))) {
    .acq_abort(paste0("Descriptor path is not fixed for ", id, "."))
  }
  file.path(root, prefix, id)
}

.acq_read_acquisition <- function(path) {
  object <- measure::read_measure_acquisition(path, max_bytes = MAX_HASH_BYTES)
  measure::validate_measure_acquisition(object)
  object
}

.acq_write_forward <- function(object, destination, delimiter) {
  measure::write_measure_acquisition(
    object,
    destination,
    delim = .acq_valid_delimiter(delimiter)
  )
  restored <- .acq_read_acquisition(destination)
  if (!identical(object, restored)) {
    .acq_abort(paste0(
      "Public acquisition roundtrip changed object bytes at ",
      destination,
      "."
    ))
  }
  restored
}

.acq_observe_bundle <- function(object, id, expected_observation = NULL) {
  observed <- observe_acquisition(object, id)
  if (!is.null(expected_observation)) {
    .acq_assert_observation_equal(
      observed,
      expected_observation,
      paste0("Acquisition observation ", id)
    )
  }
  observed
}

.acq_observation_json <- function(x) {
  as.character(jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  ))
}

.acq_assert_observation_equal <- function(actual, expected, label) {
  if (
    !identical(.acq_observation_json(actual), .acq_observation_json(expected))
  ) {
    .acq_abort(paste0(label, " did not match the frozen fixture."))
  }
  invisible(TRUE)
}

.acq_write_legacy_controls <- function(acquisitions, root) {
  destination <- file.path(root, "legacy-cross-format-controls")
  if (!dir.create(destination, recursive = FALSE, showWarnings = FALSE)) {
    .acq_abort("Could not create positive legacy cross-format controls.")
  }
  scalar_path <- file.path(destination, "scalar-record")
  scalar <- acquisitions[["ms-centroid"]]$record
  measure::write_measure_record(scalar, scalar_path, delim = ",")
  scalar_readback <- measure::read_measure_record(scalar_path)
  if (!identical(scalar, scalar_readback)) {
    .acq_abort(
      "Legacy scalar record writer and reader did not roundtrip exactly."
    )
  }

  nd_path <- file.path(destination, "nd-object")
  nd_source <- acquisitions[["dad-only-unequal"]]$arrays$.measures[[1L]]
  measure::write_measure_nd(nd_source, nd_path, delim = ",")
  nd_readback <- measure::read_measure_nd(nd_path)
  if (!identical(nd_source, nd_readback)) {
    .acq_abort("Legacy nD writer and reader did not roundtrip exactly.")
  }
  invisible(list(scalar_path = scalar_path, nd_path = nd_path))
}

.acq_run_produce <- function(output, data, installed, head, manifest_hash) {
  run <- new_run_state("produce", output)
  succeeded <- run_with_receipts(run, {
    run$stage <- "copy frozen inputs and capture runtime"
    write_run_state(run, FALSE)
    .acq_copy_frozen_inputs(HERE, output, data$inputs, manifest_hash)
    runtime_document <- capture_runtime(installed, head)
    runtime <- normalize_runtime(runtime_document)
    .acq_validate_runtime(runtime, "Producer runtime")
    write_json_file(runtime_document, file.path(output, "runtime.json"))

    run$stage <- "build fixture acquisitions"
    write_run_state(run, FALSE)
    acquisitions <- stats::setNames(
      vector("list", length(data$fixtures)),
      names(data$fixtures)
    )
    observations <- vector("list", length(data$fixtures))
    names(observations) <- names(data$fixtures)
    for (object_id in names(data$fixtures)) {
      fixture <- data$fixtures[[object_id]]
      acquisition <- build_acquisition(fixture, data$fixture_document, HERE)
      measure::validate_measure_acquisition(acquisition)
      acquisitions[[object_id]] <- acquisition
      observations[[object_id]] <- .acq_observe_bundle(acquisition, object_id)
    }
    observation_document <- list(
      profile = json_string(PROFILE),
      objects = unname(observations)
    )

    bundle_parent <- file.path(output, "bundles")
    if (!dir.create(bundle_parent, recursive = FALSE, showWarnings = FALSE)) {
      .acq_abort("Could not create the forward bundle parent.")
    }
    for (case in data$declaration$forward) {
      id <- json_field_string(case, "id")
      object_id <- json_field_string(case, "object_id")
      delimiter <- json_field_string(case, "delimiter")
      destination <- .acq_output_path(output, case, "bundles")
      start_case(run, "write and read forward bundle", id)
      restored <- .acq_write_forward(
        acquisitions[[object_id]],
        destination,
        delimiter
      )
      .acq_observe_bundle(restored, object_id, observations[[object_id]])
      record_case_complete(run, id)
    }

    trace_cases <- vector("list", length(data$fixture_document$traces))
    for (index in seq_along(data$fixture_document$traces)) {
      trace <- data$fixture_document$traces[[index]]
      id <- json_field_string(trace, "id")
      object_id <- json_field_string(trace, "object_id")
      start_case(run, "calculate declared trace", id)
      result <- do.call(
        measure::measure_ms_trace,
        trace_arguments(trace, acquisitions[[object_id]], HERE)
      )
      observed <- observe_trace(result)
      package_version <- observed$processing$package_version
      if (!identical(as.character(package_version), installed$version)) {
        .acq_abort(paste0(
          "Trace processing package version differs for ",
          id,
          "."
        ))
      }
      trace_cases[[index]] <- c(
        list(id = json_string(id), object_id = json_string(object_id)),
        observed
      )
      record_case_complete(run, id)
    }

    rejection_cases <- vector(
      "list",
      length(data$fixture_document$semantic_rejections)
    )
    for (index in seq_along(data$fixture_document$semantic_rejections)) {
      trace <- data$fixture_document$semantic_rejections[[index]]
      id <- json_field_string(trace, "id")
      object_id <- json_field_string(trace, "object_id")
      start_case(run, "check declared semantic rejection", id)
      result <- tryCatch(
        do.call(
          measure::measure_ms_trace,
          trace_arguments(trace, acquisitions[[object_id]], HERE)
        ),
        error = identity
      )
      if (!inherits(result, "error")) {
        .acq_abort(paste0(
          "Semantic rejection case unexpectedly succeeded: ",
          id
        ))
      }
      message <- conditionMessage(result)
      if (
        !is.character(message) ||
          length(message) != 1L ||
          !nzchar(trimws(message))
      ) {
        .acq_abort(paste0(
          "Semantic rejection case produced an empty error: ",
          id
        ))
      }
      rejection_cases[[index]] <- list(
        id = json_string(id),
        error = json_scalar(TRUE),
        message = json_string(message)
      )
      record_case_complete(run, id)
    }

    start_case(
      run,
      "positive legacy cross-format roundtrips",
      "legacy-controls"
    )
    .acq_write_legacy_controls(acquisitions, output)
    record_case_complete(run, "legacy-controls")

    write_json_file(
      observation_document,
      file.path(output, "observations.json")
    )
    write_json_file(
      list(profile = json_string(PROFILE), cases = trace_cases),
      file.path(output, "trace-results.json")
    )
    write_json_file(
      list(profile = json_string(PROFILE), cases = rejection_cases),
      file.path(output, "semantic-rejections.json")
    )
  })
  if (!succeeded) {
    quit(save = "no", status = 1L)
  }
  invisible(TRUE)
}

.acq_assert_complete_receipts <- function(output, data, inputs) {
  declaration <- data$declaration
  forward_ids <- vapply(
    declaration$forward,
    function(case) case$id,
    character(1)
  )
  trace_ids <- vapply(
    data$fixture_document$traces,
    function(case) case$id,
    character(1)
  )
  semantic_ids <- vapply(
    data$fixture_document$semantic_rejections,
    function(case) case$id,
    character(1)
  )
  controls_ids <- vapply(
    declaration$controls,
    function(case) case$id,
    character(1)
  )
  invalid_ids <- vapply(
    declaration$invalid,
    function(case) case$id,
    character(1)
  )
  .acq_check_phase_status(
    output,
    "produce",
    "run-status.json",
    "progress.json",
    c(forward_ids, trace_ids, semantic_ids, "legacy-controls")
  )
  .acq_check_phase_status(
    output,
    "mutate",
    "mutation-status.json",
    "mutation-progress.json",
    c(controls_ids, invalid_ids, "mutations.json")
  )
  .acq_verify_frozen_copies(
    HERE,
    output,
    inputs,
    sha256_file(
      file.path(HERE, "input-manifest.json"),
      "input-manifest.json"
    )
  )

  marker_manifest_hash <- sha256_file(
    file.path(HERE, "input-manifest.json"),
    "input-manifest.json"
  )
  marker <- read_json_file(
    file.path(output, "qualification-root.json"),
    "qualification-root.json",
    128 * 1024
  )
  .acq_fields(
    marker,
    c("format", "declaration_sha256", "input_manifest_sha256"),
    "Qualification marker"
  )
  if (
    !identical(json_field_string(marker, "format"), PROFILE) ||
      !identical(
        json_field_string(marker, "declaration_sha256"),
        inputs$hashes[["declaration.json"]]
      ) ||
      !identical(
        json_field_string(marker, "input_manifest_sha256"),
        marker_manifest_hash
      )
  ) {
    .acq_abort("Qualification marker does not match the frozen input hashes.")
  }
  invisible(TRUE)
}

.acq_verify_forward_bundle_hashes <- function(
  output,
  declaration,
  receipt,
  wire
) {
  maps <- receipt$forward_bundle_files_sha256
  ids <- vapply(declaration$forward, function(case) case$id, character(1))
  if (
    !is.list(maps) ||
      is.null(names(maps)) ||
      anyDuplicated(names(maps)) > 0L ||
      !setequal(names(maps), ids) ||
      length(maps) != length(ids)
  ) {
    .acq_abort("Python forward receipt has an invalid forward bundle hash map.")
  }
  for (case in declaration$forward) {
    path <- .acq_output_path(output, case, "bundles")
    expected_names <- .acq_fixed_member_candidates(wire)
    ext <- paste0(".", json_field_string(case, "delimiter"))
    selected <- c("manifest.dcf", paste0(.acq_table_stems(wire), ext))
    actual <- .acq_bundle_file_hashes(path, wire)
    if (
      !setequal(names(actual), selected) || length(actual) != length(selected)
    ) {
      .acq_abort(paste0(
        "Forward bundle has a missing or extra selected member: ",
        case$id
      ))
    }
    .acq_assert_hash_map(
      maps[[case$id]],
      actual,
      paste0("Forward bundle hashes ", case$id)
    )
  }
  invisible(TRUE)
}

.acq_validate_python_receipt <- function(output, data, inputs, runtime) {
  receipt <- .acq_read_receipt(
    output,
    "independent-python-receipt.json",
    4 * 1024^2
  )
  .acq_fields(
    receipt,
    c(
      "profile",
      "input_files_sha256",
      "source_artifacts_sha256",
      "installed_package",
      "forward_bundle_files_sha256",
      "objects",
      "forward_bundles",
      "trace_cases",
      "semantic_rejections",
      "verified_utc"
    ),
    "Python forward receipt"
  )
  if (!identical(json_field_string(receipt, "profile"), PROFILE)) {
    .acq_abort("Python forward receipt has the wrong profile.")
  }
  .acq_assert_hash_map(
    receipt$input_files_sha256,
    input_hash_json(inputs$hashes),
    "Python input hashes"
  )
  .acq_assert_artifact_hashes(
    output,
    receipt$source_artifacts_sha256,
    PRODUCER_RECEIPTS,
    "Python producer source hashes"
  )
  assert_json_equal(
    receipt$installed_package,
    runtime$installed_package,
    "Python installed package identity"
  )
  expected_counts <- c(
    objects = EXPECTED_OBJECTS,
    forward_bundles = EXPECTED_FORWARD,
    trace_cases = EXPECTED_TRACES,
    semantic_rejections = EXPECTED_SEMANTIC_REJECTIONS
  )
  for (name in names(expected_counts)) {
    if (
      json_field_integer(receipt, name, paste0("Python receipt ", name)) !=
        expected_counts[[name]]
    ) {
      .acq_abort(paste0(
        "Python forward receipt has the wrong ",
        name,
        " count."
      ))
    }
  }
  .acq_nonblank_string(receipt, "verified_utc", "Python verification time")
  .acq_verify_forward_bundle_hashes(
    output,
    data$declaration,
    receipt,
    data$wire
  )
  receipt
}

.acq_validate_mutations <- function(output, data, inputs, python_receipt) {
  .acq_check_phase_status(
    output,
    "mutate",
    "mutation-status.json",
    "mutation-progress.json",
    c(
      vapply(data$declaration$controls, function(case) case$id, character(1)),
      vapply(data$declaration$invalid, function(case) case$id, character(1)),
      "mutations.json"
    )
  )
  mutations <- .acq_read_receipt(output, "mutations.json", 16 * 1024^2)
  .acq_fields(
    mutations,
    c(
      "profile",
      "input_files_sha256",
      "source_artifacts_sha256",
      "invalid_bundles",
      "valid_bundles"
    ),
    "Mutation receipt"
  )
  if (!identical(json_field_string(mutations, "profile"), PROFILE)) {
    .acq_abort("Mutation receipt has the wrong profile.")
  }
  .acq_assert_hash_map(
    mutations$input_files_sha256,
    input_hash_json(inputs$hashes),
    "Mutation input hashes"
  )
  .acq_assert_artifact_hashes(
    output,
    mutations$source_artifacts_sha256,
    MUTATION_RECEIPTS,
    "Mutation source artifact hashes"
  )
  .acq_assert_artifact_hashes(
    output,
    python_receipt$source_artifacts_sha256,
    PRODUCER_RECEIPTS,
    "Python-bound producer artifact hashes"
  )

  if (
    !is.list(mutations$invalid_bundles) ||
      length(mutations$invalid_bundles) != length(data$declaration$invalid)
  ) {
    .acq_abort("Mutation receipt has the wrong invalid-bundle count.")
  }
  for (index in seq_along(data$declaration$invalid)) {
    spec <- data$declaration$invalid[[index]]
    row <- mutations$invalid_bundles[[index]]
    .acq_fields(
      row,
      c("id", "source_case_id", "path", "mutation", "files_sha256"),
      "Invalid mutation receipt"
    )
    for (field in c("id", "source_case_id", "path", "mutation")) {
      if (!identical(as.character(row[[field]]), as.character(spec[[field]]))) {
        .acq_abort(paste0(
          "Mutation receipt differs from the frozen descriptor ",
          spec$id,
          "."
        ))
      }
    }
    path <- .acq_output_path(output, spec, "mutants")
    actual <- .acq_bundle_file_hashes(path, data$wire)
    .acq_assert_hash_map(
      row$files_sha256,
      actual,
      paste0("Mutant member hashes ", spec$id)
    )
  }

  if (
    !is.list(mutations$valid_bundles) ||
      length(mutations$valid_bundles) != length(data$declaration$controls)
  ) {
    .acq_abort("Mutation receipt has the wrong valid-control count.")
  }
  for (index in seq_along(data$declaration$controls)) {
    spec <- data$declaration$controls[[index]]
    row <- mutations$valid_bundles[[index]]
    .acq_fields(
      row,
      c("id", "object_id", "kind", "path", "delimiter", "files_sha256"),
      "Valid control receipt"
    )
    for (field in c("id", "object_id", "kind", "path", "delimiter")) {
      if (!identical(as.character(row[[field]]), as.character(spec[[field]]))) {
        .acq_abort(paste0(
          "Mutation receipt differs from the frozen control ",
          spec$id,
          "."
        ))
      }
    }
    path <- .acq_output_path(output, spec, "controls")
    actual <- .acq_bundle_file_hashes(path, data$wire)
    .acq_assert_hash_map(
      row$files_sha256,
      actual,
      paste0("Control member hashes ", spec$id)
    )
  }
  mutations
}

.acq_validate_producer_documents <- function(output, data, runtime) {
  observations <- .acq_observation_map(
    .acq_read_receipt(output, "observations.json", MAX_HASH_BYTES),
    data
  )
  traces <- .acq_read_receipt(output, "trace-results.json", 16 * 1024^2)
  .acq_fields(traces, c("profile", "cases"), "Trace result document")
  if (
    !identical(json_field_string(traces, "profile"), PROFILE) ||
      !is.list(traces$cases) ||
      length(traces$cases) != EXPECTED_TRACES
  ) {
    .acq_abort("Trace result document is incomplete or has the wrong profile.")
  }
  trace_ids <- vapply(
    data$fixture_document$traces,
    function(trace) trace$id,
    character(1)
  )
  for (index in seq_along(traces$cases)) {
    case <- traces$cases[[index]]
    .acq_fields(
      case,
      c("id", "object_id", "rows", "processing"),
      "Trace result case"
    )
    trace <- data$fixture_document$traces[[index]]
    if (
      !identical(json_field_string(case, "id"), trace_ids[[index]]) ||
        !identical(json_field_string(case, "object_id"), trace$object_id)
    ) {
      .acq_abort("Trace result order differs from the frozen fixture.")
    }
    processing <- case$processing
    .acq_nonblank_string(
      processing,
      "package_version",
      paste0("Trace ", trace_ids[[index]], " package version")
    )
    if (
      !identical(processing$package_version, runtime$installed_package$version)
    ) {
      .acq_abort(paste0(
        "Trace package version differs for ",
        trace_ids[[index]],
        "."
      ))
    }
  }
  semantic <- .acq_read_receipt(output, "semantic-rejections.json", 2 * 1024^2)
  .acq_fields(semantic, c("profile", "cases"), "Semantic rejection document")
  if (
    !identical(json_field_string(semantic, "profile"), PROFILE) ||
      !is.list(semantic$cases) ||
      length(semantic$cases) != EXPECTED_SEMANTIC_REJECTIONS
  ) {
    .acq_abort(
      "Semantic rejection document is incomplete or has the wrong profile."
    )
  }
  for (index in seq_along(semantic$cases)) {
    case <- semantic$cases[[index]]
    .acq_fields(case, c("id", "error", "message"), "Semantic rejection case")
    expected_id <- data$fixture_document$semantic_rejections[[index]]$id
    if (
      !identical(json_field_string(case, "id"), expected_id) ||
        !isTRUE(case$error) ||
        !nzchar(trimws(.acq_nonblank_string(case, "message", expected_id)))
    ) {
      .acq_abort(paste0(
        "Semantic rejection receipt is invalid for ",
        expected_id,
        "."
      ))
    }
  }
  list(observations = observations, traces = traces, semantic = semantic)
}

.acq_assert_rejection_files_absent <- function(output) {
  names <- c(
    "rejections.json",
    "rejection-runtime.json",
    "rejection-status.json",
    "rejection-progress.json"
  )
  present <- names[vapply(
    names,
    function(name) is_path_entry(file.path(output, name)),
    logical(1)
  )]
  if (length(present) > 0L) {
    .acq_abort(paste0(
      "Refusing to overwrite existing rejection output: ",
      paste(present, collapse = ", "),
      "."
    ))
  }
  invisible(TRUE)
}

.acq_positive_legacy_control_paths <- function(output) {
  list(
    scalar = file.path(output, "legacy-cross-format-controls", "scalar-record"),
    nd = file.path(output, "legacy-cross-format-controls", "nd-object")
  )
}

.acq_verify_legacy_controls <- function(output, data) {
  paths <- .acq_positive_legacy_control_paths(output)
  assert_directory(
    dirname(paths$scalar),
    "Legacy cross-format control directory"
  )
  if (
    !identical(path_type(paths$scalar), "directory") ||
      !identical(path_type(paths$nd), "directory")
  ) {
    .acq_abort(
      "Legacy scalar and nD positive controls must be real directories."
    )
  }
  expected_scalar <- build_record(
    data$fixture_document$record_templates$base,
    HERE
  )
  scalar <- measure::read_measure_record(paths$scalar)
  if (!identical(scalar, expected_scalar)) {
    .acq_abort(
      "Legacy scalar positive control does not roundtrip the fixture record."
    )
  }
  expected_nd <- build_acquisition(
    data$fixtures[["dad-only-unequal"]],
    data$fixture_document,
    HERE
  )$arrays$.measures[[1L]]
  nd <- measure::read_measure_nd(paths$nd)
  if (!identical(nd, expected_nd)) {
    .acq_abort(
      "Legacy nD positive control does not roundtrip the fixture array."
    )
  }
  paths
}

.acq_preflight_reject <- function(
  output,
  data,
  inputs,
  installed,
  head,
  manifest_hash
) {
  assert_directory(output, "Qualification output")
  output <- normalizePath(output, winslash = "/", mustWork = TRUE)
  .acq_assert_rejection_files_absent(output)
  .acq_assert_complete_receipts(output, data, inputs)
  runtime <- .acq_read_receipt(output, "runtime.json", 2 * 1024^2)
  .acq_validate_runtime(runtime, "Producer runtime")
  rejection_runtime_document <- capture_runtime(installed, head)
  rejection_runtime <- normalize_runtime(rejection_runtime_document)
  .acq_runtime_consistent(runtime, rejection_runtime)
  python_receipt <- .acq_validate_python_receipt(output, data, inputs, runtime)
  mutations <- .acq_validate_mutations(output, data, inputs, python_receipt)
  documents <- .acq_validate_producer_documents(output, data, runtime)

  # Re-read every original acquisition while all Python/R source hashes are fixed.
  for (case in data$declaration$forward) {
    id <- json_field_string(case, "id")
    object_id <- json_field_string(case, "object_id")
    path <- .acq_output_path(output, case, "bundles")
    bundle <- .acq_read_acquisition(path)
    .acq_observe_bundle(bundle, object_id, documents$observations[[object_id]])
  }

  invisible(list(
    output = output,
    runtime = runtime,
    rejection_runtime = rejection_runtime_document,
    python_receipt = python_receipt,
    mutations = mutations,
    documents = documents
  ))
}

.acq_expected_error <- function(expression, label) {
  result <- tryCatch(force(expression), error = identity)
  if (!inherits(result, "error")) {
    .acq_abort(paste0("Reader unexpectedly accepted ", label, "."))
  }
  message <- conditionMessage(result)
  if (
    !is.character(message) || length(message) != 1L || !nzchar(trimws(message))
  ) {
    .acq_abort(paste0("Reader produced an empty rejection for ", label, "."))
  }
  list(error = json_scalar(TRUE), message = json_string(message))
}

.acq_read_cross_format <- function(reader, path) {
  if (identical(reader, "acquisition")) {
    return(measure::read_measure_acquisition(path, max_bytes = MAX_HASH_BYTES))
  }
  if (identical(reader, "scalar-record")) {
    return(measure::read_measure_record(path))
  }
  if (identical(reader, "nd-object")) {
    return(measure::read_measure_nd(path))
  }
  .acq_abort("Unsupported cross-format reader selector.")
}

.acq_run_reject <- function(preflight, data, installed) {
  output <- preflight$output
  run <- new_run_state("reject", output)
  succeeded <- run_with_receipts(run, {
    runtime_document <- preflight$rejection_runtime
    runtime <- normalize_runtime(runtime_document)
    .acq_validate_runtime(runtime, "Rejection runtime")
    write_json_file(
      runtime_document,
      file.path(output, "rejection-runtime.json")
    )
    observation_map <- preflight$documents$observations
    results <- list(
      profile = json_string(PROFILE),
      invalid = vector("list", length(data$declaration$invalid)),
      controls = vector("list", length(data$declaration$controls)),
      cross_format = vector("list", length(data$declaration$cross_format))
    )

    for (index in seq_along(data$declaration$controls)) {
      case <- data$declaration$controls[[index]]
      id <- json_field_string(case, "id")
      object_id <- json_field_string(case, "object_id")
      start_case(run, "read positive acquisition control", id)
      path <- .acq_output_path(output, case, "controls")
      observed <- .acq_observe_bundle(.acq_read_acquisition(path), object_id)
      .acq_assert_observation_equal(
        observed,
        observation_map[[object_id]],
        paste0("Positive control ", id)
      )
      results$controls[[index]] <- list(
        id = json_string(id),
        object_id = json_string(object_id),
        kind = json_string(json_field_string(case, "kind")),
        observed = observed
      )
      record_case_complete(run, id)
    }

    for (index in seq_along(data$declaration$invalid)) {
      case <- data$declaration$invalid[[index]]
      id <- json_field_string(case, "id")
      start_case(run, "reject fixed invalid acquisition bundle", id)
      path <- .acq_output_path(output, case, "mutants")
      outcome <- .acq_expected_error(
        measure::read_measure_acquisition(path, max_bytes = MAX_HASH_BYTES),
        paste0("mutated bundle ", id)
      )
      results$invalid[[index]] <- c(list(id = json_string(id)), outcome)
      record_case_complete(run, id)
    }

    legacy_paths <- .acq_verify_legacy_controls(output, data)
    for (index in seq_along(data$declaration$cross_format)) {
      case <- data$declaration$cross_format[[index]]
      id <- json_field_string(case, "id")
      reader <- json_field_string(case, "reader")
      start_case(run, "reject cross-format bundle", id)
      if ("source_kind" %in% names(case)) {
        path <- if (
          identical(json_field_string(case, "source_kind"), "scalar-record")
        ) {
          legacy_paths$scalar
        } else {
          legacy_paths$nd
        }
      } else {
        source_case <- json_field_string(case, "source_case_id")
        forward <- data$declaration$forward[[match(
          source_case,
          vapply(
            data$declaration$forward,
            function(value) value$id,
            character(1)
          )
        )]]
        path <- .acq_output_path(output, forward, "bundles")
      }
      outcome <- .acq_expected_error(
        .acq_read_cross_format(reader, path),
        paste0("cross-format case ", id)
      )
      results$cross_format[[index]] <- c(
        list(id = json_string(id)),
        outcome
      )
      record_case_complete(run, id)
    }

    write_json_file(results, file.path(output, "rejections.json"))
  })
  if (!succeeded) {
    quit(save = "no", status = 1L)
  }
  invisible(TRUE)
}

main <- function() {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) != 2L || !arguments[[1L]] %in% c("produce", "reject")) {
    .acq_abort(paste0(
      "Usage: Rscript qualify.R produce OUTPUT\n",
      "   or: Rscript qualify.R reject OUTPUT"
    ))
  }
  phase <- arguments[[1L]]
  output_argument <- arguments[[2L]]
  installed <- .acq_require_function_exports()
  head <- tool_checkout_head(package_root_from_script(script_path))
  inputs <- verify_fixed_inputs(HERE)
  input_manifest_state <- .acq_assert_input_manifest(HERE, inputs)
  data <- .acq_read_inputs(inputs)

  if (identical(phase, "produce")) {
    output <- reserve_output_root(output_argument)
    .acq_run_produce(
      output,
      data,
      installed,
      head,
      input_manifest_state$sha256
    )
  } else {
    output <- path.expand(output_argument)
    if (is_path_entry(output) && !identical(path_type(output), "directory")) {
      .acq_abort(
        "Reject output must be a real existing qualification directory."
      )
    }
    preflight <- .acq_preflight_reject(
      output,
      data,
      inputs,
      installed,
      head,
      input_manifest_state$sha256
    )
    .acq_run_reject(preflight, data, installed)
  }
  invisible(TRUE)
}

main()
