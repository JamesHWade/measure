#!/usr/bin/env Rscript

qualification_sha256 <- c(
  "fixtures.json" = "c5692fb3cc7cc69ad746046408779b07d5bed01d11a57d895ad33e648ad7115b",
  "declaration.json" = "036bad1ac89c7c759de2b6ae8fc2430c0e0052bf80a92c77f196c2b2da0257ef"
)

json_scalar <- function(x) {
  jsonlite::unbox(x)
}

json_array <- function(x, encode = json_scalar) {
  lapply(as.list(x), encode)
}

json_metadata <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  lapply(seq_along(x), function(i) {
    value <- x[[i]]
    if (is.null(value) || length(value) == 0L || is.na(value)) {
      NULL
    } else {
      json_scalar(as.character(value))
    }
  })
}

json_value <- function(x) {
  if (length(x) != 1L || !is.numeric(x)) {
    stop("A measurement value was not a numeric scalar.", call. = FALSE)
  }
  if (is.nan(x)) {
    return(list(state = json_scalar("nan"), number = NULL))
  }
  if (is.na(x)) {
    return(list(state = json_scalar("na"), number = NULL))
  }
  if (is.infinite(x)) {
    state <- if (x > 0) "+inf" else "-inf"
    return(list(state = json_scalar(state), number = NULL))
  }
  list(state = json_scalar("number"), number = json_scalar(as.numeric(x)))
}

result_json <- function(x) {
  if (inherits(x, "measure_nd_tbl")) {
    ndim <- measure::measure_ndim(x)
    location_cols <- paste0("location_", seq_len(ndim))
    rows <- lapply(seq_len(nrow(x)), function(i) {
      coordinates <- lapply(location_cols, function(column) {
        json_scalar(as.numeric(x[[column]][[i]]))
      })
      list(
        coordinates = coordinates,
        value = json_value(x$value[[i]])
      )
    })

    return(list(
      kind = json_scalar("nd"),
      ndim = json_scalar(as.integer(ndim)),
      dim_names = json_metadata(measure::measure_dim_names(x)),
      dim_units = json_metadata(measure::measure_dim_units(x)),
      rows = rows
    ))
  }

  if (inherits(x, "measure_tbl")) {
    rows <- lapply(seq_len(nrow(x)), function(i) {
      list(
        coordinates = list(json_scalar(as.numeric(x$location[[i]]))),
        value = json_value(x$value[[i]])
      )
    })

    return(list(
      kind = json_scalar("1d"),
      ndim = json_scalar(1L),
      dim_names = NULL,
      dim_units = NULL,
      rows = rows
    ))
  }

  if (is.numeric(x) && length(x) == 1L) {
    return(list(
      kind = json_scalar("scalar"),
      ndim = json_scalar(0L),
      dim_names = NULL,
      dim_units = NULL,
      rows = list(list(
        coordinates = list(),
        value = json_value(x)
      ))
    ))
  }

  stop(
    "Unsupported measurement result class: ",
    paste(class(x), collapse = "/"),
    call. = FALSE
  )
}

number_from_json <- function(x) {
  if (is.null(x)) {
    return(NA_real_)
  }
  as.numeric(x)
}

character_from_json <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  as.character(x)
}

fixture_from_json <- function(fixture) {
  ndim <- as.integer(fixture$ndim)
  rows <- fixture$rows
  n <- length(rows)
  locations <- lapply(seq_len(ndim), function(dimension) {
    if (n == 0L) {
      return(double())
    }
    vapply(
      rows,
      function(row) number_from_json(row[[dimension]]),
      numeric(1)
    )
  })
  names(locations) <- paste0("location_", seq_len(ndim))

  values <- if (n == 0L) {
    double()
  } else {
    vapply(
      rows,
      function(row) number_from_json(row[[ndim + 1L]]),
      numeric(1)
    )
  }

  dim_names <- if (is.null(fixture$dim_names)) {
    NULL
  } else {
    vapply(
      fixture$dim_names,
      character_from_json,
      character(1),
      USE.NAMES = FALSE
    )
  }
  dim_units <- if (is.null(fixture$dim_units)) {
    NULL
  } else {
    vapply(
      fixture$dim_units,
      character_from_json,
      character(1),
      USE.NAMES = FALSE
    )
  }

  do.call(
    measure::new_measure_nd_tbl,
    c(
      locations,
      list(
        value = values,
        dim_names = dim_names,
        dim_units = dim_units
      )
    )
  )
}

resolve_input_files <- function(script_directory) {
  candidates <- list(
    file.path(script_directory, "qualification-input"),
    script_directory
  )
  complete <- vapply(
    candidates,
    function(directory) {
      all(file.exists(file.path(
        directory,
        c("fixtures.json", "declaration.json")
      )))
    },
    logical(1)
  )

  if (!any(complete)) {
    stop(
      "Could not find fixtures.json and declaration.json beside the script ",
      "or in its qualification-input/ directory.",
      call. = FALSE
    )
  }

  selected <- candidates[[which(complete)[[1L]]]]
  stats::setNames(
    file.path(selected, names(qualification_sha256)),
    names(qualification_sha256)
  )
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

installed_runtime <- function() {
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
  if (!file.exists(file.path(namespace_path, "Meta", "package.rds"))) {
    stop(
      "The measure namespace is not an installed package; source-loaded ",
      "packages are not accepted.",
      call. = FALSE
    )
  }

  list(
    path = namespace_path,
    namespace_path = namespace_path,
    version = as.character(utils::packageVersion("measure"))
  )
}

checkout_head <- function(package_root) {
  git_entry <- file.path(package_root, ".git")
  if (!file.exists(git_entry) && !dir.exists(git_entry)) {
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
  if (length(output) != 1L || !grepl("^([0-9a-f]{40}|[0-9a-f]{64})$", output)) {
    return(NULL)
  }
  output
}

dependency_versions <- function() {
  packages <- c(
    "measure",
    "recipes",
    "dplyr",
    "tibble",
    "vctrs",
    "jsonlite",
    "digest"
  )
  versions <- lapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      return(NULL)
    }
    json_scalar(as.character(utils::packageVersion(package)))
  })
  names(versions) <- packages
  versions
}

write_json_file <- function(x, path) {
  json <- jsonlite::toJSON(
    x,
    auto_unbox = FALSE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = TRUE
  )
  writeLines(as.character(json), con = path, useBytes = TRUE)
  invisible(path)
}

run_qualification <- function() {
  trailing <- commandArgs(trailingOnly = TRUE)
  if (length(trailing) != 1L || !nzchar(trailing[[1L]])) {
    stop(
      "Usage: Rscript tools/nd-qualification/qualify.R OUTPUT_DIRECTORY",
      call. = FALSE
    )
  }

  script_arg <- grepv("^--file=", commandArgs(trailingOnly = FALSE))
  if (length(script_arg) != 1L) {
    stop("Could not determine the qualification script path.", call. = FALSE)
  }
  script_path <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    winslash = "/",
    mustWork = TRUE
  )
  script_directory <- dirname(script_path)
  package_root <- dirname(dirname(script_directory))

  output <- normalizePath(
    path.expand(trailing[[1L]]),
    winslash = "/",
    mustWork = FALSE
  )
  link_target <- Sys.readlink(output)
  has_link <- length(link_target) == 1L &&
    !is.na(link_target) &&
    nzchar(link_target)
  if (
    file.exists(output) ||
      dir.exists(output) ||
      has_link
  ) {
    stop(
      "Output root already exists; choose a new, non-existing directory: ",
      output,
      call. = FALSE
    )
  }
  if (!dir.exists(dirname(output))) {
    stop(
      "The parent directory for the output root must already exist: ",
      dirname(output),
      call. = FALSE
    )
  }
  if (!dir.create(output, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not create output root: ", output, call. = FALSE)
  }

  run <- new.env(parent = emptyenv())
  run$stage <- "initialize"
  run$failure_message <- NULL
  run$completed <- FALSE
  progress_path <- file.path(output, "qualification-progress.json")
  status_path <- file.path(output, "run-status.json")
  run$state <- NULL

  write_status <- function(is_complete, error = NULL) {
    write_json_file(
      list(
        complete = json_scalar(is_complete),
        stage = json_scalar(run$stage),
        error = if (is.null(error)) NULL else json_scalar(error),
        updated_utc = json_scalar(format(
          Sys.time(),
          tz = "UTC",
          usetz = TRUE
        ))
      ),
      status_path
    )
  }
  write_progress <- function() {
    write_json_file(
      list(
        complete = json_scalar(FALSE),
        stage = json_scalar(run$stage),
        error = if (is.null(run$failure_message)) {
          NULL
        } else {
          json_scalar(run$failure_message)
        },
        results = run$state
      ),
      progress_path
    )
  }

  write_status(FALSE)
  on.exit(
    {
      if (!run$completed) {
        try(write_progress(), silent = TRUE)
        try(write_status(FALSE, run$failure_message), silent = TRUE)
      }
    },
    add = TRUE
  )

  run_body <- function() {
    run$stage <- "input verification"
    input_files <- resolve_input_files(script_directory)
    actual_hashes <- vapply(input_files, sha256_file, character(1))
    if (!identical(unname(actual_hashes), unname(qualification_sha256))) {
      mismatch <- names(qualification_sha256)[
        actual_hashes != qualification_sha256
      ]
      stop(
        "Frozen input SHA256 mismatch for: ",
        paste(mismatch, collapse = ", "),
        call. = FALSE
      )
    }

    run$stage <- "installed runtime verification"
    installed <- installed_runtime()
    fixture_doc <- jsonlite::fromJSON(
      input_files[["fixtures.json"]],
      simplifyVector = FALSE
    )
    declaration <- jsonlite::fromJSON(
      input_files[["declaration.json"]],
      simplifyVector = FALSE
    )
    fixture_ids <- vapply(
      fixture_doc$fixtures,
      function(fixture) as.character(fixture$id),
      character(1)
    )
    fixtures <- stats::setNames(
      lapply(fixture_doc$fixtures, fixture_from_json),
      fixture_ids
    )

    head <- checkout_head(package_root)
    run$result <- list(
      profile = json_scalar("measure-nd-operations/1"),
      input_files_sha256 = list(
        "fixtures.json" = json_scalar(actual_hashes[["fixtures.json"]]),
        "declaration.json" = json_scalar(actual_hashes[["declaration.json"]])
      ),
      installed_measure = list(
        path = json_scalar(installed$path),
        namespace_path = json_scalar(installed$namespace_path),
        version = json_scalar(installed$version)
      ),
      R = json_scalar(R.version.string),
      tool_checkout_head = if (is.null(head)) {
        NULL
      } else {
        json_scalar(head)
      },
      fold = list(),
      slices = list(),
      projections = list(),
      invalid_cases = list(),
      list_selection = NULL,
      warnings = list()
    )
    run$state <- run$result

    capture_warnings <- function(id, expression) {
      withCallingHandlers(
        expression(),
        warning = function(warning) {
          run$result$warnings[[length(run$result$warnings) + 1L]] <- list(
            id = json_scalar(id),
            message = json_scalar(conditionMessage(warning))
          )
          invokeRestart("muffleWarning")
        }
      )
    }
    save_progress <- function() {
      run$state <- run$result
      write_progress()
    }
    make_list <- function(ids) {
      measure::new_measure_nd_list(unname(lapply(ids, function(id) {
        fixtures[[id]]
      })))
    }

    run$stage <- "fold qualification"
    for (cohort_index in seq_along(declaration$cohorts)) {
      cohort <- vapply(
        declaration$cohorts[[cohort_index]],
        as.character,
        character(1)
      )
      orders <- declaration$fold_orders[[as.character(
        measure::measure_ndim(fixtures[[cohort[[1L]]]])
      )]]
      for (order_json in orders) {
        order <- vapply(order_json, as.integer, integer(1))
        for (variant in declaration$fold_variants) {
          variant <- as.character(variant)
          variant_id <- paste0(
            "fold-",
            cohort_index,
            "-",
            paste(order, collapse = ""),
            "-",
            variant
          )
          unfolded <- capture_warnings(variant_id, function() {
            measure::measure_unfold(make_list(cohort), order = order)
          })
          member_ids <- cohort

          if (identical(variant, "reverse_point_rows")) {
            unfolded <- measure::new_measure_list(lapply(
              unfolded,
              function(member) {
                member$location <- rev(member$location)
                member$value <- rev(member$value)
                member
              }
            ))
          } else if (identical(variant, "edited_values")) {
            unfolded <- measure::new_measure_list(lapply(
              unfolded,
              function(member) {
                member$value <- 2 * member$value - 1
                member
              }
            ))
          } else if (identical(variant, "reverse_list")) {
            unfolded <- unfolded[rev(seq_along(unfolded))]
            member_ids <- rev(member_ids)
          } else if (identical(variant, "first_member_subset")) {
            unfolded <- unfolded[1L]
            member_ids <- member_ids[1L]
          } else if (identical(variant, "serialized_roundtrip")) {
            unfolded <- unserialize(serialize(unfolded, NULL, version = 3L))
          } else if (!identical(variant, "identity")) {
            stop(
              "Unknown fold variant in declaration: ",
              variant,
              call. = FALSE
            )
          }

          folded <- capture_warnings(variant_id, function() {
            measure::measure_fold(unfolded)
          })
          if (!inherits(folded, "measure_nd_list")) {
            stop(
              "Fold variant returned an unexpected class: ",
              variant_id,
              call. = FALSE
            )
          }
          if (length(folded) != length(member_ids)) {
            stop(
              "Fold variant returned a different member count: ",
              variant_id,
              call. = FALSE
            )
          }
          members <- lapply(seq_along(member_ids), function(i) {
            list(
              fixture_id = json_scalar(member_ids[[i]]),
              result = result_json(folded[[i]])
            )
          })
          run$result$fold[[length(run$result$fold) + 1L]] <- list(
            cohort = json_scalar(as.integer(cohort_index)),
            order = json_array(order, function(value) {
              json_scalar(as.integer(value))
            }),
            variant = json_scalar(variant),
            members = members
          )
          save_progress()
        }
      }
    }

    selector_arguments <- function(selectors) {
      args <- list()
      for (selector in selectors) {
        dimension <- as.integer(selector$dimension)
        name <- paste0("dim_", dimension)
        if (!is.null(selector$values)) {
          args[[name]] <- vapply(selector$values, as.numeric, numeric(1))
        } else if (!is.null(selector$between)) {
          bounds <- vapply(selector$between, as.numeric, numeric(1))
          args[[name]] <- local({
            lower <- bounds[[1L]]
            upper <- bounds[[2L]]
            function(coordinate) coordinate >= lower & coordinate <= upper
          })
        } else {
          stop(
            "Slice declaration has no values or between selector.",
            call. = FALSE
          )
        }
      }
      args
    }

    run$stage <- "slice qualification"
    for (entry in declaration$slices) {
      id <- as.character(entry$id)
      args <- c(
        list(x = fixtures[[as.character(entry$fixture)]]),
        selector_arguments(entry$selectors),
        list(drop = isTRUE(entry$drop))
      )
      sliced <- capture_warnings(id, function() {
        do.call(measure::measure_slice, args)
      })
      run$result$slices[[length(run$result$slices) + 1L]] <- list(
        id = json_scalar(id),
        result = result_json(sliced)
      )
      save_progress()
    }

    run$stage <- "projection qualification"
    for (entry in declaration$projections) {
      id <- as.character(entry$id)
      fn_name <- as.character(entry$fn)
      fn <- switch(
        fn_name,
        sum = base::sum,
        mean = base::mean,
        stop(
          "Unsupported projection summary in declaration: ",
          fn_name,
          call. = FALSE
        )
      )
      along <- vapply(entry$along, as.integer, integer(1))
      projected <- capture_warnings(id, function() {
        measure::measure_project(
          fixtures[[as.character(entry$fixture)]],
          along = along,
          fn = fn,
          na_rm = isTRUE(entry$na_rm)
        )
      })
      run$result$projections[[length(run$result$projections) + 1L]] <- list(
        id = json_scalar(id),
        result = result_json(projected)
      )
      save_progress()
    }

    run$stage <- "invalid input rejection qualification"
    dad <- fixtures[["dad-a"]]
    dad_pair <- make_list(c("dad-a", "dad-b"))
    cube_pair <- make_list(c("cube-a", "cube-b"))
    invalid_calls <- list(
      "slice-unnamed" = function() {
        do.call(measure::measure_slice, list(dad, 254))
      },
      "slice-alias-duplicate" = function() {
        args <- list(dad)
        args$dim_1 <- 0.5
        args$retention_time <- 0.5
        do.call(measure::measure_slice, args)
      },
      "slice-short-mask" = function() {
        predicate <- function(coordinate) rep(TRUE, 2L)
        measure::measure_slice(dad, dim_1 = predicate)
      },
      "slice-missing-mask" = function() {
        predicate <- function(coordinate) {
          mask <- rep(TRUE, length(coordinate))
          if (length(mask) > 0L) {
            mask[[1L]] <- NA
          }
          mask
        }
        measure::measure_slice(dad, dim_1 = predicate)
      },
      "slice-matrix-mask" = function() {
        predicate <- function(coordinate) {
          matrix(TRUE, nrow = length(coordinate), ncol = 1L)
        }
        measure::measure_slice(dad, dim_1 = predicate)
      },
      "slice-invalid-drop" = function() {
        measure::measure_slice(dad, dim_1 = 0.5, drop = NA)
      },
      "project-fractional-axis" = function() {
        measure::measure_project(dad, along = 1.5)
      },
      "project-duplicate-axis" = function() {
        measure::measure_project(dad, along = c(1L, 1L))
      },
      "project-empty-axis" = function() {
        measure::measure_project(dad, along = integer())
      },
      "project-missing-axis" = function() {
        measure::measure_project(dad, along = NA_real_)
      },
      "project-invalid-na-rm" = function() {
        measure::measure_project(dad, along = 2L, na_rm = NA)
      },
      "project-nonscalar-summary" = function() {
        measure::measure_project(
          dad,
          along = 2L,
          fn = function(x) c(1, 2)
        )
      },
      "mixed-slice-output" = function() {
        measure::measure_slice(
          dad_pair,
          wavelength = 254,
          drop = TRUE
        )
      },
      "mixed-projection-axes" = function() {
        measure::measure_project(cube_pair, along = 1L)
      },
      "fold-partial-metadata" = function() {
        unfolded <- measure::measure_unfold(dad_pair)
        missing <- unfolded[[2L]]
        attr(missing, "fold_info") <- NULL
        unfolded[[2L]] <- missing
        measure::measure_fold(unfolded)
      },
      "fold-duplicate-index" = function() {
        unfolded <- measure::measure_unfold(dad)
        unfolded$location[[2L]] <- unfolded$location[[1L]]
        measure::measure_fold(unfolded)
      },
      "fold-fractional-index" = function() {
        unfolded <- measure::measure_unfold(dad)
        unfolded$location[[2L]] <- unfolded$location[[2L]] + 0.5
        measure::measure_fold(unfolded)
      }
    )

    if (
      !identical(
        names(invalid_calls),
        unlist(
          declaration$invalid_cases,
          use.names = FALSE
        )
      )
    ) {
      stop(
        "Implemented invalid cases do not match the frozen declaration order.",
        call. = FALSE
      )
    }
    for (id in names(invalid_calls)) {
      failure <- tryCatch(
        capture_warnings(id, invalid_calls[[id]]),
        error = identity
      )
      if (!inherits(failure, "error")) {
        stop(
          "Invalid case was accepted by the installed package: ",
          id,
          call. = FALSE
        )
      }
      run$result$invalid_cases[[length(run$result$invalid_cases) + 1L]] <- list(
        id = json_scalar(id),
        error = json_scalar(TRUE),
        message = json_scalar(conditionMessage(failure))
      )
      save_progress()
    }

    run$stage <- "list selection qualification"
    selected <- capture_warnings("list-selection-wavelength-254", function() {
      measure::measure_slice(
        dad_pair,
        wavelength = 254,
        drop = FALSE
      )
    })
    if (!inherits(selected, "measure_nd_list") || length(selected) != 2L) {
      stop(
        "The fixed paired DAD list selection did not retain two nD members.",
        call. = FALSE
      )
    }
    if (
      !inherits(selected[[1L]], "measure_nd_tbl") ||
        !inherits(selected[[2L]], "measure_nd_tbl")
    ) {
      stop(
        "The fixed paired DAD list selection did not retain nD member types.",
        call. = FALSE
      )
    }
    run$result$list_selection <- list(
      members = lapply(seq_len(2L), function(i) {
        list(
          fixture_id = json_scalar(c("dad-a", "dad-b")[[i]]),
          result = result_json(selected[[i]])
        )
      })
    )
    save_progress()

    run$result
  }

  tryCatch(
    {
      run$state <- run_body()
      run$stage <- "writing receipt"
      receipt <- list(
        created_utc = json_scalar(format(
          Sys.time(),
          tz = "UTC",
          usetz = TRUE
        )),
        session_info = json_array(
          capture.output(utils::sessionInfo()),
          function(value) json_scalar(value)
        ),
        dependency_versions = dependency_versions()
      )
      write_json_file(run$state, file.path(output, "results.json"))
      write_json_file(receipt, file.path(output, "runtime.json"))
      run$stage <- "complete"
      write_status(TRUE)
      run$completed <- TRUE
      unlink(progress_path)
      message("Qualification output written to ", output)
    },
    error = function(error) {
      run$failure_message <- conditionMessage(error)
      stop(error)
    }
  )
}

run_qualification()
