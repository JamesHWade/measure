# Installed package-boundary acceptance for technique-pack registration.

fixture_path <- function() {
  testthat::test_path("..", "fixtures", "measure.registry.fixture")
}

install_fixture <- function() {
  fixture_lib <- tempfile("measure-registry-fixture-lib-")
  dir.create(fixture_lib, recursive = TRUE)
  output <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD",
      "INSTALL",
      "--no-multiarch",
      "--no-docs",
      "--no-help",
      "--no-demo",
      paste0("--library=", fixture_lib),
      fixture_path()
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  expect_identical(status, 0L, info = paste(output, collapse = "\n"))
  fixture_lib
}

expect_registered <- function(pack_name, technique, step_name) {
  packs <- measure::measure_packs()
  pack <- packs[packs$name == pack_name, , drop = FALSE]
  expect_equal(nrow(pack), 1L)
  expect_identical(pack$technique, technique)

  steps <- measure::measure_steps(
    packs = pack_name,
    techniques = technique
  )
  expect_gt(nrow(steps), 0L)
  expect_true(step_name %in% steps$step_name)
  steps
}

test_that("an installed fixture crosses the package boundary", {
  local_measure_registry()
  fixture_lib <- install_fixture()
  withr::local_libpaths(c(fixture_lib, .libPaths()))

  fixture_package <- "measure.registry.fixture"
  loadNamespace(fixture_package, lib.loc = fixture_lib)
  withr::defer({
    if (isNamespaceLoaded(fixture_package)) {
      unloadNamespace(fixture_package)
    }
  })

  fixture_steps <- expect_registered(
    fixture_package,
    "Fixture/Boundary",
    "step_registry_fixture"
  )
  expect_equal(nrow(fixture_steps), 1L)

  # Registry metadata belongs to the measure namespace. Unloading and loading
  # the extension must not remove or duplicate the registered keys.
  unloadNamespace(fixture_package)
  expect_registered(
    fixture_package,
    "Fixture/Boundary",
    "step_registry_fixture"
  )
  loadNamespace(fixture_package, lib.loc = fixture_lib)
  expect_equal(
    nrow(measure::measure_steps(packs = fixture_package)),
    1L
  )
})

test_that("an installed measure.sec pack is discoverable when available", {
  sec_path <- find.package("measure.sec", quiet = TRUE)
  if (length(sec_path) == 0L) {
    skip("measure.sec is not installed")
  }

  local_measure_registry()
  sec_package <- "measure.sec"
  sec_was_loaded <- isNamespaceLoaded(sec_package)
  if (!sec_was_loaded) {
    loadNamespace(sec_package, lib.loc = dirname(sec_path))
    withr::defer({
      if (isNamespaceLoaded(sec_package)) {
        unloadNamespace(sec_package)
      }
    })
  }

  sec_steps <- expect_registered(
    sec_package,
    "SEC/GPC",
    "step_sec_mw_averages"
  )
  expect_gt(nrow(sec_steps), 0L)

  if (!sec_was_loaded) {
    unloadNamespace(sec_package)
    expect_registered(
      sec_package,
      "SEC/GPC",
      "step_sec_mw_averages"
    )
    loadNamespace(sec_package, lib.loc = dirname(sec_path))
    expect_equal(
      nrow(measure::measure_steps(packs = sec_package)),
      nrow(sec_steps)
    )
  }
})
