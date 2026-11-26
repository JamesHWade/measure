# Qualify the same load hook contract for a fixture and an optional real pack.
expect_pack_discovery <- function(package, lib, technique, step) {
  was_loaded <- isNamespaceLoaded(package)
  local_measure_registry()
  loadNamespace(package, lib.loc = lib)
  if (!was_loaded) {
    withr::defer(unloadNamespace(package))
  }

  packs <- measure_packs()
  expect_identical(packs$technique[packs$name == package], technique)
  steps <- measure_steps(packs = package)
  by_technique <- measure_steps(techniques = technique)
  expect_equal(by_technique[by_technique$pack_name == package, ], steps)
  expect_true(step %in% steps$step_name)
  expect_true(all(steps$step_name %in% getNamespaceExports(package)))

  if (!was_loaded) {
    unloadNamespace(package)
    expect_identical(measure_steps(packs = package), steps)
    loadNamespace(package, lib.loc = lib)
    expect_identical(measure_packs(), packs)
    expect_identical(measure_steps(packs = package), steps)
  }
}

test_that("an installed pack registers from its namespace load hook", {
  lib <- withr::local_tempdir()
  install_test_pack("measure.registry.fixture", lib)
  expect_false("measure.registry.fixture" %in% measure_packs()$name)
  expect_pack_discovery(
    "measure.registry.fixture",
    lib,
    "Fixture/Boundary",
    "step_registry_fixture"
  )
})

test_that("the optional measure.sec pack registers its exported SEC/GPC steps", {
  package <- "measure.sec"
  path <- find.package(package, quiet = TRUE)
  if (!length(path)) {
    skip("measure.sec is not installed")
  }
  expect_pack_discovery(
    package,
    dirname(path),
    "SEC/GPC",
    "step_sec_mw_averages"
  )
})
