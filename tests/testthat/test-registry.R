# ==============================================================================
# Technique Pack Registry Tests
# ==============================================================================

test_that("registry initializes correctly on package load", {
  # measure should be registered as a pack
  packs <- measure_packs()

  expect_s3_class(packs, "tbl_df")
  expect_true("measure" %in% packs$name)
  expect_equal(packs$technique[packs$name == "measure"], "general")
})

test_that("core steps are registered on package load", {
  steps <- measure_steps()

  expect_s3_class(steps, "tbl_df")
  expect_true(nrow(steps) > 0)

  # Check some known core steps exist
  expect_true("step_measure_input_long" %in% steps$step_name)
  expect_true("step_measure_savitzky_golay" %in% steps$step_name)
  expect_true("step_measure_baseline_als" %in% steps$step_name)

  # All core steps should be from "measure" pack
  core_steps <- dplyr::filter(steps, pack_name == "measure")
  expect_true(nrow(core_steps) > 50) # We have many steps
})

test_that("measure_packs returns expected columns", {
  packs <- measure_packs()

  expect_named(packs, c("name", "technique", "version", "description"))
  expect_type(packs$name, "character")
  expect_type(packs$technique, "character")
  expect_type(packs$version, "character")
  expect_type(packs$description, "character")
})

test_that("measure_steps returns expected columns", {
  steps <- measure_steps()

  expect_named(steps, c("step_name", "pack_name", "category", "description", "technique"))
  expect_type(steps$step_name, "character")
  expect_type(steps$pack_name, "character")
  expect_type(steps$category, "character")
  expect_type(steps$description, "character")
  expect_type(steps$technique, "character")
})

test_that("measure_steps filtering by category works", {
  baseline_steps <- measure_steps(categories = "baseline")

  expect_true(nrow(baseline_steps) > 0)
  expect_true(all(baseline_steps$category == "baseline"))

  # Check specific baseline steps
  expect_true("step_measure_baseline_als" %in% baseline_steps$step_name)
  expect_true("step_measure_baseline_poly" %in% baseline_steps$step_name)
})

test_that("measure_steps filtering by technique works", {
  general_steps <- measure_steps(techniques = "general")

  expect_true(nrow(general_steps) > 0)
  expect_true(all(general_steps$technique == "general"))
})

test_that("measure_steps filtering by pack works", {
  measure_core <- measure_steps(packs = "measure")

  expect_true(nrow(measure_core) > 0)
  expect_true(all(measure_core$pack_name == "measure"))
})

test_that("measure_steps multiple category filter works", {
  steps <- measure_steps(categories = c("baseline", "smoothing"))

  expect_true(nrow(steps) > 0)
  expect_true(all(steps$category %in% c("baseline", "smoothing")))
})

test_that("register_measure_pack works correctly", {
  # Reset registry to test registration
  .measure_registry_reset()

  # Register a test pack
  register_measure_pack(
    pack_name = "test.pack",
    technique = "Testing",
    version = "0.1.0",
    description = "A test pack"
  )

  packs <- measure_packs()

  expect_true("test.pack" %in% packs$name)
  expect_equal(packs$technique[packs$name == "test.pack"], "Testing")
  expect_equal(packs$version[packs$name == "test.pack"], "0.1.0")
  expect_equal(packs$description[packs$name == "test.pack"], "A test pack")

  # Restore registry
  .measure_registry_reset()
  register_measure_pack("measure", "general", description = "Core")
  .register_core_steps()
})

test_that("register_measure_step works correctly", {
  # Reset registry
  .measure_registry_reset()
  register_measure_pack("test.pack", "Testing")

  # Register a test step
  register_measure_step(
    step_name = "step_test_example",
    pack_name = "test.pack",
    category = "testing",
    description = "Example test step"
  )

  steps <- measure_steps()

  expect_true("step_test_example" %in% steps$step_name)
  expect_equal(steps$category[steps$step_name == "step_test_example"], "testing")

  # Should inherit technique from pack
  expect_equal(steps$technique[steps$step_name == "step_test_example"], "Testing")

  # Restore registry
  .measure_registry_reset()
  register_measure_pack("measure", "general", description = "Core")
  .register_core_steps()
})

test_that("registration is idempotent (no duplicates)", {
  # Reset registry
  .measure_registry_reset()
  register_measure_pack("test.pack", "Testing")

  # Register same step multiple times
  register_measure_step("step_test_dup", "test.pack", "testing", "First")
  register_measure_step("step_test_dup", "test.pack", "testing", "Second")
  register_measure_step("step_test_dup", "test.pack", "testing", "Third")

  steps <- measure_steps(packs = "test.pack")
  dup_steps <- dplyr::filter(steps, step_name == "step_test_dup")

  # Should only have one entry

  expect_equal(nrow(dup_steps), 1)
  # Should have the latest description
  expect_equal(dup_steps$description, "Third")

  # Restore registry
  .measure_registry_reset()
  register_measure_pack("measure", "general", description = "Core")
  .register_core_steps()
})

test_that("empty registry returns empty tibbles", {
  # Reset registry without registering anything
  .measure_registry_reset()

  packs <- measure_packs()
  steps <- measure_steps()

  expect_s3_class(packs, "tbl_df")
  expect_s3_class(steps, "tbl_df")
  expect_equal(nrow(packs), 0)
  expect_equal(nrow(steps), 0)

  # Restore registry
  .measure_registry_reset()
  register_measure_pack("measure", "general", description = "Core")
  .register_core_steps()
})

test_that("measure_steps filtering returns empty when no match", {
  steps <- measure_steps(techniques = "NonExistent")

  expect_s3_class(steps, "tbl_df")
  expect_equal(nrow(steps), 0)
})

test_that("step categories are populated correctly", {
  steps <- measure_steps()

  # Check some expected categories exist
  categories <- unique(steps$category)

  expect_true("io" %in% categories)
  expect_true("baseline" %in% categories)
  expect_true("smoothing" %in% categories)
  expect_true("normalization" %in% categories)
  expect_true("alignment" %in% categories)
})
