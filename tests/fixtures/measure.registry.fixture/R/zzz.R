step_registry_fixture <- function(recipe, ...) {
  measure::step_measure_center(recipe, ...)
}

# jarl-ignore unused_function: R invokes this hook when the fixture loads.
.onLoad <- function(libname, pkgname) {
  measure::register_measure_pack(pkgname, "Fixture/Boundary", version = "0.1.0")
  measure::register_measure_step(
    "step_registry_fixture",
    pkgname,
    "fixture",
    "Center the fixture measurements"
  )
}
