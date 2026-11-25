.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("measure", quietly = TRUE)) {
    return(invisible(NULL))
  }

  measure::register_measure_pack(
    pack_name = pkgname,
    technique = "Fixture/Boundary",
    version = utils::packageVersion(pkgname),
    description = "Installed package-boundary registry fixture"
  )
  measure::register_measure_step(
    step_name = "step_registry_fixture",
    pack_name = pkgname,
    category = "fixture",
    description = "A step registered by an installed fixture package"
  )

  invisible(NULL)
}
