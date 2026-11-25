# Keep registry mutations scoped to one test. Extension packages register into
# measure's process-local namespace and are not deregistered on unload.
local_measure_registry <- function() {
  registry <- get(".measure_registry", asNamespace("measure"))
  previous <- as.list(registry)
  withr::defer(
    {
      list2env(previous, envir = registry)
    },
    envir = parent.frame()
  )
}
