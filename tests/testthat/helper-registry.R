# Keep registry mutations scoped to one test. Extension packages register into
# measure's process-local namespace and are not deregistered on unload.
local_measure_registry <- function() {
  registry <- get(".measure_registry", asNamespace("measure"))
  previous <- list(packs = registry$packs, steps = registry$steps)
  withr::defer(
    {
      registry$packs <- previous$packs
      registry$steps <- previous$steps
    },
    envir = parent.frame()
  )
}
