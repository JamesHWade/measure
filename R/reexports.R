# ==============================================================================
# Re-exports from generics package
#
# These generics are re-exported so users don't need to load generics
# separately to use tidy(), glance(), augment(), etc.
# ==============================================================================

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment

#' @importFrom stats complete.cases sd
NULL
