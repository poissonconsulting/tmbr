#' Is Converged
#'
#' @param x The object to test convergence for.
#' @param ... Unused.
#' @export
converged.tmb_ml_analysis <- function(x, ...) {
  !any(is.nan(suppressWarnings(coef(x))$sd))
}
