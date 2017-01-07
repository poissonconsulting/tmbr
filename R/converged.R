#' Is Converged
#'
#' @param x The object to test convergence for.
#' @param mcmc A flag specifying whether to get the mcmc draws as opposed to ml estimates.
#' @param rhat A number specifying the rhat threshold.
#' @param ... Unused.
#' @export
converged.tmb_analysis <- function(x, mcmc = FALSE, rhat = getOption("mb.rhat", 1.1), ...) {
  check_flag(mcmc)

  if (mcmc) NextMethod()

  warning("converged is not yet implemented for ML estimates")

  TRUE
}
