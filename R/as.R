#' Coerce to an TMB ML Analysis Object
#'
#' @param x object to coerce.
#' @param ... Unused.
# @export
as.tmb_ml_analysis <- function(x, ...) {
  UseMethod("as.tmb_ml_analysis")
}

# @export
as.tmb_ml_analysis.tmb_mcmc_analysis <- function(x, ...) {
  x$ngens <- x$ml$ngens
  x$duration <- x$ml$duration
  x$mcmcr <- x$ml$mcmcr

  x$ml <- NULL

  class(x) <- c("tmb_ml_analysis", "tmb_analysis", "mb_analysis")
  x
}
