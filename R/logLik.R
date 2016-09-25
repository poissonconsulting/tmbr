#' Log-Likelihood
#'
#' Log-Likelihood for a TMB analysis.
#'
#' @param object The tmb_analysis object.
#' @param ... unused.
#' @export
logLik.tmb_analysis <- function(object, ...) {
  object$opt$value * -1
}
