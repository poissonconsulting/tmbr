#' Glance TMB Analysis
#'
#' A single row data frame of summary model statistics.
#'
#' @param x The tmb_analysis object to convert to a single-row data frame.
#' @param ... Unused.
#' @seealso \code{\link[broom]{glance}}.
#' @export
glance.tmb_analysis <- function(x, ...) {
  data.frame(logLik = logLik(x))
}
