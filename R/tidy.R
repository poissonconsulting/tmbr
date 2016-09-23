#' Tidy TMB Analysis
#'
#' Tidys the coefficients of a TMB analysis object into a data frame.
#'
#' @param x The tmb_analysis object to tidy.
#'
#' @param terms A string of the terms to tidy. Permitted values are 'all', 'fixed',
#' 'random' and 'report'.
#' @param conf.int A flag indictating whether to calculate confidence intervals.
#' @param conf.level A number specifying the confidence level. By default 0.95.
#' @param ... Unused.
#' @seealso \code{\link[broom]{tidy}}.
#' @export
tidy.tmb_analysis <- function(x, terms = "fixed", conf.int = FALSE, conf.level = 0.95, ...) {
  coef(x, term = terms, conf.int = conf.int, conf.level = conf.level)
}
