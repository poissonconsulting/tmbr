#' Tidy TMB Analysis
#'
#' Tidys the coefficients of a TMB analysis object into a data frame.
#'
#' @param x The tmb_analysis object to tidy.
#'
#' @param terms A string of the terms to tidy. Permitted values are 'all', 'fixed',
#' 'random' and 'report'.
#' @param conf.level A number specifying the confidence level. By default 0.95.
#' @param ... Unused.
#' @seealso \code{\link[broom]{tidy}}.
#' @export
tidy.tmb_analysis <- function(x, terms = "fixed", conf.level = 0.95, ...) {
  check_number(conf.level, c(0.5, 0.99))
  coef(x, term = terms, conf_level = conf.level)
}
