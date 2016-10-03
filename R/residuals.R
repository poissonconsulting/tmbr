#' Residuals
#'
#' Extract residual values for a TMB analysis.
#'
#' @param object The tmb_analysis object.
#'
#' @param term A string of the term.
#' @inheritParams coef.tmb_analysis
#' @return The data_set with the residual values.
#' @export
residuals.tmb_analysis <- function(object, term = "residual", conf_level = 0.95, ...) {
  reported(object, term, conf_level = conf_level)
}
