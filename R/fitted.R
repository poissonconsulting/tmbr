#' Fitted Values
#'
#' Extract fitted values for a TMB analysis.
#'
#' @param object The tmb_analysis object.
#'
#' @param term A string of the terms to tidy.
#' @inheritParams coef.tmb_analysis
#' @return The data with the fitted values.
#' @export
fitted.tmb_analysis <- function(object, term = "fit", conf_level = 0.95, ...) {
  reported(object, term, conf_level = conf_level)
}
