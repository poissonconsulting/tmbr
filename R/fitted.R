#' Fitted Values
#'
#' Extract fitted values for a TMB analysis.
#'
#' @inheritParams predict.tmb_analysis
#' @return The data with the fitted values.
#' @export
fitted.tmb_analysis <- function(object, conf_int = FALSE, conf_level = 0.95, ...) {
  predict(object, term = "fit", conf_int = conf_int, conf_level = conf_level)
}

