#' Residuals
#'
#' Extract residual values for a TMB analysis.
#'
#' @inheritParams predict.tmb_analysis
#' @return The data with the residual values.
#' @export
residuals.tmb_analysis <- function(object, conf_int = FALSE, conf_level = 0.95, ...) {
  predict(object, term = "residual", conf_int = conf_int, conf_level = conf_level)
}
