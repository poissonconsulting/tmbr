#' Residuals
#'
#' Extract residual values for a TMB analysis.
#'
#' @inheritParams predict.tmb_analysis
#' @return The data with the residual values.
#' @export
residuals.tmb_analysis <- function(object, ...) {
  predict(object, term = "residual")
}
