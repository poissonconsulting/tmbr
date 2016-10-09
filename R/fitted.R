#' Fitted Values
#'
#' Extract fitted values for a TMB analysis.
#'
#' @inheritParams predict.tmb_analysis
#' @return The data with the fitted values.
#' @export
fitted.tmb_analysis <- function(object, ...) {
  predict(object, term = "fit")
}

