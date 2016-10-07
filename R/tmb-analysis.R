#' TMB Analysis
#'
#' Analyse a data frame using a TMB model.
#'
#' @inheritParams analyse
#' @export
tmb_analysis <- function(data, model, beep = TRUE, debug = FALSE, ...) {
  if (!is.tmb_model(model)) error("model must be a tmb_model")

  analyse(model, data, beep = beep, debug = debug, ...)
}
