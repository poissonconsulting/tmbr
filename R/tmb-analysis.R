tmb_analysis <- function(data_set, model, beep = TRUE, debug = FALSE, ...) {
  if (!is.tmb_model(model)) stop("model must be a tmb_model", call. = FALSE)

  analyse(model, data_set, beep = beep, debug = debug, ...)
}
