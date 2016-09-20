#' TMB Model
#'
#' @param model_code A string of the model template.
#' @param parameters A list of all parameter objects required by the user template.
#'
#' @return An object of class tmb_model.
#' @export
tmb_model <- function(model_code, parameters) {
  check_string(model_code)

  obj <- list()
  obj$model_code <- model_code
  obj$parameters <- parameters
  class(obj) <- "tmb_model"
  obj
}
