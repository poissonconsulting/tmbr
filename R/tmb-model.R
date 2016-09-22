#' TMB Model
#'
#' @param model_code A string of the model template.
#' @param parameters A list of all parameter objects required by the user template.
#' @param select A character vector specifying the variables to pass to the analysis.
#'
#' @return An object of class tmb_model.
#' @export
tmb_model <- function(model_code, parameters, select = NULL) {
  check_string(model_code)
  if (!is.null(select) && !is.character(select))
    stop("select must be a character vector or NULL" ,call. = FALSE)

  obj <- list()
  obj$model_code <- model_code
  obj$parameters <- parameters
  obj$select <- unique(select)
  class(obj) <- "tmb_model"
  obj
}
