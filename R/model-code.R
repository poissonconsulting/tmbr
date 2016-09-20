#' Model Code
#'
#' Gets the model_code for an object.
#'
#' @param x The object.
#'
#' @return The model code as a character vector for an object
#' @export
model_code <- function(x) {UseMethod("model_code")}

#' @export
model_code.tmb_model <- function(x) x$model_code

#' @export
model_code.tmb_analysis <- function(x) model_code(x$model)
