#' Predict Code
#'
#' Gets the predict_code for an object.
#'
#' @param x The object.
#'
#' @return The predict code as a character vector for an object
#' @export
predict_code <- function(x) {UseMethod("predict_code")}

#' @export
predict_code.tmb_model <- function(x) x$predict_code

#' @export
predict_code.tmb_analysis <- function(x) predict_code(x$model)
