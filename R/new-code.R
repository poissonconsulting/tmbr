#' New Code
#'
#' Gets the new_code for an object.
#'
#' @param x The object.
#'
#' @return The predict code as a character vector for an object
#' @export
new_code <- function(x) {UseMethod("new_code")}

#' @export
new_code.tmb_model <- function(x) x$new_code

#' @export
new_code.tmb_analysis <- function(x) new_code(model(model))
