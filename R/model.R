#' Model
#'
#' Gets the model for an object.
#'
#' @param x The object.
#' @export
model <- function(x) {UseMethod("model")}

#' @export
model.tmb_analysis <- function(x) x$model
