#' Parameters
#'
#' Gets the parameters for an object.
#'
#' @param x The object.
#'
#' @return The parameters as a list.
#' @export
parameters <- function(x) {UseMethod("parameters")}

#' @export
parameters.tmb_model <- function(x) x$parameters
