#' Inits
#'
#' Gets a named list of the initial values for an object.
#'
#' @param x The object.
#' @return The initial values as a named list.
#' @export
inits <- function(x) {UseMethod("inits")}

#' @export
inits.tmb_model <- function(x) {
  x$inits
}

#' @export
inits.tmb_analysis <- function(x) {
  inits(model(x))
}
