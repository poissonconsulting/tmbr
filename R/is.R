#' Is a TMB Model?
#'
#' Tests wether x is an object of class 'tmb_model'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_model <- function(x) {
  inherits(x, "tmb_model")
}

#' Is a TMB Analysis?
#'
#' Tests wether x is an object of class 'tmb_analysis'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_analysis <- function(x) {
  inherits(x, "tmb_analysis")
}
