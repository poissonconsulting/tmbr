dims <- function(x) if (is.vector(x)) length(x) else dim(x)

is.named <- function(x) {
  !is.null(names(x))
}

is.named_list <- function(x) {
  is.list(x) && is.named(x)
}

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

sort_by_names <- function(x) {
  stopifnot(is.character(x) || is.named_list(x))
  if (is.character(x)) return(sort(x))
  x[order(names(x))]
  x[order(names(x))]
}
