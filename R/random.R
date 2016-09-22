#' Random
#'
#' Gets the random effects parameters for an object.
#'
#' @param x The object.
#'
#' @return The parameters as a list.
#' @export
random <- function(x) {UseMethod("random")}

#' @export
random.tmb_model <- function(x) x$random

#' @export
random.tmb_analysis <- function(x) random(x$model)
