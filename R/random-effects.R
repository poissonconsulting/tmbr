#' Random Effects
#'
#' Gets the random effects parameters for an object.
#'
#' @param x The object.
#'
#' @return The parameters as a list.
#' @export
random_effects <- function(x) {UseMethod("random_effects")}

#' @export
random_effects.tmb_model <- function(x) x$random_effects

#' @export
random_effects.tmb_analysis <- function(x) random_effects(model(x))
