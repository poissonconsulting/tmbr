#' Parameters
#'
#' Gets the fixed or random (and in the case of an analysis report or adreport)
#' parameter names for an object.
#'
#' @param x The object.
#' @param terms A string of the terms to get the parameter names for.
#' @param ... Unused.
#' @return The parameter names as a character vector.
#' @export
parameters <- function(x, terms = "fixed", ...) {UseMethod("parameters")}

#' @export
parameters.tmb_model <- function(x, terms = "fixed", ...) {
  check_vector(terms, c("^fixed$", "^fixed$", "^random$"), max_length = 1)

  fixed <- names(x$inits)
  if (is.character(x$random_effects)) {
    random <- x$random_effects
  } else
    random <- names(x$random_effects)

  fixed <- fixed[!fixed %in% random]

  if (terms == "fixed") return(sort(fixed))
  sort(random)
}

#' @export
parameters.tmb_analysis <- function(x, terms = "fixed", ...) {
  check_vector(terms, c("^fixed$", "^random$", "^report$", "^adreport$"), max_length = 1)

  random <- unique(names(x$sd$par.random))
  fixed <- unique(names(x$sd$par.fixed))
  report <- unique(names(x$ad_fun$report()))
  adreport <- unique(names(x$sd$value))

  if (is.null(random)) random <- character(0)
  if (is.null(report)) report <- character(0)
  if (is.null(adreport)) adreport <- character(0)

  if (terms == "fixed") return(sort(fixed))
  if (terms == "random") return(sort(random))
  if (terms == "report") return(sort(report))
  sort(adreport)
}
