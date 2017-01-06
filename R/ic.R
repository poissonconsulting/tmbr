#' Akaike's Information Criteron
#'
#' Calculates marginal AICc for an analysis.
#'
#' By default the sample size is assumed to be the number of rows
#' in the analysis data set. To override manually set n to be an integer ie n = 10L.
#' To calculate AIC (as opposed to AICc) set n = Inf.
#'
#' @param object The object to calculate the AIC for.
#' @param n A count of the sample size.
#' @param ... Not used.
#' @return The Akaike's Information Criteron as a number.
#' @export
AIC.tmb_analysis <- function(object, n = NULL, ...){

  k <- nterms(object)

  if (is.null(n)) n <- sample_size(object)

  if (is.infinite(n)) {
    c <- 0
  } else {
    check_count(n)
    c <- 2 * k * (k + 1) / (n - k - 1)
  }

  2 * k - 2 * logLik(object) + c
}

#' @export
IC.tmb_analysis <- function(object, n = NULL, ...) {
  
  AIC(object, n = n)
}
