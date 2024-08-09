#' Is a TMB Code
#'
#' Tests whether x is an object of class 'tmb_code'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_code <- function(x) {
  inherits(x, "tmb_code")
}

#' Is a TMB Model
#'
#' Tests whether x is an object of class 'tmb_model'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_model <- function(x) {
  inherits(x, "tmb_model")
}

#' Is a TMB Analysis
#'
#' Tests whether x is an object of class 'tmb_analysis'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_analysis <- function(x) {
  inherits(x, "tmb_analysis")
}

#' Is a TMB ML Analysis
#'
#' Tests whether x is an object of class 'tmb_ml_analysis'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_ml_analysis <- function(x) {
  inherits(x, "tmb_ml_analysis")
}

#' Is a TMB MCMC Analysis
#'
#' Tests whether x is an object of class 'tmb_mcmc_analysis'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_mcmc_analysis <- function(x) {
  inherits(x, "tmb_mcmc_analysis")
}
