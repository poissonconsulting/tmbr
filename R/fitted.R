#' Fitted Values
#'
#' Extract fitted values for a TMB analysis.
#'
#' @param object The tmb_analysis object.
#'
#' @param term A string of the terms to tidy. Permitted values are 'all', 'fixed',
#' 'random' and 'report'.
#' @inheritParams coef.tmb_analysis
#' @return The data_set with the fitted values.
#' @export
fitted.tmb_analysis <- function(object, term = "fit", conf_level = 0.95, ...) {
  check_string(term)

  fit <- coef(object, terms = "report", conf_level = conf_level)
  fit <- fit[fit$term == term,]

  if (!nrow(fit)) error("term '", fit, "' is not in reported terms")

  fit$term <- NULL
  names(fit) <- paste(term, names(fit), sep = ".")

  if (nrow(fit) != nrow(data_set(object))) {
    error("the length of term '", fit, "' does not match the number of rows of data_set")
  }
  fit <- dplyr::bind_cols(data_set(object), fit)
  fit
}
