#' Coef TMB Analysis
#'
#' Coefficients for a TMB analysis.
#'
#'  Permitted values for terms are 'fixed',
#' 'random' and 'adreport'.
#'
#' The \code{statistic} is the z value.
#' The \code{p.value} is \code{Pr(>|z^2|)}.
#' The (95\%) \code{lower} and \code{upper} confidence intervals are
#' the \code{estimate} +/- 1.96 * \code{std.error}.
#'
#' @param object The tmb_analysis object.
#' @param terms A string of the type of terms to get the coefficients for.
#' @param scalar A flag indicating whether to only return scalar terms.
#' @param conf_level A number specifying the confidence level. By default 0.95.
#' @param ... Not used.
#' @return A tidy tibble of the coeffcient terms.
#' @export
coef.tmb_analysis <- function(object, terms = "fixed", scalar = FALSE,
                              conf_level = 0.95, ...) {
  check_vector(terms, c("^fixed$", "^random$", "^adreport$"), max_length = 1)
  check_flag(scalar)
  check_number(conf_level, c(0.5, 0.99))
  check_unused(...)

  # get all estimates (scalar = FALSE) and filter later
  estimates <- estimates(object, terms = terms, scalar = FALSE) %>% named_estimates()

  if (!length(estimates)) {
    return(dplyr::data_frame(term = character(0), estimate = numeric(0), std.error = numeric(0),
                      statistic = numeric(0), p.value = numeric(0), lower = numeric(0),
                      upper = numeric(0)))
  }

  # necessary due to summary args!
  if (terms == "adreport") terms <- "report"

  coef <- summary(object$sd, select = terms, p.value = TRUE) %>% as.data.frame()
  coef %<>% dplyr::mutate_(term = ~row.names(coef))
  coef %<>% dplyr::select_(term = ~term, estimate = ~Estimate, std.error = ~`Std. Error`,
                    statistic = ~`z value`, p.value = ~`Pr(>|z^2|)`)
  coef %<>% dplyr::mutate_(lower = ~estimate + std.error * qnorm((1 - conf_level) / 2),
                    upper = ~estimate + std.error * qnorm((1 - conf_level) / 2 + conf_level))
  coef %<>% dplyr::arrange_(~term)
  coef$term <- names(estimates)
  if (scalar) coef %<>% dplyr::filter_(~!str_detect(term, "\\["))
  coef %<>% dplyr::as.tbl()
  coef
}
