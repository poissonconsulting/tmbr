#' Coef TMB Analysis
#'
#' Coefficients for a TMB analysis.
#'
#' The \code{statistic} is the z value.
#' The \code{p.value} is \code{Pr(>|z^2|)}.
#' The (95\%) \code{lower} and \code{upper} confidence intervals are
#' the \code{estimate} +/- 1.96 * \code{std.error}.
#'
#' @param object The tmb_analysis object.
#'
#' @param terms A string of the terms to tidy. Permitted values are 'all', 'fixed',
#' 'random' and 'report'.
#' @param conf_level A number specifying the confidence level. By default 0.95.
#' @param ... unused.
#' @export
coef.tmb_analysis <- function(object, terms = "fixed",
                              conf_level = 0.95, ...) {
  check_vector(terms, c("^all$", "^fixed$", "^random$", "^report$"), max_length = 1)
  check_number(conf_level, c(0.5, 0.99))

  coef <- summary(object$sd, select = terms, p.value = TRUE) %>% as.data.frame()
  coef %<>% mutate_(term = ~row.names(coef))
  coef %<>% select_(term = ~term, estimate = ~Estimate, std.error = ~`Std. Error`,
                    statistic = ~`z value`, p.value = ~`Pr(>|z^2|)`)
  coef %<>% mutate_(lower = ~estimate + std.error * qnorm((1 - conf_level) / 2),
                    upper = ~estimate + std.error * qnorm((1 - conf_level) / 2 + conf_level))
  coef %<>% as.tbl()
  coef
}
