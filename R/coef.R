#' Coef TMB Analysis
#'
#' Coefficients for a TMB analysis.
#'
#' @param object The tmb_analysis object.
#'
#' @param terms A string of the terms to tidy. Permitted values are 'all', 'fixed',
#' 'random' and 'report'.
#' @param conf.int A flag indictating whether to calculate confidence intervals.
#' @param conf.level A number specifying the confidence level. By default 0.95.
#' @param ... Unused.
#' @export
coef.tmb_analysis <- function(object, terms = "fixed", conf.int = FALSE, conf.level = 0.95, ...) {
  check_vector(terms, c("^all$", "^fixed$", "^random$", "^report$"), max_length = 1)
  check_flag(conf.int)
  check_number(conf.level, c(0.5, 0.99))

  sd <- sd(obj = object, terms)
  if (conf.int) {
    confints <- confints(obj = object, terms = sd$term, level = conf.level)
    sd %<>% bind_cols(confints)
  }
  sd
}
