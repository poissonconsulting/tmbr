profile <- function(term, obj) {
  ad_fun <- obj$ad_fun
#  ad_fun$retape()
  TMB::tmbprofile(obj = ad_fun, name = term, trace = FALSE)
}

conf_int <- function(obj, level) {
  confint(object = obj, level = level) %>% as.data.frame()
}

confints <- function(obj, terms, level) {
  profile <- lapply(terms, FUN = profile, obj = obj)
  confints <- lapply(profile, FUN = conf_int, level = level)
  confints %<>% bind_rows()
  confints
}

sd <- function(obj, terms) {
  ad_fun <- obj$ad_fun
#  ad_fun$retape()
  sd <- TMB::sdreport(ad_fun)
  sd %<>% summary(select = terms, p.value = TRUE) %>% as.data.frame()
  sd %<>% mutate_(term = ~row.names(sd))
  sd %<>% select_(term = ~term, estimate = ~Estimate, std.error = ~`Std. Error`,
                  statistic = ~`z value`, p.value = ~`Pr(>|z^2|)`)
  sd
}

#' Tidy TMB Analysis
#'
#' Tidys the coefficients of a TMB analysis object into a data frame.
#'
#' @param x The tmb_analysis object to tidy.
#'
#' @param terms A string of the terms to tidy. Permitted values are 'all', 'fixed',
#' 'random' and 'report'.
#' @param conf.int A flag indictating whether to calculate confidence intervals.
#' @param conf.level A number specifying the confidence level. By default 0.95.
#' @param ... Unused.
#' @seealso \code{\link[broom]{tidy}}.
#' @export
tidy.tmb_analysis <- function(x, terms = "all", conf.int = FALSE, conf.level = 0.95, ...) {
  check_vector(terms, c("^all$", "^fixed$", "^random$", "^report$"), max_length = 1)
  check_flag(conf.int)
  check_number(conf.level, c(0.5, 0.99))

  sd <- sd(obj = x, terms)
  if (conf.int) {
    confints <- confints(obj = x, terms = sd$term, level = conf.level)
    sd %<>% bind_cols(confints)
  }
  sd
}
