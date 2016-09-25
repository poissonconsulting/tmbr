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

sds <- function(obj, terms) {
  ad_fun <- obj$ad_fun
#  ad_fun$retape()

  sds <- TMB::sdreport(ad_fun)
  sds %<>% summary(select = terms, p.value = TRUE) %>% as.data.frame()
  sds %<>% mutate_(term = ~row.names(sds))
  sds %<>% select_(term = ~term, estimate = ~Estimate, std.error = ~`Std. Error`,
                  statistic = ~`z value`, p.value = ~`Pr(>|z^2|)`)
  sds
}

#' Coef TMB Analysis
#'
#' Coefficients for a TMB analysis.
#'
#' @param object The tmb_analysis object.
#'
#' @param terms A string of the terms to tidy. Permitted values are 'all', 'fixed',
#' 'random' and 'report'.
#' @param conf_level A number specifying the confidence level. By default 0.95.
#' If conf_level = 0 confidence intervals are not calculated.
#' @param ... unused.
#' @export
coef.tmb_analysis <- function(object, terms = "fixed", conf_level = 0.95, ...) {
  check_vector(terms, c("^all$", "^fixed$", "^random$", "^report$"), max_length = 1)
  check_number(conf_level, c(0, 0.99))

  sds <- sds(obj = object, terms)
  if (conf_level > 0) {
    confints <- confints(obj = object, terms = sds$term, level = conf_level)
    sds %<>% bind_cols(confints)
  }
  sds
}
