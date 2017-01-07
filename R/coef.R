c_name <- function(x) {
  x[[1]] %<>% stringr::str_c(names(x), .)
  x
}

par_names_indices <- function(estimates) {
  estimates %<>% lapply(dims) %>% lapply(dims_to_dimensions_vector)
  estimates %<>% purrr::lmap(c_name)
  estimates %<>% sort_nlist()
  estimates
}

named_estimates <- function(estimates) {
  stopifnot(is_nlist(estimates))
  indices <- par_names_indices(estimates) %>% unlist()
  estimates %<>% unlist()
  names(estimates) <- indices
  estimates
}

#' Coef TMB Analysis
#'
#' Coefficients for a TMB analysis.
#'
#' The (95\%) \code{lower} and \code{upper} confidence intervals are
#' the \code{estimate} +/- 1.96 * \code{std.error}.
#'
#' @param object The mb_analysis object.
#' @param fixed A flag specifying whether fixed or random terms.
#' @param include_constant A flag specifying whether to include constant terms.
#' @param mcmc A flag specifying whether to get the mcmc draws as oppsed to ml estimates.
#' @param conf_level A number specifying the confidence level. By default 0.95.
#' @param ... Not used.
#' @return A tidy tibble of the coefficient terms.
#' @export
coef.tmb_analysis <- function(object, fixed = TRUE, include_constant = TRUE, mcmc = FALSE, conf_level = 0.95, ...) {
  check_flag(fixed)
  check_flag(include_constant)
  check_flag(mcmc)
  check_number(conf_level, c(0.5, 0.99))

  if (mcmc) NextMethod()

  estimates <- estimates(object, fixed = fixed) %>% named_estimates()

  if (!length(estimates)) {
    return(dplyr::data_frame(term = character(0), estimate = numeric(0), sd = numeric(0),
                             zscore = numeric(0), lower = numeric(0),
                             upper = numeric(0), significance = numeric(0)))
  }

  coef <- summary(object$sd, select = ifelse(fixed, "fixed", "random"), p.value = TRUE) %>% as.data.frame()

  coef %<>% dplyr::mutate_(term = ~row.names(coef))
  coef %<>% dplyr::select_(term = ~term, estimate = ~Estimate, sd = ~`Std. Error`,
                    zscore = ~`z value`, significance = ~`Pr(>|z^2|)`)
  coef %<>% dplyr::mutate_(lower = ~estimate + sd * qnorm((1 - conf_level) / 2),
                    upper = ~estimate + sd * qnorm((1 - conf_level) / 2 + conf_level))
  coef %<>% dplyr::arrange_(~term)

  coef %<>% remap_coef(object$map)
  coef$term <- names(estimates)
  if (!include_constant) coef %<>% dplyr::filter_(~!constant)
  coef %<>% dplyr::mutate_(constant = ~NULL)
  coef %<>% dplyr::as.tbl()
  coef
}
