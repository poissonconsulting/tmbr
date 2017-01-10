#' Coef TMB Analysis
#'
#' Coefficients for a TMB analysis.
#'
#' The (95\%) \code{lower} and \code{upper} confidence intervals are
#' the \code{estimate} +/- 1.96 * \code{std.error}.
#'
#' @param object The mb_analysis object.
#' @param param_type A flag specifying whether 'fixed', 'random' or 'derived' terms.
#' @param include_constant A flag specifying whether to include constant terms.
#' @param conf_level A number specifying the confidence level. By default 0.95.
#' @param ... Not used.
#' @return A tidy tibble of the coefficient terms.
#' @export
coef.tmb_ml_analysis <- function(object, param_type = "fixed", include_constant = TRUE, conf_level = 0.95, ...) {
  check_scalar(param_type, c("fixed", "random", "derived"))
  check_flag(include_constant)
  check_number(conf_level, c(0.5, 0.99))

  coef <- dplyr::data_frame(term = character(0), estimate = numeric(0), sd = numeric(0),
                            zscore = numeric(0), lower = numeric(0),
                            upper = numeric(0), significance = numeric(0))

  # there should always be fixed parameters
  if (param_type == "random") {
    if (is.null(object$sd$par.random)) {
      return(coef)
    }
  }

  if (param_type == "derived") {
    if (!length(object$sd$value)) {
      return(coef)
    }
  }

  # necessary due to summary args!
  if (param_type == "derived") param_type <- "report"
  coef <- summary(object$sd, select = param_type, p.value = TRUE) %>% as.data.frame()

  coef %<>% dplyr::mutate_(term = ~row.names(coef))
  coef %<>% dplyr::select_(term = ~term, estimate = ~Estimate, sd = ~`Std. Error`,
                    zscore = ~`z value`, significance = ~`Pr(>|z^2|)`)

  coef %<>% dplyr::mutate_(lower = ~estimate + sd * qnorm((1 - conf_level) / 2),
                    upper = ~estimate + sd * qnorm((1 - conf_level) / 2 + conf_level))
  coef %<>% dplyr::select_(~term, ~estimate, ~sd, ~zscore, ~lower, ~upper, ~significance)
  coef %<>% dplyr::arrange_(~term)

  coef %<>% remap_coef(object$map)
  if (!include_constant) coef %<>% dplyr::filter_(~!constant)
  coef %<>% dplyr::mutate_(constant = ~NULL)
  coef %<>% dplyr::as.tbl()
  coef
}
