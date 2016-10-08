#' Augment TMB Analysis
#'
#' Augments the data of a TMB analysis object.
#'
#' @param x The tmb_analysis object.
#' @inheritParams predict.tmb_analysis
#' @return The data with the fitted and residual values.
#' @param ... Unused.
#' @seealso \code{\link[broom]{augment}}.
#' @export
augment.tmb_analysis <- function(x, conf_int = FALSE, conf_level = 0.95, ...) {
  fit <- fitted(x, conf_int = conf_int, conf_level = conf_level)
  residual <- residuals(x, conf_int = conf_int, conf_level = conf_level)
  fit %<>% bind_cols(residual[!names(residual) %in% names(fit)])
  fit
}
