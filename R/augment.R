#' Augment TMB Analysis
#'
#' Augments the data of a TMB analysis object.
#'
#' @param x The tmb_analysis object.
#' @inheritParams predict.tmb_analysis
#' @return The data with the fitted and residual values.
#' @seealso \code{\link[broom]{augment}}.
#' @export
augment.tmb_analysis <- function(x, ...) {
  fit <- fitted(x)
  residual <- residuals(x)
  fit %<>% bind_cols(residual[!names(residual) %in% names(fit)])
  fit
}
