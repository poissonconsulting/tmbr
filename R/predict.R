#' Predict
#'
#' Calculate predictions.
#'
#' @param object An object inheriting from class mb_analysis.
#' @param mcmc A flag specifying whether to get the mcmc draws as oppsed to ml estimates.
#' @param new_data The data frame to calculate the predictions for.
#' @inheritParams mbr::predict_data
#' @export
predict.tmb_analysis <- function(object,
                                new_data = data_set(object),
                                new_expr = NULL,
                                new_values = list(),
                                term = "prediction",
                                conf_level = 0.95,
                                modify_new_data = NULL,
                                mcmc = FALSE,
                                parallel = getOption("mb.parallel", FALSE),
                                quick = getOption("mb.quick", FALSE),
                                quiet = getOption("mb.quiet", TRUE),
                                beep = getOption("mb.beep", FALSE),
                                ...) {
  check_flag(mcmc)

  if (!mcmc) object$mcmcr <- object$lmcmcr

  NextMethod()
}
