#' Estimates
#'
#' estimates
#'
#' @param object The mb_analysis object.
#' @param fixed A flag specifying whether fixed or random terms.
#' @param mcmc A flag specifying whether to get the mcmc draws as oppsed to ml estimates.
#' @param ... Not used.
#' @export
estimates.mb_analysis <- function(object, fixed = TRUE, mcmc = FALSE, ...) {
  check_flag(fixed)
  

  if (mcmc) NextMethod()

  if (fixed) {
    estimates <- object$sd$par.fixed
  } else
    estimates <- object$sd$par.random

  estimates %<>% list_by_name()
  estimates %<>% remap_estimates(object$map)
  inits <- object$inits[names(estimates)]
  inits %<>% lapply(dims)
  estimates %<>% purrr::map2(inits, by_dims)
#}
  if (scalar_only) estimates %<>% scalar_nlist()
  estimates %<>% sort_nlist()
  estimates
}
