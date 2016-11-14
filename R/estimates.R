#' @export
estimates.tmb_analysis <- function(object, terms = "fixed", scalar_only = FALSE, ...) {
  check_vector(terms, c("^fixed$", "^random$", "^report$", "^adreport$"), max_length = 1)
  check_flag(scalar_only)

  if (terms == "report") {
    estimates <- object$report
    estimates %<>% remap_estimates(object$map)
  } else if (terms == "adreport") {
    estimates <- list_by_name(object$sd$value)
    estimates %<>% remap_estimates(object$map)
  } else {
    if (terms == "fixed") {
      estimates <- object$sd$par.fixed
    } else
      estimates <- object$sd$par.random

    estimates %<>% list_by_name()
    estimates %<>% remap_estimates(object$map)
    inits <- object$inits[names(estimates)]
    inits %<>% lapply(dims)
    estimates %<>% purrr::map2(inits, by_dims)
  }
  if (scalar_only) estimates %<>% scalar_nlist()
  estimates %<>% sort_nlist()
  estimates
}
