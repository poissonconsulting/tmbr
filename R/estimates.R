#' @export
estimates.tmb_analysis <- function(object, terms = "fixed", scalar = FALSE, ...) {
  check_vector(terms, c("^fixed$", "^random$", "^report$", "^adreport$"), max_length = 1)
  check_flag(scalar)

  if (terms == "report") {
    estimates <- object$report
  } else if (terms == "adreport") {
    estimates <- list_by_name(object$sd$value)
  } else {
    if (terms == "fixed") {
      estimates <- object$sd$par.fixed
    } else
      estimates <- object$sd$par.random

    estimates %<>% list_by_name()
    inits <- object$inits[names(estimates)]
    inits %<>% lapply(dims)
    estimates %<>% purrr::map2(inits, by_dims)
  }
  if (scalar) estimates %<>% scalar_nlist()
  estimates %<>% sort_nlist()
  estimates
}
