#' @export
estimates.tmb_analysis <- function(object, terms = "fixed", ...) {
  check_vector(terms, c("^fixed$", "^random$", "^report$", "^adreport$"), max_length = 1)

  if (terms == "report") return(sort_by_names(object$report))
  if (terms == "adreport") return(sort_by_names(list_by_name(object$sd$value)))

  if (terms == "fixed") {
    estimates <- object$sd$par.fixed
  } else
    estimates <- object$sd$par.random

  estimates %<>% list_by_name()
  inits <- lapply(object$inits, dims)
  inits <- inits[names(estimates)]
  estimates %<>% purrr::map2(inits, by_dims)
  estimates %<>% sort_by_names()
  estimates
}
