seq_to <- function(to) {
  seq(from = 1, to = to)
}

paste_rows <- function(x) {
  x %<>% unlist()
  x %<>% stringr::str_c(collapse = ",")
  data.frame(x = x)
}

dims_to_dimensions_vector <- function(dims) {
  if (identical(dims, 1L)) return("")
  dims %<>% lapply(seq_to)
  dims %<>% expand.grid()
  dims %<>% plyr::adply(1, paste_rows)
  dims <- dims$x
  dims %<>% stringr::str_c("[", ., "]")
  dims
}

by_dims <- function(x, dims) {
  stopifnot(is.vector(x))
  stopifnot(is.integer(dims))
  stopifnot(length(x) == prod(dims))
  if (length(dims) == 1) return(x)
  if (length(dims) == 2) return(matrix(x, nrow = dims[1], ncol = dims[2]))
  return(array(x, dim = dims))
}

list_by_name <- function(x) {
  list <- list()
  names <- unique(names(x))
  for (name in names) {
    list %<>% c(list(unname(x[names(x) == name])))
  }
  names(list) <- names
  list
}

#' Estimates
#'
#' estimates
#'
#' @param object The mb_analysis object.
#' @param fixed A flag specifying whether fixed or random terms.
#' @param mcmc A flag specifying whether to get the mcmc draws as oppsed to ml estimates.
#' @param ... Not used.
#' @export
estimates.tmb_analysis <- function(object, fixed = TRUE, mcmc = FALSE, ...) {
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

  estimates %<>% sort_nlist()
  estimates
}
