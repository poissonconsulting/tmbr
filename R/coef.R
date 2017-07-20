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
  dims %<>% llply(seq_to)
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

c_name <- function(x) {
  x[[1]] %<>% stringr::str_c(names(x), .)
  x
}

par_names_indices <- function(estimates) {
  estimates %<>% llply(dims) %>% llply(dims_to_dimensions_vector)
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

# Remaps vector x based on missing values in y.
remap_vector <- function(x, y) {
  stopifnot(length(x) == sum(!is.na(y)))
  y %<>% as.numeric()
  y[!is.na(y)] <- x
  y[is.na(y)] <- 0
  y
}

# Adds fixed values to estimates.
# estimates A named list of the estimates to add as 0 values that were fixed.
# A named list indicating the fixed values.
remap_estimates <- function(estimates, map) {
  check_uniquely_named_list(estimates)
  check_uniquely_named_list(map)
  if (!length(map)) return(estimates)
  map <- map[names(map) %in% names(estimates)]
  if (!length(map)) return(estimates)

  estimates[names(map)] %<>% purrr::map2(map, remap_vector)
  estimates
}

terms_internal <- function(param_type, object) {
  if (param_type == "derived") {
    estimates <- list_by_name(object$sd$value) %>%
      remap_estimates(object$map) %>%
      sort_nlist() %>%
      named_estimates() %>%
      names() # derived names should not be sorted like primary names
  } else {
    if (param_type == "fixed") {
      estimates <- object$sd$par.fixed
    } else if (param_type == "random") {
      estimates <- object$sd$par.random
    } else stop()

    estimates %<>%
      list_by_name() %>%
      remap_estimates(object$map)
    inits <- object$inits[names(estimates)]
    inits %<>% llply(dims)
    estimates %<>%
      purrr::map2(inits, by_dims) %>%
      sort_nlist() %>%
      named_estimates() %>%
      names()
    if (!is.null(estimates)) estimates %<>% sort()
  }
  estimates
}

remap_data <- function(x, y) {
  stopifnot(nrow(x) == sum(!is.na(y)))
  x$constant <- FALSE
  z <- dplyr::data_frame(term = "", estimate = 0, sd = 0, zscore = NaN,
                         lower = 0, upper = 0, pvalue = 0, constant = TRUE)
  z <- z[rep(1,length(y)),]
  z[!is.na(y),] <- x
  z
}

# Adds fixed values to coef table.
remap_coef <- function(coef, map) {
  check_uniquely_named_list(map)
  coef$constant <- FALSE

  if (!length(map)) return(coef)
  map <- map[names(map) %in% unique(coef$term)]
  if (!length(map)) return(coef)

  coef %<>% plyr::dlply(.variables = c("term"))
  coef[names(map)] %<>% purrr::map2(map, remap_data)
  coef %<>% plyr::ldply()
  coef %<>% dplyr::mutate_(.id = ~NULL)
  coef
}

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
  check_scalar(param_type, c("fixed", "random", "derived", "primary", "all"))
  check_flag(include_constant)
  check_number(conf_level, c(0.5, 0.99))

  if (param_type %in% c("primary", "all")) {
    coef <- c("fixed", "random")
    if (param_type == "all") coef %<>% c("derived")

    coef %<>%  purrr::map_df(coef_arg2to1, object = object,
                             include_constant = include_constant,
                             conf_level = conf_level, ...)
    coef$term %<>% as.term()
    coef <- coef[order(coef$term),]
    class(coef) %<>% c("mb_analysis_coef", .)
    return(coef)
  }

  terms <- terms_internal(param_type, object)

  if (!length(terms)) {
    coef <- dplyr::data_frame(term = as.term(character(0)), estimate = numeric(0), sd = numeric(0),
                              zscore = numeric(0), lower = numeric(0),
                              upper = numeric(0), pvalue = numeric(0))
    class(coef) %<>% c("mb_analysis_coef", .)
    return(coef)
  }

  if (param_type == "derived") param_type <- "report"

  coef <- summary(object$sd, select = param_type, p.value = TRUE) %>%
    as.data.frame()

  coef %<>% dplyr::mutate_(term = ~row.names(coef))

  coef %<>%
    dplyr::select_(term = ~term, estimate = ~Estimate, sd = ~`Std. Error`,
                   zscore = ~`z value`, pvalue = ~`Pr(>|z^2|)`) %>%
    dplyr::mutate_(lower = ~estimate + sd * qnorm((1 - conf_level) / 2),
                   upper = ~estimate + sd * qnorm((1 - conf_level) / 2 + conf_level)) %>%
    dplyr::select_(~term, ~estimate, ~sd, ~zscore, ~lower, ~upper, ~pvalue) %>%
    dplyr::arrange_(~term) %>%
    remap_coef(object$map)

  coef$term <- as.term(terms)

  if (!include_constant) coef %<>% dplyr::filter_(~!constant)
  coef %<>%
    dplyr::mutate_(constant = ~NULL) %>%
    dplyr::as.tbl()

  coef <- coef[order(coef$term),]

  class(coef) %<>% c("mb_analysis_coef", .)
  coef
}
