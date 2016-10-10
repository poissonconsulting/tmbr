any_blank <- function(x) {
  return(any(x == ""))
}

by_dims <- function(x, dims) {
  stopifnot(is.vector(x))
  stopifnot(is.integer(dims))
  if (length(dims) == 1) return(x)
  if (length(dims) == 2) return(as.matrix(x, nrow = dims[1], ncol = dims[2]))
  return(array(x, dim = dims))
}

dims <- function(x) if (is.vector(x)) length(x) else dim(x)

error <- function(...) stop(..., call. = FALSE)

is_named <- function(x) {
  !is.null(names(x))
}

is_named_list <- function(x) {
  is.list(x) && (is_named(x) || !length(x))
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

#' Is a TMB Model?
#'
#' Tests wether x is an object of class 'tmb_model'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_model <- function(x) {
  inherits(x, "tmb_model")
}

#' Is a TMB Analysis?
#'
#' Tests wether x is an object of class 'tmb_analysis'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.tmb_analysis <- function(x) {
  inherits(x, "tmb_analysis")
}

replace_values <- function(string, list) {
  for (i in seq_along(list)) {
    pattern <- names(list)[i]
    pattern %<>% str_c("(^|(?<!\\w))", ., "((?!\\w)|$)")
    string %<>% str_replace_all(pattern, list[i])
  }
  string
}

parse_string <- function(string) {
  string <- str_split(string, "\\s*[+]\\s*")[[1]] %>% str_split("\\s*[*]\\s*") %>% lapply(str_trim)
  string
}

sort_by_names <- function(x) {
  stopifnot(is_named_list(x))
  if(!length(x)) return(x)
  x[order(names(x))]
}
