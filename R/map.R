#' Map
#'
#' Constructs a list identifying which parameters to fix (based on missing values in inits).
#'
#' @param inits A named list of initial values.
#' @seealso \code{\link[TMB]{MakeADFun}}
map <- function(inits) {
  check_uniquely_named_list(inits)

  if (!length(inits)) return(list())

  inits <- inits[vapply(inits, function(x) (any(is.na(x))), TRUE)]

  if (!length(inits)) return(list())

  inits %<>% lapply(as.vector)
  map <- lapply(inits, function(x) 1:length(x))
  map <- purrr::map2(map, inits, function(x, y) {is.na(x) <- is.na(y); x})
  map %<>% lapply(function(x) factor(x))
  map
}

remap_data <- function(x, y) {
  stopifnot(nrow(x) == sum(!is.na(y)))
  x$constant <- FALSE
  z <- dplyr::data_frame(term = "", estimate = 0, sd = 0, zscore = NaN,
                        lower = 0, upper = 0, significance = 0, constant = TRUE)
  z <- z[rep(1,length(y)),]
  z[!is.na(y),] <- x
  z
}

#' Remap Coef
#'
#' Adds fixed values to coef table.
#'
#' @param coef A coef table to add as 0 values that were fixed.
#' @param map A named list indicating the fixed values.
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

