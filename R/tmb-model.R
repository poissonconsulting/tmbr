check_select <- function(select) {
  if (!(is.character(select) || is_named_list(select)))
    error("select must be a character vector or named list specifying the columns and their associated classes and values")

  if (is.character(select)) {
    check_unique(select)
    return(TRUE)
  }
  check_unique(names(select))
  select
}

check_center <- function(center, select) {
  check_vector(center, "", min_length = 0)

  check_unique(center)

  if (length(select)) {
    if (is_named_list(select)) select %<>% names()
    if (!all(center %in% select)) error("columns in center must also be in select")
  }
  center
}

check_scale <- function(scale, select) {
  check_vector(scale, "", min_length = 0)

  check_unique(scale)

  if (length(select)) {
    if (is_named_list(select)) select %<>% names()
    if (!all(scale %in% select)) error("columns in scale must also be in select")
  }
  scale
}

check_inits <- function(inits) {
  if (!is_named_list(inits))
    error("inits must be a named list specifying the parameters and their starting values" )

  check_unique(names(inits))
  inits
}

check_random <- function(random, select, center, scale, inits) {
  if (!is.character(random) && !is_named_list(random))
    error("random must be a character vector or named list specifying the random effects and their associated factors" )

  if (!length(random)) return(random)

  if (is.character(random)) {
    check_unique(random)
    if (!all(random %in% names(inits))) error("random effects must also be in inits")
    return(random)
  }

 # random is a named list

  check_unique(names(random))

  if (!all(names(random) %in% names(inits))) error("random effects must also be in inits")

  class <- lapply(random, class) %>% unlist()

  if (!all(class == "character")) error("random effects factors must named as character vectors")

  if (length(select)) {
    if (!all(unlist(random) %in% select)) error("random effects factors must also be in select")
    if (any(unlist(random) %in% center)) error("random effects factors must not be centered")
    if (any(unlist(random) %in% scale)) error("random effects factors must not be scaled")
  }

  inits %<>% lapply(dims) %<>% lapply(length)
  inits <- inits[names(random)]
  if (!identical(inits, lapply(random, length))) error("random effects must have the same number of dimensions as corresponding inits")
  random
}

#' TMB Model
#'
#' Creates TMB model.
#'
#' With the exception of \code{model_code} and \code{select} all arguments are sorted.
#'
#' @param model_code A string of the model template code.
#' @param inits A named list of initial values for all fixed and random parameters.
#' @param select A character vector or a named list specifying the columns to select (and in the case of a named list the associated classes and values).
#' @param random A character vector or a named list specifying of the random effects (and in the case of a named list the associated factors).
#' @param modify A single argument function to modify the data (in list form) immediately prior to the analysis.
#' @inheritParams rescale::rescale
#' @return An object of class tmb_model.
#' @seealso \code{\link[datacheckr]{check_data}} \code{\link[rescale]{rescale}}
#' @export
tmb_model <- function(model_code, inits, select = character(0),
                      center = character(0), scale = character(0),
                      random = character(0), modify = function(x) x) {
  check_string(model_code)
  check_select(select)
  check_center(center, select)
  check_scale(scale, select)
  check_inits(inits)
  check_random(random, select, center, scale, inits)
  if (!is.function(modify)) error("modify must be a function")
  if (length(formals(modify)) != 1)  error("modify must take a single argument")

  obj <- list(model_code = model_code,
              inits = sort_by_names(inits),
              select = select,
              center = sort_by_names(center),
              scale = sort_by_names(scale),
              random = sort_by_names(random),
              modify = modify)
  class(obj) <- "tmb_model"
  obj
}
