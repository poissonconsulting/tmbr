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
  if (any(names(inits) %in% c("all", "both", "fixed", "random", "report")))
    error("inits names cannot be 'all', 'both', 'fixed', 'random' or 'report'")
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
#' @param model_code A string of the model template code.
#' @param inits A named list of initial values for all fixed and random parameters.
#' @param random A character vector or a named list specifying of the random effects (and in the case of a named list the associated factors).
#' @param select A character vector or a named list specifying the columns to select (and in the case of a named list the associated classes and values).
#' @inheritParams rescale::rescale
#' @param modify A single argument function to modify the data (in list form) immediately prior to the analysis.
#' @param predict_code A string of the R code specifying the predictive relationships.
#' @param modify_new A single argument function to modify the new_data (in list form) immediately prior to the predictions.
#' @return An object of class tmb_model.
#' @seealso \code{\link[datacheckr]{check_data}} \code{\link[rescale]{rescale}}
#' @export
tmb_model <- function(
  model_code, inits, random = character(0), select = character(0),
  center = character(0), scale = character(0), modify = function(x) x,
  predict_code = character(0), modify_new = modify)
{
  check_string(model_code)
  check_select(select)
  check_center(center, select)
  check_scale(scale, select)
  check_inits(inits)
  check_random(random, select, center, scale, inits)
  check_vector(predict_code, "", min_length = 0, max_length = 1)

  if (!is.function(modify)) error("modify must be a function")
  if (length(formals(modify)) != 1)  error("modify must take a single argument")

  if (!is.function(modify_new)) error("modify_new must be a function")
  if (length(formals(modify_new)) != 1)  error("modify_new must take a single argument")

  obj <- list(model_code = model_code,
              inits = sort_by_names(inits),
              select = select,
              center = sort_by_names(center),
              scale = sort_by_names(scale),
              random = sort_by_names(random),
              modify = modify,
              predict_code = predict_code,
              modify_new = modify_new)
  class(obj) <- "tmb_model"
  obj
}
