check_select_data <- function(select_data) {
  if (!(is.character(select_data) || is_named_list(select_data)))
    error("select_data must be a character vector or named list specifying the columns and their associated classes and values")

  if (is.character(select_data)) {
    check_unique(select_data)
    return(TRUE)
  }
  check_unique(names(select_data))
  select_data
}

check_center <- function(center, select_data) {
  check_vector(center, "", min_length = 0)

  check_unique(center)

  if (length(select_data)) {
    if (is_named_list(select_data)) select_data %<>% names()
    if (!all(center %in% select_data)) error("columns in center must also be in select_data")
  }
  center
}

check_scale <- function(scale, select_data) {
  check_vector(scale, "", min_length = 0)

  check_unique(scale)

  if (length(select_data)) {
    if (is_named_list(select_data)) select_data %<>% names()
    if (!all(scale %in% select_data)) error("columns in scale must also be in select_data")
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

check_random_effects <- function(random_effects, select_data, center, scale, inits) {
  if (!is.character(random_effects) && !is_named_list(random_effects))
    error("random_effects must be a character vector or named list specifying the random effects and their associated factors" )

  if (!length(random_effects)) return(random_effects)

  if (is.character(random_effects)) {
    check_unique(random_effects)
    if (!all(random_effects %in% names(inits))) error("random effects must also be in inits")
    return(random_effects)
  }

 # random_effects is a named list

  check_unique(names(random_effects))

  if (!all(names(random_effects) %in% names(inits))) error("random effects must also be in inits")

  class <- lapply(random_effects, class) %>% unlist()

  if (!all(class == "character")) error("random effects factors must named as character vectors")

  if (length(select_data)) {
    if (!all(unlist(random_effects) %in% select_data)) error("random effects factors must also be in select_data")
    if (any(unlist(random_effects) %in% center)) error("random effects factors must not be centered")
    if (any(unlist(random_effects) %in% scale)) error("random effects factors must not be scaled")
  }

  inits %<>% lapply(dims) %<>% lapply(length)
  inits <- inits[names(random_effects)]
  if (!identical(inits, lapply(random_effects, length))) error("random effects must have the same number of dimensions as corresponding inits")
  random_effects
}

#' TMB Model
#'
#' Creates TMB model.
#'
#' @param model_code A string of the model template code.
#' @param inits A named list of initial values for all fixed and random parameters.
#' @param random_effects A character vector or a named list specifying of the random effects (and in the case of a named list the associated factors).
#' @param select_data A character vector or a named list specifying the columns to select (and in the case of a named list the associated classes and values).
#' @inheritParams rescale::rescale
#' @param modify_data A single argument function to modify the data (in list form) immediately prior to the analysis.
#' @param new_code A string of the R code specifying the predictive relationships.
#' @param modify_new_data A single argument function to modify the new_data (in list form) immediately prior to the predictions.
#' @return An object of class tmb_model.
#' @seealso \code{\link[datacheckr]{check_data}} \code{\link[rescale]{rescale}}
#' @export
tmb_model <- function(
  model_code, inits, random_effects = character(0), select_data = character(0),
  center = character(0), scale = character(0), modify_data = function(x) x,
  new_code = character(0), modify_new_data = modify_data)
{
  check_string(model_code)
  check_select_data(select_data)
  check_center(center, select_data)
  check_scale(scale, select_data)
  check_inits(inits)
  check_random_effects(random_effects, select_data, center, scale, inits)
  check_vector(new_code, "", min_length = 0, max_length = 1)

  if (!is.function(modify_data)) error("modify_data must be a function")
  if (length(formals(modify_data)) != 1)  error("modify_data must take a single argument")

  if (!is.function(modify_new_data)) error("modify_new_data must be a function")
  if (length(formals(modify_new_data)) != 1)  error("modify_new_data must take a single argument")

  obj <- list(model_code = model_code,
              inits = sort_by_names(inits),
              select_data = select_data,
              center = sort_by_names(center),
              scale = sort_by_names(scale),
              random_effects = sort_by_names(random_effects),
              modify_data = modify_data,
              new_code = new_code,
              modify_new_data = modify_new_data)
  class(obj) <- "tmb_model"
  obj
}
