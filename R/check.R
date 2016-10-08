check_select_data <- function(select_data) {
  name <- deparse(substitute(select_data))

  if (!(is.character(select_data) || is_named_list(select_data)))
    error(name, " must be a character vector or named list specifying the columns and their associated classes and values")

  if (is.character(select_data)) {
    check_unique(select_data, name)
    return(TRUE)
  }
  check_unique(names(select_data), name)
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

check_modify_data <- function(modify_data) {

  name <- deparse(substitute(modify_data))

  if (!is.function(modify_data)) error(name, " must be a function")
  if (length(formals(modify_data)) != 1)  error(name, " must take a single argument")
  modify_data
}
