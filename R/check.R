check_all_x_in_vector <- function(x, vector, x_name = substitute(x), vector_name = substitute(vector), elements_x = FALSE, elements_vector = FALSE) {
  if (is.name(x_name)) x_name %<>% deparse()
  if (is.name(vector_name)) vector_name %<>% deparse()

  if (is.null(vector)) return(x)
  if (!length(x)) return(x)

  stopifnot(is.vector(vector))

  elements_x <- ifelse(elements_x, "elements", "names")
  elements_vector <- ifelse(elements_vector, "elements", "names")

  if (!all(x %in% vector)) error(elements_x, " in ", x_name, " must also be in ", elements_vector, " of ", vector_name)
  x
}

check_no_x_in_vector <- function(x, vector, x_name = substitute(x), vector_name = substitute(vector),
                                 elements_x = FALSE, elements_vector = FALSE) {
  if (is.name(x_name)) x_name %<>% deparse()
  if (is.name(vector_name)) vector_name %<>% deparse()

  if (is.null(vector)) return(x)
  if (!length(x)) return(x)

  stopifnot(is.vector(vector))

  elements_x <- ifelse(elements_x, "elements", "names")
  elements_vector <- ifelse(elements_vector, "elements", "names")

  if (any(x %in% vector)) error(elements_x, " in ", x_name, " must not be in ", elements_vector, " of ", vector_name)
  x
}

check_single_arg_fun <- function(fun) {
  fun_name <- deparse(substitute(fun))

  if (!is.function(fun)) error(fun_name, " must be a function")
  if (length(formals(fun)) != 1)  error(fun_name, " must take a single argument")
  fun
}

check_unique_character_vector <- function(x, x_name = substitute(x)) {
  check_vector(x, "", min_length = 0, vector_name = x_name)
  check_unique(x, x_name = x_name)
  x
}

check_uniquely_named_list <- function(x, x_name = substitute(x)) {
  if (is.name(x)) x_name %<>% deparse()

  if (!is_named_list(x))
    error(x_name, " must be a named list")
  check_unique(names(x), x_name = x_name)
  x
}

check_all_elements_class_character <- function(x, x_name = substitute(x)) {
  if (is.name(x)) x_name %<>% deparse()

  if (!length(x)) return(x)

  if (!all(unlist(lapply(x, class)) == "character"))
    error("elements of ", x_name, "must be character vectors")
  x
}
