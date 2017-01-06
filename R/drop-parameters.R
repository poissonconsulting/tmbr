#' @export
drop_parameters.tmb_code <- function(x, parameters = character(0), ...) {
  check_vector(parameters, "", min_length = 0)
  check_unique(parameters)
  

  if (!length(parameters))
    return(x)

  template <- template(x)

  for (parameter in parameters) {
    if (!str_detect(template, str_c("PARAMETER[(]", parameter, "[)]")))
      error("fixed scalar parameter '", parameter, "' not found in model code")
    template %<>% str_replace(str_c("PARAMETER[(]", parameter, "[)]"),
                              str_c("Type ", parameter, " = 0.0"))
  }

  x$template <- template
  x
}

#' @export
drop_parameters.tmb_model <- function(x, parameters = character(0), ...) {
  check_vector(parameters, "", min_length = 0)
  check_unique(parameters)
  

  if (!length(parameters))
    return(x)

  x$code %<>% drop_parameters(parameters = parameters)
  x$new_expr %<>% drop_parameters(parameters = parameters)
  x
}
