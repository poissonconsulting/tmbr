#' @export
drop_pars.tmb_code <- function(x, pars = character(0), ...) {
  check_vector(pars, "")
  check_unique(pars)

  if (!length(pars)) return(x)

  template <- template(x)

  for (parameter in pars) {
    if (!str_detect(template, str_c("PARAMETER[(]", parameter, "[)]")))
      error("fixed scalar parameter '", parameter, "' not found in model code")
    template %<>% str_replace(str_c("PARAMETER[(]", parameter, "[)]"),
                              str_c("Type ", parameter, " = 0.0"))
  }

  x$template <- template
  x
}
