pars_arg2to1 <- function(param_type, x, scalar, ...) {
  pars(x = x, param_type = param_type, scalar = scalar, ...)
}

#' @export
pars.tmb_code <- function(x, param_type = "all", scalar = NA, ...) {
  chk_string(param_type)
  chk_subset(param_type, c("fixed", "random", "derived", "primary", "all"))
  chk_lgl(scalar)
  chk_unused(...)

  if (param_type %in% c("fixed", "random"))
    error("parameters.tmb_code is not currently able to separate 'fixed' or 'random' parameter types - set param_type = 'primary' instead")

  if (param_type == "derived" && !is.na(scalar))
    error("parameters.tmb is not currently able to identify scalar 'derived' parameter types - set scalar = NA instead")

  if (param_type == "all") {
    pars <- c("primary", "derived")

    pars %<>%
      purrr::map(pars_arg2to1, x = x, scalar = scalar, ...) %>%
      unlist() %>%
      sort()

    return(pars)
  }

  x %<>% template() %>% str_replace_all(" ", "")

  if (param_type == "primary") {
    if(is.na(scalar)) {
      x %<>% str_extract_all("\\s(PARAMETER(|_VECTOR|_MATRIX|_ARRAY))[(]\\w+[)]", simplify = TRUE)
    } else if(scalar) {
      x %<>% str_extract_all("\\s(PARAMETER)[(]\\w+[)]", simplify = TRUE)
    } else
      x %<>% str_extract_all("\\s(PARAMETER(_VECTOR|_MATRIX|_ARRAY))[(]\\w+[)]", simplify = TRUE)
  } else # ignore REPORT parameters as easily generate using predict
    x %<>% str_extract_all("\\s(ADREPORT)[(]\\w+[)]", simplify = TRUE)

  x %<>%
    as.vector() %>%
    str_replace_all("\\s\\w+[(](\\w+)[)]", "\\1") %>%
    sort()
  x
}
