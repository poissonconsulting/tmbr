pars_arg2to1 <- function(param_type, x, scalar_only, ...) {
  pars(x = x, param_type = param_type, scalar_only = scalar_only, ...)
}

#' @export
pars.tmb_code <- function(x, param_type = "all", scalar_only = FALSE, ...) {
  check_vector(param_type, c("fixed", "random", "derived", "primary", "all"),
               length = 1)
  check_flag(scalar_only)

  if (param_type %in% c("fixed", "random"))
    error("parameters.tmb_code is not currently able to separate 'fixed' or 'random' parameter types - set param_type = 'primary' instead")

  if (param_type == "derived" && scalar_only)
    error("parameters.tmb is not currently able to identify scalar 'derived' parameter types - set scalar_only = FALSE instead")

  if (param_type == "all") {
    pars <- c("primary", "derived")

    pars %<>%
      purrr::map(pars_arg2to1, x = x, scalar_only = scalar_only, ...) %>%
      unlist() %>%
      sort()

    return(pars)
  }

  x %<>% template() %>% str_replace_all(" ", "")

  if (param_type == "primary") {
    if (scalar_only) {
      x %<>% str_extract_all("\\s(PARAMETER)[(]\\w+[)]", simplify = TRUE)
    } else
      x %<>% str_extract_all("\\s(PARAMETER(|_VECTOR|_MATRIX|_ARRAY))[(]\\w+[)]", simplify = TRUE)
  } else # ignore REPORT parameters as easily generate using predict
    x %<>% str_extract_all("\\s(ADREPORT)[(]\\w+[)]", simplify = TRUE)

  x %<>%
    as.vector() %>%
    str_replace_all("\\s\\w+[(](\\w+)[)]", "\\1") %>%
    sort()
  x
}
