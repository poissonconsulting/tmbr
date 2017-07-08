#' @export
parameters.tmb_code <- function(x, param_type = "fixed", scalar = TRUE, ...) {
  check_scalar(param_type, c("fixed", "random", "derived", "primary", "all"))
  check_flag(scalar)

  if (param_type %in% c("primary", "all")) {
    parameters <- c("fixed") # can't currently separate fixed and random from tmb code but can separate scalar
    if (param_type == "all") parameters %<>% c("derived")

    parameters %<>%
      purrr::map(parameters_arg2to1, x = x, scalar = scalar, ...) %>%
      unlist() %>%
      sort()

    return(parameters)
  }

  x %<>% template() %>% str_replace_all(" ", "")

  # ignore REPORT parameters as easily generate using predict
  if (param_type == "derived") { # can't deterimne which derived parameters are scalar
    x %<>% str_extract_all("\\s(ADREPORT)[(]\\w+[)]", simplify = TRUE)
  } else {# can't currently separate fixed and random from tmb code but can separate scalar
    if(scalar) {
      x %<>% str_extract_all("\\s(PARAMETER)[(]\\w+[)]", simplify = TRUE)
    } else {
      x %<>% str_extract_all("\\s(PARAMETER(_VECTOR|_MATRIX|_ARRAY))[(]\\w+[)]", simplify = TRUE)
    }
  }
  x %<>%
    as.vector() %>%
    str_replace_all("\\s\\w+[(](\\w+)[)]", "\\1") %>%
    sort()
  x
}
