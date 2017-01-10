#' @export
parameters.tmb_code <- function(x, param_type = "fixed", scalar = TRUE, ...) {
  check_scalar(param_type, c("fixed", "random", "derived"))
  check_flag(scalar)

  x %<>% template() %>% str_replace_all(" ", "")

  if (param_type == "derived") {
    x %<>% str_extract_all("\\s(ADREPORT)[(]\\w+[)]", simplify = TRUE)
  } else {# can't separate fixed and random from tmb code but can separate scalar
    if(scalar) {
      x %<>% str_extract_all("\\s(PARAMETER)[(]\\w+[)]", simplify = TRUE)
    } else {
      x %<>% str_extract_all("\\s(PARAMETER(_VECTOR|_MATRIX|_ARRAY))[(]\\w+[)]", simplify = TRUE)
    }
  }
  x %<>% as.vector() %>% str_replace_all("\\s\\w+[(](\\w+)[)]", "\\1") %>% sort()
  x
}
