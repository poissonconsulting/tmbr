#' @export
parameters.tmb_code <- function(x, ...) {
  x %<>% template() %>% str_replace_all(" ", "")

  x %<>% str_extract_all("\\sPARAMETER(|_VECTOR|_MATRIX|_ARRAY)[(]\\w+[)]", simplify = TRUE)
  x %<>% as.vector() %>% str_replace_all("\\s\\w+[(](\\w+)[)]", "\\1") %>% sort()
  x
}

#' @export
parameters.tmb_analysis <- function(x, fixed = TRUE, mcmc = FALSE, ...) {
  check_flag(fixed)
  check_flag(mcmc)

  if (mcmc) NextMethod()

  random <- names(random_effects(x))

  if (!fixed) return(random)

  parameters <- parameters(code(x))

  setdiff(parameters, random)
}
