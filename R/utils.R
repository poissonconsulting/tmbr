is.try_error <- function(x) inherits(x, "try-error")

is.sdreport <- function(x) inherits(x, "sdreport")

list_by_name <- function(x) {
  list <- list()
  names <- unique(names(x))
  for (name in names) {
    list %<>% c(list(unname(x[names(x) == name])))
  }
  names(list) <- names
  list
}

parameters_arg2to1 <- function(param_type, x, scalar, ...) {
  parameters(x = x, param_type = param_type, scalar = scalar, ...)
}

coef_arg2to1 <- function(param_type, object, include_constant, conf_level, ...) {
  coef(object, param_type = param_type, include_constant = include_constant, conf_level = conf_level, ...)
}

dprint <- function(x, note = NULL, do = getOption("dprint.do", TRUE)) {
  if (!do) return(invisible())
  if (!is.null(note))
    cat("\n**", note, "**\n")
  cat("\n", deparse(substitute(x)), ": \n", sep = "")
  print(x)
}
