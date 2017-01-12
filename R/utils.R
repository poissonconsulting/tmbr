list_by_name <- function(x) {
  list <- list()
  names <- unique(names(x))
  for (name in names) {
    list %<>% c(list(unname(x[names(x) == name])))
  }
  names(list) <- names
  list
}

dprint <- function(x, note = NULL, do = getOption("dprint.do", TRUE)) {
  if (!do) return(invisible())
  if (!is.null(note))
    cat("\n**", note, "**\n")
  cat("\n", deparse(substitute(x)), ": \n", sep = "")
  print(x)
}
