dprint <- function(x, note = NULL, do = getOption("dprint.do", TRUE)) {
  if (!do) return(invisible())
  if (!is.null(note))
    cat("\n**", note, "**\n")
  cat("\n", deparse(substitute(x)), ": \n", sep = "")
  print(x)
}
