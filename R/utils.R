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

dprint <- function(x, note = NULL, do = getOption("dprint.do", TRUE)) {
  if (!do) return(invisible())
  if (!is.null(note))
    cat("\n**", note, "**\n")
  cat("\n", deparse(substitute(x)), ": \n", sep = "")
  print(x)
}

#' Parallel lapply on chunks
#'
#' Applys function FUN to entire list X or possibly on chunks of X in parallel.
#'
#' @inheritParams base::lapply
#' @param .parallel A flag indicating whether to analyse chunks in parallel.
#' @export
analyse_chunks <- function(X, FUN, .parallel = TRUE, ...) {
  if (!is.list(X)) error("X must be a list")

  if (!length(X)) return(X)

  nworkers <- foreach::getDoParWorkers()

  if (!.parallel || nworkers == 1 || length(X) == 1) {
    return(FUN(X, ...))
  }

  names <- names(X)
  indices <- parallel::splitIndices(length(X), nworkers)
  print(X)
  print(indices)
  X <- plyr::llply(indices, function(i, x) x[i], x = X)
  print("ou")
  print(X)
  X %<>% plapply(FUN = FUN, .parallel = TRUE, ...)
  print("after")
  print(X)
  print(length(X))
  print(lapply(X, length))
  X %<>% unlist(recursive = FALSE)
  print(X)
  print(length(X))
  names(X) <- names
  X
}
