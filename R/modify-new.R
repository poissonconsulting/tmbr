#' Modify New
#'
#' Gets the modify_new function for an object.
#'
#' @param x The object.
#'
#' @return The modify_new function.
#' @export
modify_new <- function(x) {UseMethod("modify_new")}

#' @export
modify_new.tmb_model <- function(x) x$modify_new

#' @export
modify_new.tmb_analysis <- function(x) modify_new(x$model)
