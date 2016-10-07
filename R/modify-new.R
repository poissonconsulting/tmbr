#' Modify New Data
#'
#' Gets the modify_new_data function for an object.
#'
#' @param x The object.
#'
#' @return The modify_new_data function.
#' @export
modify_new_data <- function(x) {UseMethod("modify_new_data")}

#' @export
modify_new_data.tmb_model <- function(x) x$modify_new_data

#' @export
modify_new_data.tmb_analysis <- function(x) modify_new_data(x$model)
