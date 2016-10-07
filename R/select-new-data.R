#' Select New Data
#'
#' Gets the select_new_data object for an object.
#'
#' @param x The object.
#'
#' @return The select_new_data object
#' @export
select_new_data <- function(x) {UseMethod("select_new_data")}

#' @export
select_new_data.tmb_model <- function(x) x$select_new_data

#' @export
select_new_data.tmb_analysis <- function(x) select_new_data(model(x))
