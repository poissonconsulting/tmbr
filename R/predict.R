#' Predict
#'
#' Calculate predictions.
#'
#' @param object The tmb_analysis object.
#' @param new_data The data frame to calculate the predictions for.
#' @param term A string of the term in new_code.
#' @param new_code A string of the R code specifying the predictive relationships.
#' @param select_new_data A character vector or a named list specifying the columns in a new data frame to select (and in the case of a named list the associated classes and values).
#' @param modify_new_data A single argument function to modify the new_data (in list form) immediately prior to the predictions.
#' @param ... Unused.
#' @return The new data with the predictions.
#' @export
predict.tmb_analysis <- function(
  object, new_data = data_set(object), term = "prediction",
  new_code = NULL, select_new_data = NULL, modify_new_data = NULL, ...) {

  model <- model(object)

  if (is.null(new_code)) new_code <- model$new_code
  if (is.null(select_new_data)) select_new_data <- model$select_new_data
  if (is.null(modify_new_data)) modify_new_data <- model$modify_new_data

  check_data1(new_data)
  check_string(term)
  check_string(new_code)
  check_select_data(select_new_data)
  check_modify_data(modify_new_data)

  data <- process_data(new_data, data2 = data_set(object),
                       select_data = select_new_data,
                       center = model$center, scale = model$scale,
                       random_effects = model$random_effects,
                       modify_data = modify_new_data)

  data %<>% c(object$opt$par)
  new_code %<>% parse(text = .)

  vars <- all.vars(new_code)
  data[vars[!vars %in% names(data)]] <- NA

  data %<>% within(eval(new_code))

  if (!is.vector(data[[term]])) {
    error("term '", term, "' in new code must be a vector")
  }
  if (!length(data[[term]]) %in% c(1, nrow(new_data))) {
    error("term '", term, "' in new code must be a scalar or a vector of length ", nrow(new_data))
  }

  new_data[term] <- data[[term]]
  new_data
}
