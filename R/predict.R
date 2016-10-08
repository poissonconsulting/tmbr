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
#' @param profile A flag indicating whether to calculate the predictions using likelihood profiling.
#' @inheritParams coef.tmb_analysis
#' @return The new data with the predictions.
#' @export
predict.tmb_analysis <- function(
  object, new_data = data_set(object), term = "prediction",
  new_code = new_code(object), select_new_data = select_new_data(object),
  modify_new_data = modify_new_data(object),
  profile = FALSE, conf_level = 0.95,  ...) {
  check_data1(new_data)
  check_string(term)
  check_string(new_code)
  check_select_data(select_new_data)
  check_modify_data(modify_new_data)
  check_flag(profile)
  check_number(conf_level, c(0.5, 0.99))

  if (profile) error("profile predicting is not currently implemented")

  data <- process_data(new_data, data2 = data_set(object),
                             select_data = select_new_data,
                             center = model$center, scale = model$scale,
                             random_effects = model$random_effects,
                             modify_data = modify_new_data)

  data %<>% c(inits(object))

  # no likelihood profiling so no confidence intervals
  if (!profile) {
    print(data)
#    with(data)
  }
  data
}
