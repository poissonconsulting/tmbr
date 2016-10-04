#' Predict
#'
#' Calculate predictions.
#'
#' @param object The tmb_analysis object.
#' @param new_data The data frame to calculate the predictions for.
#' @param term A string of the term in predict_code.
#' @param predict_code A string of the R code specifying the predictive relationships.
#' @param modify_new A single argument function to modify the new_data (in list form) immediately prior to the predictions.
#' @param profile A flag indicating whether to calculate the predictions using likelihood profiling.
#' @inheritParams coef.tmb_analysis
#' @return The newdata with the predictions.
#' @export
predict.tmb_analysis <- function(
  object, new_data = data_set(object), term = "prediction",
  predict_code = predict_code(object), modify_new = modify_new(object),
  profile = FALSE, conf_level = 0.95,  ...) {
  #check_data.....

  check_string(term)
  check_string(predict_code)

  if (!is.function(modify_new)) error("modify_new must be a function")
  if (length(formals(modify_new)) != 1)  error("modify_new must take a single argument")

  check_flag(profile)
  check_number(conf_level, c(0.5, 0.99))

  analysis_data <- process_data(data_set, data_set2 = data_set(object),
                                model(object), fun = modify_new)
  data_set
}
