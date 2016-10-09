#' Predict
#'
#' Calculate predictions.
#'
#' @param object The tmb_analysis object.
#' @param new_data The data frame to calculate the predictions for.
#' @param term A string of the term in new_expr.
#' @inheritParams tmb_model
#' @param ... Unused.
#' @return The new data with the predictions.
#' @export
predict.tmb_analysis <- function(
  object, new_data = data_set(object), term = "prediction",
  new_expr = NULL, select_new_data = NULL, modify_new_data = NULL, ...) {

  model <- model(object)

  if (is.null(new_expr)) new_expr <- model$new_expr
  if (is.null(select_new_data)) select_new_data <- model$select_new_data
  if (is.null(modify_new_data)) modify_new_data <- model$modify_new_data

  center <- model$center
  scale <- model$scale
  random_effects <- model$random_effects

  check_data1(new_data)
  check_string(term)
  check_string(new_expr)
  check_uniquely_named_list(select_new_data)
  check_single_arg_fun(modify_new_data)

  check_x_in_y(center, names(select_new_data), y_name = "select_new_data", type_y = "names")
  check_x_in_y(scale, names(select_new_data), y_name = "select_new_data", type_y = "names")
  check_x_in_y(unlist(random_effects), names(select_new_data),
               x_name = "random_effects", y_name = "select_new_data",
               type_x = "elements", type_y = "names")
  check_x_not_in_y(unlist(random_effects), center, x_name = "random_effects", type_x = "elements")
  check_x_not_in_y(unlist(random_effects), scale, x_name = "random_effects", type_x = "elements")

  data <- process_data(new_data, data2 = data_set(object),
                       select_data = select_new_data,
                       center = center, scale = scale,
                       random_effects = random_effects,
                       modify_data = modify_new_data)

  data %<>% c(object$opt$par)
  new_expr %<>% parse(text = .)

  vars <- all.vars(new_expr)
  data[vars[!vars %in% names(data)]] <- NA

  data %<>% within(eval(new_expr))

  if (!is.vector(data[[term]])) {
    error("term '", term, "' in new code must be a vector")
  }
  if (!length(data[[term]]) %in% c(1, nrow(new_data))) {
    error("term '", term, "' in new code must be a scalar or a vector of length ", nrow(new_data))
  }

  new_data[term] <- data[[term]]
  new_data
}
