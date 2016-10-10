#' Predict
#'
#' Calculate predictions.
#'
#' @param object The tmb_analysis object.
#' @param new_data The data frame to calculate the predictions for.
#' @param term A string of the term in predict_expr.
#' @inheritParams tmb_model
#' @param ... Unused.
#' @return The new data with the predictions.
#' @export
predict.tmb_analysis <- function(
  object, new_data = data_set(object), term = "prediction",
  predict_expr = NULL, ...) {

  model <- model(object)

  if (is.null(predict_expr)) predict_expr <- model$predict_expr

  check_data1(new_data)
  check_string(term)
  check_string(predict_expr)

  data <- process_data(new_data, data2 = data_set(object),
                       select_data = model$select_data,
                       center = model$center, scale = model$scale,
                       random_effects = model$random_effects,
                       modify_data = identity)

  data %<>% lapply(as.numeric)

  data %<>% c(estimates(object, "fixed"), estimates(object, "random"),
              estimates(object, "report"), estimates(object, "adreport"))

  predict_expr %<>% parse(text = .)

  vars <- all.vars(predict_expr)
  data[vars[!vars %in% names(data)]] <- NA

  data %<>% within(eval(predict_expr))

  if (!is.vector(data[[term]])) {
    error("term '", term, "' in predict_expr must be a vector")
  }
  if (!length(data[[term]]) %in% c(1, nrow(new_data))) {
    error("term '", term, "' in predict_expr must be a scalar or a vector of length ", nrow(new_data))
  }

  new_data[term] <- data[[term]]
  new_data
}
