#' Predict
#'
#' Calculate predictions.
#'
#' @param object The tmb_analysis object.
#' @param new_data The data frame to calculate the predictions for.
#' @param new_expr A string of R code specifying the predictive relationship.
#' @param term A string of the term in new_expr.
#' @param ... Unused.
#' @return The new data with the predictions.
#' @export
predict.tmb_analysis <- function(
  object, new_data = data_set(object), new_expr = NULL, term = "prediction", ...) {

  model <- model(object)

  if (is.null(new_expr)) new_expr <- model$predict_expr

  check_data2(new_data)
  check_string(new_expr)
  check_string(term)

  data <- process_data(new_data, data2 = data_set(object),
                       select_data = model$select_data,
                       center = model$center, scale = model$scale,
                       random_effects = model$random_effects,
                       modify_data = identity)

  data %<>% lapply(as.numeric)

  data %<>% c(estimates(object, "fixed"), estimates(object, "random"),
              estimates(object, "report"), estimates(object, "adreport"))

  new_expr %<>% parse(text = .)

  vars <- all.vars(new_expr)
  data[vars[!vars %in% names(data)]] <- NA

  data %<>% within(eval(new_expr))

  if (!is.vector(data[[term]])) {
    error("term '", term, "' in new_expr must be a vector")
  }
  if (!length(data[[term]]) %in% c(1, nrow(new_data))) {
    error("term '", term, "' in new_expr must be a scalar or a vector of length ", nrow(new_data))
  }

  new_data[term] <- data[[term]]
  new_data
}
