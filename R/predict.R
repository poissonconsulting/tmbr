#' Predict
#'
#' Calculate predictions.
#'
#' If \code{conf_int = TRUE} then the confidence intervals are calculated by profiling.
#' The profiling only takes uncertainty in the fixed parameters into account.
#' When profiling \code{new_expr} must be linear (the sum of a series of terms)
#' and can only include one function that encompasses the whole expression,
#' i.e., \code{prediction <- exp(bIntercept + bSlope * y)}.
#'
#' @param object The tmb_analysis object.
#' @param new_data The data frame to calculate the predictions for.
#' @param new_expr A string of R code specifying the predictive relationship.
#' @param term A string of the term in new_expr.
#' @param conf_int A flag indicating whether to calculate confidence intervals.
#' @param conf_level A number specifying the confidence level. By default 0.95.
#' @param ... Unused.
#' @return The new data with the predictions.
#' @export
predict.tmb_analysis <- function(
  object, new_data = data_set(object), new_expr = NULL, term = "prediction",
  conf_int = FALSE, conf_level = 0.95, ...) {

  check_data2(new_data)
  check_string(term)
  check_flag(conf_int)
  check_number(conf_level, c(0.5, 0.99))

  model <- model(object)

  if (is.null(new_expr)) new_expr <- model$new_expr
  check_string(new_expr)
  check_new_expr(new_expr, term)

  if (conf_int) {
    new_expr %<>% select_profile_expr(term)
    back_transform <- names(new_expr)
  }

  data <- process_data(new_data, data2 = data_set(object),
                       select_data = model$select_data,
                       center = model$center, scale = model$scale,
                       random_effects = model$random_effects,
                       modify_data = identity)

  fixed <- estimates(object)
  random <- estimates(object, "random")
  random %<>% zero_random_effects(data, model$random_effects)
  report <- estimates(object, "report")
  adreport <- estimates(object, "adreport")

  data %<>% lapply(as.numeric)


  if (!conf_int) {

    data %<>% c(fixed, random, report, adreport)

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

    new_data$estimate <- data[[term]]
    return(new_data)
  }

  fixed %<>% named_estimates() %>% as.list()
  random %<>% named_estimates() %>% as.list()
  report %<>% named_estimates() %>% as.list()
  adreport %<>% named_estimates() %>% as.list()

  data %<>% as.data.frame()

  data %<>% plyr::adply(1,  profile_row, profile_expr = new_expr,
                        analysis = object, conf_level = conf_level,
                        fixed = fixed, random = random, report = report, adreport = adreport)

  data %<>% dplyr::select_(~estimate, ~lower, ~upper)

  data[] %<>% purrr::map(eval(parse(text = back_transform)))

  new_data %<>% dplyr::bind_cols(data)
}
