tmb_model_object <- function(model_code, parameters, select_data, random_effects) {
  obj <- list()
  obj$model_code <- model_code
  obj$parameters <- parameters
  obj$select_data <- select_data
  obj$random_effects <- random_effects
  class(obj) <- "tmb_model"
  obj
}

#' TMB Model
#'
#' If select is a named list then check_data2 is used to check the consistency of
#' the data.
#'
#' @param model_code A string of the model template.
#' @param parameters A list of all parameter objects required by the user template.
#' @param select_data NULL or a character vector or a named list specifying the variables to pass to the analysis (and in the case of a named list the associated classes and values).
#' @param random_effects A character vector specifying the random effects parameters.
#' @return An object of class tmb_model.
#' @export
tmb_model <- function(model_code, parameters, select_data = NULL,
                      random_effects = NULL) {
  check_string(model_code)
  if (!is.null(select_data) && !is.character(select_data) && !is.named_list(select_data))
    stop("select_data must be a NULL or a character vector or named list specifying the columns and their associated classes and values" ,call. = FALSE)

  if (!is.null(random_effects) && !is.character(random_effects))
    stop("random_effects must be a character vector or NULL" ,call. = FALSE)

  tmb_model_object(model_code = model_code, parameters = parameters, select_data = select_data,
                   random_effects = random_effects)
}
