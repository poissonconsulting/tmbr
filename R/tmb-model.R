tmb_model_object <- function(model_code, parameters, select, random) {
  obj <- list()
  obj$model_code <- model_code
  obj$parameters <- parameters
  obj$select <- unique(select)
  obj$random <- unique(random)
  class(obj) <- "tmb_model"
  obj
}

#' TMB Model
#'
#' @param model_code A string of the model template.
#' @param parameters A list of all parameter objects required by the user template.
#' @param select A character vector specifying the variables to pass to the analysis.
#' @param random A character vector specifying the random effects parameters.
#' @return An object of class tmb_model.
#' @export
tmb_model <- function(model_code, parameters, select = NULL,
                      random = NULL) {
  check_string(model_code)
  if (!is.null(select) && !is.character(select))
    stop("select must be a character vector or NULL" ,call. = FALSE)

  if (!is.null(random) && !is.character(random))
    stop("random must be a character vector or NULL" ,call. = FALSE)

  tmb_model_object(model_code = model_code, parameters = parameters, select = select,
                   random = random)
}
