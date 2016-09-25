#' TMB Model
#'
#' @param model_code A string of the model template.
#' @param parameters A list of all parameter objects required by the user template (both random and fixed).
#' @param select_data A character vector or a named list specifying the columns to select (and in the case of a named list the associated classes and values).
#' @param random_effects A character vector specifying the random effects parameters.
#' @inheritParams rescale::rescale
#' @return An object of class tmb_model.
#' @seealso \code{\link[datacheckr]{check_data}} \code{\link[rescale]{rescale}}
#' @export
tmb_model <- function(model_code, parameters, select_data = character(0),
                      center = character(0), scale = character(0),
                      random_effects = character(0)) {
  check_string(model_code)

  if (!is.character(select_data) && !is.named_list(select_data))
    stop("select_data must be a character vector or named list specifying the columns and their associated classes and values" ,call. = FALSE)

  check_vector(center, "", min_length = 0)
  check_vector(scale, "", min_length = 0)
  check_vector(random_effects, "", min_length = 0)

  if (length(select_data)) {
    cols <- ifelse(is.named_list(select_data), names(select_data), select_data)
    if (!all(center %in% cols)) stop("columns in center must also be in select_data")
    if (!all(scale %in% cols)) stop("columns in scale must also be in select_data")
  }

  obj <- list(model_code = model_code, parameters = parameters, select_data = select_data,
                   center = center, scale = scale, random_effects = random_effects)
  class(obj) <- "tmb_model"
  obj
}
