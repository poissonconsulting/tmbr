#' TMB Model
#'
#' @param model_code A string of the model template code.
#' @param inits A named list of initial values for all fixed and random parameters.
#' @param select A character vector or a named list specifying the columns to select (and in the case of a named list the associated classes and values).
#' @param random A character vector naming the random effects parameters.
#' @inheritParams rescale::rescale
#' @return An object of class tmb_model.
#' @seealso \code{\link[datacheckr]{check_data}} \code{\link[rescale]{rescale}}
#' @export
tmb_model <- function(model_code, inits, select = character(0),
                      center = character(0), scale = character(0),
                      random = character(0)) {
  check_string(model_code)

  if (!is.character(select) && !is.named_list(select))
    stop("select must be a character vector or named list specifying the columns and their associated classes and values" ,call. = FALSE)

  check_vector(center, "", min_length = 0)
  check_vector(scale, "", min_length = 0)
  check_vector(random, "", min_length = 0)

  if (length(select)) {
    cols <- select
    if (is.named_list(select)) cols %<>% names()
    if (!all(center %in% cols)) stop("columns in center must also be in select")
    if (!all(scale %in% cols)) stop("columns in scale must also be in select")
  }

  obj <- list(model_code = model_code, inits = inits, select = select,
                   center = center, scale = scale, random = random)
  class(obj) <- "tmb_model"
  obj
}
