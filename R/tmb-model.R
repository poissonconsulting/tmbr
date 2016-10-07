#' TMB Model
#'
#' Creates TMB model.
#'
#' @param model_code A string of the model template code.
#' @param inits A named list of initial values for all fixed and random parameters.
#' @param random_effects A character vector or a named list specifying of the random effects (and in the case of a named list the associated factors).
#' @param select_data A character vector or a named list specifying the columns to select (and in the case of a named list the associated classes and values).
#' @inheritParams rescale::rescale
#' @param modify_data A single argument function to modify the data (in list form) immediately prior to the analysis.
#' @param new_code A string of the R code specifying the predictive relationships.
#' @param select_new_data A character vector or a named list specifying the columns in a new data frame to select (and in the case of a named list the associated classes and values).
#' @param modify_new_data A single argument function to modify the new_data (in list form) immediately prior to the predictions.
#' @return An object of class tmb_model.
#' @seealso \code{\link[datacheckr]{check_data}} \code{\link[rescale]{rescale}}
#' @export
tmb_model <- function(
  model_code, inits, random_effects = character(0), select_data = character(0),
  center = character(0), scale = character(0), modify_data = function(x) x,
  new_code = character(0), select_new_data = select_data, modify_new_data = modify_data)
{
  check_string(model_code)
  check_select_data(select_data)
  check_center(center, select_data)
  check_scale(scale, select_data)
  check_inits(inits)
  check_random_effects(random_effects, select_data, center, scale, inits)
  check_vector(new_code, "", min_length = 0, max_length = 1)
  check_modify_data(modify_data)
  check_modify_data(modify_new_data)
  check_select_data(select_new_data)

  obj <- list(model_code = model_code,
              inits = sort_by_names(inits),
              select_data = select_data,
              center = sort_by_names(center),
              scale = sort_by_names(scale),
              random_effects = sort_by_names(random_effects),
              modify_data = modify_data,
              new_code = new_code,
              select_new_data = select_new_data,
              modify_new_data = modify_new_data)
  class(obj) <- "tmb_model"
  obj
}
