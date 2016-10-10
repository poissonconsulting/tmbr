#' TMB Model
#'
#' Creates TMB model.
#'
#' @param model_code A string of the model template code.
#' @param gen_inits A single argument function taking the modified data and
#' returning a named list of initial values for all fixed parameters.
#' Missing random parameters are assigned the value 0.
#' @param random_effects A named list specifying the random effects and the associated factors.
#' @param select_data A named list specifying the columns to select and their associated classes and values.
#' @inheritParams rescale::rescale
#' @param modify_data A single argument function to modify the data (in list form) immediately prior to the analysis.
#' @param predict_expr A string of R code specifying the predictive relationships.
#' @param profile_expr A string of R code specifying the linear relationships.
#' @return An object of class tmb_model.
#' @seealso \code{\link[datacheckr]{check_data}} \code{\link[rescale]{rescale}}
#' @export
tmb_model <- function(
  model_code, gen_inits, random_effects = list(), select_data = list(),
  center = character(0), scale = character(0), modify_data = identity,
  predict_expr = character(0), profile_expr = character(0))
{
  check_string(model_code)
  check_single_arg_fun(gen_inits)
  check_uniquely_named_list(random_effects)
  check_uniquely_named_list(select_data)
  check_unique_character_vector(center)
  check_unique_character_vector(scale)
  check_single_arg_fun(modify_data)
  check_vector(predict_expr, "", min_length = 0, max_length = 1)
  check_vector(profile_expr, "", min_length = 0, max_length = 1)

  check_all_elements_class_character(random_effects)
  check_x_in_y(unlist(random_effects), names(select_data),
                        x_name = "random_effects", y_name = "select_data",
                        type_x = "elements", type_y = "names")

  check_x_not_in_y(unlist(random_effects), center, x_name = "random_effects",
                       type_x = "elements")
  check_x_not_in_y(unlist(random_effects), scale, x_name = "random_effects",
                      type_x = "elements")

  check_x_in_y(center, names(select_data), y_name = "select_data", type_y = "names")
  check_x_in_y(scale, names(select_data), y_name = "select_data", type_y = "names")

  center %<>% sort()
  scale %<>% sort()
  random_effects %<>% sort_by_names()

  obj <- list(model_code = model_code,
              gen_inits = gen_inits,
              select_data = select_data,
              center = center,
              scale = scale,
              random_effects =  random_effects,
              modify_data = modify_data,
              predict_expr = predict_expr,
              profile_expr = profile_expr)
  class(obj) <- "tmb_model"
  obj
}
