# profile_row <- function(data, profile_expr, ad_fun) {
#   stopifnot(nrow(data) == 1)
#   data %<>% as.list()
#
#   string %<>% replace_values(data)
#   string %<>% parse_string()
#
#
# }
#
# profile <- function(object, new_data, profile_expr = NULL, back_transform = identity) {
#   model <- model(object)
#
#   if (is.null(profile_expr)) profile_expr <- model$profile_expr
#
#   check_data1(new_data)
#   check_string(profile_expr)
#   check_single_arg_fun(back_transform)
#
#   check_profile_expr(profile_expr)
#
#   data <- process_data(new_data, data2 = data_set(object),
#                        select_data = model$select_data,
#                        center = model$center, scale = model$scale,
#                        random_effects = model$random_effects,
#                        modify_data = identity)
#
#   data %<>% lapply(as.numeric)
#   data %<>% as.data.frame()
#
#   plyr::adply(data, 1,  profile_row, profile_expr = profile_expr, ad_fun = model$ad_fun)
#
#
#
#   data
# }
