replace_names_with_values <- function(string, list) {
  for (i in seq_along(list)) {
    pattern <- names(list)[i]
    pattern %<>% stringr::str_replace_all("(.*)(\\[)(.*)", "\\1\\\\\\2\\3")
    pattern %<>% stringr::str_replace_all("(.*)(\\])(.*)", "\\1\\\\\\2\\3")
    pattern %<>% stringr::str_c("(^|(?<!\\w))", ., "((?!\\w)|$)")
    string %<>% stringr::str_replace_all(pattern, list[i])
  }
  string
}

parse_string <- function(string) {
  string %<>% stringr::str_replace_all("\\s", "")
  string <- stringr::str_split(string, "[+]")[[1]] %>% stringr::str_split("[*]")
  string
}

get_name_weight <- function(x) {
  stopifnot(is.character(x))
  suppressWarnings(weights <- as.numeric(x))
  if (sum(is.na(weights)) >= 2) error("profile_expr must be linear")
  if (!any(is.na(weights))) return(c("all" = prod(weights)))
  if (length(weights) == 1) {
    y <- 1
  } else
    y <- prod(weights, na.rm = TRUE)
  names(y) <- x[is.na(weights)]
  y
}

c_name <- function(x) {
  x[[1]] %<>% stringr::str_c(names(x), .)
  x
}

par_names_indices <- function(estimates) {
  estimates %<>% lapply(dims) %>% lapply(dims_to_dimensions_vector)
  estimates %<>% purrr::lmap(c_name)
  estimates %<>% sort_by_names()
  estimates
}

lincomb_names <- function(analysis) {
  names <- names(analysis$ad_fun$env$last.par.best)
  if (!is.null(analysis$ad_fun$env$random)) names <- names[-analysis$ad_fun$env$random]

  indices <- estimates(analysis) %>% par_names_indices()
  stopifnot(setequal(names, names(indices)))
  indices <- indices[unique(names)]
  indices %<>% unlist()
  indices %<>% unname()
  indices
}

named_estimates <- function(estimates) {
  stopifnot(is_named_list(estimates))
  indices <- par_names_indices(estimates) %>% unlist()
  estimates %<>% unlist()
  names(estimates) <- indices
  estimates
}

lincomb0 <- function(analysis) {
  names <- lincomb_names(analysis)
  lincomb <- rep(0, length(names))
  names(lincomb) <- names
  lincomb
}

calculate_expr <- function(profile_expr, data) {
  profile_expr %<>% replace_names_with_values(data)
  profile_expr %<>% parse_string()
  profile_expr %<>% lapply(get_name_weight) %>% unlist()
  profile_expr
}

profile_row <- function(data, profile_expr, analysis, conf_level, fixed, random, report, adreport) {
  stopifnot(nrow(data) == 1)

  data %<>% as.list() %>% c(random, report, adreport)

  profile_expr %<>% calculate_expr(data)

  sum <- sum(profile_expr[names(profile_expr) == "all"])
  profile_expr <- profile_expr[names(profile_expr) != "all"]

  if (!length(profile_expr)) return(data.frame(estimate = sum, lower = sum, upper = sum))

  lincomb <- lincomb0(analysis)

  if (!all(names(profile_expr) %in% names(lincomb))) error("unrecognised parameter name")

  lincomb[names(profile_expr)] <- profile_expr

  profile <- TMB::tmbprofile(analysis$ad_fun, lincomb = lincomb, trace = FALSE) %>%
    confint(level = conf_level) %>% as.data.frame()

  profile_expr <- stringr::str_c(names(profile_expr), " * ", profile_expr, collapse = " + ") %>%
    calculate_expr(fixed)

  estimate <- sum(profile_expr)
  data.frame(estimate = estimate + sum, lower = profile$lower + sum, upper = profile$upper + sum)
}

#' Profile
#'
#' @param object The tmb_analysis object to profile.
#' @param new_data A data.frame to use for profile.
#' @param profile_expr The profiling expression. Must be linear.
#' @param back_transform A single argument function specifying the transformation for the estimate and confidence intervals.
#' @inheritParams coef.tmb_analysis
#' @return The new data with estimates and lower and upper confidence intervals.
#' @export
profile <- function(object, new_data, profile_expr = NULL, back_transform = identity, conf_level = 0.95) {
  check_number(conf_level, c(0.5, 0.99))
  model <- model(object)

  if (is.null(profile_expr)) profile_expr <- model$profile_expr

  check_data2(new_data)
  check_string(profile_expr)
  check_single_arg_fun(back_transform)

  check_profile_expr(profile_expr)

  data <- process_data(new_data, data2 = data_set(object),
                       select_data = model$select_data,
                       center = model$center, scale = model$scale,
                       random_effects = model$random_effects,
                       modify_data = identity)

  data %<>% lapply(as.numeric) %>% as.data.frame()

  fixed <- estimates(object) %>% named_estimates() %>% as.list()
  random <- estimates(object, "random") %>% named_estimates() %>%  as.list()
  report <- estimates(object, "report") %>% named_estimates() %>% as.list()
  adreport <- estimates(object, "adreport") %>% named_estimates() %>% as.list()

  data %<>% plyr::adply(1,  profile_row, profile_expr = profile_expr,
                        analysis = object, conf_level = conf_level,
                        fixed = fixed, random = random, report = report, adreport = adreport)

  data %<>% dplyr::select_(~estimate, ~lower, ~upper)

  data[] %<>% purrr::map(back_transform)

  new_data %<>% dplyr::bind_cols(data)
  new_data
}
