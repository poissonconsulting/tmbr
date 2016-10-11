replace_names_with_values <- function(string, list) {
  for (i in seq_along(list)) {
    pattern <- names(list)[i]
    pattern %<>% str_replace_all("(.*)(\\[)(.*)", "\\1\\\\\\2\\3")
    pattern %<>% str_replace_all("(.*)(\\])(.*)", "\\1\\\\\\2\\3")
    pattern %<>% str_c("(^|(?<!\\w))", ., "((?!\\w)|$)")
    string %<>% str_replace_all(pattern, list[i])
  }
  string
}

parse_string <- function(string) {
  string %<>% str_replace_all("\\s", "")
  string <- str_split(string, "[+]")[[1]] %>% str_split("[*]")
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
  x[[1]] %<>% str_c(names(x), .)
  x
}

par_names_indices <- function(analysis, terms = "fixed") {
  est <- estimates(analysis, terms)
  est %<>% lapply(dims)
  est %<>% lapply(dims_to_dimensions_vector)
  est %<>% purrr::lmap(c_name)
  est %<>% sort_by_names()
  est
}

lincomb_names <- function(analysis) {
  names <- names(analysis$ad_fun$env$last.par.best)
  if (!is.null(analysis$ad_fun$env$random)) names <- names[-analysis$ad_fun$env$random]

  indices <- par_names_indices(analysis, "fixed")
  stopifnot(setequal(names, names(indices)))
  indices <- indices[unique(names)]
  indices %<>% unlist()
  indices %<>% unname()
  indices
}

named_estimates <- function(analysis, terms = "fixed") {
  estimates <- estimates(analysis, terms = terms)
  indices <- par_names_indices(analysis, terms = terms)
  stopifnot(identical(names(estimates), names(indices)))
  estimates %<>% unlist()
  indices %<>% unlist()
  names(estimates) <- indices
  estimates
}

profile_row <- function(data, profile_expr, analysis, conf_level, fixed, random, report, adreport) {
  stopifnot(nrow(data) == 1)

  data %<>% as.list()
  data %<>% c(random, report, adreport)

  profile_expr %<>% replace_names_with_values(data)
  profile_expr %<>% parse_string()
  profile_expr %<>% lapply(get_name_weight) %>% unlist()
  sum <- sum(profile_expr[names(profile_expr) == "all"])
  profile_expr <- profile_expr[names(profile_expr) != "all"]

  if (!length(profile_expr)) return(data.frame(estimate = sum, lower = sum, upper = sum))

  names <- lincomb_names(analysis)

  if (!all(names(profile_expr) %in% names)) error("unrecognised parameter name")

  lincomb <- rep(0, length(names))
  names(lincomb) <- names

  lincomb[names(profile_expr)] <- profile_expr
  profile <- tmbprofile(analysis$ad_fun, lincomb = lincomb, trace = FALSE)
  profile %<>% confint(level = conf_level) %>% as.data.frame()

  profile_expr <- str_c(names(profile_expr), " * ", profile_expr, collapse = " + ")

  profile_expr %<>% replace_names_with_values(fixed)
  profile_expr %<>% parse_string()
  profile_expr %<>% lapply(get_name_weight) %>% unlist()
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

  check_data1(new_data)
  check_string(profile_expr)
  check_single_arg_fun(back_transform)

  check_profile_expr(profile_expr)

  data <- process_data(new_data, data2 = data_set(object),
                       select_data = model$select_data,
                       center = model$center, scale = model$scale,
                       random_effects = model$random_effects,
                       modify_data = identity)

  data %<>% lapply(as.numeric) %>% as.data.frame()

  fixed <- named_estimates(object) %>% as.list()
  random <- named_estimates(object, "random") %>% as.list()

  report <- named_estimates(object, "report") %>% as.list()
  adreport <- named_estimates(object, "adreport") %>% as.list()

  data %<>% plyr::adply(1,  profile_row, profile_expr = profile_expr,
                        analysis = object, conf_level = conf_level,
                        fixed = fixed, random = random, report = report, adreport = adreport)

  data %<>% select_(~estimate, ~lower, ~upper)

  data[] %<>% purrr::map(back_transform)

  new_data %<>% bind_cols(data)
  new_data
}
