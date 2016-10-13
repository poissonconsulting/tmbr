select_expr <- function(string, term) {
  string %<>% stringr::str_replace_all(" ", "")
  string %<>% stringr::str_split(pattern = "\\n")
  string <- string[[1]]
  pattern <- stringr::str_c("^", term, "(<-)|(=)")
  string <- string[stringr::str_detect(string, pattern)]
  if (!length(string)) error("term '", term, "' is not defined in new_expr")
  if (length(string) == 2) error("term '", term, "' is defined more than once in new_expr")
  string %<>% stringr::str_replace(pattern, "")
  names(string) <- "identity"
  pattern <- "(^\\w+)([(])(.*)([)]$)"
  if (stringr::str_detect(string, pattern)) {
    fun <- stringr::str_replace(string, pattern, "\\1")
    string %<>% stringr::str_replace(pattern, "\\3")
    names(string) <- fun
  }
  check <- parse_string(string) %>% vapply(any_blank, TRUE)
  if (any(check)) error("new_expr is incomplete")
  string
}

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
  if (sum(is.na(weights)) >= 2) error("new_expr must be linear")
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

calculate_expr <- function(new_expr, data) {
  new_expr %<>% replace_names_with_values(data)
  new_expr %<>% parse_string()
  new_expr %<>% lapply(get_name_weight) %>% unlist()
  new_expr
}

predict_row <- function(data, new_expr, analysis, term, conf_int, conf_level, fixed, random, report, adreport) {
  stopifnot(nrow(data) == 1)

  data %<>% as.list()

  fixed %<>% named_estimates() %>% as.list()
  random %<>% named_estimates() %>% as.list()
  report %<>% named_estimates() %>% as.list()
  adreport %<>% named_estimates() %>% as.list()

  new_expr %<>% select_expr(term)
  back_transform <- names(new_expr)

  data %<>% c(random, report, adreport)

  new_expr %<>% calculate_expr(data)

  sum <- sum(new_expr[names(new_expr) == "all"])
  new_expr <- new_expr[names(new_expr) != "all"]

  if (!length(new_expr)) {
    data <- data.frame(estimate = sum, lower = sum, upper = sum)
    data[] %<>% purrr::map(eval(parse(text = back_transform)))
    return(data)
  }
  lincomb <- lincomb0(analysis)

  if (!all(names(new_expr) %in% names(lincomb))) error("unrecognised parameter name")

  lincomb[names(new_expr)] <- new_expr

  profile <- TMB::tmbprofile(analysis$ad_fun, lincomb = lincomb, trace = FALSE) %>%
    confint(level = conf_level) %>% as.data.frame()

  new_expr <- stringr::str_c(names(new_expr), " * ", new_expr, collapse = " + ") %>%
    calculate_expr(fixed)

  estimate <- sum(new_expr)
  data <- data.frame(estimate = estimate + sum, lower = profile$lower + sum, upper = profile$upper + sum)
  data[] %<>% purrr::map(eval(parse(text = back_transform)))
  data
}

calculate_predictions <- function(data, new_expr, term) {
  new_expr %<>% parse(text = .)
  data %<>% within(eval(new_expr))
  if (is.null(data[[term]])) error("term '", term, "' is undefined")
  if (!is.vector(data[[term]])) error("term '", term, "' is not a vector")
  data[[term]]
}

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
#' @inheritParams mbr::predict_data
#' @return The new data with the predictions.
#' @export
predict.tmb_analysis <- function(object, new_data = data_set(object),
                                 new_expr = NULL, term = "prediction",
                                 conf_int = FALSE, conf_level = 0.95,
                                 quick = getOption("mb.quick", FALSE),
                                 quiet = getOption("mb.quiet", TRUE),
                                 beep = getOption("mb.beep", FALSE),
                                 ...) {

  check_data2(new_data)
  check_string(term)
  check_flag(conf_int)
  check_number(conf_level, c(0.5, 0.99))
  check_flag(quick)
  check_flag(quiet)
  check_flag(beep)
  check_unused(...)

  if (beep) on.exit(beepr::beep())

  model <- model(object)

  if (is.null(new_expr)) new_expr <- model$new_expr
  check_string(new_expr)

  data <- process_data(new_data, data2 = data_set(object),
                       select_data = model$select_data,
                       center = model$center, scale = model$scale,
                       random_effects = model$random_effects,
                       modify_data = identity)

  fixed <- estimates(object)
  random <- estimates(object, "random") %>%
    zero_random_effects(data, model$random_effects)
  report <- estimates(object, "report")
  adreport <- estimates(object, "adreport")

  data %<>% lapply(as.numeric)

  if (!conf_int) {
    data %<>% c(fixed, random, report, adreport)
    estimate <- calculate_predictions(data, new_expr, term)
    if (!length(estimate) %in% c(1, nrow(new_data)))
        error("length of term '", term, "' is invalid")
    new_data$estimate <- estimate
    return(new_data)
  }

  data %<>% as.data.frame()

  data %<>% plyr::adply(1, predict_row, new_expr = new_expr,
                        analysis = object, term = term, conf_int = conf_int, conf_level = conf_level,
                        fixed = fixed, random = random, report = report, adreport = adreport)

  data %<>% dplyr::select_(~estimate, ~lower, ~upper)

  new_data %<>% dplyr::bind_cols(data)
  new_data
}
