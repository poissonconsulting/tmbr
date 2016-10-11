select_profile_expr <- function(string, term) {
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
