reported <- function(object, term, conf_level, ...) {
  check_string(term)

  reported <- coef(object, terms = "report", conf_level = conf_level)
  reported <- reported[reported$term == term,]

  if (!nrow(reported)) error("term '", term, "' is not in reported terms")

  reported$term <- NULL
  names(reported) <- paste(term, names(reported), sep = ".")

  if (nrow(reported) != nrow(data_set(object))) {
    error("the length of term '", term, "' does not match the number of rows of data")
  }
  reported <- dplyr::bind_cols(data_set(object), reported)
  reported
}
