select_data <- function(data_set, model) {
  select <- model$select

  if (!length(select)) {
    cols <- c(model$center, model$scale)
    if (is_named_list(model$random)) cols %<>% c(unlist(model$random))
    check_cols(data_set, sort(cols))
    return(data_set)
  }

  if (is.character(select)) {
    select %<>% unique()
    check_cols(data_set, select)
    data_set <- data_set[select]
    return(data_set)
  }
  check_data2(data_set, select)
  data_set <- data_set[names(select)]
  data_set
}
