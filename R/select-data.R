select_data <- function(data_set, select) {
  if (is.character(select) && length(select)) {
    select %<>% unique()
    check_cols(data_set, select)
    data_set <- data_set[select]
  } else if (is_named_list(select)) {
    check_data2(data_set, select)
    data_set <- data_set[names(select)]
  }
  data_set
}
