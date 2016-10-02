select_data <- function(data_set, select) {
  if (!length(select)) return(data_set)

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
