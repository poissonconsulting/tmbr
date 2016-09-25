select_data <- function(data_set, select_data) {
  if (is.character(select_data) && length(select_data)) {
    select_data %<>% unique()
    check_cols(data_set, select_data)
    data_set <- data_set[select_data]
  } else if (is.named_list(select_data)) {
    check_data2(data_set, select_data)
    data_set <- data_set[names(select_data)]
  }
  data_set
}
