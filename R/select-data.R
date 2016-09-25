select_data <- function(data, select_data) {
  if (is.character(select_data) && length(select_data)) {
    select_data %<>% unique()
    check_cols(data, select_data)
    data <- data[select_data]
  } else if (is.named_list(select_data)) {
    data %<>% check_data2(select_data)
    data <- data[names(select_data)]
  }
  data
}
