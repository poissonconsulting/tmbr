select_data <- function(data, select_data, center, scale, random_effects) {
  if (!length(select_data)) {
    cols <- c(center, scale)
    cols %<>% c(unlist(random_effects))
    check_cols(data, sort(cols))
    return(data)
  }

  check_data2(data, select_data)
  data <- data[names(select_data)]
  data
}

process_data <- function(data, data2, select_data, center, scale, random_effects, modify_data) {
  data %<>% select_data(select_data, center, scale, random_effects)
  data %<>% rescale::rescale(data2 = data2, center = center, scale = scale)
  data %<>% as.list()
  data %<>% modify_data()
  data
}
