select_data <- function(data_set, select_data, center, scale, random_effects) {
  if (!length(select_data)) {
    cols <- c(center, scale)
    if (is_named_list(random_effects)) cols %<>% c(unlist(random_effects))
    check_cols(data_set, sort(cols))
    return(data_set)
  }

  if (is.character(select_data)) {
    select_data %<>% unique()
    check_cols(data_set, select_data)
    data_set <- data_set[select_data]
    return(data_set)
  }
  check_data2(data_set, select_data)
  data_set <- data_set[names(select_data)]
  data_set
}

process_data <- function(data_set, data_set2, select_data, center, scale, random_effects, modify_data) {
  data_set %<>% select_data(select_data, center, scale, random_effects)
  data_set %<>% rescale::rescale(data2 = data_set2, center = center, scale = scale)
  data_set %<>% as.list()
  data_set %<>% modify_data()
  data_set
}
