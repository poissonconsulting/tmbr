select_data <- function(data_set, model) {
  select_data <- model$select_data

  if (!length(select_data)) {
    cols <- c(model$center, model$scale)
    if (is_named_list(model$random)) cols %<>% c(unlist(model$random))
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

process_data <- function(data_set, data_set2, model, fun) {
  data_set %<>% select_data(model)
  data_set %<>% rescale::rescale(data2 = data_set2, center = model$center, scale = model$scale)
  data_set %<>% as.list()
  data_set %<>% fun()
  data_set
}
