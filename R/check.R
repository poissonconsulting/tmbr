check_data_model <- function(data, model) {
  if (is.data.frame(data)) {
    data %<>% mbr::modify_data(model = model)
    inits(data, model$gen_inits, model$random_effects)
  } else
    llply(data, check_data_model, model)
  TRUE
}
