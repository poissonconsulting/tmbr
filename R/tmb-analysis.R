select_data <- function(data, select_data) {
  if (is.character(select_data)) {
    select_data %<>% unique()
    check_cols(data, select_data)
    data <- data[select_data]
  } else if (is.named_list(select_data)) {
    data %<>% check_data2(select_data)
    data <- data[names(select_data)]
  } else stopifnot(is.null(select_data))
  data
}

#' TMB Analysis
#'
#' @param data_set The data.frame to analyse.
#' @param model The tmb_model to analyse.
#'
#' @return An object of class tmb_analysis.
#' @export
tmb_analysis <- function(data_set, model) {
  check_data1(data_set)
  if (!is.tmb_model(model)) stop("model must be a tmb_model", call. = FALSE)

  obj <- list()
  obj$model <- model
  obj$data_set <- data_set

  data_set %<>% select_data(model$select_data)
  data_set %<>% as.list()

  tempfile <- tempfile()

  write(model_code(model), file = paste0(tempfile, ".cpp"))

  TMB::compile(paste0(tempfile, ".cpp"))
  dyn.load(TMB::dynlib(tempfile))

  ad_fun <- TMB::MakeADFun(data = data_set,  parameters = parameters(model),
                           random = random(model),
                           DLL = basename(tempfile), silent = TRUE)

  opt <- do.call("optim", ad_fun)

  obj$ad_fun <- ad_fun
  obj$opt <- opt
  class(obj) <- "tmb_analysis"
  obj
}
