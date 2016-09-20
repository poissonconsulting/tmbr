#' TMB Analysis
#'
#' @param data The data.frame to analyse.
#' @param model The tmb_model to analyse.
#'
#' @return An object of class tmb_analysis.
#' @export
tmb_analysis <- function(data, model) {
  check_data1(data)
  if (!is.tmb_model(model)) stop("model must be a tmb_model", call. = FALSE)

  tempfile <- tempfile()

  write(model_code(model), file = paste0(tempfile, ".cpp"))

  TMB::compile(paste0(tempfile, ".cpp"))
  dyn.load(TMB::dynlib(tempfile))

  ad_fun <- TMB::MakeADFun(data = as.list(data),  parameters = parameters(model),
                           DLL = basename(tempfile), silent = TRUE)

  opt <- do.call("optim", ad_fun)

  obj <- list()
  obj$data <- data
  obj$model <- model
  obj$ad_fun <- ad_fun
  obj$opt <- opt
  class(obj) <- "tmb_analysis"
  obj
}
