coef_tmb_analysis1 <- function(object, parm) {
  object <- TMB::sdreport(object)
  object <- summary(object, select = parm, p.value = TRUE)
  object
}

#' @export
coef.tmb_analysis <- function(object, parm = "all") {
  check_vector(parm, c("^all$", "^fixed$", "^random$", "^report$"), max_length = 1)
  lapply(object$analyses, coef_tmb_analysis1, parm = parm)
}

is.tmb_model1 <- function(x) {
  inherits(x, "tmb_model")
}

is.tmb_model <- function(x) {
  inherits(x, "tmb_model")
}

#' Model Code
#'
#' Gets the model_code for an object.
#'
#' @param x The object.
#'
#' @return The model code as a character vector for an object
#' @export
model_code <- function(x) {UseMethod("model_code")}

#' @export
model_code.tmb_model1 <- function(x) x$model_code

#' @export
model_code.tmb_model <- function(x) vapply(x, model_code, "")

#' Parameters
#'
#' Gets the parameters for an object.
#'
#' @param x The object.
#'
#' @return The parameters as a list.
#' @export
parameters <- function(x) {UseMethod("parameters")}

#' @export
parameters.tmb_model1 <- function(x) x$parameters

#' @export
parameters.tmb_model <- function(x) lapply(x, parameters)

tmb_model1 <- function(model_code, parameters) {
  check_string(model_code)

  obj <- list()
  obj$model_code <- model_code
  obj$parameters <- parameters
  class(obj) <- "tmb_model1"
  obj
}

#' TMB Model
#'
#' @param model_code A string of the model template.
#' @param parameters A list of all parameter objects required by the user template.
#'
#' @return An object of class tmb_model.
#' @export
tmb_model <- function(model_code, parameters, name = "Model_1") {
  check_string(model_code)
  check_string(name)

  obj <- list()
  obj[[name]] <- tmb_model1(model_code, parameters)
  class(obj) <- "tmb_model"
  obj
}

model_code <- function(x) UseMethod("model_code")

tmb_analysis1 <- function(x) {
  stopifnot(is.list(x))
  stopifnot(identical(names(x), c("par", "fn",  "gr",  "he",  "hessian",
                                  "method", "retape", "env", "report")))
  class(x) <- "tmb_analysis1"
  x
}

#' TMB Analysis
#'
#' @param data The data.frame to analyse.
#' @param model The tmb_model to analyse.
#'
#' @return An object of class tmb_analysis.
#' @export
tmb_analysis <- function(data, model) {
  check_data1(data)
  stopifnot(is.tmb_model(model))
  stopifnot(length(model) == 1)

  tempfile <- tempfile()
  tempfile <- "regression"

  write(model_code(model), file = paste0(tempfile, ".cpp"))

  TMB::compile(paste0(tempfile, ".cpp"))
  dyn.load(TMB::dynlib(tempfile))

  ad_fun <- TMB::MakeADFun(data = as.list(data),  parameters = parameters(model)[[1]],
                             DLL = tempfile)

  opt <- do.call("optim", ad_fun)

  obj <- list()
  obj$data <- data
  obj$model <- model
  obj$analyses <- list()
  obj$analyses[[names(model)]] <- tmb_analysis1(ad_fun)
  class(obj) <- "tmb_analysis"
  obj
}
