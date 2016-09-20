#' @method coef tmb_analysis
#' @export
coef.tmb_analysis <- function(object, parm = "all", ...) {
  check_vector(parm, c("^all$", "^fixed$", "^random$", "^report$"), max_length = 1)
  object <- TMB::sdreport(object)
  object <- summary(object, select = parm, p.value = TRUE)
  object
}
