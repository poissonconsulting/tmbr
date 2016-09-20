#' @export
coef.tmb_analysis <- function(object, parm = "all", ...) {
  check_vector(parm, c("^all$", "^fixed$", "^random$", "^report$"), max_length = 1)

  sd <- TMB::sdreport(object$ad_fun)
  sd %<>% summary(sd, select = parm, p.value = TRUE)
  sd
}
