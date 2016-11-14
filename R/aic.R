# #' AIC
# #'
# #' Calculates marginal AICc for an analysis.
# #'
# #' @inheritParams AIC
# #' @param p the penalty on additional fixed effects (default=2, for AIC)
# #' @param n the sample size, for use in AICc calculation (default=Inf, for which AICc=AIC)
# #'
# #' @return A number of the AAIC, where a parsimonious model has a AIC relative to other candidate models
# #' @export
# AIC.tmb_analysis <- function (object, p = 2, n=Inf, k = 2, ...){
#   k = length(opt[["par"]])
#   if( all(c("par","objective") %in% names(opt)) ) negloglike = opt[["objective"]]
#   if( all(c("par","value") %in% names(opt)) ) negloglike = opt[["value"]]
#   Return = p*k + 2*negloglike + 2*k*(k+1)/(n-k-1)
#   return( Return )
# }
