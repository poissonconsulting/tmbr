glance.tmb_analysis <- function(x, n = NULL, ...) {
  dplyr::data_frame(
    n = sample_size(x),
    K = nterms(x, include_constant = FALSE),
    logLik = logLik(x),
    AICc = AICc(x, n = n),
    minutes = elapsed(x),
    converged = converged(x)
  )
}
