#' @export
as.tmb_ml_analysis.tmb_mcmc_analysis <- function(x, ...) {
  x$ngens <- NULL
  x$duration <- x$ml$duration
  x$mcmcr <- x$ml$mcmcr

  x$ml <- NULL

  class(x) <- c("tmb_ml_analysis", "tmb_analysis", "mb_analysis")
  x
}


tmb_mcmc_reanalyse_chain(mcmcr, ad_fun, niters, nthin, quiet) {
  #  pass alpha as option as in future TMB should optimise (will just ignore then)

  #  inits <- from mcmcr
  mcmc <- TMB::run_mcmc.rwm(nsim = niters, fn = ad_fun,  alpha = getOption("tmbr.alpha", 1))
  # then need to convert mcmc to mcmcr perhaps using mcmcr dims
  # then need to thin
  mcmcr
}


tmb_mcmc_reanalyse_internal <- function(analysis, parallel, quiet) {
  timer <- timer::Timer$new()
  timer$start()

  niters <- analysis$ngens * 2
  nchains <- nchains(analysis)
  nthin <- niters * nchains / (2000 * 2)

  mcmcr <- list()
  for (i in 1:chains() {
    mcmcr[[i]] <- subset(analysis$mcmcr, chains = i)
  }

  mcmcr %<>% plapply(tmb_mcmc_reanalyse_chain, .parallel = parallel, ad_fun = analysis$ad_fun,
                             niters = niters, nthin = nthin, quiet = quiet)

  analysis$mcmcr %<>% purrr::reduce(mcmcr)

  analysis$ngens <- as.integer(niters)
  analysis$duration %<>% magrittr::add(timer$elapsed())
  analysis
}

tmb_mcmc_reanalyse <- function(analysis, rhat = rhat, minutes = minutes, quick = quick,
                             quiet = quiet, parallel = parallel) {

  if (quick || converged(analysis, rhat) || minutes < elapsed(analysis) * 2) return(analysis)

  while (!converged(analysis, rhat) && minutes >= elapsed(analysis) * 2)
    analysis %<>% tmb_mcmc_reanalyse_internal(parallel = parallel, quiet = quiet)
  analysis
}

#' @export
reanalyse.tmb_mcmc_analysis <- function(analysis,
                                      rhat = getOption("mb.rhat", 1.1),
                                      minutes = getOption("mb.minutes", 60L),
                                      parallel = getOption("mb.parallel", FALSE),
                                      quick = getOption("mb.quick", FALSE),
                                      quiet = getOption("mb.quiet", TRUE),
                                      beep = getOption("mb.beep", TRUE), ...) {
  check_count(minutes)
  check_flag(quick)
  check_flag(quiet)
  check_flag(parallel)
  check_flag(beep)
  check_number(alpha)

  if (beep) on.exit(beepr::beep())

  tmb_mcmc_reanalyse(analysis, rhat = rhat, minutes = minutes, quick = quick, quiet = quiet, parallel = parallel)
}

#' @export
reanalyse.tmb_ml_analysis <- function(analysis,
                                      rhat = getOption("mb.rhat", 1.1),
                                      minutes = getOption("mb.minutes", 60L),
                                      parallel = getOption("mb.parallel", FALSE),
                                      quick = getOption("mb.quick", FALSE),
                                      quiet = getOption("mb.quiet", TRUE),
                                      beep = getOption("mb.beep", TRUE), ...) {

  analysis$ml <- list()
  analysis$ml$duration <- analysis$duration
  analysis$ml$mcmcr <- analysis$mcmcr

  analysis$ngens <- analysis$model$niters

  analysis$mcmc %<>% subset(chains = rep(1, 4), iterations = rep(1, 10))

  class(analysis) <- c("tmb_mcmc_analysis", "tmb_analysis", "mb_analysis")

  reanalyse(analysis, rhat = rhat, minutes = minute, quick = quick,
            quiet = quiet, parallel = parallel)

}
