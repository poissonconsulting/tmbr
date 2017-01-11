#' @export
as.tmb_ml_analysis.tmb_mcmc_analysis <- function(x, ...) {
  x$ngens <- x$ml$ngens
  x$duration <- x$ml$duration
  x$mcmcr <- x$ml$mcmcr

  x$ml <- NULL

  class(x) <- c("tmb_ml_analysis", "tmb_analysis", "mb_analysis")
  x
}


tmb_mcmc_reanalyse_chain <- function(mcmcr, ad_fun, niters, nthin, quiet) {
  stopifnot(identical(nchains(mcmcr), 1L))

  #  inits <- from mcmcr? # default is to use obj$par....
  #  pass alpha as option as in future TMB should optimise (can just ignore then)
  mcmc <- TMB::run_mcmc(ad_fun,  nsim = niters, algorithm = "RWM", alpha = getOption("tmbr.alpha", 1))

#  mcmc %<>% as.mcmc()
#  x <<- mcmc

  # then need to convert mcmc to mcmcr perhaps using mcmcr dims
  # then need to thin
  mcmcr
}

tmb_mcmc_reanalyse_internal <- function(analysis, parallel, quiet) {

  timer <- timer::Timer$new()
  timer$start()

  niters <- analysis$ngens * 2
  nchains <- nchains(analysis)
  nthin <- niters * nchains %/% (2000 * 2)

  nthin %<>% max(nthin, 1L)

  mcmcr <- list()
  for (i in 1:nchains) {
    mcmcr[[i]] <- subset(analysis$mcmcr, chains = i)
  }

  mcmcr %<>% plapply(tmb_mcmc_reanalyse_chain, .parallel = parallel, ad_fun = analysis$ad_fun,
                     niters = niters, nthin = nthin, quiet = quiet)

  analysis$mcmcr %<>% purrr::reduce(mcmcr, bind_chains)
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
  check_number(rhat)
  check_count(minutes)
  check_flag(quick)
  check_flag(quiet)
  check_flag(parallel)
  check_flag(beep)

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

  if (beep) on.exit(beepr::beep())

  if (!file.exists(paste0(analysis$tempfile, ".cpp"))) {
    # recompile and run analysis
    analysis <- analyse(analysis$model, data_set(analysis),
                          parallel = parallel, quick = quick,
                          quiet = quiet, beep = FALSE)
  }

  analysis$ml <- list()
  analysis$ml$duration <- analysis$duration
  analysis$ml$mcmcr <- analysis$mcmcr
  analysis$ml$ngens <- analysis$ngens

  nchains <- ifelse(quick, 2L, 4L)
  ngens <- ifelse(quick, 5L, analysis$model$niters %/% 2L)

  analysis$duration <- lubridate::duration(0)
  analysis$mcmcr %<>% subset(chains = rep(1L, nchains))
  analysis$ngens <- ngens

  class(analysis) <- c("tmb_mcmc_analysis", "tmb_analysis", "mb_analysis")

  analysis %<>% tmb_mcmc_reanalyse_internal(parallel = parallel, quiet = quiet)

  tmb_mcmc_reanalyse(analysis, rhat = rhat, minutes = minutes, quick = quick,
                     quiet = quiet, parallel = parallel)
}
