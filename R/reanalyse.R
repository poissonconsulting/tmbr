#' @export
as.tmb_ml_analysis.tmb_mcmc_analysis <- function(x, ...) {
  x$ngens <- x$ml$ngens
  x$duration <- x$ml$duration
  x$mcmcr <- x$ml$mcmcr

  x$ml <- NULL

  class(x) <- c("tmb_ml_analysis", "tmb_analysis", "mb_analysis")
  x
}

# Remaps matrix x based on missing values in y.
remap_mcmc <- function(x, y) {
  stopifnot(ncol(x) == sum(!is.na(y)))
  z <- matrix(0, nrow = nrow(x), ncol = length(y))
  z[,!is.na(y)] <- x
  z
}

remap_mcmcs <- function(mcmc, map){
  check_uniquely_named_list(mcmc)
  check_uniquely_named_list(map)
  map <- map[names(map) %in% names(mcmc)]
  if (!length(map)) return(mcmc)

  mcmc[names(map)] %<>% purrr::map2(map, remap_mcmc)
  mcmc
}

# Demaps vector x based on missing values in y.
demap_vector <- function(x, y) {
  stopifnot(length(x) == length(x))
  x[which(!is.na(y))]
}

# Removes fixed valuse from estimates estimates.
# estimates A named list of the estimates to demap.
demap_estimates <- function(estimates, map) {
  check_uniquely_named_list(estimates)
  check_uniquely_named_list(map)
  if (!length(map)) return(estimates)
  map <- map[names(map) %in% names(estimates)]
  if (!length(map)) return(estimates)

  estimates[names(map)] %<>% purrr::map2(map, demap_vector)
  estimates
}

redim_mcmc <- function(mcmc, mcmcarray) {
  stopifnot(is.matrix(mcmc))
  dim <- dims(mcmcarray)
  dim[1:2] <- c(1L, nrow(mcmc))
  dim(mcmc) <- dim
  class(mcmc) <- "mcmcarray"
  mcmc
}

redim <- function(mcmc, mcmcr) {
  stopifnot(all(names(mcmc) %in% names(mcmcr)))
  mcmcr <- mcmcr[names(mcmc)]
  mcmc %<>% purrr::map2(mcmcr, redim_mcmc)
  class(mcmc) <- "mcmcr"
  mcmc
}

tmb_mcmc_reanalyse_chain <- function(mcmcr, analysis, niters, nthin, quiet) {
  stopifnot(identical(nchains(mcmcr), 1L))

  # get last iteration to continue stepping through
  mcmcr %<>% subset(iterations = niters(mcmcr), parameters = unique(names(analysis$ad_fun$par)))
  estimates <- estimates(mcmcr)
  estimates %<>% demap_estimates(analysis$map)
  estimates %<>% unlist()
  stopifnot(identical(names(estimates), names(analysis$ad_fun$par)))

  #  pass alpha as option as in future TMB should optimise (will just ignore then)
  mcmc <- TMB::run_mcmc(obj = analysis$ad_fun, nsim = niters, params.init = estimates,
                        algorithm = "RWM", alpha = getOption("tmbr.alpha", 1))

  mcmc %<>% list_by_name()
  mcmc %<>% lapply(as.matrix)
  mcmc %<>% remap_mcmcs(analysis$map)
  mcmc %<>% redim(mcmcr)
  mcmc %<>% thin(as.integer(nthin))
  mcmc
}

tmb_mcmc_reanalyse_internal <- function(analysis, parallel, quiet) {

  timer <- timer::Timer$new()
  timer$start()

  niters <- analysis$ngens * 2
  nchains <- nchains(analysis)
  nthin <- niters * nchains / 2000

  mcmcr <- list()
  for (i in 1:nchains) {
    mcmcr[[i]] <- subset(analysis$mcmcr, chains = i)
  }

  mcmcr %<>% plapply(tmb_mcmc_reanalyse_chain, .parallel = parallel, analysis = analysis,
                     niters = niters, nthin = nthin, quiet = quiet)

  mcmcr %<>% purrr::reduce(bind_chains)
  analysis$mcmcr <- mcmcr
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
