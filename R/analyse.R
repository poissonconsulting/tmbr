lmcmcarray <- function(x) {

  nrow <- nrow(x)

  if (identical(nrow, 1L)) {
    dims <- 1L
  } else {
    dims <- str_replace(x$term[nrow], "^(\\w+)(.*)", "\\2") %>% str_replace("^(\\[)(.*)(\\])$", "\\2")
    dims %<>% str_split(",", simplify = FALSE) %>% unlist()
    dims %<>% as.integer()
  }

  dims %<>% c(1L, 1L, .)

  samples <- array(x$estimate, dim = dims)
  class(samples) <- "mcmcarray"

  samples
}

lmcmcr <- function(object) {

  coef <- coef(object, mcmc = FALSE)
  random <- coef(object, fixed = FALSE, mcmc = FALSE)

  coef %<>% dplyr::bind_rows(random) %>% dplyr::select_(~term, ~estimate)

  coef %<>% dplyr::mutate_(parameter = ~str_replace(term, "^(\\w+)(.*)", "\\1"))

  coef %<>% plyr::dlply(~parameter, lmcmcarray)

  class(coef) <- "mcmcr"
  coef
}

compile_code <- function(model, tempfile) {
    write(template(model), file = paste0(tempfile, ".cpp"))
    TMB::compile(paste0(tempfile, ".cpp"))
    dyn.load(TMB::dynlib(tempfile))
}

tmb_analysis <- function(data, model, tempfile, quick, quiet, compiled = FALSE) {
  timer <- timer::Timer$new()
  timer$start()

  if (!compiled) compile_code(model, tempfile)

  obj <- list(model = model, data = data)

  data %<>% mbr::modify_data(model = model)

  inits <- inits(data, model$gen_inits, model$random_effects)

  map <- map(inits)

  inits %<>% lapply(function(x) {x[is.na(x)] <- 0; x})

  ad_fun <- TMB::MakeADFun(data = data, inits, map = map,
                           random = names(model$random_effects),
                           DLL = basename(tempfile), silent = quiet)

  opt <- do.call("optim", ad_fun)

  sd <- TMB::sdreport(ad_fun, getReportCovariance = FALSE)
  report <- ad_fun$report()

  obj %<>% c(inits = list(inits), map = list(map), ad_fun = list(ad_fun), opt = list(opt),
             sd = list(sd), report = list(report), lmcmcr = list(lmcmcr), duration = timer$elapsed())
  class(obj) <- c("tmb_analysis", "mb_analysis")

  obj$lmcmcr <- lmcmcr(obj)

  obj
}

#' @export
analyse.tmb_model <- function(model, data, drop = character(0),
                              parallel = getOption("mb.parallel", FALSE),
                              quick = getOption("mb.quick", FALSE),
                              quiet = getOption("mb.quiet", TRUE),
                              beep = getOption("mb.beep", TRUE),
                              ...) {
  if (is.data.frame(data)) {
    check_data2(data)
  } else if (is.list(data)) {
    lapply(data, check_data2)
  } else error("data must be a data.frame or a list of data.frames")

  check_vector(drop, "", min_length = 0)
  check_flag(parallel)
  check_flag(quick)
  check_flag(quiet)
  check_flag(beep)


  if (beep) on.exit(beepr::beep())

  model %<>% drop_parameters(parameters = drop)

  tempfile <- tempfile()

  if (is.data.frame(data)) {
    return(tmb_analysis(data = data, model = model, tempfile = tempfile,
                        quick = quick, quiet = quiet))
  }

  compile_code(model, tempfile)

  lapply(data, tmb_analysis, model = model, tempfile = tempfile,
         quick = quick, quiet = quiet, compiled = TRUE)
}
