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

  if (any(names(inits) %in% c("fixed", "random", "report", "adreport", "all")))
    error("parameters cannot be named 'fixed', 'random', 'report', 'adreport' or 'all'")

  map <- map(inits)

  inits %<>% lapply(function(x) {x[is.na(x)] <- 0; x})

  ad_fun <- TMB::MakeADFun(data = data, inits, map = map,
                           random = names(model$random_effects),
                           DLL = basename(tempfile), silent = quiet)

  opt <- do.call("optim", ad_fun)

  sd <- TMB::sdreport(ad_fun)
  report <- ad_fun$report()

  obj %<>% c(inits = list(inits), map = list(map), ad_fun = list(ad_fun), opt = list(opt),
             sd = list(sd), report = list(report), duration = timer$elapsed())
  class(obj) <- c("tmb_analysis", "mb_analysis")
  obj
}

#' @export
analyse.tmb_model <- function(model, data, drop = character(0),
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
  check_flag(quick)
  check_flag(quiet)
  check_flag(beep)
  check_unused(...)

  if (beep) on.exit(beepr::beep())

  if (quiet) {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

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
