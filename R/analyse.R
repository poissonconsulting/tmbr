#' @export
analyse.tmb_model <- function(model, data, beep = TRUE, debug = FALSE, ...) {
  check_data2(data)
  check_flag(beep)
  check_flag(debug)

  if (beep) on.exit(beepr::beep())
  if (!debug) {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  timer <- timer::Timer$new()
  timer$start()

  obj <- list(model = model, data = data)

  data %<>% process_data(data2 = data, select_data = model$select_data,
                             center = model$center, scale = model$scale,
                             random_effects = model$random_effects,
                             modify_data = model$modify_data)

  inits <- inits(data, model$gen_inits, model$random_effects)

  if (any(names(inits) %in% c("fixed", "random", "report", "adreport", "all")))
    error("parameters cannot be named 'fixed', 'random', 'report', 'adreport' or 'all'")

  tempfile <- tempfile()

  write(model_code(model), file = paste0(tempfile, ".cpp"))

  TMB::compile(paste0(tempfile, ".cpp"))

  dyn.load(TMB::dynlib(tempfile))

  ad_fun <- TMB::MakeADFun(data = data, inits,
                           random = names(model$random_effects),
                           DLL = basename(tempfile), silent = !debug)

  opt <- do.call("optim", ad_fun)

  sd <- TMB::sdreport(ad_fun)
  report <- ad_fun$report()

  obj %<>% c(inits = list(inits), ad_fun = list(ad_fun), opt = list(opt),
             sd = list(sd), report = list(report), duration = timer$elapsed())
  class(obj) <- c("tmb_analysis", "mb_analysis")
  obj
}
