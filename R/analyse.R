#' Analyse
#'
#' Analyse a data set and model.
#'
#' @param data_set The data frame to analyse.
#' @param model The model to use for the analysis.
#' @param beep A flag indicating whether to beep on completion of the analysis.
#' @param debug A flag indicating whether to run in debug mode.
#' @param ...  Not used.
#' @export
analyse <- function(model, data_set, beep = TRUE, debug = FALSE, ...) {UseMethod("analyse")}

#' @export
analyse.tmb_model <- function(model, data_set, beep = TRUE, debug = FALSE, ...) {
  check_data1(data_set)
  check_flag(beep)
  check_flag(debug)

  if (beep) on.exit(beepr::beep())
  if (!debug) {
    sink(tempfile())
    on.exit(sink(), add = TRUE)
  }

  timer <- timer::Timer$new()
  timer$start()

  obj <- list(model = model, data_set = data_set)

  data_set %<>% select_data(model$select_data)
  data_set %<>% rescale::rescale(center = model$center, scale = model$scale)
  data_set %<>% as.list()

  tempfile <- tempfile()

  write(model_code(model), file = paste0(tempfile, ".cpp"))

  TMB::compile(paste0(tempfile, ".cpp"))

  dyn.load(TMB::dynlib(tempfile))

  ad_fun <- TMB::MakeADFun(data = data_set,  parameters = parameters(model),
                           random = random_effects(model),
                           DLL = basename(tempfile), silent = !debug)

  opt <- do.call("optim", ad_fun)

  obj %<>% c(ad_fun = list(ad_fun), opt = list(opt), duration = timer$elapsed())
  class(obj) <- "tmb_analysis"
  obj
}

#' TMB Analysis
#'
#' Analyse a data frame using a TMB model.
#'
#' @inheritParams analyse
#' @export
tmb_analysis <- function(data_set, model, beep = TRUE, debug = FALSE, ...) {
  if (!is.tmb_model(model)) stop("model must be a tmb_model", call. = FALSE)

  analyse(model, data_set, beep = beep, debug = debug, ...)
}

