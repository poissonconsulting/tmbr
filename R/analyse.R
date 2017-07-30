load_dynlib <- function(model, tempfile) {
  file <- paste0(tempfile, ".cpp")
  stopifnot(!file.exists(file))

  write(template(model), file = file)
  TMB::compile(file)
  dyn.load(TMB::dynlib(tempfile))
}

unload_dynlibs <- function(tempfiles) {
  for (tempfile in tempfiles) try(dyn.unload(TMB::dynlib(tempfile)), silent = TRUE)
}

# Constructs a list identifying which parameters to fix (based on missing values in inits).
map <- function(inits) {
  check_uniquely_named_list(inits)

  if (!length(inits)) return(list())

  inits <- inits[vapply(inits, function(x) (any(is.na(x))), TRUE)]

  if (!length(inits)) return(list())

  inits %<>% llply(as.vector)
  map <- llply(inits, function(x) 1:length(x))
  map <- purrr::map2(map, inits, function(x, y) {is.na(x) <- is.na(y); x})
  map %<>% llply(function(x) factor(x))
  map
}

tmb_analysis <- function(data, model, tempfile, quick, glance, quiet) {
  timer <- timer::Timer$new()
  timer$start()

  obj <- list(model = model, data = data)
  class(obj) <- c("mb_null_analysis", "tmb_ml_analysis", "tmb_analysis", "mb_analysis")

  if (glance) on.exit(print(glance(obj)))

  data %<>% mbr::modify_data(model = model)

  inits <- inits(data, model$gen_inits, model$random_effects)

  map <- map(inits)

  inits %<>% llply(function(x) {x[is.na(x)] <- 0; x})

  ad_fun <- TMB::MakeADFun(data = data, inits, map = map,
                               random = names(model$random_effects),
                               DLL = basename(tempfile), silent = quiet)

  opt <- try(do.call("optim", ad_fun))

  if (is.try_error(opt)) return(obj)

  sd <- try(TMB::sdreport(ad_fun))

  if (is.try_error(sd)) return(obj)

  obj %<>% c(inits = list(inits), tempfile = tempfile, map = list(map),
             opt = list(opt),
             sd = list(sd))

  class(obj) <- c("tmb_ml_analysis", "tmb_analysis", "mb_analysis")

  obj$ngens <- 1L
  obj$model$derived <- names(list_by_name(obj$sd$value))
  obj$duration <- timer$elapsed()

  obj
}

analyse_tmb_data <- function(data, model, tempfile, quick, glance, quiet) {
  load_dynlib(model, tempfile)

  if (is.data.frame(data)) {
    return(tmb_analysis(data = data, model = model, tempfile = tempfile, quick = quick,
                        glance = glance, quiet = quiet))
  }

  plyr::llply(data, tmb_analysis, model = model, tempfile = tempfile, quick = quick,
              glance = glance, quiet = quiet)
}

analyse_tmb_data_chunk <- function(data, model, quick, quiet) {
  analyse_tmb_data(data = data$data, model = model, tempfile = data$tempfile,
                   quick = quick, quiet = quiet)
}

analyse_tmb_data_parallel <- function(data, model, quick, quiet) {

  nworkers <- foreach::getDoParWorkers()

  indices <- parallel::splitIndices(length(data), nworkers)

  data <- plyr::llply(indices, function(i, x) x[i], x = data)

  tempfiles <- replicate(length(data), tempfile())

  data %<>% purrr::map2(tempfiles, function(x, y) list(data = x, tempfile = y))

  on.exit(unload_dynlibs(tempfiles))

  data %<>% llply(.fun = analyse_tmb_data_chunk, .parallel = TRUE,
                  model = model, quick = quick, quiet = quiet)

  data %<>% unlist(recursive = FALSE)

  data
}

#' @export
analyse.tmb_model <- function(x, data,
                              parallel = getOption("mb.parallel", FALSE),
                              quick = getOption("mb.quick", FALSE),
                              quiet = getOption("mb.quiet", TRUE),
                              glance = getOption("mb.glance", TRUE),
                              beep = getOption("mb.beep", TRUE),
                              ...) {
  if (is.data.frame(data)) {
    check_data2(data)
  } else if (is.list(data)) {
    llply(data, check_data2)
  } else error("data must be a data.frame or a list of data.frames")

  check_flag(parallel)
  check_flag(quick)
  check_flag(quiet)
  check_flag(glance)
  check_flag(beep)

  if (beep) on.exit(beepr::beep())

  check_data_model(data, x)

  if (!parallel || is.data.frame(data)) {
    tempfile <- tempfile()
    on.exit(unload_dynlibs(tempfile))
    return(analyse_tmb_data(data = data, model = x, tempfile = tempfile,
                            quick = quick, quiet = quiet, glance = glance))
  }

  analyse_tmb_data_parallel(data, model = x, quick = quick, quiet = quiet)
}
