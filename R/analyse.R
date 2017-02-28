compile_code <- function(model, tempfile) {
  file <- paste0(tempfile, ".cpp")
  stopifnot(!file.exists(file))

  write(template(model), file = file)
  TMB::compile(file)
}

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
  coef <- coef(object)
  derived <- coef(object, "derived")
  random <- coef(object, "random")

  coef %<>% dplyr::bind_rows(derived) %>% dplyr::bind_rows(random) %>% dplyr::select_(~term, ~estimate)

  coef %<>% dplyr::mutate_(parameter = ~str_replace(term, "^(\\w+)(.*)", "\\1"))

  coef %<>% plyr::dlply(~parameter, lmcmcarray)

  class(coef) <- "mcmcr"

  coef %<>% sort()
  coef
}

# Constructs a list identifying which parameters to fix (based on missing values in inits).
map <- function(inits) {
  check_uniquely_named_list(inits)

  if (!length(inits)) return(list())

  inits <- inits[vapply(inits, function(x) (any(is.na(x))), TRUE)]

  if (!length(inits)) return(list())

  inits %<>% lapply(as.vector)
  map <- lapply(inits, function(x) 1:length(x))
  map <- purrr::map2(map, inits, function(x, y) {is.na(x) <- is.na(y); x})
  map %<>% lapply(function(x) factor(x))
  map
}

tmb_analysis <- function(data, model, tempfile, quick, quiet) {
  timer <- timer::Timer$new()
  timer$start()

  obj <- list(model = model, data = data)

  data %<>% mbr::modify_data(model = model)

  inits <- inits(data, model$gen_inits, model$random_effects)

  map <- map(inits)

  inits %<>% lapply(function(x) {x[is.na(x)] <- 0; x})

  ad_fun <- TMB::MakeADFun(data = data, inits, map = map,
                           random = names(model$random_effects),
                           DLL = basename(tempfile), silent = quiet)

  opt <- try(do.call("optim", ad_fun))

  if (is.try_error(opt)) {
    class(obj) <- c("mb_null_analysis", "tmb_ml_analysis", "tmb_analysis", "mb_analysis")
  } else{
    sd <- try(TMB::sdreport(ad_fun))

    if (is.try_error(sd)) {
      class(obj) <- c("mb_null_analysis", "tmb_ml_analysis", "tmb_analysis", "mb_analysis")
    } else {

      report <- ad_fun$report()

      obj %<>% c(inits = list(inits), tempfile = tempfile, map = list(map), ad_fun = list(ad_fun), opt = list(opt),
                 sd = list(sd), report = list(report))

      class(obj) <- c("tmb_ml_analysis", "tmb_analysis", "mb_analysis")

      obj$mcmcr <- lmcmcr(obj)
      obj$ngens <- 1L
      obj$model$derived <- names(list_by_name(obj$sd$value))
      obj$duration <- timer$elapsed()
    }
  }

  print(glance(obj))

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

  check_data_model(data, model)

  tempfile <- tempfile()
  compile_code(model, tempfile)
  dynlib <- TMB::dynlib(tempfile)
  dyn.load(dynlib)
  on.exit(dyn.unload(dynlib))

  if (is.data.frame(data)) {
    return(tmb_analysis(data = data, model = model, tempfile = tempfile, quick = quick, quiet = quiet))
  }

  lapply(data, tmb_analysis, model = model, tempfile = tempfile, quick = quick, quiet = quiet)
}
