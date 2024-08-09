list_by_name <- function(x) {
  list <- list()
  names <- unique(names(x))
  for (name in names) {
    list %<>% c(list(unname(x[names(x) == name])))
  }
  names(list) <- names
  list
}

add_report <- function(x) {
  x %<>% str_replace_all("\\sREPORT[(][^)]+[)];", "\n")
  x %<>% str_replace_all("\\sADREPORT[(]([^)]+)[)]", "REPORT(\\1);\nADREPORT(\\1)")
  x
}

load_dynlib <- function(model, tempfile) {
  file <- paste0(tempfile, ".cpp")
  stopifnot(!file.exists(file))

  template <- template(model)

  template %<>% add_report()

  write(template, file = file)
  TMB::compile(file)
  dyn.load(TMB::dynlib(tempfile))
}

unload_dynlibs <- function(tempfiles) {
  for (tempfile in tempfiles) try(dyn.unload(TMB::dynlib(tempfile)), silent = TRUE)
}

# Constructs a list identifying which parameters to fix (based on missing values in inits).
map <- function(inits) {
  check_uniquely_named_list(inits)

  if (!length(inits)) {
    return(list())
  }

  inits <- inits[vapply(inits, function(x) (any(is.na(x))), TRUE)]

  if (!length(inits)) {
    return(list())
  }

  inits %<>% llply(as.vector)
  map <- llply(inits, function(x) 1:length(x))
  map <- purrr::map2(map, inits, function(x, y) {
    is.na(x) <- is.na(y)
    x
  })
  map %<>% llply(function(x) factor(x))
  map
}

set_dim <- function(x, dim) {
  x <- x$value
  if (length(dim) == 1) {
    return(x)
  }
  dim(x) <- dim
  x
}

adreport <- function(ad_fun, sd) {
  dims <- purrr::map(ad_fun$report(), dims)

  if (!length(dims)) {
    return(list(estimate = list(), sd = list()))
  }

  sum <- summary(sd, select = "report") %>%
    as.data.frame()

  sum$parameter <- row.names(sum)
  row.names(sum) <- NULL

  sum$parameter <- str_replace(sum$parameter, "[.]\\d+$", "")

  estimate <- dplyr::select(sum, value = "Estimate", "parameter") %>%
    plyr::dlply("parameter", function(x) {
      dplyr::select(x, !"parameter")
    })

  sd <- dplyr::select(sum, value = "Std. Error", "parameter") %>%
    plyr::dlply("parameter", function(x) {
      dplyr::select(x, !"parameter")
    })

  attr(estimate, "split_type") <- NULL
  attr(estimate, "split_labels") <- NULL
  attr(sd, "split_type") <- NULL
  attr(sd, "split_labels") <- NULL

  estimate <- estimate[order(names(estimate))]
  sd <- sd[order(names(sd))]
  dims <- dims[order(names(dims))]

  estimate %<>% purrr::map2(dims, set_dim)
  sd %<>% purrr::map2(dims, set_dim)

  return(list(estimate = estimate, sd = sd))
}

tmb_analysis <- function(data, model, tempfile, glance, quiet) {
  timer <- timer::Timer$new()
  timer$start()

  obj <- list(model = model, data = data)
  class(obj) <- c("mb_null_analysis", "tmb_ml_analysis", "tmb_analysis", "mb_analysis")

  if (glance) on.exit(print(glance(obj)))

  data %<>% modify_data(model = model)

  inits <- inits(data, model$gen_inits, model$random_effects)

  map <- map(inits)

  inits %<>% llply(function(x) {
    x[is.na(x)] <- 0
    x
  })

  ad_fun <- TMB::MakeADFun(
    data = data, inits, map = map,
    random = names(model$random_effects),
    DLL = basename(tempfile), silent = quiet
  )

  opt <- try(do.call("optim", ad_fun))

  if (is.try_error(opt)) {
    return(obj)
  }

  sd <- try(TMB::sdreport(ad_fun))

  if (is.try_error(sd)) {
    return(obj)
  }

  adreport <- adreport(ad_fun, sd)

  logLik <- opt$value * -1

  estimate <- as.list(sd, "Est")
  sd <- as.list(sd, "Std")

  attr(estimate, which = "what") <- NULL
  attr(sd, which = "what") <- NULL

  estimate %<>% c(adreport$estimate)
  sd %<>% c(adreport$sd)

  estimate <- estimate[order(names(estimate))]
  sd <- sd[order(names(sd))]

  mcmcr <- as.mcmcr(estimate)

  obj %<>% c(
    inits = list(inits),
    logLik = logLik,
    mcmcr = list(mcmcr),
    sd = list(sd),
    opt = list(opt)
  )

  class(obj) <- c("tmb_ml_analysis", "tmb_analysis", "mb_analysis")

  obj$ngens <- 1L
  obj$duration <- timer$elapsed()

  obj
}

analyse_tmb_data <- function(data, model, tempfile, glance, quiet) {
  load_dynlib(model, tempfile)

  if (is.data.frame(data)) {
    return(tmb_analysis(data = data, model = model, tempfile = tempfile, glance = glance, quiet = quiet))
  }

  plyr::llply(data, tmb_analysis, model = model, tempfile = tempfile, glance = glance, quiet = quiet)
}

analyse_tmb_data_chunk <- function(data, model, quiet) {
  analyse_tmb_data(
    data = data$data, model = model, tempfile = data$tempfile,
    quiet = quiet, glance = FALSE
  )
}

analyse_tmb_data_parallel <- function(data, model, quiet) {
  nworkers <- foreach::getDoParWorkers()

  indices <- parallel::splitIndices(length(data), nworkers)

  data <- plyr::llply(indices, function(i, x) x[i], x = data)

  tempfiles <- replicate(length(data), tempfile())

  data %<>% purrr::map2(tempfiles, function(x, y) list(data = x, tempfile = y))

  on.exit(unload_dynlibs(tempfiles))

  data %<>% llply(
    .fun = analyse_tmb_data_chunk, .parallel = TRUE,
    model = model, quiet = quiet
  )

  data %<>% unlist(recursive = FALSE)

  data
}

#' @export
analyse.tmb_model <- function(x, data,
                              nchains = getOption("mb.nchains", 3L),
                              niters = getOption("mb.niters", 1000L),
                              nthin = getOption("mb.thin", NULL),
                              parallel = getOption("mb.parallel", FALSE),
                              quiet = getOption("mb.quiet", TRUE),
                              glance = getOption("mb.glance", TRUE),
                              beep = getOption("mb.beep", TRUE),
                              ...) {
  chk_flag(beep)
  if (beep) on.exit(beepr::beep())

  if (is.data.frame(data)) {
    check_data(data)
  } else if (is.list(data)) {
    llply(data, check_data)
  } else {
    error("data must be a data.frame or a list of data.frames")
  }

  chk_flag(parallel)
  chk_flag(quiet)
  chk_flag(glance)

  check_data_model(data, x)

  if (!parallel || is.data.frame(data)) {
    tempfile <- tempfile()
    on.exit(unload_dynlibs(tempfile))
    return(analyse_tmb_data(
      data = data, model = x, tempfile = tempfile,
      quiet = quiet, glance = glance
    ))
  }

  analyse_tmb_data_parallel(data, model = x, quiet = quiet)
}
