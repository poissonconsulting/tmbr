profile_confint <- function(term, ad_fun, level = level, ...) {
  confint <- TMB::tmbprofile(ad_fun, term, trace = FALSE, ...) %>%
    confint(level = level) %>%
    tibble::as_tibble()
  confint$term <- term
  confint[c("term", "lower", "upper")]
}

adfun_confint <- function(terms, object, tempfile, level, ...) {
  model <- model(object)
  data <- data_set(object) %>%
    modify_data(model = model)

  inits <- estimates(object, "primary")
  map <- map(inits(data, model$gen_inits, model$random_effects))

  load_dynlib(model, tempfile)

  ad_fun <- TMB::MakeADFun(
    data = data, inits, map = map,
    random = names(model$random_effects),
    DLL = basename(tempfile), silent = TRUE
  )

  opt <- try(do.call("optim", ad_fun))

  confint <- purrr::map_df(terms, profile_confint, ad_fun, level = level, ...)
  confint$term %<>% as_term()
  confint
}

adfun_confint_chunk <- function(terms, object, level, ...) {
  confint <- adfun_confint(
    terms = terms$terms, object = object, tempfile = terms$tempfile,
    level = level, ...
  )
}

adfun_confint_parallel <- function(terms, object, level, ...) {
  nworkers <- foreach::getDoParWorkers()

  indices <- parallel::splitIndices(length(terms), nworkers)

  terms <- plyr::llply(indices, function(i, x) x[i], x = terms)

  tempfiles <- replicate(length(terms), tempfile())

  terms %<>% purrr::map2(tempfiles, function(x, y) list(terms = x, tempfile = y))

  on.exit(unload_dynlibs(tempfiles))

  terms %<>% llply(
    .fun = adfun_confint_chunk, .parallel = TRUE,
    object = object, level = level, ...
  )

  terms %<>% dplyr::bind_rows()
  terms$term %<>% as_term()

  terms
}

#' Confint
#'
#' @param object The tmb_ml_analysis object.
#' @param parm A character vector of the \emph{terms} to get the confidence interval for.
#' @param level A number specifying the confidence level. By default 0.95.
#' @param parallel A flag indicating whether to using parallel backend provided by foreach.
#' @param beep A flag indicating whether to beep on completion of the analysis.
#' @param ... Addtional arguments passed to TMB::tmbprofile
#' @export
# need to parallelise and deal with terms
confint.tmb_ml_analysis <- function(object, parm = terms(object),
                                    level = getOption("mb.conf_level", 0.95),
                                    parallel = getOption("mb.parallel", FALSE),
                                    beep = getOption("mb.beep", FALSE),
                                    ...) {
  chk_flag(beep)
  if (beep) on.exit(beepr::beep())
  beep <- FALSE

  chk_scalar(level)
  chk_vector(level, c(0.5, 0.99))
  chk_flag(parallel)

  if (!all(parm %in% terms(object, "all"))) error("not all terms recognised")

  if (!parallel) {
    tempfile <- tempfile()
    on.exit(unload_dynlibs(tempfile))
    return(adfun_confint(terms = parm, object = object, tempfile = tempfile, level = level, ...))
  }
  adfun_confint_parallel(terms = parm, object = object, level = level, ...)
}
