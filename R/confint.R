profile_confint <- function(term, ad_fun, level = level, ...) {
  confint <- TMB::tmbprofile(ad_fun, term, trace = FALSE, ...) %>%
    confint(level = level) %>%
    tibble::as_data_frame()
  confint$term <- term
  confint %<>% dplyr::select(UQ(parse_quosure("term")),
                             UQ(parse_quosure("lower")),
                             UQ(parse_quosure("upper")))
  confint
}

#' Confint
#'
#' @param object The tmb_ml_analysis object.
#' @param parm A character vector of the \emph{terms} to get the confidence interval for.
#' @param level A number specifying the confidence level. By default 0.95.
#' @param ... Addtional arguments passed to TMB::tmbprofile
#' @export
# need to parallelise and deal with terms
confint.tmb_ml_analysis <- function(object, parm = terms(object), level = getOption("mb.conf_level", 0.95), ...) {

  check_vector(parm, "")
  check_number(level, c(0.5, 0.99))

  if(!all(parm %in% terms(object, "all"))) error("not all terms recognised")

  model <- model(object)
  tempfile <- tempfile()
  on.exit(unload_dynlibs(tempfile))
  load_dynlib(model(object), tempfile)

  data <- data_set(object)

  data %<>% mbr::modify_data(model = model)

  inits <- inits(data, model$gen_inits, model$random_effects)

  map <- map(inits)

  inits %<>% llply(function(x) {x[is.na(x)] <- 0; x})

  ad_fun <- TMB::MakeADFun(data = data, inits, map = map,
                           random = names(model$random_effects),
                           DLL = basename(tempfile), silent = TRUE)

  opt <- try(do.call("optim", ad_fun))

  confint <- purrr::map_df(parm, profile_confint, ad_fun, level = level, ...)
  confint$term %<>% as.term()
  confint
}
