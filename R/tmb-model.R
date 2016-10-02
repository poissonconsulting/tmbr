check_select <- function(select) {
  if (!(is.character(select) || is.named_list(select)))
    stop("select must be a character vector or named list specifying the columns and their associated classes and values", call. = FALSE)

  if (is.character(select)) {
    check_unique(select)
    return(TRUE)
  }
  check_unique(names(select))
  select
}

check_center <- function(center, select) {
  check_vector(center, "", min_length = 0)

  check_unique(center)

  if (length(select)) {
    if (is.named_list(select)) select %<>% names()
    if (!all(center %in% select)) stop("columns in center must also be in select")
  }
  center
}

check_scale <- function(scale, select) {
  check_vector(scale, "", min_length = 0)

  check_unique(scale)

  if (length(select)) {
    if (is.named_list(select)) select %<>% names()
    if (!all(scale %in% select)) stop("columns in scale must also be in select")
  }
  scale
}

check_inits <- function(inits) {
  if (!is.named_list(inits))
    stop("inits must be a named list specifying the parameters and their starting values" ,call. = FALSE)

  check_unique(names(inits))
  inits
}

check_random <- function(random, select, inits) {
  if (!is.character(random) && !is.named_list(random))
    stop("random must be a character vector or named list specifying the random effects and their associated factors" ,call. = FALSE)

  if (!length(random)) return(random)

  if (is.character(random)) {
    check_unique(random)
    if (!all(random %in% names(inits))) stop("random effects must also be in inits")
    return(random)
  }

 # random is a named list

  check_unique(names(random))

  if (!all(names(random) %in% names(inits))) stop("random effects must also be in inits", call. = FALSE)

  class <- lapply(random, class) %>% unlist()

  if (!all(class == "character")) stop("random effects factors must named as character vectors", call. = FALSE)

  if (length(select)) {
    if (!all(unlist(random) %in% select))
    stop("random effects factors must also be in select", call. = FALSE)
  }

  inits %<>% lapply(dims) %<>% lapply(length)
  inits <- inits[names(random)]
  if (!identical(inits, lapply(random, length))) stop("random effects must have the same number of dimensions as corresponding inits", call. = FALSE)
  random
}

#' TMB Model
#'
#' Creates TMB model.
#'
#' With the exception of \code{model_code} all arguments are sorted.
#'
#' @param model_code A string of the model template code.
#' @param inits A named list of initial values for all fixed and random parameters.
#' @param select A character vector or a named list specifying the columns to select (and in the case of a named list the associated classes and values).
#' @param random A character vector or a named list specifying of the random effects (and in the case of a named list the associated factors).
#' @inheritParams rescale::rescale
#' @return An object of class tmb_model.
#' @seealso \code{\link[datacheckr]{check_data}} \code{\link[rescale]{rescale}}
#' @export
tmb_model <- function(model_code, inits, select = character(0),
                      center = character(0), scale = character(0),
                      random = character(0)) {
  check_string(model_code)
  check_select(select)
  check_center(center, select)
  check_scale(scale, select)
  check_inits(inits)
  check_random(random, select, inits)

  obj <- list(model_code = model_code,
              inits = sort_by_names(inits),
              select = sort_by_names(select),
              center = sort_by_names(center),
              scale = sort_by_names(scale),
              random = sort_by_names(random))
  class(obj) <- "tmb_model"
  obj
}
