inits <- function(data, gen_inits, random_effects) {
  inits <- gen_inits(data)
  random_effects <- random_effects[!names(random_effects) %in% names(inits)]
  inits %<>% c(random_inits(data, random_effects))
  inits %<>% sort_by_names()
  inits
}

random_inits <- function(data, random_effects) {
  if (!length(random_effects)) return(NULL)

  if (!all(unlist(random_effects) %in% names(data)))
    error("elements in random_effects must in data")

  data <- data[vapply(data, is.factor, TRUE)]

  if (!all(unlist(random_effects) %in% names(data)))
    error("elements in random_effects must be factors in data")

  data %<>% vapply(nlevels, 1L) %>% unlist()

  random_effects %<>% lapply(replace_n, data)
  random_effects %<>% lapply(zeros)
  random_effects
}

zeros <- function (dims) {
  stopifnot(is.integer(dims))
  if (length(dims) == 1)
    return(numeric(dims))
  if (length(dims) == 2)
    return(matrix(0, nrow = dims[1], ncol = dims[2]))
  array(0, dims)
}

replace_n <- function(random_effect, data) {
  stopifnot(is.character(random_effect))
  stopifnot(is.integer(random_effect))
  stopifnot(all(names(random_effect) %in% names(data)))

  return(data[names(random_effect)])
}

