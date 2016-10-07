#' Augment TMB Analysis
#'
#' Augments the data of a TMB analysis object.
#'
#' @param x The tmb_analysis object to augment.
#' @param terms The terms to augment the data with.
#' @param conf_level A number specifying the confidence level. By default 0.95.
#' @param ... Unused.
#' @seealso \code{\link[broom]{augment}}.
#' @export
augment.tmb_analysis <- function(x, terms = c("fit", "residual"), conf_level = 0.95, ...) {
  check_vector(terms, "", min_length = 1)
  check_unique(terms)
  data <- data_set(x)
  for (term in terms) {
    reported <- reported(x, term = term, conf_level = conf_level)
    reported <- reported[!colnames(reported) %in% colnames(data)]
    data %<>% dplyr::bind_cols(reported)
  }
  data
}
