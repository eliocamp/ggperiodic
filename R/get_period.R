#' Get period information from an object
#'
#' @param object a periodic object
#'
#' @export
get_period <- function(object) {
  UseMethod("get_period")
}


#' @export
get_period.default <- function(object) {
  attr(object, "period")
}

#' @export
get_period.data.frame <- function(object) {
  periods <- lapply(colnames(object), function(col) get_period(object[[col]]))
  names(periods) <- colnames(object)
  Filter(Negate(is.null), periods)
}