#' Quickly wrap data
#'
#' Wraps periodic data from one specified range to the other in one line.
#'
#' @param object, the object to wrap
#' @param ..., named formulas with the form `from ~ to` (see examples)
#' @param .group optional group column (see [wrap])
#'
#' @details
#' `qwrap` is a shortcut to `wrap(periodic(obejct, x = range_from), x = range_to)`
#'
#' @examples
#' x <- seq(0, 360 - 20, by = 20)
#' df <- data.frame(x = x, y = cos(x*pi/180))
#' qwrap(df, x = c(0, 360) ~ c(-180, 180))
#'
#' @export
qwrap <- function(object, ..., .group = NULL) {
  cols <- as.list(substitute(list(...))[-1])
  from <- lapply(cols, function(x) x[[2]])
  to <- lapply(cols, function(x) x[[3]])

  .group <- substitute(.group)
  if (!is.null(.group) & !is.character(.group)) {
    .group <- deparse(substitute(.group))
  }

  data_p <- do.call(periodic, c(list(object = object), from))
  data_w <- do.call(wrap, c(list(object = data_p), to, .group = .group))
  data_w
}
