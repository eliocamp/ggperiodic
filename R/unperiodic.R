#' Remove periodic specifications
#'
#' @param object the object to remove periodicities
#' @param ... arguments to methods
#'
#' @export
unperiodic <- function(object, ...) {
  UseMethod("unperiodic")
}

#' @export
unperiodic.periodic_df <- function(object, ...) {
  cols <- as.list(substitute(list(...))[-1])

  if (length(cols) == 0) {
    cols <- colnames(object)
  } else {
    cols <- vapply(cols, as.character, "a")
  }

  for (i in seq_along(cols)) {
    attr(object[[cols[i]]], "period") <- NULL
    class(object[[cols[i]]]) <- class(object[[cols[i]]])[class(object[[cols[i]]]) != "sticky"]
    # .data <- sticky::sticky(.data)
  }
  class(object) <- class(object)[class(object) != "periodic_df"]
  return(object)
}

#' @export
unperiodic.default <- function(object, ...) {
  attr(object, "period") <- NULL
  class(object) <- class(object)[class(object) != "periodic_v"]
  object <- sticky::unstick(object)
  object
}

