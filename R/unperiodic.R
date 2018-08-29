#' Remove periodic specifications
#'
#' @param object the object to remove periodicities
#' @param ... arguments to methods
#'
#' @return
#'
#' An object of the same class as `object` but with no periodic subclass or
#' periodicity specifications.
#'
#' If `object` is of class `data.table`, then it will modify the object by
#' reference. To modify this behaviour, use
#' `options(ggperiodic.data.table.copy = TRUE)`. `setperiodic()` will modify a
#' `data.table` by reference bypassing the global option.
#'
#'
#' @export
unperiodic <- function(object, ...) {
  UseMethod("unperiodic")
}

#' @export
#' @importFrom data.table copy setattr
unperiodic.periodic_df <- function(object, ...) {
  cols <- as.list(substitute(list(...))[-1])
  if (.should.copy(object)) {
    object <- data.table::copy(object)
  }

  if (length(cols) == 0) {
    cols <- colnames(object)
  } else {
    cols <- vapply(cols, as.character, "a")
  }

  for (i in seq_along(cols)) {
    data.table::setattr(object[[cols[i]]], "period", NULL)
    data.table::setattr(object[[cols[i]]], "class", class(object[[cols[i]]])[class(object[[cols[i]]]) != "sticky"])
    data.table::setattr(object, "class", class(object)[class(object) != "periodic_df"])

    # class(object[[cols[i]]]) <- class(object[[cols[i]]])[class(object[[cols[i]]]) != "sticky"]
    # class(object) <- class(object)[class(object) != "periodic_df"]
  }

  if (.should.copy(object)) {
    return(object)
  } else {
    return(invisible(object))
  }
}

#' @export
unperiodic.default <- function(object, ...) {
  attr(object, "period") <- NULL
  class(object) <- class(object)[class(object) != "periodic_v"]
  object <- sticky::unstick(object)
  object
}

#' @export
#' @rdname unperiodic
setunperiodic <- function(object, ...) {
  old <- .set.copy(FALSE)
  on.exit(.set.copy(old))
  unperiodic(object, ...)
}