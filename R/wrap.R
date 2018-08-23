#' Wrap periodic data to an arbitary range
#'
#' @param object a periodic data frame
#' @param ... name-value pairs of expressions defining range specifications
#' @param .group optional group column
#'
#'
#'
#' @export
wrap <- function(object, ...) {
  UseMethod("wrap")
}

#' @export
#' @rdname wrap
wrap.periodic_df <- function(object, ..., .group = NULL) {
  if (nrow(object) == 0) return(object)

  cols <- as.list(substitute(list(...))[-1])

  if (length(cols) == 0) {
    wraps <- lapply(object, function(x) attr(x, "period"))
    wraps <- Filter(Negate(is.null), wraps)
    # if (length(wraps) > 1) stop("more than one circular dimension")
    if (length(wraps) == 0) {
      warning("no circular dimension specified; returning unchanged data.")
      return(object)
    }
    circular <- names(wraps)
  } else {
    p <- parent.frame()
    wraps <- lapply(cols, function(x) eval(x, object, p))
    circular <- names(wraps)
  }

  bad.cols <- vector()
  for (i in seq_along(circular)) {
    period <- attr(object[[circular[i]]], "period")
    if (is.null(period)) {
      bad.cols <- c(bad.cols,circular[i])
    } else {
      if (is.null(wraps[[i]])) wraps[[i]] <- period
      object <- .wrap_df(object, circular[i], wraps[[i]], group = .group)
    }
  }
  if (length(bad.cols) != 0) {
    warning(paste0("There are columns with no period attribute. Did not wrap in these dimensions: ",
                   paste0(bad.cols, collapse= ", ")))
  }

  object <- unperiodic(object)
  attr(object, "wrapped") <- TRUE
  return(object)
}

.wrap_df <- function(data, column, wrap, group) {
  period <- attr(data[[column]], "period")
  start.p <- min(period)

  width.w <- diff(range(wrap))
  width.p <- diff(range(period))

  shift <- (min(wrap) - start.p)/width.p
  shift <- floor(shift)

  times <- ceiling(width.w/width.p) + 1

  x <- data[[column]]
  if (!is.null(group)) new_group <- data[[group]]

  new_x <- rep(x + shift*width.p, times = times)
  new_x <- new_x + width.p*rep(0:(times-1), each = length(x))
  index <- rep(seq_along(x), times = times)

  keep <- new_x <= max(wrap) & new_x >= min(wrap)
  index <- index[keep]
  new_x <- new_x[keep]

  data <- data[index, ]
  data[[column]] <- new_x

  if (!is.null(group)) {
    new_group <- rep(new_group, times = times)
    new_group <- paste0(new_group, "_", rep(0:(times-1), each = length(x)))
    new_group <- new_group[keep]
    data[[group]] <- new_group
  }

  return(data)
}

#' #' @export
#' #' @rdname wrap
#' wrap.Layer <- function(object, ...) {
#'   cols <- as.list(substitute(list(...))[-1])
#'   structure(list(layer = object, cols = cols), class = "periodic_layer")
#' }