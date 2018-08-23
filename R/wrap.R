#' Wrap periodic data to an arbitary range
#'
#' @param data a periodic data frame
#' @param ... name-value pairs of expressions defining range specifications
#' @param .group optional group column
#'
#'
#'
#' @export
wrap <- function(data, ..., .group = NULL) {
  if (nrow(data) == 0) return(data)

  cols <- as.list(substitute(list(...))[-1])

  if (length(cols) == 0) {
    wraps <- lapply(data, function(x) attr(x, "period"))
    wraps <- Filter(Negate(is.null), wraps)
    # if (length(wraps) > 1) stop("more than one circular dimension")
    if (length(wraps) == 0) {
      warning("no circular dimension specified; returning unchanged data.")
      return(data)
    }
    circular <- names(wraps)
  } else {
    p <- parent.frame()
    wraps <- lapply(cols, function(x) eval(x, data, p))
    circular <- names(wraps)
  }

  bad.cols <- vector()
  for (i in seq_along(circular)) {
    period <- attr(data[[circular[i]]], "period")
    if (is.null(period)) {
      bad.cols <- c(bad.cols,circular[i])
    } else {
      if (is.null(wraps[[i]])) wraps[[i]] <- period
      data <- .wrap_df(data, circular[i], wraps[[i]], .group)
    }
  }
  if (length(bad.cols) != 0) {
    warning(paste0("There are columns with no period attribute. Did not wrap in these dimensions: ",
                   paste0(bad.cols, collapse= ", ")))
  }

  data <- unperiodic(data)
  attr(data, "wrapped") <- TRUE
  return(data)
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
  new_x <- rep(x + shift*width.p, times = times)
  new_x <- new_x + width.p*rep(0:(times-1), each = length(x))
  index <- rep(seq_along(x), times = times)

  keep <- new_x <= max(wrap) & new_x >= min(wrap)
  index <- index[keep]
  new_x <- new_x[keep]

  data <- data[index, ]
  data[[column]] <- new_x

  if (!is.null(group)) {
    new_group <- rep(data[[group]], times = times)
    new_group <- paste0(new_group, "_", rep(0:(times-1), each = length(x)))
    group <- group[keep]
    data[[group]] <- new_group
  }

  return(data)
}
