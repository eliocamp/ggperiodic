#' Wrap periodic data to an arbitrary range
#'
#' @param object a periodic data frame
#' @param ... name-value pairs of expressions defining range specifications
#' @param .group optional group column (see examples)
#'
#' @return
#'
#' An object of the same class as `object` but with no periodic subclass or
#' periodicity specifications and wrapped dimensions.
#'
#' @examples
#'
#' x <- seq(0, 360 - 20, by = 20)
#' df <- data.frame(x = x, y = cos(x*pi/180))
#' df_p <- periodic(df, x = c(0, 360))
#'
#' # wrap in default rante
#' df_wrapped <- wrap(df_p)
#' range(df_wrapped$x)
#' range(df$x)
#'
#' # specify range
#' df_wrapped <- wrap(df_p, x = c(-145, 365))
#' range(df_wrapped$x)
#'
#' # with non regular intervals
#' x <- runif(30, 0, 360)
#' df <- periodic(data.frame(x = x, y = cos(x*pi/180)),
#'                x = c(0, 360))
#' df_wrapped <- wrap(df, x = c(-180, 540))
#' range(df_wrapped$x)
#' range(df$x)
#' \dontrun{
#' # This example illustrates the use of the .group parameter
#' library(ggplot2)
#' map <- periodic(map_data("world"), long = long)
#'
#' # If wrapped without .group, the repated parts of the map
#' # have the same group and so polygons are not correctly defined.
#' map_wrapped <- wrap(map, long = c(-180, 360))
#' ggplot(map_wrapped, aes(long, lat, group = group)) +
#'     geom_path()
#'
#' # Using groups, you get the correct grouping.
#' map_wrapped <- wrap(map, long = c(-180, 360), .group = group)
#' ggplot(map_wrapped, aes(long, lat, group = group)) +
#'     geom_path()
#'}
#' @export
wrap <- function(object, ...) {
  if (!is.periodic(object)) {
    stop("object must first be periodic with periodic(object, ...)")
  }
  UseMethod("wrap")
}


#' @export
#' @rdname wrap
wrap.periodic_df <- function(object, ..., .group = NULL) {
  if (nrow(object) == 0) {
    return(object)
  }

  cols <- as.list(substitute(list(...))[-1])
  .group <- substitute(.group)
  if (!is.null(.group) & !is.character(.group)) {
    .group <- deparse(substitute(.group))
  }

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
  return(object)
}

.wrap_df <- function(data, column, wrap, group) {
  period <- attr(data[[column]], "period")
  start.p <- min(period)

  width.w <- diff(range(wrap))
  width.p <- diff(range(period))
  x <- data[[column]]

  shift <- (min(wrap) - min(x))/width.p
  shift <- floor(shift)

  times <- ceiling(width.w/width.p) + 1

  if (!is.null(group))  new_group <- data[[group]]

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


