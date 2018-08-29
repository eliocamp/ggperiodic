#' Add or remove periodic variables
#'
#' Creates a periodic object by specifying the periodic variables and their
#' periods.
#'
#' @param object the object to coerce to periodic
#' @param ... name-value pairs of expressions defining the period
#' @param period a numeric vector whose range defines the period
#'
#' @details
#'
#' This is a generic function and ggperiodic provides methods for vectors, data frames
#' and ggplot2 Layers (geoms and stats).
#'
#' @examples
#' library(ggplot2)
#'
#' x <- seq(0, 360 - 20, by = 20)
#' df <- data.frame(x = x, y = cos(x*pi/180))
#' df_p <- periodic(df, x = c(0, 360))
#'
#' ggplot(df_p, aes(x, y)) +
#'    geom_line() +            # periodic data
#'    geom_point(data = df)    # non periodic data
#'
#' # Extend domain
#' ggplot(df_p, aes(x, y), x = c(-180, 540)) +
#'    geom_line() +
#'    geom_point(data = df)
#'
#' # with non regular intervals
#' x <- runif(30, 0, 360)
#' df <- periodic(data.frame(x = x, y = cos(x*pi/180)),
#'                x = c(0, 360))
#' ggplot(df, aes(x, y), x = c(-180, 540)) +
#'    geom_point()
#'
#' @export
periodic <- function(object, ...) {
  UseMethod("periodic")
}

#' @importFrom sticky sticky
#' @export
#' @rdname periodic
periodic.default <- function(object, period, ...) {
  attr(object, "period") <- range(period)
  object <- sticky::sticky(object)
  if (!is.periodic(object)) class(object) <- c("periodic_v", class(object))
  object
}

#' @export
#' @rdname periodic
#' @method periodic data.frame
periodic.data.frame <- function(object, ...) {
  cols <- as.list(substitute(list(...))[-1])

  if(length(cols) == 0) {
    warning("No columns defined. Returning unchanged data.")
    return(object)
  }

  iscol <- names(cols) %in% colnames(object)

  if (sum(!iscol) == length(cols)) {
    warning(paste0("All columns are not found on data. Returning unchanged data."))
    return(object)
  }

  if (any(!iscol)) {
    warning(paste0("Some columns not found on data: ",
                   paste0(names(cols)[!iscol], collapse= ", ")))
  }

  cols <- cols[iscol]

  bad.cols <- vector()
  for (i in seq_along(cols)) {
    if (is.null(cols[[i]])) {
      period <- range(object[[names(cols)[i]]])
    } else {
      period <- range(eval(cols[[i]], object, parent.frame()))
    }

    r <- range(object[[names(cols)[i]]])
    if (r[1] < period[1] | r[2] > period[2]) {
      bad.cols <- c(bad.cols, names(cols)[i])
    } else {
      attr(object[[names(cols)[i]]], "period") <- period
      class(object[[names(cols)[i]]]) <- c("sticky", class(object[[names(cols)[i]]]))
    }
  }
  if (length(bad.cols) == length(cols)) {
    warning("All columns have data outside the defined period. Returning unchanged data")
    return(object)
  }
  if (length(bad.cols) != 0) {
    warning(paste0("Some columns have data outside the defined period: ",
                   paste0(bad.cols, collapse= ", ")))
  }

  if (!inherits(object, "periodic_df")) {
    class(object) <- c("periodic_df", class(object))
  }
  object
}


#' @export
print.periodic_df <- function(x, ...) {
  NextMethod("print")
  period <- get_period(x)
  for (i in seq_along(period)) {
    cat(names(period)[i], " = [", period[[i]][1] , "; ",
        period[[i]][2], "]\n",
        sep = "")
  }
}

