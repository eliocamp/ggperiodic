
#' @export
#' @method rename periodic_df
#' @importFrom dplyr rename
#' @importFrom tidyselect vars_rename
rename.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  vars <- tidyselect::vars_rename(names(.data), !!!dplyr::quos(...))
  periods <- periods[vars]
  names(periods) <- names(vars)
  periods <- Filter(Negate(is.null), periods)
  .data <- unperiodic(.data)
  .data <- NextMethod("rename")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}


#' @export
#' @method select periodic_df
#' @importFrom dplyr select
#' @importFrom tidyselect vars_rename
select.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  vars <- tidyselect::vars_rename(names(.data), !!!dplyr::quos(...))
  periods <- periods[vars]
  names(periods) <- names(vars)
  periods <- Filter(Negate(is.null), periods)
  .data <- unperiodic(.data)
  .data <- NextMethod("select")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method mutate periodic_df
#' @importFrom dplyr mutate
mutate.periodic_df <- function(.data, ...) {
  expr <- eval(substitute(alist(...)))

  periods <- get_period(.data)
  periods.mod <- which(names(periods) %in% names(expr))
  if (length(periods.mod) != 0) {
    periods <- periods[-periods.mod]

    # # Mutate periods
    # for (p in periods.mod) {
    #   var <- names(periods)[[p]]
    #   env <- as.list(.data)
    #
    #   env[[var]] <- periods[[p]][1]
    #   min.p <- min(eval(expr[[var]], env, parent.frame()), na.rm = TRUE)
    #
    #   env[[var]] <- periods[[p]][2]
    #   max.p <- max(eval(expr[[var]], env, parent.frame()), na.rm = TRUE)
    #
    #   periods[[p]] <- c(min.p, max.p)
    # }
  }

  .data <- unperiodic(.data)
  .data <- NextMethod("mutate")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}


#' @export
#' @method ungroup periodic_df
#' @importFrom dplyr ungroup
ungroup.periodic_df <- function(x, ...) {
  periods <- get_period(x)
  x <- unperiodic(x)
  x <- NextMethod("ungroup")
  suppressWarnings(do.call(periodic, c(list(object = x), periods)))
}