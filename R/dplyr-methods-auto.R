#' @export
#' @method arrange periodic_df
#' @importFrom dplyr arrange
arrange.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("arrange")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method distinct periodic_df
#' @importFrom dplyr distinct
distinct.periodic_df <- function(.data, ..., .keep_all = FALSE) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("distinct")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method filter periodic_df
#' @importFrom dplyr filter
filter.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("filter")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method group_by periodic_df
#' @importFrom dplyr group_by
group_by.periodic_df <- function(.data, ..., add = FALSE) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("group_by")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method sample_frac periodic_df
#' @importFrom dplyr sample_frac
sample_frac.periodic_df <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = NULL, ...) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("sample_frac")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method sample_n periodic_df
#' @importFrom dplyr sample_n
sample_n.periodic_df <- function(tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("sample_n")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method slice periodic_df
#' @importFrom dplyr slice
slice.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("slice")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method summarise periodic_df
#' @importFrom dplyr summarise
summarise.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("summarise")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method ungroup periodic_df
#' @importFrom dplyr ungroup
ungroup.periodic_df <- function(x, ...) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("ungroup")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
dplyr::filter
