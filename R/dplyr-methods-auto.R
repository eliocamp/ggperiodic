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
sample_frac.periodic_df <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = NULL) {
  periods <- get_period(.data)
  .data <- unperiodic(.data)
  .data <- NextMethod("sample_frac")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @method sample_n periodic_df
#' @importFrom dplyr sample_n
sample_n.periodic_df <- function(tbl, size, replace = FALSE, weight = NULL, .env = NULL) {
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
#' @method inner_join periodic_df
#' @importFrom dplyr inner_join
inner_join.periodic_df <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  periods.x <- get_period(x)
  periods.y <- get_period(y)
  periods <- join(periods.x, periods.y)
  x <- unperiodic(x)
  y <- unperiodic(y)
  j <- NextMethod("inner_join")
  suppressWarnings(do.call(periodic, c(list(object = j), periods)))
}

#' @export
#' @method left_join periodic_df
#' @importFrom dplyr left_join
left_join.periodic_df <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  periods.x <- get_period(x)
  periods.y <- get_period(y)
  periods <- join(periods.x, periods.y)
  x <- unperiodic(x)
  y <- unperiodic(y)
  j <- NextMethod("left_join")
  suppressWarnings(do.call(periodic, c(list(object = j), periods)))
}

#' @export
#' @method right_join periodic_df
#' @importFrom dplyr right_join
right_join.periodic_df <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  periods.x <- get_period(x)
  periods.y <- get_period(y)
  periods <- join(periods.x, periods.y)
  x <- unperiodic(x)
  y <- unperiodic(y)
  j <- NextMethod("right_join")
  suppressWarnings(do.call(periodic, c(list(object = j), periods)))
}

#' @export
#' @method full_join periodic_df
#' @importFrom dplyr full_join
full_join.periodic_df <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  periods.x <- get_period(x)
  periods.y <- get_period(y)
  periods <- join(periods.x, periods.y)
  x <- unperiodic(x)
  y <- unperiodic(y)
  j <- NextMethod("full_join")
  suppressWarnings(do.call(periodic, c(list(object = j), periods)))
}

#' @export
#' @method semi_join periodic_df
#' @importFrom dplyr semi_join
semi_join.periodic_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  periods.x <- get_period(x)
  periods.y <- get_period(y)
  periods <- join(periods.x, periods.y)
  x <- unperiodic(x)
  y <- unperiodic(y)
  j <- NextMethod("semi_join")
  suppressWarnings(do.call(periodic, c(list(object = j), periods)))
}

#' @export
#' @method anti_join periodic_df
#' @importFrom dplyr anti_join
anti_join.periodic_df <- function(x, y, by = NULL, copy = FALSE, ...) {
  periods.x <- get_period(x)
  periods.y <- get_period(y)
  periods <- join(periods.x, periods.y)
  x <- unperiodic(x)
  y <- unperiodic(y)
  j <- NextMethod("anti_join")
  suppressWarnings(do.call(periodic, c(list(object = j), periods)))
}

#' @export
dplyr::filter
