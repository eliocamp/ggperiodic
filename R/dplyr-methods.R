


#' @export
#' @importFrom dplyr mutate
mutate.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- NextMethod("mutate")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @importFrom dplyr select
select.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- NextMethod("select")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @importFrom dplyr filter
filter.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- NextMethod("filter")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
dplyr::filter

#' @export
#' @importFrom dplyr summarise
summarise.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- NextMethod("summarise")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @importFrom dplyr arrange
arrange.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- NextMethod("arrange")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}

#' @export
#' @importFrom dplyr group_by
group_by.periodic_df <- function(.data, ...) {
  periods <- get_period(.data)
  .data <- NextMethod("group_by")
  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))
}
