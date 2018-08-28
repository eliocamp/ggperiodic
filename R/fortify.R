#' @importFrom ggplot2 fortify
#' @export
fortify.periodic_df <- function(data, ...) {
  call <- as.list(parent.frame())
  .group <- call$mapping$group
  if (!is.null(.group)) .group <- as.character(.group)[[2]]

  if (inherits(call$geom, "Geom") | inherits(call$stat, "Stat")) {
    params <- vector()
    if (inherits(call$geom, "Geom")) {
      # the call is from a geom
      params <- call$geom$parameters()
      stat <- call$stat
      stat <- paste0("Stat", .capitalize(stat))
      stat <- eval(as.name(stat))
      params <- c(params, stat$parameters())
    } else if (inherits(call$stat, "Stat")) {
      # the call is from a stat
      parmas <- call$stat$parameters()
      geom <- call$geom
      geom <- paste0("Geom", .capitalize(geom))
      geom <- eval(as.name(geom))
      params <- c(params, geom$parameters())
    }
    periodic.vars <- names(get_period(data))

    common <- intersect(periodic.vars, params)
    if (length(common) != 0) {
      warning(paste0("Periodic columns match geom or stat parameters. Not wrapping.",
                     "Bad columns: ", paste0(common, collapse = ", ")))
      return(data)
    }

    vars <- call$params[periodic.vars]
    vars <- Filter(Negate(is.null), vars)

    do.call(wrap, c(list(object = data, .group = .group),
                    vars))
  } else {
    do.call(wrap, c(list(object = data, .group = .group),
                    list(...)))
  }
}


#' #' @importFrom ggplot2 ggplot_add
#' #' @export
#' ggplot_add.periodic_layer <- function(object, plot, object_name) {
#'   gr <- as.character(object$layer$mapping$group)[2]
#'   if (!is.null(object$layer$data) & !is.waive(object$layer$data)) {
#'     # If data is defined for this layer
#'     if (!is.periodic(object$layer$data)) {
#'       object$layer$data <- do.call(periodic, c(list(object = object$layer$data),
#'                                                object$cols))
#'     }
#'     if (is.periodic(object$layer$data)) {
#'       object$layer$data <- do.call(wrap, c(list(object = object$layer$data,
#'                                                 .group = gr),
#'                                            object$cols))
#'     }
#'
#'   } else {
#'     # If data comes from plot
#'     if (!is.periodic(plot$data)) {
#'       if (!isTRUE(attr(plot$data, "wrapped"))) {
#'         # If is not a wrapped periodic dataframe
#'         object$layer$data <- do.call(periodic, c(list(object = plot$data), object$cols))
#'       }
#'     }
#'     if (is.periodic(object$layer$data)) {
#'       object$layer$data <- do.call(wrap, c(list(object = object$layer$data,
#'                                                 .group = gr),
#'                                            object$cols))
#'     }
#'   }
#'
#'   plot$layers <- append(plot$layers, object$layer)
#'   plot
#' }