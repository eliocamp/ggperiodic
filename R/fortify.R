#' @importFrom ggplot2 fortify
#' @export
fortify.periodic_df <- function(data, ...) {
  wrap(data, ...)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.periodic_layer <- function(object, plot, object_name) {
  if (!is.null(object$layer$data) & !is.waive(object$layer$data)) {
    # If data is defined for this layer
    if (!is.periodic(object$layer$data)) {
      object$layer$data <- do.call(periodic, c(list(.x = object$layer$data), object$cols))
    }
    if (is.periodic(object$layer$data)) {
      object$layer$data <- do.call(wrap, c(list(data = object$layer$data), object$cols))
    }

  } else {
    # If data comes from plot
    if (!is.periodic(plot$data)) {
      if (!isTRUE(attr(plot$data, "wrapped"))) {
        # If is not a wrapped periodic dataframe
        object$layer$data <- do.call(periodic, c(list(.x = plot$data), object$cols))
      }
    }
    if (is.periodic(object$layer$data)) {
      object$layer$data <- do.call(wrap, c(list(data = object$layer$data), object$cols))
    }
  }

  plot$layers <- append(plot$layers, object$layer)
  plot
}