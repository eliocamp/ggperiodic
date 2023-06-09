#' @importFrom ggplot2 fortify
#' @export
fortify.periodic_df <- function(model, data, ...) {
  call <- as.list(parent.frame())
  .group <- call$mapping$group
  if (!is.null(.group)) .group <- as.character(.group)[[2]]

  if (inherits(call$geom, "Geom") | inherits(call$stat, "Stat")) {
    params <- vector()
    if (inherits(call$geom, "Geom")) {
      # the call is from a geom
      params <- call$geom$parameters()
      stat <- call$stat
      if (is.character(stat)) {
        stat <- paste0("Stat", .capitalize(.camelize(stat)))
        stat <- eval(as.name(stat))
      }

      params <- c(params, stat$parameters())
    } else if (inherits(call$stat, "Stat")) {
      # the call is from a stat
      parmas <- call$stat$parameters()
      geom <- call$geom
      geom <- paste0("Geom", .capitalize(.camelize(geom)))
      geom <- eval(as.name(geom))
      params <- c(params, geom$parameters())
    }
    periodic.vars <- names(get_period(model))

    common <- intersect(periodic.vars, params)
    if (length(common) != 0) {
      warning(paste0("Periodic columns match geom or stat parameters. Not wrapping.",
                     "Bad columns: ", paste0(common, collapse = ", ")))
      return(model)
    }

    vars <- call$params[periodic.vars]
    vars <- Filter(Negate(is.null), vars)

    do.call(wrap, c(list(object = model, .group = .group),
                    vars))
  } else {
    do.call(wrap, c(list(object = model, .group = .group),
                    list(...)))
  }
}

