#' Check if an object is periodic
#'
#' @param object an object
#'
#' @export
is.periodic <- function(object) {
  inherits(object, "periodic_df") | inherits(object, "periodic_v") | inherits(object, "periodic_layer")
}
