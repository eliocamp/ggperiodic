
is.waive <- function (x) {
  inherits(x, "waiver")
}


.capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)),  substr(x, 2, nchar(x)))
}
