
# nocov start
is.waive <- function (x) {
  inherits(x, "waiver")
}


.capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)),  substr(x, 2, nchar(x)))
}

join <- function(x, y) {
  j <- c(x, y)
  j[unique(names(j))]
}


.set.copy <- function(value) {
  old <- .get.copy()
  options(ggperiodic.data.table.copy = value)
  old
}

.get.copy <- function() {
  getOption("ggperiodic.data.table.copy", default = TRUE)
}

.should.copy <- function(object) {
  !inherits(object, "data.table") | .get.copy()
}
# nocov end