
# nocov start
is.waive <- function (x) {
  inherits(x, "waiver")
}


.capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)),  substr(x, 2, nchar(x)))
}




# From ggplot2
.camelize <- function (x, first = FALSE)  {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first)
    x <- firstUpper(x)
  x
}

firstUpper <- function (s) {
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
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