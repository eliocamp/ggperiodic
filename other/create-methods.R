library(dplyr)
dplyr.methods <- c("arrange", "distinct", "filter", "group_by",
                   "sample_frac", "sample_n", "slice", "summarise", "ungroup")
dplyr.joins <- c("inner_join", "left_join", "right_join", "full_join",
                 "semi_join", "anti_join")
dplyr.joins <- character()

dplyr.exports <- c("filter")


make.args <- function(args) {
  args_text <- character()
  for (a in seq_along(args)) {
    x <- args[a]
    val <- deparse(x[[1]])
    if (length(val) == 0) {
      args_text <- c(args_text, paste(c(names(x), "NULL"), collapse = " = "))
    } else if (val != "") {
      args_text <- c(args_text, paste(c(names(x), val), collapse = " = "))
    } else {
      args_text <-  c(args_text, names(x))
    }
  }
  paste0(args_text, collapse = ", ")
}

automethod <-"R/dplyr-methods-auto.R"

file.create(automethod)

fileConn <- file(automethod)

lines <- character()
for (m in seq_along(dplyr.methods)) {
  method <- dplyr.methods[m]
  args <- as.list(match.fun(method))
  args <- args[-length(args)]
  lines <- c(lines,
             "#' @export",
             paste0("#' @method ", method, " periodic_df"),
             paste0("#' @importFrom dplyr ", method),
             paste0(method, ".periodic_df <- function(", make.args(args), ") {"),
             "  periods <- get_period(.data)",
             "  .data <- unperiodic(.data)",
             paste0('  .data <- NextMethod("', method, '")'),
             "  suppressWarnings(do.call(periodic, c(list(object = .data), periods)))",
             "}",
             "")
}


for (j in seq_along(dplyr.joins)) {
  method <- dplyr.joins[j]
  args <- as.list(match.fun(method))
  args <- args[-length(args)]
  lines <- c(lines,
             "#' @export",
             paste0("#' @method ", method, " periodic_df"),
             paste0("#' @importFrom dplyr ", method),
             paste0(method, ".periodic_df <- function(", make.args(args), ") {"),
             "  periods.x <- get_period(x)",
             "  periods.y <- get_period(y)",
             "  periods <- join(periods.x, periods.y)",
             "  x <- unperiodic(x)",
             "  y <- unperiodic(y)",
             paste0('  j <- NextMethod("', method, '")'),
             "  suppressWarnings(do.call(periodic, c(list(object = j), periods)))",
             "}",
             "")
}

for (e in seq_along(dplyr.exports)) {
  method <- dplyr.exports[e]
  lines <- c(lines,
             "#' @export",
             paste0("dplyr::", method))

}



writeLines(lines, fileConn)
close(fileConn)
