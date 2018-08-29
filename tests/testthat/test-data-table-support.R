context("data.table support")
library(data.table)

example <- function() {
  set.seed(42)
  df <- setDT(expand.grid(x = seq(0, 360 - 10, by = 10),
                          y = seq(-90, 90, by = 10)))
  df <- df[, z := cos(x*pi/180)*sin(y*pi/180)]
  return(df)
}


df <- example()

test_that("Default behaviour is copy", {
  options(ggperiodic.data.table.copy = NULL)
  df2 <- periodic(df, x = c(0, 360))
  expect_s3_class(df2, "periodic_df")
  expect_false(is.periodic(df))
  df3 <- unperiodic(df2)
  expect_s3_class(df2, "periodic_df")
  expect_false(is.periodic(df3))
})

df <- example()

test_that("Can be set to copy", {
  options(ggperiodic.data.table.copy = TRUE)
  df2 <- periodic(df, x = c(0, 360))
  expect_s3_class(df2, "periodic_df")
  expect_false(is.periodic(df))
  df3 <- unperiodic(df2)
  expect_s3_class(df2, "periodic_df")
  expect_false(is.periodic(df3))
})

df <- example()
test_that("Setting option to not copy works", {
  options(ggperiodic.data.table.copy = FALSE)
  periodic(df, x = c(0, 360))
  expect_s3_class(df, "periodic_df")
  unperiodic(df)
  expect_false(is.periodic(df))
})

df <- example()

test_that("set operations work", {
  options(ggperiodic.data.table.copy = TRUE)
  setperiodic(df, x = c(0, 360))
  expect_s3_class(df, "periodic_df")
  setunperiodic(df)
  expect_false(is.periodic(df))
})


df <- as.data.frame(example())

test_that("Setting option to not copy, still copies a data.frame", {
  options(ggperiodic.data.table.copy = FALSE)
  df2 <- periodic(df, x = c(0, 360))
  expect_s3_class(df2, "periodic_df")
  expect_false(is.periodic(df))
  df3 <- unperiodic(df2)
  expect_s3_class(df2, "periodic_df")
  expect_false(is.periodic(df3))
})

df <- as.data.frame(example())

test_that("Set operations copy data.frames", {
  options(ggperiodic.data.table.copy = FALSE)
  df2 <- setperiodic(df, x = c(0, 360))
  expect_s3_class(df2, "periodic_df")
  expect_false(is.periodic(df))
  df3 <- setunperiodic(df2)
  expect_s3_class(df2, "periodic_df")
  expect_false(is.periodic(df3))
})