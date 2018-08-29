context("Core functions")

set.seed(42)
x <- runif(30, 0, 360)
df <- data.frame(x = x, y = cos(x*pi/180))
period <- c(0, 360)
period2 <- range(x)
period2[2] <- period2[2] - 1

test_that("periodic returns a periodic object", {
  df_p <- expect_s3_class(periodic(df, x = period), "periodic_df")
  expect_equal(get_period(df_p), list(x = period))
  # expect_equal(unperiodic(df_p), df)
  df_p2 <- expect_warning(periodic(df, x= period2))
  expect_equal(df_p2, df)
  expect_s3_class(periodic(x, c(0, 360)), "periodic_v")
})

test_that("periodic reports bad columns", {
  df2 <- expect_warning(periodic(df, x = c(0, 360), z = c(0, 1)))
  expect_equal(periodic(df, x = c(0, 360)), df2)
  df2 <- expect_warning(periodic(df, z = c(0, 1)))
  expect_equal(df, df2)
  df2 <- expect_warning(periodic(df, x = c(0, 50)))
  expect_equal(df, df2)
})

test_that("NULL period works", {
  expect_equal(periodic(df, x = NULL), periodic(df, x = x))
})

test_that("print method works", {
  expect_output_file(print(periodic(df, x = c(0, 360))), "print_periodic_df.txt")
})

ranges <- list(c(-190, 540),
               c(-190, 300),
               c(  50, 540),
               c(  50, 300),
               c(-300, -50),
               c( 540, 600))

between <- function(range1, range2) {
  range1[1] >= range2[1] & range1[2] <= range2[2]
}

df_p <- periodic(df, x = period)
test_that("wrap works", {
  expect_error(wrap(df), "object must first be periodic with periodic\\(object, ...\\)")

  unlist(lapply(ranges, function(.x) {
    r <- range(wrap(df_p, x = .x)$x)
    expect_true(between(r, .x))
  }))

  df_w <- expect_s3_class(wrap(df_p, x = c(0, 360)), "data.frame")
  expect_true(!is.periodic(df_w))
})

test_that("wraps uses default range", {
  expect_equal(wrap(df_p), wrap(df_p, x = c(0, 360)))
})

test_that("wrap returns empty dataframe", {
  a <- wrap(subset(df_p, x == 1))
  b <- subset(df_p, x == 1)
  expect_equal(a, b)
})

test_that("wrap detects no periodic dimensions", {
  df_p_bad <- df_p
  attr(df_p_bad$x, "period") <- NULL
  df_w <- expect_warning(wrap(df_p_bad))
  expect_equal(df_p_bad, df_w)
})

test_that("wrap reports bad columns", {
  df_w <- expect_warning(wrap(df_p, y = c(0, 1)))
  # expect_equal(df_w, df)
})

map <- periodic(map_data("world"), long = long)

test_that("wrap works with groups", {
  expect_equal( wrap(map, .group = group),  wrap(map, .group = "group"))
})