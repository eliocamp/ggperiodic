context("Core functions")

set.seed(42)
x <- runif(30, 0, 360)
df <- data.frame(x = x, y = cos(x*pi/180))
period <- c(0, 360)
period2 <- range(x)
period2[2] <- period2[2] - 1

test_that("periodic returns a periodic object", {
  df_p <- expect_s3_class(periodic(df, x = period), "periodic_df")
  expect_identical(get_period(df_p), list(x = period))
  expect_identical(unperiodic(df_p), df)
  df_p2 <- expect_warning(periodic(df, x= period2))
  expect_identical(df_p2, df)
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


