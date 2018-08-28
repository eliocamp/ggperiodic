context("dplyr support")

library(dplyr)

set.seed(42)
df <- expand.grid(x = seq(0, 360 - 10, by = 10),
                  y = seq(-90, 90, by = 10))
df$z <- with(df, cos(x*pi/180)*sin(y*pi/180))

df <- periodic(df, x = c(0, 360))

period <- list(x = c(0, 360))


return_periodic <- list(
  arrange(df, x),
  distinct(df),
  filter(df, x > 50 & x < 100),
  filter(df, x > 365),
  group_by(df, y),
  group_by(df, x),
  mutate(df, z1 = z*x),
  rename(df, z2 = z),
  rename(df, u = x),
  select(df, u = x),
  slice(df, 1:4),
  slice(df, 0),
  summarise(df, x = mean(x)),
  summarise(group_by(df, x), y = mean(y)),
  ungroup(group_by(df, x)),
  ungroup(group_by(df, y)),
  sample_n(df, 1),
  sample_n(df, 0),
  sample_frac(df, 1/2),
  sample_frac(df, 0)

)

are_periodic <- unlist(lapply(return_periodic, is.periodic))

return_unperiodic <- list(
  select(df, u = y),
  mutate(df, x = y*x/2, y = y*2),
  mutate(df, x = 2*x),
  mutate(df, x = x/2),
  summarise(df, f = mean(x)),
  summarise(df, y = mean(y))
)

are_unperiodic <- unlist(lapply(return_unperiodic, is.periodic))

test_that("verbs that return periodic", {
  lapply(return_periodic, function(x) expect_s3_class(x, "periodic_df"))
  # lapply(return_periodic, function(x) expect_identical(get_period(x)[[1]], period[[1]]))
})

test_that("verbs that dont return periodic", {
  lapply(return_unperiodic, function(x) expect_false(is.periodic(x)))
})
