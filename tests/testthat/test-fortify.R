context("Fortify")

set.seed(42)
x <- seq(0, 360 - 10, by = 10)
df <- data.frame(x = x, y = cos(x*pi/180))
library(ggplot2)

# library(maps)

map <- periodic(map_data("world"), long = long)

# test_that("Fortify works", {
#   expect_doppelganger("basic_plot", ggplot(df, aes(x, y)) + geom_point())
#
#   expect_doppelganger("map",
#                       ggplot(map, aes(long, lat, group = group), long = c(0, 360)) +
#                         geom_path())
#
#   # expect_warning(
#     expect_doppelganger("map2",
#                         ggplot() +
#                           geom_path(data = map, aes(long, lat, group = group), long = c(0, 360)))
#     # )
#
#   # expect_warning(
#     expect_doppelganger("map3",
#                         ggplot() +
#                           stat_identity(data = map, aes(long, lat, group = group),
#                                         long = c(0, 360),
#                                         geom = "path"))
#     # )
#
#   map2 <- map
#   map2$lineend <- map2$long
#   map2 <- periodic(unperiodic(map2), lineend = lineend)
#
#   expect_warning(ggplot() +
#                    stat_identity(data = map2, aes(lineend, lat, group = group),
#                                  lineend = c(0, 360),
#                                  geom = "path"))
# })











