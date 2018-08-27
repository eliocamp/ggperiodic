library(ggplot2)
library(ggperiodic)
library(hexSticker)
library(magrittr)

d <- 20
t <- seq(0, 360 - d, by = d)
x <- cos(t*pi/180)


data<- data.frame(x = x, t = t)
data_p <- ggperiodic::periodic(data, t = c(0, 360))

(p <- ggplot(data_p, aes(t, -x), t = c(0 - 180, 360 + 180)) +
    geom_line(size = 2, color = "#715344") +
    geom_point(data = data, size = 4, shape = 21, fill = "#a3907c",
               color = "#3d211b") +
    theme_void() +
    coord_fixed(200) +
    scale_y_continuous(expand = c(0, 1)) +
    theme_transparent())

ggsave("hex_plot.svg")



ggplot(data_p, aes(t, -x), t = c(0 - 180, 360 + 180)) +
  geom_line(color = "#715344", size = 0.2) +
  geom_point(data = data, size = 0.2, color = "#3d211b") +
  # coord_fixed(200) +
  scale_y_continuous("", expand = c(0, 0.15)) +
  scale_x_continuous("", expand = c(0, 0)) +
  theme_minimal() +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.title = element_blank(),
        text = element_text(size = 5), panel.grid = element_line(size = 0.2))

ggsave("gallery_plot.png", width = 800/300, height = 450/300)
