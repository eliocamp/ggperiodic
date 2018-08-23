
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggperiodic

ggperiodic is an attempt to solve the issue of plotting periodic data in
ggplot2.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eliocamp/ggperiodic")
```

## Example

Let’s create some artificial data with periodic domain

``` r
x <- seq(0, 360 - 10, by = 10)*pi/180
y <- seq(0, 360 - 10, by = 10)*pi/180

z <- outer(1.2*sin(x), 0.4*sin(y*2)) +
  outer(0.5*cos(2*x), -0.5*sin(3*y)) +
  outer(0.2*sin(4*x), 0.45*cos(2*x))

dimnames(z) <- list(x = x*180/pi, y = y*180/pi)

Z <- reshape2::melt(z, value.name = "z")
```

If you try to plot it, you’ll notice problems at the limits

``` r
library(ggplot2)
ggplot(Z, aes(x, y, z = z, color = ..level..)) +
  geom_contour() +
  coord_polar()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

With ggperiodic you can define the periodic dimensions and ggplot2 does
the rest.

``` r
library(ggperiodic)
Z <- periodic(Z, x = c(0, 360), y = c(0, 360))

ggplot(Z, aes(x, y, color = ..level..)) +
  geom_contour(aes(z = z)) +
  coord_polar()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
