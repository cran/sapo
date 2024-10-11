## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sapo)
library(sf)

set.seed(2024)

## -----------------------------------------------------------------------------
poly1 <- system.file("extdata", "poly1.rds", package = "sapo") |>
  readRDS()
poly2 <- system.file("extdata", "poly2.rds", package = "sapo") |>
  readRDS()

str(poly1)
str(poly2)

## -----------------------------------------------------------------------------
plot(sf::st_geometry(poly1), col = "gray70")
plot(sf::st_geometry(poly2), add = TRUE)

## -----------------------------------------------------------------------------
my_ht <- cmc_psat(poly1, poly2)

## -----------------------------------------------------------------------------
sig_level <- my_ht$alpha
sig_plot <-
  data.frame(r = my_ht$distances,
             h = my_ht$mc_funct[NROW(my_ht$mc_funct), ],
             l = apply(my_ht$mc_funct, 2, quantile,
                       prob = .5 * sig_level),
             u = apply(my_ht$mc_funct, 2, quantile,
                       prob = 1 - .5 * sig_level))
## par(mfrow = c(1, 2))
## ## plot density part
## den <- density(my_ht$mc_sample)
## plot(den, main = sprintf("Test statistic (p-value = %.4f)",
##                          my_ht$p_value),
##      xlab = "u", ylab = "density")
## value <- my_ht$mc_sample[length(my_ht$mc_sample)]
## polygon(c(den$x[den$x >= value], value),
##         c(den$y[den$x >= value], 0),
##         col = "coral1",
##         border = "transparent")
## lines(den)
## H function
with(sig_plot, plot(x = r, y = h, type = "l",
                    col = 2,
                    main = "Local envelope",
                    xlab = "r",
                    ylab = expression(H[12](r)),
                    ylim = range(c(h, l, u))))
with(sig_plot, lines(x = r, y = l, lty = 2, col = 2))
with(sig_plot, lines(x = r, y = u, lty = 2, col = 2))
with(sig_plot,
     polygon(c(r, rev(r)), c(l, rev(u)),
             col = "coral1", lty = 0))
with(sig_plot, lines(x = r, y = h,
                     col = 1))

