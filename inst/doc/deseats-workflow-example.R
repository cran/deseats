## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, eval = TRUE, message = FALSE------------------------
library(deseats)

## ----echo = TRUE, eval = FALSE------------------------------------------------
# install.packages("deseats")    # Install the package from CRAN
# library(deseats)               # Attach the package

## -----------------------------------------------------------------------------
yt <- NOLABORFORCE
decomp <- deseats(yt, smoothing_options = set_options())
decomp

## -----------------------------------------------------------------------------
decomp@bwidth

## -----------------------------------------------------------------------------
trend_est <- trend(decomp)           # Fitted trend
season_est <- season(decomp)         # Fitted seasonal component
error_est <- residuals(decomp)       # Residuals
season_adj <- deseasonalize(decomp)  # Seasonally adjusted series

## ----fig.align = 'center', fig.height = 5, fig.width = 7----------------------
plot(decomp, which = 1, xlab = "Year")

## ----fig.align = 'center', fig.height = 4, fig.width = 7----------------------
plot(decomp, which = 5, xlab = "Year", ylab = "Millions of person", 
     main = "US persons not in the US labor force", s_around = 55)

## -----------------------------------------------------------------------------
full_model <- s_semiarma(yt, smoothing_options = set_options())
full_model

## -----------------------------------------------------------------------------
set.seed(1)
fc <- predict(full_model, n.ahead = 12, method = "boot")

## ----fig.align = 'center', fig.height = 4, fig.width = 7----------------------
plot(fc, xlab = "Year", ylab = "Millions of person", main = "US persons not in the US labor force",
     xlim = c(2010, 2021 - 1 / 12), ylim = c(80, 97))

## ----fig.align = 'center', fig.height = 5, fig.width = 7----------------------
decomp2 <- BV4.1(yt)
plot(decomp2, which = 1)

