#ARIMA Function Wrapper Without Warnings
#
#For internal use only.

arima_no_warn <- function(xt, order = c(0L, 0L, 0L), include.mean = FALSE) {
  suppressWarnings(stats::arima(xt, order = order, include.mean = include.mean))
}
