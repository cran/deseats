
setClass("deseats",
  contains = "decomp",
  slots = c(
    bwidth = "numeric",
    order_poly = "numeric",
    weights = "array",
    weights_wfun = "matrix",
    boundary_method = "character",
    kernel_fun = "character",
    sum_autocov = "numeric"
  )
)

create_deseats <- function(decomp, bwidth, ts_name, frequency, order_poly,
                           weights, weights_wfun, boundary_method,
                           kernel_fun, sum_autocov) {
  
  methods::new("deseats",
    decomp = decomp,
    bwidth = bwidth,
    ts_name = ts_name,
    frequency = frequency,
    order_poly = order_poly,
    weights = weights,
    weights_wfun = weights_wfun,
    boundary_method = boundary_method,
    kernel_fun = kernel_fun,
    sum_autocov = sum_autocov
  )
  
}

#------------------------------------------------------------------------------#

#'Printing of \code{deseats} Function Results
#'
#'Print results of the function \code{deseats}.
#'
#'@param object an object of class \code{"deseats"}.
#'
#'@details
#'After trend and seasonality in a time series have been estimated using the
#'function \code{deseats}, basic estimation results can be easily printed to 
#'the console.
#'
#'@return
#'The function returns \code{NULL}.
#'
#'@export
setMethod("show", "deseats",
  function(object){
    
    kfun <- cap_1st(object@kernel_fun)
    bound_m <- cap_1st(object@boundary_method)
    
    cat(paste0("\n",
      "************************************************\n",
      "*                                              *\n",
      "*               Smoothing Results              *\n",
      "*                                              *\n",
      "************************************************\n\n",
      " Series:    ", object@ts_name, "\n",
      " Estimated: Trend + Seasonality\n",
      " Kernel:    ", kfun,"\n",
      " Boundary:  ", bound_m,"\n",      
      " Bandwidth: ", sprintf("%.4f", object@bwidth), "\n\n",      
      " Trend:\n",
      " ------\n",
      "   Order of local polynomial: ", object@order_poly, "\n\n",
      " Seasonality:\n",
      " ------------\n",
      "   Frequency: ", object@frequency, "\n\n"
    ))
  }
)

#----------------

#-----------------

#'Animate Locally Weighted Regression Results
#'
#'The results of locally weighted regression results
#'acquired through decomposition of seasonal time series 
#'via the function \code{\link{deseats}} can be 
#'animated automatically.
#'
#'@param object an object of class \code{"deseats"}.
#'@param col.obs the color to use for the observations.
#'@param col.fit the color to use for the fitted values.
#'@param col.weights the color to use for the active kernel weights.
#'@param col.window the color to use for the window defined through the
#'bandwidth
#'@param col.spot the color to highlight the estimation time point.
#'@param save whether to save the animation or not; for \code{NULL},
#'it will not be saved; to save it in the current working directory, use 
#'either \code{save = "PDF"} or \code{save = "HTML"}, which also specifies 
#'the file format.
#'@param xlab the label for the x-axis.
#'@param ylab1 the label for the y-axis on the left-hand side.
#'@param ylab2 the label for the second y-axis on the right-hand side.
#'@param main the plot title.
#'@param ... currently without use; implemented for possible future 
#'compatibility.
#'
#'@details
#'\code{\link{deseats}} estimation results are automatically animated 
#'through this method. It shows the observed series together with fitted
#'values (trend + seasonality), the smoothing window, the fitted values from 
#'the local regression, and the active kernel weights.
#'
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@export
#'
#'@return
#'The function returns \code{NULL}.
#'
#'@examples
#'\donttest{
#'### Creating the animation might take a while
#'Xt <- log(EXPENDITURES)
#'smoothing_options <- set_options(order_poly = 3)
#'est <- deseats(Xt, smoothing_options = smoothing_options)
#'animate(est)
#'}
#'
methods::setMethod("animate", "deseats", 
function(object, col.obs = "grey74", col.fit = "red", 
                    col.weights = "#00D40E", col.window = "deepskyblue4", 
                    col.spot = "orange", save = NULL,
                    xlab = "Time", ylab1 = "Estimated trend + seasonality",
                    ylab2 = "Active kernel weights",
                    main = NULL, ...) {
  
  n <- length(object@decomp[, 1])                  # Number of observations
  habs <- (length(object@weights[, 1, "Trend"]) - 1) / 2  # Number of boundary points 
  
  t <- c(stats::time(object@decomp))
  tsub <- 1:n
  
  oldPar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldPar), add = TRUE)
  
  parNew <- list(
    mar = c(5, 4, 4, 4) + 0.1
  )

  d <- abs(diff(range(object@decomp[, 1])))
  min_y <- min(object@decomp[, 1])
  max_y <- max(object@decomp[, 1])  
  
  mat_reg <- as.data.frame(matrix(0, ncol = object@order_poly + object@frequency,
                           nrow = 2 * habs + 1))
  names(mat_reg) <- c("y", paste0("t", 1:object@order_poly), 
                      paste0("cos", 1:trunc((object@frequency - 1) / 2)),
                      paste0("sin", 1:trunc((object@frequency - 1) / 2)),
                      paste0("cos", (trunc(object@frequency - 1) / 2) + 1))
  mat_reg[["y"]] <- object@decomp[1:(2 * habs + 1), 1]
  
  ani_opts_old <- animation::ani.options()
  on.exit(animation::ani.options(ani_opts_old), add = TRUE)
  animation::ani.options(nmax = 2 * (habs) + 7 * 10, interval = 0.1, ani.width = 1200)  
    
  if (is.null(main)) {
    main <- paste0('Locally weighted regression illustrated for the series "', 
                       object@ts_name, '"')
  }
  
  .f <- function() {
  
  graphics::par(parNew)    
    
  for (frame in seq_len(habs)) {
    
    tt <- (tsub[1:(2 * habs + 1)] - tsub[[frame]]) / habs
    for (i in 1:object@order_poly) {
      mat_reg[[paste0("t", i)]] <- tt^i
    }
    for (i in 1:((trunc(object@frequency - 1) / 2))) {
      mat_reg[[paste0("cos", i)]] <- cos(2 * i * pi * tt / (object@frequency / habs))
      mat_reg[[paste0("sin", i)]] <- sin(2 * i * pi * tt / (object@frequency / habs))      
    }
    mat_reg[[paste0("cos", trunc(object@frequency - 1) / 2 + 1)]] <- cos(pi * tt * habs)
    
    lm_fit <- (stats::lm(y ~ ., data = mat_reg, weights = object@weights_wfun[frame, ]))$fitted.values
    
    plot(object@decomp[, 1], col = col.obs,
         main = main, ylab = "", xlab = xlab)
    graphics::axis(side = 2, col = col.fit, col.ticks = col.fit, col.axis = col.fit)
    graphics::mtext(ylab1, side = 2, line = 3, col = col.fit)      
    graphics::polygon(c(rep(t[[1]], 2), rep(t[[2 * habs + 1]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha(col.window, 0.1))
    graphics::abline(v = c(t[[1]], t[[2 * habs + 1]]), col = col.window, lwd = 2)
    graphics::abline(v = t[[frame]], col = col.spot, lty = "dashed", lwd = 2)
    graphics::lines(t[1:frame], fitted(object)[1:frame], col = col.fit, lwd = 1)    
    graphics::lines(t[1:(2 * habs + 1)], lm_fit, col = ggplot2::alpha("black", 0.9), lwd = 1, lty = 3)
    graphics::points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.4), pch = 19, cex = 1.2)    
    
    graphics::par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    graphics::abline(h = 0, col = "lightgray", lty = "dotted")
    graphics::lines(t[1:(2 * habs + 1)], object@weights[frame, , "Combined"], col = ggplot2::alpha(col.weights, 0.9))
    graphics::axis(side = 4, col = col.weights, col.ticks = col.weights,
         col.axis = col.weights)       # Add second axis
    graphics::mtext(ylab2, side = 4, line = 3, col = col.weights) 
    
    animation::ani.pause()
  }
  
  t_in <- floor(seq(from = habs + 1, to = n - habs, length.out = 6))
  tt <- (tsub[1:(2 * habs + 1)] - tsub[[habs + 1]])  / habs  
  
  for (i in 1:object@order_poly) {
    mat_reg[[paste0("t", i)]] <- tt^i
  }
  for (i in 1:((trunc(object@frequency - 1) / 2))) {
    mat_reg[[paste0("cos", i)]] <- cos(2 * i * pi * tt / (object@frequency / habs))
    mat_reg[[paste0("sin", i)]] <- sin(2 * i * pi * tt / (object@frequency / habs))      
  }
  mat_reg[[paste0("cos", trunc(object@frequency - 1) / 2 + 1)]] <- cos(pi * tt * habs)
  
  counter <- 0
  
  for (frame in t_in) {
    
    for(re in 1:10){
    
    counter <- counter + 1  
      
    mat_reg[["y"]] <- object@decomp[(frame - habs):(frame + habs), 1]
    
    lm_fit <- (stats::lm(y ~ ., data = mat_reg, weights = object@weights_wfun[habs + 1, ]))$fitted.values    
    
    plot(object@decomp[, 1], col = col.obs,
         main = main, 
         ylab = "", xlab = xlab)
    graphics::points(0, counter, type = "n")    
    graphics::axis(side = 2, col = col.fit, col.ticks = col.fit, col.axis = col.fit)
    graphics::mtext(ylab1, side = 2, line = 3, col = col.fit)      
    graphics::polygon(c(rep(t[[frame - habs]], 2), rep(t[[frame + habs]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha(col.window, 0.1))
    graphics::abline(v = c(t[[frame - habs]], t[[frame + habs]]), col = col.window, lwd = 2)
    graphics::abline(v = t[[frame]], col = col.spot, lty = "dashed", lwd = 2)
    graphics::lines(t[1:frame], fitted(object)[1:frame], col = col.fit, lwd = 1)
    graphics::lines(t[(frame - habs):(frame + habs)], lm_fit, col = ggplot2::alpha("black", 0.9), lwd = 1, lty = 3)    
    graphics::points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.4), pch = 19, cex = 1.2)
    
    graphics::par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    graphics::abline(h = 0, col = "lightgray", lty = "dotted")
    
    graphics::lines(t[(frame - habs):(frame + habs)], object@weights[habs + 1, , "Combined"], col = ggplot2::alpha(col.weights, 0.9))
    graphics::axis(side = 4, col = col.weights, col.ticks = col.weights,
         col.axis = col.weights)       # Add second axis
    graphics::mtext(ylab2, side = 4, line = 3, col = col.weights) 
    animation::ani.pause()    
    
    }
    
  }
    
  mat_reg[["y"]] <- object@decomp[(n - 2 * habs):n, 1]
    
  for (frame in c((n - habs + 1):n, rep(n, 10))) {
    
    counter <- counter + 1
    
    tt <- (tsub[(n - 2 * habs):n] - tsub[[frame]]) / habs
    for (i in 1:object@order_poly) {
      mat_reg[[paste0("t", i)]] <- tt^i
    }
    for (i in 1:((trunc(object@frequency - 1) / 2))) {
      mat_reg[[paste0("cos", i)]] <- cos(2 * i * pi * tt / (object@frequency / habs))
      mat_reg[[paste0("sin", i)]] <- sin(2 * i * pi * tt / (object@frequency / habs))      
    }
    mat_reg[[paste0("cos", trunc(object@frequency - 1) / 2 + 1)]] <- cos(pi * tt * habs)
    
    lm_fit <- (stats::lm(y ~ ., data = mat_reg, weights = object@weights_wfun[frame + 2 * habs - n + 1, ]))$fitted.values
    
    plot(object@decomp[, 1], col = col.obs,
         main = main, 
         ylab = "", xlab = xlab)
    graphics::points(0, counter, type = "n")
    graphics::axis(side = 2, col = col.fit, col.ticks = col.fit, col.axis = col.fit)
    graphics::mtext(ylab1, side = 2, line = 3, col = col.fit)      
    graphics::polygon(c(rep(t[[n - 2 * habs]], 2), rep(t[[n]], 2)), 
            c(min_y - 0.1 * d, max_y + 0.1 * d, max_y + 0.1 * d, min_y - 0.1 * d),
            border = NA, col = ggplot2::alpha(col.window, 0.1))
    graphics::abline(v = c(t[[n - 2 * habs]], t[[n]]), col = col.window, lwd = 2)
    graphics::abline(v = t[[frame]], col = col.spot, lty = "dashed", lwd = 2)
    graphics::lines(t[1:frame], fitted(object)[1:frame], col = col.fit, lwd = 1)
    graphics::lines(t[(n - 2 * habs):n], lm_fit, col = ggplot2::alpha("black", 0.9), lwd = 1, lty = 3)    
    graphics::points(t[[frame]], fitted(object)[[frame]], col = ggplot2::alpha("purple", 0.4), pch = 19, cex = 1.2)
    
    graphics::par(new = TRUE)                             # Add new plot
    plot(0,                                     # Create second plot without axes
    axes = FALSE, xlab = "", ylab = "", type = "n", xlim = c(min(t), max(t)),
    ylim = c(min(object@weights[, , "Combined"]), max(object@weights[, , "Combined"])))
    graphics::abline(h = 0, col = "lightgray", lty = "dotted")
    
    graphics::lines(t[(n - 2 * habs):n], object@weights[frame + 2 * habs - n + 1, , "Combined"], col = ggplot2::alpha(col.weights, 0.9))
    graphics::axis(side = 4, col = col.weights, col.ticks = col.weights,
         col.axis = col.weights)       # Add second axis
    graphics::mtext(ylab2, side = 4, line = 3, col = col.weights) 
    animation::ani.pause()
  }
  
  }

if (is.null(save)) {
  .f()
} else {
  .savefun <- aniSave(save)
  .savefun({.f()}, interval = 0.1, ani.width = 1200)
}
}
)

#'Retrieve the Used Bandwidth from an Estimation Object 
#'
#'If either \code{\link{deseats}} or \code{\link{s_semiarma}} are 
#'used to fit a model to time series data, this method retrieves 
#'the applied bandwidth from the output object.
#'
#'@param object an object either of class \code{"deseats"} or 
#'\code{"s_semiarma"}.
#'
#'@details
#'When applying \code{\link{deseats}} or \code{link{s_semiarma}},
#'one approach is to let the functions automatically choose a 
#'bandwidth for locally weighted regression. Using this method,
#'the applied bandwidth can be retrieved.
#'
#'@export
#'
#'@return
#'A numeric vector of length one that represents the bandwidth used in the
#'smoothing procedure is returned.
#'
#'@examples
#'\donttest{
#'Xt <- log(EXPENDITURES)
#'smoothing_options <- set_options(order_poly = 3)
#'est <- deseats(Xt, smoothing_options = smoothing_options)
#'bwidth(est)
#'}
#'
setMethod("bwidth", "deseats",
  function(object){
    object@bwidth
  }
)
