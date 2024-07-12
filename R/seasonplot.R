#'Creation of Seasonal Plots
#'
#'Simplified seasonal plot creation of time series in order to identify 
#'seasonal patterns more easily.
#'
#'@param x a time series object of class \code{"ts"}.
#'@param xlab a label for the plot's x-axis.
#'@param ylab a label for the plot's y-axis.
#'@param main a title for the plot.
#'@param labels whether or not to show tick labels.
#'@param colorpalette a color palette (either a numeric or character vector) that 
#'gives an alternative color palette to use for the subseries.
#'@param rm_xticks whether to remove x-axis ticks and tick labels; the default is
#'\code{FALSE}.
#'
#'@export
#'
#'@importFrom graphics axis lines text
#'
#'@details
#'The function creates a seasonal plot of the provided time series object. The
#'series is split into different subseries, each reflecting one season, and then 
#'these subseries are plotted together. This helps identifying possible seasonal
#'patterns in the data.
#'
#'@return
#'The function does not return anything, however a plot is created in the plot 
#'window.
#'
#'@examples
#'seasonplot(TEMPERATURE, xlab = "Degrees Celsius", 
#'  main = "Seasonal plot of German temperature data")
#'

seasonplot <- function(x, xlab = NULL, ylab = NULL, main = NULL, labels = TRUE,
                       colorpalette = NULL, rm_xticks = FALSE) {
  t <- c(time(x))
  t_floor <- floor(t)
  t_u <- unique(t_floor)
  
  n_u <- length(t_u)
  l_whole <- frequency(x)
  
  mat_u <- matrix(NA, ncol = n_u, nrow = l_whole)
  
  l <- sum(t_floor == t_u[[1]])
  mat_u[(l_whole - l + 1):l_whole, 1] <- x[t_floor == t_u[[1]]]
  l <- sum(t_floor == t_u[[n_u]])
  mat_u[1:l, n_u] <- x[t_floor == t_u[[n_u]]]  
  
  if (n_u > 2) {
  
    for (i in 2:(n_u - 1)) {
      mat_u[, i] <- x[t_floor == t_u[[i]]]
    }
    
  }
  
  colnames(mat_u) <- as.character(t_u)
  if (frequency(x) == 12) {
    rnames <- month.abb
  } else if (frequency(x) == 4) {
    rnames <- paste0("Q", 1:4)
  } else {
    rnames <- as.character(1:frequency(x))
  }
  
  
  if (is.null(xlab)) {
    xlab <- switch(as.character(frequency(x)),
      "12" = "Month",
      "4" = "Quarter",
      "Time"
    )
  }
  
  if (is.null(ylab)) {
    ylab <- deparse(substitute(x))
  }
  
  if (is.null(main)) {
    main <- paste0('Seasonal plot of "', deparse(substitute(x)), '"')
  }
  
  if (is.null(colorpalette)) {
    colorpalette <- 1:length(t_u)
  }
  
  mat_u <- ts(mat_u, start = c(1, 1), frequency = frequency(x))
  
  max_y <- max(x)
  min_y <- min(x)
  
  plot(x, type = "n", xaxt = "n", xlim = c(1, frequency(x)), ylim = c(min_y, max_y),
       xlab = xlab, ylab = ylab, main = main)
  if (!rm_xticks) {
    axis(side = 1, at = 1:frequency(x), labels = rnames)
  }
  
  lines(1:frequency(x), mat_u[, 1], col = colorpalette[[1]])
  if (labels) {
    l <- sum(t_floor == t_u[[i]]) 
    text(frequency(x), mat_u[frequency(x), 1], labels = colnames(mat_u)[[1]], col = colorpalette[[1]],
         pos = c(3), offset = 0.2)  
  }
  
  for (i in 2:length(t_u)) {
    lines(1:frequency(x), mat_u[, i], col = colorpalette[[i]])
    if (labels) {
      l <- sum(t_floor == t_u[[i]])    
      text(l, mat_u[l, i], labels = colnames(mat_u)[[i]], col = colorpalette[[i]],
           pos = c(3), offset = 0.2)
    }
  }
  
}

#'Creation of Seasonal Plots in the Style of ggplot2
#'
#'Simplified seasonal plot creation of time series objects in order to
#'identify seasonal patterns.
#'
#'@param x a time series object of class \code{"ts"}.
#'
#'@export
#'
#'@details
#'The function returns an plot object in the style of \code{ggplot2}. The plot
#'can therefore be adjusted easily using common \code{ggplot2} syntax.
#'
#'@return
#'The function returns a \code{ggplot2} plot object.
#'
#'@examples
#'seasonplot_gg(TEMPERATURE) +
#'  ggplot2::ylab("Degrees Celsius") +
#'  ggplot2::ggtitle("Seasonal plot of German temperature data")
#'

seasonplot_gg <- function(x) {
  
  t <- c(time(x))
  t_floor <- floor(t)
  t_u <- unique(t_floor)
  
  n_u <- length(t_u)
  l_whole <- frequency(x)
  
  mat_u <- matrix(NA, ncol = n_u, nrow = l_whole)
  
  l <- sum(t_floor == t_u[[1]])
  mat_u[(l_whole - l + 1):l_whole, 1] <- x[t_floor == t_u[[1]]]
  l <- sum(t_floor == t_u[[n_u]])
  mat_u[1:l, n_u] <- x[t_floor == t_u[[n_u]]]  
  
  if (n_u > 2) {
  
    for (i in 2:(n_u - 1)) {
      mat_u[, i] <- x[t_floor == t_u[[i]]]
    }
    
  }
  
  colnames(mat_u) <- as.character(t_u)
  if (frequency(x) == 12) {
    rnames <- month.abb
  } else if (frequency(x) == 4) {
    rnames <- paste0("Q", 1:4)
  } else {
    rnames <- as.character(1:frequency(x))
  }
  
  
  xlab <- switch(as.character(frequency(x)),
    "12" = "Month",
    "4" = "Quarter",
    "Time"
  )
  ylab <- deparse(substitute(x))

  
  mat_u <- as.data.frame(mat_u, start = c(1, 1), frequency = frequency(x))
  mat_u[["TP"]] <- 1:frequency(x)
  mat_u_l <- tidyr::pivot_longer(mat_u, cols = 1:n_u, names_to = "Subseries", values_to = "Value")
  
  ggplot2::ggplot(mat_u_l, ggplot2::aes(x = .data[["TP"]], y = .data[["Value"]])) +
    ggplot2::geom_line(ggplot2::aes(color = .data[["Subseries"]])) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(paste0('Seasonal plot of "', ylab, '"')) +
    ggplot2::scale_x_continuous(breaks = 1:frequency(x), labels = rnames)
  
}