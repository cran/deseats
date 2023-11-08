setClass("decomp",
  slots = c(
    decomp = "mts",
    ts_name = "character",
    frequency = "numeric"
  )
)

#------------------------------------------------------------------------------#

### Base R plot method

#'Plot Method for Decomposition Results in the Style of Base R Plots
#'
#'This is method for producing various plots of the decomposition results
#'returned by this package.
#'
#'@param x an object returned by the function \code{\link{deseats}}.
#'@param which various plots can be selected either via a keyword or a number; 
#'enter \code{"facets"} or \code{1} to show a facet plot of the estimated
#'time series components; enter \code{"observations"} or \code{2} to show the
#'input time series; enter \code{"fitted"} or \code{3} to show the observations 
#'alongside the estimated trend with seasonality; enter \code{"detailed_fit"} or
#'\code{4} to show the observations together with the fitted values and the trend;
#'enter \code{"trend_season"} or \code{5} to show the observations together with 
#'the trend and with the seasonality (the latter shown around the series mean);
#'enter \code{"residuals"} or 
#'\code{6} to plot the both detrended and seasonally adjusted series; use 
#'\code{7} or \code{"deseasonalized"} to show the seasonally adjusted 
#'series; enter \code{8} or \code{"detrended"} to plot the detrended 
#'series; the 
#'default is \code{which = NULL} which then lets you select a plot 
#'interactively in the R console.
#'@param ... further arguments to pass to \code{\link[stats]{plot.ts}} or 
#'\code{\link[graphics]{matplot}} (depending on whether only one time series
#'or multiple time series are shown in the plot).
#'
#'@details
#'Create predefined standard plots of the decomposition objects returned by the 
#'\code{deseats} package, e.g. returned by the function \code{\link{deseats}}. 
#'Plots are created in the base R plot style. The type of plot can be chosen 
#'either interactively from the console, or the argument \code{which} can be 
#'used to directly  select the kind of plot to create (see also the description
#'of the argument \code{which}) within the function call.
#'
#'If plot type 5 (\code{which = 5}) is selected, the estimated 
#'seasonality will be displayed around the mean of the observations by default.
#'Setting the additional argument \code{s_around} to some other value, will lead 
#'to the seasonality being displayed around that constant value.
#'
#'@importFrom ggplot2 autoplot
#'@export 
#'
#'@return
#'A graphic is created in the plots windows, the function itself, however,
#'returns \code{NULL}.
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@examples
#'Xt <- log(EXPENDITURES)
#'est <- deseats(Xt)
#'plot(est, which = 3)
#'

setMethod("plot", "decomp", function(x, which = NULL, ...) {
  
  which <- check_which(which)
  
  if (which == "0") {
    return(invisible(NULL))
  }
  
  plot_fun <- switch(
    which,
    "facets" = plot_facets,
    "observations" = plot_obs,
    "fitted" = plot_fit,
    "detailed_fit" = plot_fit_detailed,
    "trend_season" = plot_components,
    "residuals" = plot_resid,
    "deseasonalized" = plot_deseason,
    "detrended" = plot_detrend,    
    "1" = plot_facets,
    "2" = plot_obs,
    "3" = plot_fit,
    "4" = plot_fit_detailed,
    "5" = plot_components,
    "6" = plot_resid,
    "7" = plot_deseason,
    "8" = plot_detrend   
  )
  
  plot_fun(x, ...)
  
})


### ggplot2 plot method

#'Plot Method for Decomposition Results in the Style of ggplot2
#'
#'This is method for producing various plots of the decomposition results
#'returned by this package.
#'
#'@param object an object returned by the function \code{\link{deseats}}.
#'@param which various plots can be selected either via a keyword or a number; 
#'enter \code{"facets"} or \code{1} to show a facet plot of the estimated
#'time series components; enter \code{"observations"} or \code{2} to show the
#'input time series; enter \code{"fitted"} or \code{3} to show the observations 
#'alongside the estimated trend with seasonality; enter \code{"detailed_fit"} or
#'\code{4} to show the observations together with the fitted values and the trend;
#'enter \code{"trend_season"} or \code{5} to show the observations together with 
#'the trend and with the seasonality (the latter shown around the series mean);
#' enter \code{"residuals"} or 
#'\code{6} to plot the both detrended and seasonally adjusted series; use 
#'\code{7} or \code{"deseasonalized"} to show the seasonally adjusted 
#'series; enter \code{8} or \code{"detrended"} to plot the detrended 
#'series; the 
#'default is \code{which = NULL} which then lets you select a plot 
#'interactively in the R console.
#'@param ... no purpose and only implemented for compatibility.
#'
#'@details
#'Create predefined standard plots of the decomposition objects returned by the 
#'\code{deseats} package, e.g. returned by the function \code{\link{deseats}}. 
#'Plots are created in the ggplot2 plot style. The type of plot can be chosen 
#'either interactively from the console, or the argument \code{which} can be 
#'used to directly  select the kind of plot to create (see also the description
#'of the argument \code{which}) within the function call.
#'
#'If plot type 5 (\code{which = 5}) is selected, the estimated 
#'seasonality will be displayed around the mean of the observations by default.
#'Setting the additional argument \code{s_around} to some other value, will lead 
#'to the seasonality being displayed around that constant value.
#'
#'@export
#'@importFrom ggplot2 autoplot
#'
#'@return
#'A ggplot2-graphic object is returned, i.e. an object of classes 
#'\code{"gg"} and \code{"ggplot"}.
#'
#'@importFrom rlang .data
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@examples
#'\donttest{
#'Xt <- log(EXPENDITURES)
#'est <- deseats(Xt)
#'autoplot(est, which = 3)
#'}
#'

setMethod("autoplot", "decomp", function(object, which = NULL, ...) {
  
  which <- check_which(which)
  
  if (which == "0") {
    return(invisible(NULL))
  }
  
  plot_fun <- switch(
    which,
    "facets" = plot_facets_gg,
    "observations" = plot_obs_gg,
    "fitted" = plot_fit_gg,
    "detailed_fit" = plot_fit_detailed_gg,
    "trend_season" = plot_components_gg,
    "residuals" = plot_resid_gg,
    "deseasonalized" = plot_deseason_gg,
    "detrended" = plot_detrend_gg,    
    "1" = plot_facets_gg,
    "2" = plot_obs_gg,
    "3" = plot_fit_gg,
    "4" = plot_fit_detailed_gg,
    "5" = plot_components_gg,
    "6" = plot_resid_gg,
    "7" = plot_deseason_gg,
    "8" = plot_detrend_gg     
  )
  
  P <- plot_fun(object, ...)
  P
  
})

#--------------------------------------------------------------

#'Obtain Individual Components of a Decomposed Time Series
#'
#'The provided methods work for decomposition objects created 
#'within this package. They allow the user to retrieve individual
#'components among the estimated ones.
#'
#'@param object the estimation / decomposition object, for example 
#'of class \code{"deseats"}.
#'
#'@details
#'Apply these functions directly to an estimation object, i.e. 
#'the result of a decomposition of a seasonal time series, in order 
#'to retrieve individual estimated components.
#'
#'@return
#'These methods return time series objects of class \code{"ts"} that represent
#'the corresponding estimated component in the time series originally used in
#'the estimation process.
#'
#'@export
#'
#'@examples
#'\donttest{
#'Xt <- log(EXPENDITURES)
#'smoothing_options <- set_options(order_poly = 3)
#'est <- deseats(Xt, smoothing_options = smoothing_options)
#'trend_e <- trend(est)           # Trend estimates
#'season_e <- season(est)         # Seasonality estimates
#'trend_season_e <- fitted(est)   # Trend + seasonality estimates
#'resid_e <- residuals(est)       # Residuals (observ. - trend - seasonality)
#'ts_adj <- deseasonalize(est)    # Seasonally adjusted series
#'ts_notrend <- detrend(est)      # Detrended series
#'}
#'

methods::setMethod("trend", "decomp", function(object, ...) {
  tryCatch({
    object@decomp[, "Trend"]
  }, error = function(c1) {stop("No trend component to retrieve.", call. = FALSE)})
  
})
#'@rdname trend-decomp-method
#'@export
methods::setMethod("season", "decomp", function(object, ...) {
  tryCatch({
    object@decomp[, "Seasonality"]
  }, error = function(c1) {stop("No seasonal component to retrieve.", call. = FALSE)})
  
})
#'@rdname trend-decomp-method
#'@export
#'
#'@param ... without further use; implemented for compatibility only.
#'
setMethod("fitted", "decomp",
  function(object, ...){
    trend(object) + season(object)
  }
)
#'@rdname trend-decomp-method
#'@export
setMethod("residuals", "decomp",
  function(object, ...){
    object@decomp[, "Rest"]
  }
)
#'@rdname trend-decomp-method
#'@export
setMethod("deseasonalize", "decomp",
  function(object, ...){
    object@decomp[, "Observations"] - season(object)
  }
)
#'@rdname trend-decomp-method
#'@export
setMethod("detrend", "decomp",
  function(object, ...){
    object@decomp[, "Observations"] - trend(object)
  }
)
