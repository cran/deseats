#'Gain Function Generic
#' 
#'@param object an input object; for the linear filters considered in this 
#'object, gain function values should be obtained.
#'@param lambda a vector of frequencies (from 0 to 0.5) for which 
#'to obtain the gain function.
#'@param ... currently without use; for possible future compatibility.
#' 
#'@details
#'A standard generic function. The purpose is to build various methods 
#'to instantaneously obtain gain function values for linear filters of 
#'different decomposition objects.
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
#'@details
#'A generic that can be extended by methods to automatically obtain gain 
#'function values of linear filters described in certain objects.
#'
#'@return
#'This generic does not return anything and is just the basis for more 
#'sophisticated methods.
#'

methods::setGeneric(
  "gain", 
  function(object, lambda = seq(0, 0.5, 0.0001), ...) {
    standardGeneric("gain")
  }
)

#' Obtain gain function values for DeSeaTS Trend and Detrend Filters
#' 
#' @param object an object of class \code{"deseats"}.
#' @param lambda a numeric vector with the frequencies at which to get 
#' the gain function values. 
#' @param ... no current purpose for this ellipsis.
#' 
#'@details
#'The various filters obtained via \code{\link{deseats}} (represented by 
#'the returned weighting systems) have a representation in the frequency 
#'domain. Using this method, those gain function values can be easily 
#'obtained.
#' 
#' @export
#' 
#'@return
#'A list is returned. Each element represents gain function values 
#'at the specified frequencies \code{lambda} for the filter defined 
#'through the element name.
#'\describe{
#'\item{\code{gain_trend}}{gain function values for the trend filter.}
#'\item{\code{gain_detrend}}{gain function values for the detrending filter.}
#'\item{\code{gain_season}}{gain function values for the seasonality filter.}
#'\item{\code{gain_deseason}}{gain function values for the seasonal 
#'adjustment filter.}
#'\item{\code{gain_comb}}{gain function values for the trend + seasonality 
#'filter.}
#'\item{\code{gain_decomb}}{gain function values for the detrending + seasonal 
#'adjustment filter.}
#'} 
#' 
#' 
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#' 
#' @examples
#' \donttest{
#'xt <- log(EXPENDITURES)
#'est <- deseats(xt)
#'
#'lambda <- seq(0, 0.5, 0.01)
#'gain_values <- gain(est, lambda = lambda)
#'m <- length(gain_values$gain_trend[, 1])
#'k <- (m - 1) / 2
#'colF <- colorRampPalette(c("deepskyblue4", "deepskyblue"))
#'cols <- colF(m)
#'
#'matplot(lambda, t(gain_values$gain_decomb[1:(k + 1), ]), 
#'  type = paste0(rep("l", k + 1), collapse = ""),
#'  col = cols, lty = rep(1, k + 1))
#'title("Gain functions of the combined detrend and deseasonalization filters")
#'
#'matplot(lambda, t(gain_values$gain_trend[1:(k + 1), ]), 
#'  type = paste0(rep("l", k + 1), collapse = ""),
#'  col = cols, lty = rep(1, k + 1))
#'title("Gain functions of the trend filters")
#' 
#'matplot(lambda, t(gain_values$gain_deseason[1:(k + 1), ]), 
#'  type = paste0(rep("l", k + 1), collapse = ""),
#'  col = cols, lty = rep(1, k + 1))
#'title("Gain functions of the seasonal adjustment filters")
#'} 
#' 

methods::setMethod("gain", "deseats", function(object, lambda = seq(0, 0.5, 0.0001), ...) {
  
  ws_trend <- object@weights[, , "Trend"]
  ws_season <- object@weights[, , "Season"]
  ws_comb <- object@weights[, , "Combined"]
  
  l <- length(lambda)
  n <- length(ws_trend[, 1])
  k <- (n - 1) / 2
  mat_trend <- matrix(0, ncol = l, nrow = n)
  mat_detrend <- mat_season <- mat_deseason <- mat_trend
  mat_trendseason <- mat_detrendseason <- mat_trend
  
  for (i in 1:k) {
    
    ### For trend filters
    
    gain_fun <- create.gain(filter.coefs = rev(ws_trend[i, ]), zero.at = n - (i - 1))
   
    mat_trend[i, ] <- gain_fun(lambda)
    
    gain_fun <- create.gain(filter.coefs = rev(ws_trend[n - (i - 1), ]), zero.at = i)
    
    mat_trend[n - (i - 1), ] <- gain_fun(lambda)
    
    ### For seasonality filters
    
    gain_fun <- create.gain(filter.coefs = rev(ws_season[i, ]), zero.at = n - (i - 1))
   
    mat_season[i, ] <- gain_fun(lambda)
    
    gain_fun <- create.gain(filter.coefs = rev(ws_season[n - (i - 1), ]), zero.at = i)
    
    mat_season[n - (i - 1), ] <- gain_fun(lambda)  
    
    ### For trend + seasonality filters
    
    gain_fun <- create.gain(filter.coefs = rev(ws_comb[i, ]), zero.at = n - (i - 1))
   
    mat_trendseason[i, ] <- gain_fun(lambda)
    
    gain_fun <- create.gain(filter.coefs = rev(ws_comb[n - (i - 1), ]), zero.at = i)
    
    mat_trendseason[n - (i - 1), ] <- gain_fun(lambda)    
    
    ### For detrend filters 1
    
    zero_v <- rep(0, n)
    zero_v[[i]] <- 1
    
    gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_trend[i, ]), zero.at = n - (i - 1))
   
    mat_detrend[i, ] <- gain_fun(lambda)
    
    ### For deseasonalize filters 1
    
    gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_season[i, ]), zero.at = n - (i - 1))
   
    mat_deseason[i, ] <- gain_fun(lambda)  
    
    ### For detrend + deseasonalize filters 1
    
    gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_comb[i, ]), zero.at = n - (i - 1))
   
    mat_detrendseason[i, ] <- gain_fun(lambda)
    
    ### For detrend filters 2    
    
    zero_v <- rep(0, n)
    zero_v[[n - (i - 1)]] <- 1
    
    gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_trend[n - (i - 1), ]), zero.at = i)
    
    mat_detrend[n - (i - 1), ] <- gain_fun(lambda)  
    
    ### For deseasonalize filters 2    
    
    gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_season[n - (i - 1), ]), zero.at = i)
    
    mat_deseason[n - (i - 1), ] <- gain_fun(lambda) 
    
    ### For detrend + deseasonalize filters 2    
    
    gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_comb[n - (i - 1), ]), zero.at = i)
    
    mat_detrendseason[n - (i - 1), ] <- gain_fun(lambda)     
     
  }
  
  # Middle trend filter
  
  gain_fun <- create.gain(filter.coefs = rev(ws_trend[k + 1, ]), zero.at = k + 1)
   
  mat_trend[k + 1, ] <- gain_fun(lambda)
  
  # Middle seasonality filter
  
  gain_fun <- create.gain(filter.coefs = rev(ws_season[k + 1, ]), zero.at = k + 1)
   
  mat_season[k + 1, ] <- gain_fun(lambda)
  
  # Middle trend + seasonality filter
  
  gain_fun <- create.gain(filter.coefs = rev(ws_comb[k + 1, ]), zero.at = k + 1)
   
  mat_trendseason[k + 1, ] <- gain_fun(lambda)
  
  # Middle detrend filter  
  
  zero_v <- rep(0, n)
  zero_v[[k + 1]] <- 1  
  
  gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_trend[k + 1, ]), zero.at = k + 1)
   
  mat_detrend[k + 1, ] <- gain_fun(lambda)
  
  # Middle deseasonalize filter  
  
  gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_season[k + 1, ]), zero.at = k + 1)
   
  mat_deseason[k + 1, ] <- gain_fun(lambda)
  
  # Middle detrend + deseasonalize filter  
  
  gain_fun <- create.gain(filter.coefs = rev(zero_v - ws_comb[k + 1, ]), zero.at = k + 1)
   
  mat_detrendseason[k + 1, ] <- gain_fun(lambda)    
  
  list(
    gain_trend = mat_trend, gain_detrend = mat_detrend,
    gain_season = mat_season, gain_deseason = mat_deseason,
    gain_comb = mat_trendseason, gain_decomb = mat_detrendseason
  )
  
  
})

#' Create Gain Function from a Linear Time Series Filter
#' 
#' This function takes a coefficient series of a linear
#' time series filter as an input and then returns the 
#' corresponding gain function as an R function.
#' 
#' @param filter.coefs a numeric vector with the filter coefficients ordered 
#' by coefficient index; see details for more info.
#' @param zero.at a numeric vector of length one that indicates the position 
#' of the coefficient for the present observation in \code{filter.coefs};
#' by default, the position is in the middle or just below the midpoint.
#' 
#' @export
#' 
#' @details
#' This is a functional. The function returns a function that 
#' represents the gain function for the input filter 
#' \code{filter.coefs}. The returned function only has the 
#' argument \code{lambda}, which is the frequency for which 
#' the value of the gain function should be obtained.
#' 
#' Let \eqn{(y_t)} be the input series and \eqn{(c_j)} the linear filter; 
#' then the element \eqn{c_j} is the weight assigned to \eqn{y_{t-j}}. The 
#' corresponding index \eqn{j} is important for the order of the argument
#' \code{filter.coefs}.
#' 
#' @importFrom grDevices colorRampPalette
#' 
#' @return 
#' The function returns a "gain function" function that has the numeric 
#' argument \code{lambda} only that represents frequencies to calculate 
#' the values of the gain function for.
#' 
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#' 
#' @examples
#'
#' # Moving average with smoothing over three values
#' a <- 1 / 3
#' gain_ma <- create.gain(rep(a, 3))
#' lambda <- seq(0, 0.5, 0.001)
#' GF <- gain_ma(lambda)
#' plot(lambda, GF, type = "l")
#' 
#' 
#'
#' # First differences filter
#' b <- c(1, -1)
#' gain_diff <- create.gain(b)
#' lambda <- seq(0, 0.5, 0.001)
#' GF2 <- gain_diff(lambda)
#' plot(lambda, GF2, type = "l")
#' 
#' \donttest{
#' # For a fully data-driven local linear trend + 
#' # trigonometric polynomial seasonality
#' # (Note: we get various filters for different observation time points)
#' 
#' xt <- EXPENDITURES
#' est <- deseats(log(xt), set_options(order_poly = 3))
#' ws <- est@weights[, , "Combined"]
#' l <- (length(ws[, 1]) - 1) / 2
#' 
#' lambda <- seq(0, 0.5, 0.001)
#' mat <- matrix(0, ncol = length(lambda), nrow = l + 1)
#' colF <- colorRampPalette(c("deepskyblue4", "deepskyblue"))
#' cols <- colF(l)
#' 
#' for (j in 1:(l + 1)) {
#' 
#'   gainF <- create.gain(ws[j, ], zero.at = j)
#'   mat[j, ] <- gainF(lambda)
#' 
#' }
#' 
#' matplot(lambda, t(mat), type = paste0(rep("l", l + 1), collapse = ""),
#'         lty = rep(1, l + 1), col = cols)
#' title(
#'   main = paste0(
#'     "Gain functions for the applied data-driven locally weighted ",
#'     "regression\napproach at boundary points and the first interior ",
#'     "point"
#'   )
#' )
#' 
#' # Same example as before but not for the trend but for the detrending filters
#' # (Note: we get various filters for different observation time points)
#' 
#' ll <- l * 2 + 1
#' mat2 <- mat
#' 
#' for (j in 1:(l + 1)) {
#' 
#'   zero.vec <- rep(0, ll)
#'   zero.vec[[j]] <- 1
#'   gainF <- create.gain(zero.vec - ws[j, ], zero.at = j)
#'   mat2[j, ] <- gainF(lambda)
#' 
#' }
#' 
#' matplot(lambda, t(mat2), type = paste0(rep("l", l + 1), collapse = ""),
#'         lty = rep(1, l + 1), col = cols)
#' title(
#'   main = paste0(
#'     "Gain functions for the applied data-driven detrending filter\n",
#'     "at boundary points and the first interior ",
#'     "point"
#'   )
#' )
#' }
#' 

create.gain <- function(filter.coefs = c(1), 
                        zero.at = ceiling(length(filter.coefs) / 2)
) {
  
  u <- seq_along(filter.coefs) - zero.at
  
  i <- 0 + 1i
  
  function(lambda = 0) {
    
    l <- length(lambda)
    
    out <- rep(0, l)
    
    for (s in 1:l) {
      
      out[[s]] <- sum(filter.coefs * exp(i * 2 * pi * lambda[[s]] * u))
      
    }
    
    abs(out)
    
  }
  
}
