setOldClass("Arima")

setClass("s_semiarma",
  contains = "decomp",
  slots = c(
    nonpar_model = "deseats",
    par_model = "Arima"
  )
)

create_s_semiarma <- function(decomp, nonpar_model, par_model) {

  methods::new("s_semiarma",
    decomp = decomp,
    nonpar_model = nonpar_model,
    par_model = par_model,
    ts_name = nonpar_model@ts_name,
    frequency = nonpar_model@frequency
    
  )

}

#---------------

#'Point and Interval Forecasts for Seasonal Semi-ARMA Models
#'
#'Obtain point and interval forecasts based on fitted Seasonal Semi-ARMA models.
#'
#'@param object an object of class \code{"s_semiarma"}.
#'@param n.ahead a numeric vector of length one that represents the forecasting horizon; assume that \code{object} is based on 
#'observations at time points \eqn{t=1,2,\dots,n}; forecasts are then obtained for time 
#'points \eqn{t=n+1,n+2,\dots,n+\code{n.ahead}}; the default is \code{n.ahead = 1}.
#'@param intervals a logical vector of length one that indicates whether or not 
#'forecasting intervals should be obtained; the default is \code{intervals = TRUE}.
#'@param alpha a numeric vector of variable length that indicates the confidence levels
#' at which to obtain forecasting intervals; the default is \code{alpha = c(0.95, 0.99)},
#'i.e. confidence levels of 95 and 99 percent.
#'@param method a character vector that indicates the method used to obtain forecasting
#'intervals; available are theoretical intervals based on the assumption of normal 
#'innovations (\code{"norm"}) and intervals through a bootstrap (\code{"boot"}); 
#'the default is \code{method = "norm"}.
#'@param bootMethod a character vector that allows the user to select a bootstrap 
#'procedure for the forecasting intervals when \code{method = "boot"} is selected;
#'the default \code{bootMethod = "simple"} simulates future observations by resampling
#'the obtained residuals; the second approach \code{bootMethod = "advanced"} also 
#'considers the variation in the ARMA coefficient estimates by simulating and 
#'reestimating complete ARMA paths upon which forecasts are obtained 
#'(see also the B-ARMARoots algorithm in Lu and Wang, 2020); the second approach
#'is often time-consuming.
#'@param npaths the number of paths to simulate, if the forecasting intervals 
#'are obtained via a bootstrap.
#'@param quant.type the method to obtain sample quantiles from the simulated
#'forecasting errors; see also the argument \code{type} of the function 
#'\code{\link[stats]{quantile}}.
#'@param expo a logical vector of length one; indicates whether the forecasting 
#'results should be exponentiated at the end; the default is \code{expo = FALSE}.
#'@param ... only for comparability with the standard \code{predict} method.
#'
#'@details
#'Assume a Seasonal Semi-ARMA model was fitted using \code{\link{s_semiarma}}.
#'Pass the resulting object to this method, in order to obtain point and 
#'interval forecasts.
#'
#'@export
#'
#'@return
#'A list with the following elements is returned.
#'\describe{
#'\item{\code{pred}}{the obtained point forecasts.}
#'\item{\code{interv}}{the obtained forecasting intervals.}
#'\item{\code{obs}}{the observation series.}
#'\item{\code{ts_name}}{the name of the observation series object.}
#'}
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
#'xt <- log(EXPENDITURES)
#'est <- s_semiarma(xt)
#'predict(est, n.ahead = 10)
#'}
#'

methods::setMethod("predict", "s_semiarma", 
  function(object, n.ahead = 1, intervals = TRUE, alpha = c(0.95, 0.99), method = c("norm", "boot"),
           bootMethod = c("simple", "advanced"), npaths = 5000, quant.type = 8,
           expo = FALSE, ...) {
    
    if (intervals) {
    
      method <- match.arg(method)
    
    }
    
    freq <- object@nonpar_model@frequency
    trend_e <- utils::tail(trend(object), 2)
    
    times <- ceiling(n.ahead / freq)    
    season_e <- rep(utils::tail(season(object), freq), times)
    
    trend_incr <- trend_e[[2]] - trend_e[[1]]
    
    trend_fc <- trend_e[[2]] + (1:n.ahead) * trend_incr
    
    season_fc <- season_e[1:n.ahead]
    
    arma_fc <- predict(object@par_model, n.ahead = n.ahead, se.fit = FALSE)
    
    fc_total <- trend_fc + season_fc + arma_fc
    
    if (intervals) {
      
      p <- object@par_model$arma[[1]]
      q <- object@par_model$arma[[2]]
      
      if (method == "norm") {
        
        fi <- normCast(model = object@par_model,
          p = p, q = q, h = n.ahead, alpha = alpha)        
        
      } else if (method == "boot") {
        
        bootMethod <- match.arg(bootMethod)
        
        fi <- bootCast(model = object@par_model, X = object@decomp[, 4],
          p = p, q = q, h = n.ahead, alpha = alpha, quant.type = quant.type,
          bootstrap_type = bootMethod, it = npaths)

      }
      
      fi_names = colnames(fi)

      fi <- ts(fi, start = c(time(fc_total)[[1]]), frequency = frequency(fc_total))      
      
      interv <- fc_total + fi

      
    } else {
      
      interv <- ts(NA_real_)
      
    }

    obs <- object@decomp[, 1]   
    ts_name <- object@nonpar_model@ts_name    
    
    if (expo) {
      fc_total <- exp(fc_total)
      interv <- exp(interv)
      obs <- exp(obs)
      ts_name <- paste0("exp(", ts_name, ")")
    }
    
    out <- create_deseats_fc(
      pred = fc_total,
      interv = interv,
      obs = obs,
      ts_name = ts_name
    )
    
    colnames(out@interv) <- fi_names
    
    out
    
  }
)

#'Show Method for Objects of Class \code{"s_semiarma"}
#'
#'Print results of the function \code{\link{s_semiarma}} to the console.
#'
#'@param object an object of class \code{"s_semiarma"}.
#'
#'@details
#'Use this method to create a nice looking overview of the contents of 
#'objects of class \code{"s_semiarma"}.
#'
#'@return
#'This method returns \code{NULL}.
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@export
methods::setMethod("show", "s_semiarma", 
  function(object) {
    
    kfun <- cap_1st(object@nonpar_model@kernel_fun)
    bound_m <- cap_1st(object@nonpar_model@boundary_method)
        
    text_out <- paste0(
    "\n*******************************************\n",
    "*                                         *\n",
    "*        Fitted Seasonal Semi-ARMA        *\n",
    "*                                         *\n",    
    "*******************************************\n\n",
    
    "Series: ", object@ts_name, "\n\n",
    
    "Nonparametric part (Trend + Seasonality):\n",
    "-----------------------------------------\n\n",
    " Kernel:    ", kfun,"\n",
    " Boundary:  ", bound_m,"\n",      
    " Bandwidth: ", sprintf("%.4f", object@nonpar_model@bwidth), "\n\n",   
    "  Trend:\n",
    "  ------\n",
    "    Order of local polynomial: ", object@nonpar_model@order_poly, "\n\n",
    "  Seasonality:\n",
    "  ------------\n",
    "    Frequency: ", object@nonpar_model@frequency, "\n\n",   
    "Parametric part (Rest):\n",
    "-----------------------\n",
    paste0(utils::capture.output(print(object@par_model)), collapse = "\n"), "\n\n"
  )
  cat(text_out)
}
)

#'@rdname bwidth-deseats-method
#'@export
setMethod("bwidth", "s_semiarma",
  function(object){
    object@nonpar_model@bwidth
  }
)