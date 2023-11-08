#'Optimal Bandwidth Estimation for Locally Weighted Regression in Equidistant 
#'Time Series under Short Memory
#'
#'@param y a numerical vector or a time series object of class \code{ts} or 
#'that can be transformed with \code{\link[stats]{as.ts}} to an object of class 
#'\code{ts}; for these observations, trend and seasonality will be obtained.
#'@param smoothing_options an S4 object of class \code{smoothing_options}, which 
#'is returned by the function \code{\link{set_options}}; it 
#'includes details about the
#'options to consider in the locally weighted regression such as the order of
#'polynomial and the bandwidth for smoothing among others.
#'@param bwidth_start a single numeric value that is only relevant if the slot
#'\code{bwidth} in \code{smoothing_options} is set to \code{NA}; 
#'as the bandwidth will then
#'be selected automatically, \code{bwidth_start} sets the initial bandwidth for 
#'the algorithm; the default, \code{bwidth_start = NULL}, corresponds to 
#'\code{bwidth_start = 0.1} for a local linear trend and to 
#'\code{bwidth_start = 0.2} for a local cubic trend.
#'@param inflation_rate a character vector of length one that indicates, which inflation rate 
#' to use in the bandwidth selection; for a local linear trend, we have 
#'\code{inflation_rate = "optimal"} as the default, for a local cubic trend
#'it is \code{inflation_rate = "naive"}, which correspond to inflation rates
#'of 5/7 and 9/13, respectively.
#'@param correction_factor A logical vector of length one; theoretically, a 
#'larger bandwidth to estimate the sum of autocovariances from residuals of 
#'pilot trend and seasonality estimates is advisable than for estimating trend
#'and seasonality; for \code{correction_factor = TRUE}, this is implemented.
#'@param autocor a logical vector of length one; indicates whether to consider
#'autocorrelated errors (\code{TRUE}) or independent but identically 
#'distributed errors (\code{FALSE}); the default is \code{autocor = TRUE}.
#'@param drop a numeric vector of length one that indicates the proportion of 
#'the observations to not include at each boundary in the bandwidth estimation 
#'process, if a bandwidth is selected automatically; the default is 
#'\code{drop = NULL}, which corresponds to \code{drop = 0.05} for a 
#'local linear trend and to \code{drop = 0.1} for a local cubic trend.
#'@param error_model a character vector of length one that indicates whether
#'for \code{autocor = TRUE} the sum of autocovariances of the errors is 
#'obtained purely nonparametrically (\code{"free"}) or whether an
#'autoregressive moving-average (ARMA) model is assumed \code{"ARMA"}; the
#'default is \code{error_model = "free"}.
#'
#'@export
#'
#'
#'@return The function returns a list with different components:
#'\describe{
#'\item{\code{bopt}}{the obtained optimal bandwidth.}
#'\item{\code{bwidths}}{the obtained bandwidth for each iteration of the 
#'IPI-algorithm.}
#'\item{\code{Imk}}{the final estimate of \eqn{I[m^{(k)}]}.}
#'\item{\code{sum_autocov}}{the final estimate of the sum of autocovariances.}
#'}
#'
#'
#'@details
#'See further details in the documentation of the function 
#'\code{\link[deseats]{deseats}}, where this function is applied internally
#'by default to select an optimal bandwidth.
#'
#'@references
#'\itemize{
#'\item{Feng, Y. (2013). An iterative plug-in algorithm for decomposing seasonal 
#'time series using the Berlin Method. Journal of Applied Statistics, 40(2): 
#'266-281. DOI: 10.1080/02664763.2012.740626.}
#'\item{Feng, Y., Gries. T, and Fritz, M. (2020). Data-driven local polynomial 
#'for the trend and its derivatives in economic time series. Journal of 
#'Nonparametric Statistics, 32(2): 510-533. DOI: 10.1080/10485252.2020.1759598.}
#'}
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'\item Yuanhua Feng (Department of Economics, Paderborn
#'University), \cr
#'Author
#'}
#'
#'@examples
#'Xt <- log(EXPENDITURES)
#'select_bwidth(Xt)
#'

select_bwidth <- function(y, smoothing_options = set_options(), bwidth_start = NULL, inflation_rate = c("optimal", "naive"), correction_factor = TRUE, autocor = TRUE, drop = NULL, error_model = c("free", "ARMA")) {
  
  check_input_deseats(
    y,
    smoothing_options,
    bwidth_start,
    inflation_rate,
    correction_factor,
    autocor,
    drop,
    error_model
  )
  
  # Identify exact smoothing options from input
  det_options <- determine_options(y, smoothing_options)

  s <- det_options[["s"]]
  mu <- det_options[["mu"]] 
  p <- det_options[["p"]]
  bb <- det_options[["bb"]]
  
  # Identify algorithm options from input
  alg_options <- determine_alg(
    inflation_rate = inflation_rate,
    bwidth_start = bwidth_start,
    correction_factor = correction_factor,
    autocor = autocor,
    drop = drop,
    error_model = error_model,
    p = p
  )

  infr <- alg_options[["infr"]]
  err <- alg_options[["err"]]
  CF <- alg_options[["CF"]]
  err_m <- alg_options[["err_m"]]
  drop <- alg_options[["drop"]]
  bwidth_start <- alg_options[["bwidth_start"]]
  
  if (CF == 1 && err_m == 1 && err == 1 && !(s %in% c(1, 4, 7, 12))) {
    CF <- 0
    message("correction_factor switched to FALSE, because no correction factor is available for the frequency of the input time series")
  }
  
  tryCatch({
    alg_result <- algorithmCpp(y, p, s, mu, bwidth_start, CF, err, drop, bb, err_m, infr)
  }, error = function(e1) {
    stop(paste0(
      "Convergence of the bandwidth selection algorithm failed. Please try ",
      "other settings or select a bandwidth manually."
    ), call. = FALSE)
  }
  )
  
  bwidths <- c(alg_result$bwidths)
  alg_result$bopt <- tail(bwidths, 1)  
  
  names(bwidths) <- paste0("i=", 0:(length(bwidths) - 1))
  
  alg_result$bwidths <- bwidths
  
  alg_result
  
}

#' Locally Weighted Regression for Trend and Seasonality in Equidistant Time 
#' Series under Short Memory
#' 
#' Simultaneously estimate the trend and the 
#' seasonality via locally weighted regression in an equidistant time series 
#' under short memory. The default setting uses an iterative plug-in algorithm 
#' for identifying the asymptotically globally optimal bandwidth for smoothing.
#'
#'@param y a numerical vector or a time series object of class \code{ts} or 
#'that can be transformed with \code{\link[stats]{as.ts}} to an object of class 
#'\code{ts}; for these observations, trend and seasonality will be obtained.
#'@param smoothing_options an S4 object of class \code{smoothing_options}, which 
#'is returned by the function \code{\link{set_options}}; it 
#'includes details about the
#'options to consider in the locally weighted regression such as the order of
#'polynomial and the bandwidth for smoothing among others.
#'@param bwidth_start a single numeric value that is only relevant if the slot
#'\code{bwidth} in \code{smoothing_options} is set to \code{NA}; 
#'as the bandwidth will then
#'be selected automatically, \code{bwidth_start} sets the initial bandwidth for 
#'the algorithm; the default, \code{bwidth_start = NULL}, corresponds to 
#'\code{bwidth_start = 0.1} for a local linear trend and to 
#'\code{bwidth_start = 0.2} for a local cubic trend.
#'@param inflation_rate a character vector of length one that indicates, which inflation rate 
#' to use in the bandwidth selection; for a local linear trend, we have 
#'\code{inflation_rate = "optimal"} as the default, for a local cubic trend
#'it is \code{inflation_rate = "naive"}, which correspond to inflation rates
#'of 5/7 and 9/13, respectively.
#'@param correction_factor A logical vector of length one; theoretically, a 
#'larger bandwidth to estimate the sum of autocovariances from residuals of 
#'pilot trend and seasonality estimates is advisable than for estimating trend
#'and seasonality; for \code{correction_factor = TRUE}, this is implemented;
#'for \code{error_model = "ARMA"}, \code{correction_factor = FALSE} is 
#'enforced.
#'@param autocor a logical vector of length one; indicates whether to consider
#'autocorrelated errors (\code{TRUE}) or independent but identically 
#'distributed errors (\code{FALSE}); the default is \code{autocor = TRUE}.
#'@param drop a numeric vector of length one that indicates the proportion of 
#'the observations to not include at each boundary in the bandwidth estimation 
#'process, if a bandwidth is selected automatically; the default is 
#'\code{drop = NULL}, which corresponds to \code{drop = 0.05} for a 
#'local linear trend and to \code{drop = 0.1} for a local cubic trend.
#'@param error_model a character vector of length one that indicates whether
#'for \code{autocor = TRUE} the sum of autocovariances of the errors is 
#'obtained purely nonparametrically (\code{"free"}) or whether an
#'autoregressive moving-average (ARMA) model is assumed \code{"ARMA"}; the
#'default is \code{error_model = "free"}.
#'
#'@export
#'
#'@useDynLib deseats
#'
#'@details
#'
#'Trend and seasonality are estimated based on the additive
#'nonparametric regression model for an equidistant time series
#'\deqn{y_t = m(x_t) + s(x_t) + \epsilon_t,}
#'where \eqn{y_t} is the observed time series with \eqn{t=1,...n}, \eqn{x_t = t / n} is the rescaled time
#'on the interval \eqn{[0, 1]}, \eqn{m(x_t)} is a smooth and deterministic
#'trend function, \eqn{s(x_t)} is a (slowly changing) seasonal component with 
#'seasonal period \eqn{p_s} and \eqn{\epsilon_t} are stationary errors 
#'with \eqn{E(\epsilon_t) = 0} and short-range dependence (see for example also 
#'Feng, 2013, for a similar model, where the stochastic term is however i.i.d.).
#'
#'It is assumed that \eqn{m} and \eqn{s} can be approximated locally by a polynomial of
#'small order and by a trigonometric polynomial, respectively. Through locally
#'weighted regression, \eqn{m} and \eqn{s} can therefore be estimated given 
#'a suitable bandwidth.
#'
#'The iterative-plug-in (IPI) algorithm, which numerically minimizes the
#'asymptotic mean squared error (AMISE) to select a bandwidth is 
#'an extension of Feng (2013) to the case with short-range dependence in the 
#'errors. To achieve this goal, the error variance in the AMISE in Feng (2013) 
#'is replaced by the sum of autocovariances of the error process and this 
#'quantity is being estimated using a slightly adjusted version of the 
#'Bühlmann (1996) algorithm. This procedure is similar to the method described 
#'in Feng, Gries and Fritz (2020), 
#'where data-driven local polynomial regression with an automatically selected 
#'bandwidth is used to estimate the trend according to a model without 
#'seasonality and where the same adjusted Bühlmann (1996) algorithm is 
#'considered to estimate the sum of autocovariances in the error process.
#'
#'Define \eqn{I[m^{(k)}] = \int_{c_b}^{d_b} [m^{(k)}(x)]^2 dx}{I[m^(k)] =
#'int_[c_b]^[d_b] \{m^(k)\}^2 dx}, \eqn{\beta_{(k)} = \int_{-1}^{1} u^k
#'K(u) du}{\beta_(k) = int_[-1]^[1] u^k K(u) du}
#'and \eqn{R(K) = \int_{-1}^{1} K^{2}(u) du}{R(K) = int_[-1]^[1]
#'\{K\}^2 du}, where \eqn{p} is the order of the (local) polynomial 
#'considered for \eqn{m},
#'\eqn{k = p + 1} is the order of the asymptotically equivalent kernel \eqn{K} 
#'for estimating \eqn{m}, \eqn{0 \leq c_{b}< d_{b} \leq 1}{0 \le c_b < d_b \le 1}, and
#'\eqn{c_f} is the variance factor, i.e. the sum of autocovariances divided by 
#'\eqn{2\pi}.
#'
#'Furthermore, we define
#'\deqn{C_{1} = \frac{I[m^{(k)}] \beta_{(k)}^2}{(k!)^2}}{C_1 =
#'[I[m^(k)]\{\beta_(k)\}^2] / (k!)^2}
#'and
#'\deqn{C_{2} = \frac{2 \pi c_{f} (d_b - c_b)[R(K) + (p_s - 1) R(W)]}{nh}}{C_2 =
#'2\pi(d_b - c_b)R(K)c_f / (nh)}
#'with \eqn{h} being the bandwidth, \eqn{n} being the number of
#'observations and \eqn{W} being the weighting function considered in the 
#'weighted least squares approach, for example a second-order kernel function 
#'with support on \eqn{[-1,1]}. The AMISE is then
#'\deqn{AMISE(h) = h^{2k}C_{1} + C_{2}.}{AMISE(h) = h^[2k] C_1 +
#'C_2.}
#'
#'The function calculates suitable estimates for \eqn{c_f}, the variance
#'factor, and \eqn{I[m^{(k)}]}{I[m^(k)]} over different iterations. In each
#'iteration, a bandwidth is obtained in accordance with the AMISE that once
#'more serves as an input for the following iteration. The process repeats
#'until either convergence or the 40th iteration is reached. For further
#'details on the asymptotic theory or the algorithm, please consult Feng,
#'Gries and Fritz (2020) or Feng et al. (2019).
#'
#'To apply the function, only few arguments are needed: a data input \code{y},
#'an object with smoothing options \code{smoothing_options} returned by 
#'\code{\link{set_options}} and 
#'a starting value for the relative bandwidth
#'\code{bwidth_start}. Aside from \code{y}, each argument has a default setting.
#'By default, a local linear trend is considered. In some cases, a local cubic 
#'trend may, however, be more suitable. For more specific information on the input arguments
#'consult the section \emph{Arguments}.
#'
#'When applying the function, an optimal bandwidth is obtained based on the
#'IPI algorithm proposed by Feng, Gries and Fritz (2020). In a second step,
#'the nonparametric trend of the series and the seasonality are calculated with 
#'respect to the chosen bandwidth.
#'
#'Note that with this function \eqn{m(x_t)} and \eqn{s(x_t)} can be 
#'estimated without a parametric
#'model assumption for the error series. Thus, after estimating and removing
#'the trend and the seasonality, any suitable parametric model, e.g. an 
#'ARMA(\eqn{p,q}) model for \code{errors = "autocor"}, can be fitted to the 
#'residuals (see \code{\link[stats]{arima}}).
#'
#'Usually, a local cubic trend (\code{smoothing_options = set_options(order_poly = 3)})
#'gives more suitable results. Moreover, if the resulting bandwidth is too large, 
#'adjustments to the arguments \code{inflation_rate}, \code{drop} and 
#'\code{correction_factor} should be tried first in that order before any other changes 
#'to the input arguments.
#'
#'The default print method for this function delivers key numbers such as the 
#'bandwidth considered for smoothing.
#'
#'NOTE:
#'
#'This function implements C++ code by means
#'of the \code{\link[Rcpp:Rcpp-package]{Rcpp}} and
#'\code{\link[RcppArmadillo:RcppArmadillo-package]{RcppArmadillo}} packages for
#'better performance.
#'
#'@return
#'
#'The function returns and S4 object with the following elements (access them 
#'via \code{@}):
#'
#'\describe{
#'\item{\code{boundary_method}}{the applied boundary method.}
#'\item{\code{bwidth}}{the globally applied bandwidth in the smoothing process; if not 
#'if no input is given in the function call, this is the automatically selected 
#'optimal bandwidth.}
#'\item{\code{decomp}}{An object of class \code{"mts"} that consists of the
#'decomposed time series data.}
#'\item{\code{frequency}}{the frequency of the time series.}
#'\item{\code{kernel_fun}}{the second-order kernel function considered for weighting.}
#'\item{\code{order_poly}}{the order of polynomial considered locally for the trend.}
#'\item{\code{order_poly}}{the order of polynomial considered locally for the trend.}
#'\item{\code{sum_autocov}}{the estimated sum of autocovariances.}
#'\item{\code{ts_name}}{the object name of the initially provided time series object.}
#'\item{\code{weights_wfun}}{a matrix that gives the weights of the weighting 
#'function \eqn{K} at each estimation time point; ; if 
#'\eqn{n} is the length of the given time series and \eqn{b} is the applied 
#'(relative) bandwidth, then the first row of the weighting system gives the 
#'weighting function weights when estimating at \eqn{t=1}, the second row gives 
#'the weights when estimating at \eqn{t=2} and so on for all left-hand side 
#'boundary points 
#'until the middle row, which contains the 
#'weights used at all interior points; the rows following the middle row contain
#'the weights for right-hand side boundary points (the rows are ordered
#'chronologically)}
#'\item{\code{weights}}{an array with many slices that represent the weighting 
#'systems for various filters; each slice is a matrix, which gives the weighting 
#'system to estimate a component, for example trend + seasonality, as a weighted 
#'average from the given time series; if 
#'\eqn{n} is the length of the given time series and \eqn{b} is the applied 
#'(relative) bandwidth, then the first row of the weighting system gives the 
#'weights to obtain estimates at \eqn{t=1}, the second row gives the weights to 
#'obtain estimates at \eqn{t=2} and so on for all left-hand side boundary points 
#'until the middle row, which contains the 
#'weights used at all interior points; the rows following the middle row contain
#'the weights for right-hand side boundary points (the rows are ordered
#'chronologically); 
#'the slice names are \code{"Trend"}, \code{"Season"} and \code{"Combined"},
#'where \code{"Combined"} are the weights to estimate trend + seasonality 
#'combined.}
#'}
#'
#'@references
#'\itemize{
#'\item{Bühlmann, P. (1996). Locally Adaptive Lag-Window Spectral Estimation.
#'Journal of Time Series Analysis, 17(3): 247-270. 
#'DOI: 10.1111/j.1467-9892.1996.tb00275.x.}
#'\item{Feng, Y. (2013). An iterative plug-in algorithm for decomposing seasonal 
#'time series using the Berlin Method. Journal of Applied Statistics, 40(2): 
#'266-281. DOI: 10.1080/02664763.2012.740626.}
#'\item{Feng, Y., Gries. T, and Fritz, M. (2020). Data-driven local polynomial 
#'for the trend and its derivatives in economic time series. Journal of 
#'Nonparametric Statistics, 32(2): 510-533. DOI: 10.1080/10485252.2020.1759598.}
#'}
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'\item Yuanhua Feng (Department of Economics, Paderborn
#'University), \cr
#'Author
#'}
#'
#'@examples
#'\donttest{
#'Xt <- log(EXPENDITURES)
#'smoothing_options <- set_options(order_poly = 3)
#'est <- deseats(Xt, smoothing_options = smoothing_options)
#'est
#'plot(est, which = 1)
#'}
#'

deseats <- function(y, smoothing_options = set_options(), bwidth_start = NULL, inflation_rate = c("optimal", "naive"), correction_factor = TRUE, autocor = TRUE, drop = NULL, error_model = c("free", "ARMA")) {

  ts_name <- deparse(substitute(y))
  
  y <- stats::as.ts(y)
  att <- attributes(y)
  
  check_input_deseats(
    y,
    smoothing_options,
    bwidth_start,
    inflation_rate,
    correction_factor,
    autocor,
    drop,
    error_model
  )
  
  # Identify exact smoothing options from input
  det_options <- determine_options(y, smoothing_options)
  s <- det_options[["s"]]
  mu <- det_options[["mu"]] 
  p <- det_options[["p"]]
  bb <- det_options[["bb"]]
  err <- determine_err(autocor)
  err_m <- determine_err_m(error_model)
  CF <- determine_CF(correction_factor, err_m)
  bwidth <- smoothing_options@bwidth
  
  
  # Select bandwidth automatically if required
  if (is.na(bwidth)) {
    
    bwidth <- select_bwidth(
      y = y, 
      smoothing_options = smoothing_options, 
      bwidth_start = bwidth_start, 
      inflation_rate = inflation_rate, 
      correction_factor = correction_factor, 
      autocor = autocor, 
      drop = drop, 
      error_model = error_model
    )$bopt
  
  }

  est <- fitteddeseatsCpp(y, p, s, mu, bwidth, bb)
  Trend <- est[[2]]
  Season <- est[[3]]
  Residuals <- est[[4]]
  if (CF) {
    adjfactor <- CF_table$CF[CF_table$s == s & CF_table$p == p & CF_table$mu == mu]
    
    stopifnot("the use of correction_factor = TRUE inflates the bandwidth too strongly; try correction_factor = FALSE or use a smaller bandwidth" = bwidth * adjfactor <= 0.49)   
    
    ResidsAdj <- tryCatch({
      residdeseatsCpp(y, p, s, mu, bwidth * adjfactor, bb)
    }, error = function (e1){
      stop(paste0(
        "Convergence of the bandwidth selection algorithm failed. Please try ",
        "other settings or select a bandwidth manually."
      ), call. = FALSE)      
    }
    )
    if (err == 1) {
      sum_autocov <- cf0Cpp(ResidsAdj)
    } else if (err == 0) {
      sum_autocov <- mean(ResidsAdj^2)
    }
  } else {
    if (err == 1) {
      sum_autocov <- cf0Cpp(Residuals)      
    } else if (err == 0) {
      sum_autocov <- mean(Residuals^2) 
    }

  }
  hfull <- length(est[[5]][, 1])
  weights <- array(c(est[[5]], est[[6]], est[[7]]), dim = c(hfull, hfull, 3))
  dimnames(weights)[[3]] <- c("Trend", "Season", "Combined")
  attributes(Trend) <- att
  attributes(Season) <- att
  attributes(Residuals) <- att
  decomp <- cbind(
    Observations = y,
    Trend = Trend,
    Seasonality = Season,
    Rest = Residuals
  )
  
  out <- create_deseats(
    decomp = decomp,
    bwidth = bwidth,
    ts_name = ts_name,
    frequency = s,
    order_poly = p, 
    weights = weights,
    weights_wfun = est[[8]],
    boundary_method = smoothing_options@boundary_method,
    kernel_fun = smoothing_options@kernel_fun,
    sum_autocov = sum_autocov
  )
  
  out
}

#' Fitting of a Seasonal Semiparametric ARMA Model
#' 
#' Fit a seasonal semiparametric autoregressive moving-average 
#' (S-Semi-ARMA) model to a univariate time series. The estimation is 
#' in two steps: firstly, the series is detrended and seasonally adjusted using the
#' function \code{\link{deseats}}. Then an ARMA model is fitted to the 
#' residuals using \code{\link[stats]{arima}}. 
#'
#'@param yt a numerical vector or a time series object of class \code{ts} or 
#'that can be transformed with \code{\link[stats]{as.ts}} to an object of class 
#'\code{ts}; for these observations, trend and seasonality will be obtained.
#'@param smoothing_options an S4 object of class \code{smoothing_options}, which 
#'is returned by the function \code{\link{set_options}}; it 
#'includes details about the
#'options to consider in the locally weighted regression, such as the order of
#'polynomial and the bandwidth for smoothing among others, for the nonparametric
#'part of the model; the nonparametric model is fitted using 
#'\code{\link{deseats}}.
#'@param arma_options a list with the two elements \code{ar_order} and 
#'\code{ma_order} that indicates the AR and MA orders to consider for the 
#'parametric part of the model.
#'@param bwidth_start a single numeric value that is only relevant if the slot
#'\code{bwidth} in \code{smoothing_options} is set to \code{NA}; 
#'as the bandwidth will then
#'be selected automatically, \code{bwidth_start} sets the initial bandwidth for 
#'the algorithm.
#'@param inflation_rate a character vector of length one that indicates, which inflation rate 
#' to use in the bandwidth selection; for a local linear trend, we have 
#'\code{inflation_rate = "optimal"} as the default, for a local cubic trend
#'it is \code{inflation_rate = "naive"}, which correspond to inflation rates
#'of 5/7 and 9/13, respectively.
#'@param correction_factor A logical vector of length one; theoretically, a 
#'larger bandwidth to estimate the sum of autocovariances from residuals of 
#'pilot trend and seasonality estimates is advisable than for estimating trend
#'and seasonality; for \code{correction_factor = TRUE}, this is implemented;
#'for \code{error_model = "ARMA"}, \code{correction_factor = FALSE} is 
#'enforced.
#'@param drop a numeric vector of length one that indicates the proportion of 
#'the observations to not include at each boundary in the bandwidth estimation 
#'process, if a bandwidth is selected automatically; the default is 
#'\code{drop = 0.1}.
#'@param error_model a character vector of length one that indicates whether
#'for \code{autocor = TRUE} the sum of autocovariances of the errors is 
#'obtained purely nonparametrically (\code{"free"}) or whether an
#'autoregressive moving-average (ARMA) model is assumed \code{"ARMA"}; the
#'default is \code{error_model = "free"}.
#'
#'@export
#'
#'@details
#'
#'For information on the nonparametric regression step, see 
#'\code{\link[deseats]{deseats}}. After the trend and the seasonality have 
#'been removed from the data, an autoregressive moving-average (ARMA) model
#'is fitted to the residuals either with orders selected by the Bayesian
#'information criterion (BIC) or with manually selected orders. The ARMA
#'model is fitted using \code{\link[stats]{arima}}.
#'
#'All function arguments except for \code{arma_options} are identical to 
#'those in \code{\link[deseats]{deseats}}. If all elements in 
#'\code{arma_options} are set to \code{NULL}, the ARMA model orders are 
#'selected from \eqn{p, q = 0, 1, 2, 3} according to the BIC.
#'
#'
#'@return
#'The function returns and S4 object with the following elements (access them 
#'via \code{@}):
#'\describe{
#'\item{\code{decomp}}{an object of class \code{"mts"} that includes the 
#'observed time series and its estimated components.}
#'\item{\code{nonpar_model}}{an object of class \code{"deseats"}; this is 
#'the result of applying \code{\link{deseats}}.}
#'\item{\code{par_model}}{an object of class \code{"Arima"}; the result of 
#'applying \code{\link[stats]{arima}} to the residuals of the nonparametric 
#'estimation step.}
#'}
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'\item Yuanhua Feng (Department of Economics, Paderborn
#'University), \cr
#'Author
#'}
#'
#'@examples
#'\donttest{
#'Xt <- log(EXPENDITURES)
#'est <- s_semiarma(Xt)
#'est
#'}
#'
s_semiarma <- function(yt, smoothing_options = set_options(),
                       arma_options = list(ar_order = NULL, ma_order = NULL),
                       bwidth_start = 0.2, inflation_rate = c("optimal", "naive"),
                       correction_factor = TRUE, drop = 0.1,
                       error_model = c("free", "ARMA")) {
  
  ts_name <- deparse(substitute(yt))
  
  nonpar_model <- deseats(yt, smoothing_options, bwidth_start, inflation_rate = inflation_rate,
                          autocor = TRUE, correction_factor = correction_factor, drop = drop,
                          error_model = error_model)
  nonpar_model@ts_name <- ts_name
  
  res <- residuals(nonpar_model)
  
  ar_ma_select <- select_arma_orders(res, arma_options$ar_order, 
                                     arma_options$ma_order)  
  
  ar <- ar_ma_select[[1]]
  ma <- ar_ma_select[[2]]

  par_est <- stats::arima(res, order = c(ar, 0, ma),
                        include.mean = FALSE)

  par_est <- stats::arima(res, order = c(ar, 0, ma),
                        include.mean = FALSE)

  arma_res <- par_est$residuals
  attributes(arma_res) <- attributes(nonpar_model@decomp[, 1])
  arma_fitted <- residuals(nonpar_model) - arma_res
  
  decomp <- cbind(nonpar_model@decomp, arma_fitted, arma_res)
  colnames(decomp) <- c(
    "Observations",
    "Trend",
    "Seasonality",
    "Rest",
    "Fitted ARMA",
    "Residuals ARMA"
  )
  
  out <- create_s_semiarma(decomp = decomp, nonpar_model = nonpar_model, 
                           par_model = par_est)
  out
}

#------------------------------------------------------------------------------#

#'Trend and Seasonality Estimation Using the Berlin Procedure 4.1
#'
#'Economic data can be decomposed into a trend, a seasonal and a remainder 
#'component using the Berlin procedure  4.1 (German: Berliner Verfahren 
#'4.1), as used by the National Statistical Office of Germany. Currently with 
#'this version of the R package, only the trend and seasonal components can be 
#'estimated following BV4.1. All further 
#'component estimations, for example the estimation of the
#'calendar component, of the official procedure BV4.1 are not yet implemented. 
#'The function supports quarterly and monthly data. 
#'
#'@param yt a time series object of class \code{ts} or an object that can be 
#'converted into such an object with \code{\link[stats]{as.ts}}.
#'@param type a single character value that indicates, whether the data was
#'quarterly (\code{"quarterly"}) or monthly (\code{"monthly"}) observed; the 
#'default is \code{"monthly"}; if a time series object is passed to \code{yt},
#'the value for this argument will be automatically selected according to the 
#'frequency in \code{yt}. 
#'
#'@details
#'The BV4.1 base model is as follows:
#'
#'trend and seasonality are estimated based on the additive
#'nonparametric regression model for an equidistant time series
#'\deqn{y_t = m(x_t) + s(x_t) + \epsilon_t,}
#'where \eqn{y_t} is the observed time series with \eqn{t=1,...n}, \eqn{x_t = t / n} is the rescaled time
#'on the interval \eqn{[0, 1]}, \eqn{m(x_t)} is a smooth trend function, 
#'\eqn{s(x_t)} is a (slowly changing) seasonal component with 
#'seasonal period \eqn{p_s} and \eqn{\epsilon_t} are stationary errors 
#'with \eqn{E(\epsilon_t) = 0} that are furthermore assumed to be independent 
#'but identically distributed (i.i.d.).
#'
#'It is assumed that \eqn{m} and \eqn{s} can be approximated locally by a
#'polynomial of
#'small order and by a trigonometric polynomial, respectively. Through locally
#'weighted regression, \eqn{m} and \eqn{s} can therefore be estimated 
#'suitably.
#'
#'The advantage of the Berlin Procedure 4.1 (BV4.1) is that it makes use of
#'fixed filters based on locally weighted regression (both with a weighted 
#'mixture of local linear and local cubic components for the trend) at all 
#'observation time points. Thus, BV4.1 results in fixed weighting matrices 
#'both for the trend estimation step and for the seasonality estimation step 
#'that can be immediately applied to all economic time series. 
#'Those matrices are saved internally in the package and when applying 
#'\code{\link{BV4.1}}, 
#'only weighted sums of the observations (with already obtained weights) have 
#'to be obtained at all time points. Thus, this procedure is quite fast.
#'
#'Permission to include the BV4.1 base model procedure was kindly provided by 
#'the Federal Statistical Office of Germany.
#'
#'@export
#'
#'@references
#'\itemize{
#'\item{Speth, H.-T. (2004). Komponentenzerlegung und Saisonbereinigung ökonomischer 
#'Zeitreihen mit dem Verfahren BV4.1. Methodenberichte 3. Statistisches 
#'Bundesamt. URL: https://www.destatis.de/DE/Methoden/Saisonbereinigung/BV41-methodenbericht-Heft3_2004.pdf?__blob=publicationFile.}
#'}
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@return
#'An S4 object with the following elements is returned.
#'\describe{
#'\item{\code{decomp}}{An object of class \code{"mts"} that consists of the
#'decomposed time series data.}
#'\item{\code{frequency}}{the frequency of the time series.}
#'\item{\code{ts_name}}{the object name of the initially provided time series object.}
#'}
#'
#'@examples
#'
#'Xt <- log(EXPENDITURES)
#'est <- BV4.1(Xt)
#'est
#'
BV4.1 <- function(yt, type = NULL) {
  
  stopifnot(
    'yt is missing without default' = !missing(yt),
    'yt must be numeric, preferably an object of class "ts"' = is.numeric(yt),
    'type muste be either NULL, "quarterly" or "monthly"' = is.null(type) || (length(type) == 1 && is.character(type) && type %in% c("quarterly", "monthly"))
  )
  
  ts_name <- deparse(substitute(yt))
  
  yt <- stats::as.ts(yt)
  att <- attributes(yt)
  freq <- stats::frequency(yt)
  if (is.null(type)) {
    type <- switch(
      as.character(freq),
      "4" = "quarterly",
      "12" = "monthly",
      stop(paste0("The procedure cannot be applied to data with frequency ", freq, "."))
    )
  }
  
  max_vals <- switch(
    type,
    monthly = 30,
    quarterly = 11
  )
  max_vals2 <- switch(
    type,
    monthly = 51,
    quarterly = 17
  )  
  n_rows <- switch(
    type,
    monthly = 13,
    quarterly = 4
  )
  n_rows2 <- switch(
    type,
    monthly = 23,
    quarterly = 7
  )
  
  trend_mat <- bv_mats[[type]][["trend"]]
  season_mat <- bv_mats[[type]][["season"]]
  
  results <- BV41filterCpp(
    xt = c(yt),
    wst = trend_mat,
    wss = season_mat,
    nrowst = n_rows,
    nrowss = n_rows2,
    maxt = max_vals,
    maxs = max_vals2
  )
  
  trend_e <- results$trend_e
  season_e <- results$season_e
  
  attributes(trend_e) <- att
  attributes(season_e) <- att  
  
  rest <- yt - trend_e - season_e
  
  decomp <- cbind(Observations = yt, Trend = trend_e, Seasonality = season_e, Rest = rest)
  out <- create_bv41(decomp, ts_name, freq)
  out
  
}


#------------------------------------------------------------------------------#

#'Time Series Filtering Using the Hamilton Filter
#'
#'A stationary remainder is obtained from a univariate time series using the 
#'filter proposed by Hamilton. The filter is capable of estimating the trend 
#'together with the seasonality in a series.
#'
#'@param yt a time series object of class \code{ts} or an object that can be 
#'transformed to that class using \code{\link[stats]{as.ts}}.
#'@param h the backwards time skip for the first regressor; the default is 
#'the seasonal period in \code{yt} multiplied by 2.
#'@param p the number of regressors; the default is the seasonal period in
#'\code{yt}.
#'
#'@details
#'Implement the filter by Hamilton (2018) to decompose a time series.
#'
#'@export
#'
#'@references
#'Hamilton, J. D. (2018). Why You Should Never Use the Hodrick-Prescott Filter.
#'The Review of Economics and Statistics, 100(5): 831–843.
#'DOI: 10.1162/rest_a_00706.
#'
#'@return
#'A list with the following elements is returned.
#'\describe{
#'\item{decomp}{an object of class \code{"mts"} that consists of the
#'decomposed time series data.}
#'\item{ts_name}{the object name of the initially provided time series object.}
#'\item{frequency}{the frequency of the time series.}
#'\item{regression_output}{an object of class \code{"lm"}, i.e. basic regression
#'output.}
#'}
#'
#'@examples
#'est <- hamilton_filter(log(EXPENDITURES))
#'est
#'

hamilton_filter <- function(yt, h = NULL, p = NULL) {

  ts_name <- deparse(substitute(yt))
  
  s <- stats::frequency(yt)
  h <- 2 * s
  p <- s
  
  n <- length(yt)
  
  t <- c(stats::time(yt))
  t_start <- utils::tail(t, n - (h + p - 1))[[1]]
  
  # The dependent variable for OLS
  X.dep <- utils::tail(yt, n - h - (p - 1))
  
  # Create a matrix with all independent variables
  X.ind <- matrix(NA, ncol = p, nrow = length(X.dep))
  for (i in 1:p) {
    X.ind[, i] <- yt[(p - (i - 1)):(n - h - (i - 1))]
  }
  
  # Combine the dependent and independent variables and create 
  # a dataframe with specific column names
  dataset <- as.data.frame(cbind(X.dep, X.ind))
  colnames(dataset) <- c("X.dep", paste0("X.ind", 1:p))
  
  # Conduct OLS and retrieve the coefficients, the fitted values and
  # the residuals
  est <- stats::lm(X.dep ~ ., data = dataset)
  coefs <- est[["coefficients"]]

  fit <- stats::ts(est[["fitted.values"]], start = t_start, frequency = s)
  res <- stats::ts(est[["residuals"]], start = t_start, frequency = s)
  
  # Create and return the output
  out <- create_hfilter(
    decomp = cbind(Observations = yt, Fitted = fit, Rest = res),
    ts_name = ts_name,
    frequency = s,
    regression_output = est
  )
  out  
  
}

#------------------------------------------------------------------------------#

#'Decomposition of Time Series Using Linear Regression
#'
#'Trend and seasonality are simultaneously modelled by considering a polynomial
#'for the trend and a polynomial in the seasonality (via dummy variables and 
#'their interactions with time) for the different time units (e.g. months).
#'
#'@param yt a time series object of class \code{"ts"} or an object that can be 
#'transformed to that class using \code{\link[stats]{as.ts}}.
#'@param order_poly the order of the polynomial considered for the trend; the
#'default is \code{order_poly = 1}.
#'@param order_poly_s the order of the polynomial considered for the 
#'seasonality; the default is \code{order_poly_s = 1}.
#'@param season the seasonal period in \code{yt}; by default, the seasonal 
#'period is obtained automatically from \code{yt}.
#'
#'@details
#'Apply ordinary least squares to estimate trend and seasonality simultaneously
#'in a given time series. This a global approach in contrast to for example 
#'\code{\link{deseats}}, which is a local estimation method.
#'
#'@export
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@return
#'An S4 object with the following elements is returned.
#'\describe{
#'\item{decomp}{an object of class \code{"mts"} that consists of the
#'decomposed time series data.}
#'\item{ts_name}{the object name of the initially provided time series object.}
#'\item{frequency}{the frequency of the time series.}
#'\item{regression_output}{an object of class \code{"lm"}, i.e. basic regression
#'output; the time variable \code{t} used in the regression is encoded as 
#'\code{seq_along(yt)}; the dummy variable \code{S2} encodes 
#'the first observation time point (and the yearly corresponding time points) as 
#'\code{-1} and the second observation time point (and the yearly corresponding 
#'time points) as \code{1}, the dummy variable \code{S3} does the same but has 
#'instead for the third observation time point (and the yearly corresponding 
#'time points) a \code{1}, and so on.}
#'}
#'
#'@examples
#'est <- lm_decomp(log(EXPENDITURES), order_poly = 3, order_poly_s = 2)
#'est
#'

lm_decomp <- function(yt, order_poly = 1, order_poly_s = 1, season = NULL) {

  ts_name <- deparse(substitute(yt))
  if (is.null(season)) {
    season <- frequency(yt)
  }
  
  N <- length(yt)
  t <- seq_along(yt)
  season_add_col <- if (season > 1 && !is.na(order_poly_s)) {(season - 1) * (order_poly_s + 1)} else {0}
  X_mat <- matrix(0, nrow = N, ncol = order_poly + season_add_col)
  if (season > 1 && !is.na(order_poly_s)) {
    
    for (i in 1:(season - 1)) {
      vec_i <- rep(0, season)
      vec_i[[1]] <- -1
      vec_i[[i + 1]] <- 1
      for (j in 1:(order_poly_s + 1)) {
        X_mat[, order_poly + i + (j - 1) * (season - 1)] <- rep(vec_i, length.out = N) * t^(j - 1)
      }
    }
    name_season <- paste0(rep(paste0("S", 2:season), order_poly_s + 1), rep(paste0("*t^", 0:order_poly_s), each = season - 1))
    
  } else {
    name_season <- c()
  }
  if (order_poly > 0) {
    for (i in 1:order_poly) {
      X_mat[, i] <- t^i
    }
    name_trend <- paste0("t^", 1:order_poly)
  } else {
    name_trend <- c()
  }
  X_df <- as.data.frame(cbind(
    as.numeric(yt),
    X_mat
  ))
  names(X_df) <- c("yt", name_trend, name_season)
  reg <- stats::lm(yt ~ ., data = X_df)

  # if (season > 1) {
    l_coef <- length(reg$coefficients)
    s_coef <- reg$coefficients[(2 + order_poly):l_coef]
    seasonality <- c(X_mat[, (order_poly + 1):(l_coef - 1)] %*% s_coef)
    remainder <- yt - seasonality
    name_s <- "Season"
  # } else {
  #   seasonality <- c()
  #   name_s <- seasonality
  #   remainder <- yt
  # }
  # if (order_poly > 1) {
    # if (season > 1) {
      X_df[, (2 + order_poly):(1 + order_poly + season_add_col)] <- 0
    # }
    trend <- unname(predict(reg, X_df))
    remainder <- remainder - trend
    name_t <- "Trend"
  # } else {
  #   trend <- c()
  #   name_t <- trend
  # }
  
  out <- create_lmdecomp(
    decomp = cbind("Observations" = yt, "Trend" = trend, 
          "Seasonality" = seasonality, "Rest" = remainder),
    ts_name = ts_name,
    frequency = season,
    regression_output = reg
  )

  out
}

#------------------------------------------------------------------------------#

#'Decomposition of Time Series Using Moving Averages
#'
#'Trend and seasonality are modelled in a two-step approach, where first the 
#'trend is being estimated using moving averages and then trend + seasonality 
#'are being estimated using moving averages. The difference is then the 
#'estimated seasonality.
#'
#'@param yt a time series object of class \code{"ts"} or an object that can be 
#'transformed to that class using \code{\link[stats]{as.ts}}.
#'@param k_trend the complete absolute bandwidth (in years); represents the data 
#'of how many years to use around the estimation time point to consider 
#'for trend smoothing.
#'@param k_season the complete absolute bandwidth (in years); represents the data 
#'of how many years (only from the same quarter, month, etc.) to use around the 
#'estimation time point for trend + seasonality 
#'smoothing.
#'@param season the seasonal period in \code{yt}; by default, the seasonal 
#'period is obtained automatically from \code{yt}.
#'
#'@details
#'Apply moving averages to estimate trend and seasonality 
#'in a given time series. This approach results in missings \code{NA} at 
#'boundary points.
#'
#'@export
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@return
#'An S4 object with the following elements is returned.
#'\describe{
#'\item{decomp}{an object of class \code{"mts"} that consists of the
#'decomposed time series data.}
#'\item{ts_name}{the object name of the initially provided time series object.}
#'\item{frequency}{the frequency of the time series.}
#'\item{k_trend}{the same as the input argument \code{k_trend}.}
#'\item{k_season}{the same as the input argument \code{k_season}.}
#'}
#'
#'@examples
#'est <- ma_decomp(log(EXPENDITURES), k_trend = 6, k_season = 7)
#'est
#'

ma_decomp <- function(yt, k_trend = 4, k_season = 5, season = NULL) {

  ts_name <- deparse(substitute(yt))
  if (is.null(season)) {
    season <- frequency(yt)
  }
  
  P <- season
  k <- k_trend
  kP <- k * P
  n <- length(yt)
  kP_even <- kP %% 2 == 0  # TRUE if kP is even, FALSE otherwise
  
  m <- ifelse(kP_even,
    yes = kP / 2,          # Case 1: kP is even
    no = (kP - 1) / 2      # Case 2: kP is odd
  )
  
  g_hat <- yt              # Copy time series ...
  g_hat[1:n] <- NA         # and fill it with missing values
  
  for (i in (m + 1):(n - m)) {
    g_hat[[i]] <- (1 / kP) * (sum(yt[(i - m):(i + m)]) - kP_even * 0.5 * (yt[[i - m]] + yt[[i + m]]))
  }
  
  k <- k_season
  
  if (k %% 2 == 0) {
    stop("k_season must be an odd number.")
  }
  
  ms <- (k - 1) / 2
  
  gS_hat <- yt              # Copy time series ...
  gS_hat[1:n] <- NA         # and fill it with missing values
  
  for (j in (ms * P + 1):(n - ms * P)) {
    gS_hat[[j]] <- (1 / k) * sum(yt[seq(from = j - P * ms, to = j + P * ms, by = P)])
  }
  
  trend <- g_hat
  seasonality <- gS_hat - g_hat
  remainder <- yt - trend - seasonality
  
  out <- create_madecomp(
    decomp = cbind("Observations" = yt, "Trend" = trend, 
          "Seasonality" = seasonality, "Rest" = remainder),
    ts_name = ts_name,
    frequency = season,
    k_trend = k_trend,
    k_season = k_season
  )

  out
}

#------------------------------------------------------------------------------#

#'Decomposition of Time Series Using Local Linear Regression
#'
#'Trend and seasonality are modelled in a two-step approach, where first the 
#'trend is being estimated using local linear regression and then the 
#'seasonality is being estimated using various local linear regressions as 
#'well. In both cases a manually selected bandwidth is required.
#'
#'@param yt a time series object of class \code{"ts"} or an object that can be 
#'transformed to that class using \code{\link[stats]{as.ts}}.
#'@param bwidth_trend half of the absolute bandwidth (in years); represents the 
#'amount of data to use around the estimation time point to consider 
#'for trend smoothing.
#'@param bwidth_season half of the absolute bandwidth (in years); represents the  
#'amount of data (only from the same quarter, month, etc.) to use around the 
#'estimation time point for the seasonality estimation.
#'@param kernel_par the smoothness parameter for the second-order kernel function 
#'used in the weighting process; for \code{kernel_par = 0} a uniform kernel 
#'is used, for \code{kernel_par = 1} an epanechnikov kernel, and so on.
#'@param boundary_method a single character value; it indicates, what bandwidth 
#'method to use at boundary points; for \code{"extend"}, the default, the 
#'smoothing window around boundary points will be extended towards the center of 
#'the data; for \code{"shorten"}, the window width will keep decreasing at 
#'boundary points when approaching the very first and the very last observation.
#'@param season the seasonal period in \code{yt}; by default, the seasonal 
#'period is obtained automatically from \code{yt}.
#'
#'@details
#'Apply local linear regression to estimate trend and seasonality 
#'in a given time series \eqn{y_t}. Assume that \eqn{y_t} follows an additive 
#'component model with trend and seasonality components. First, a local linear 
#'regression with a first (absolute) bandwidth is conducted to estimate the trend 
#'from the series. If the seasonal period is \eqn{s}, then afterwards \eqn{s} 
#'local linear regressions (for each individual seasonal subseries of the 
#'detrended series) are conducted with a second (absolute) bandwidth to obtain 
#'seasonality estimates.
#'
#'@export
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
#'@return
#'An S4 object with the following elements is returned.
#'\describe{
#'\item{decomp}{an object of class \code{"mts"} that consists of the
#'decomposed time series data.}
#'\item{ts_name}{the object name of the initially provided time series object.}
#'\item{frequency}{the frequency of the time series.}
#'\item{bwidth_trend}{the same as the input argument \code{bwidth_trend}.}
#'\item{bwidth_season}{the same as the input argument \code{bwidth_season}.}
#'\item{boundary_method}{the same as the input argument \code{boundary_method}.}
#'\item{kernel_par}{the same as the input argument \code{kernel_par}.}
#'}
#'
#'@examples
#'est <- llin_decomp(log(EXPENDITURES), bwidth_trend = 4, bwidth_season = 28)
#'est
#'


llin_decomp <- function(yt, bwidth_trend = 4, bwidth_season = 5, kernel_par = 1, boundary_method = c("extend", "shorten"), season = NULL) {
  
  boundary_method <- match.arg(boundary_method)  
  
  stopifnot(
    "yt is missing with no default" = !(missing(yt)),
    'boundary_method must be either "extend" or "shorten"' = is.character(boundary_method) && boundary_method %in% c("extend", "shorten"),
    "kernel_par must be a single numeric value" = is.numeric(kernel_par)
  )

  bb <- switch(
    boundary_method,
    "extend" = 1,
    "shorten" = 0
  )
  
  mu <- trunc(kernel_par)
  
  ts_name <- deparse(substitute(yt))
  if (is.null(season)) {
    season <- frequency(yt)
  }    
  
  P <- season
  t <- as.numeric(time(yt))
  
  
  trend_e <- ts(c(llin_calc_Cpp(yt, t, bwidth_trend, P, mu, bb, 0)), start = attributes(yt)$tsp[[1]], frequency = P)
  
  resids <- yt - trend_e
  season_e <- ts(c(llin_calc_Cpp(resids, t, bwidth_season, P, mu, bb, 1)), start = attributes(yt)$tsp[[1]], frequency = P)

  out <- create_llindecomp(
    decomp = cbind("Observations" = yt, "Trend" = trend_e, 
          "Seasonality" = season_e, "Rest" = resids - season_e),
    ts_name = ts_name,
    frequency = season,
    bwidth_trend = bwidth_trend,
    bwidth_season = bwidth_season,
    boundary_method = boundary_method,
    kernel_par = mu
  )

  out  
  
}
