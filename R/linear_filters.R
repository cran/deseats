#'AR Representation of an ARMA Model
#' 
#'Calculate the coefficients of the infinite-order AR-representation
#'of a given ARMA model.
#' 
#'@param ar a numeric vector with the AR parameters of the ARMA model;
#'should be ordered from \eqn{a_1} to \eqn{a_p} (see Details).
#'@param ma a numeric vector with the MA parameters of the ARMA model;
#'should be ordered from \eqn{b_1} to \eqn{b_q} (see Details).
#'@param max_i a single numeric value that indicates how many coefficients
#'should be returned; returned will be \code{max_i + 1} coefficients (the
#'coefficient for index 0 is also returned as the first value).
#' 
#'@details
#'Consider an ARMA model
#'\deqn{X_t = a_1 X_{t-1} + ... + a_p X_{t-p} + b_1 \epsilon_{t-1} + ... + b_q \epsilon_{t-q} + \epsilon_t,}
#'where \eqn{a_1, ..., a_p} and \eqn{b_1, ..., b_q} are its real-valued 
#'coefficients. The function \code{arma_to_ar()} uses these coefficients as 
#'input to calculate the coefficients of the truncated infinite-order 
#'AR-representation of the model defined through these coefficients. Note that 
#'the stationarity and invertibility of the model defined through the provided 
#'coefficients is not being checked.
#'
#'NOTE:
#'
#'This function implements C++ code by means
#'of the \code{\link[Rcpp:Rcpp-package]{Rcpp}} and
#'\code{RcppArmadillo} packages for
#'better performance.
#'
#'@return
#'A numeric vector is returned.
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
#'@examples
#'ar <- c(1.2, -0.4)
#'ma <- c(0.5)
#'arma_to_ar(ar = ar, ma = ma, max_i = 100)
#' 

arma_to_ar <- function(ar = numeric(0), ma = numeric(0), max_i = 1000) {
  stopifnot(
    "ar must be a numeric vector" = (is.numeric(ar)),
    "ma must be a numeric vector" = (is.numeric(ma)),
    "max_i must be a numeric vector of length one" = (is.numeric(max_i) && length(max_i) == 1)
  )
  c(ARinftySHORT(ar = ar, ma = ma, max_i = max_i))
}

#'MA Representation of an ARMA Model
#' 
#'Calculate the coefficients of the infinite-order MA-representation
#'of a given ARMA model.
#' 
#'@param ar a numeric vector with the AR parameters of the ARMA model;
#'should be ordered from \eqn{a_1} to \eqn{a_p} (see Details).
#'@param ma a numeric vector with the MA parameters of the ARMA model;
#'should be ordered from \eqn{b_1} to \eqn{b_q} (see Details).
#'@param max_i a single numeric value that indicates how many coefficients
#'should be returned; returned will be \code{max_i + 1} coefficients (the
#'coefficient for index 0 is also returned as the first value).
#' 
#'@details
#'Consider an ARMA model
#'\deqn{X_t = a_1 X_{t-1} + ... + a_p X_{t-p} + b_1 \epsilon_{t-1} + ... + b_q \epsilon_{t-q} + \epsilon_t,}
#'where \eqn{a_1, ..., a_p} and \eqn{b_1, ..., b_q} are its real-valued 
#'coefficients. The function \code{arma_to_ar()} uses these coefficients as 
#'input to calculate the coefficients of the truncated infinite-order 
#'MA-representation of the model defined through these coefficients. Note that 
#'the stationarity and invertibility of the model defined through the provided 
#'coefficients is not being checked.
#'
#'NOTE:
#'
#'This function implements C++ code by means
#'of the \code{\link[Rcpp:Rcpp-package]{Rcpp}} and
#'\code{RcppArmadillo} packages for
#'better performance.
#'
#'@return
#'A numeric vector is returned.
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
#' @importFrom Rcpp sourceCpp
#' 
#'@examples
#'ar <- c(1.2, -0.4)
#'ma <- c(0.5)
#'arma_to_ma(ar = ar, ma = ma, max_i = 100)
#' 

arma_to_ma <- function(ar = numeric(0), ma = numeric(0), max_i = 1000) {
  stopifnot(
    "ar must be a numeric vector" = (is.numeric(ar)),
    "ma must be a numeric vector" = (is.numeric(ma)),
    "max_i must be a numeric vector of length one" = (is.numeric(max_i) && length(max_i) == 1)
  )
  
  c(MAinftySHORT(ar = ar, ma = ma, max_i = max_i))
}
