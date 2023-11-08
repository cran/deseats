#'Calculation of Theoretically Optimal Bandwidth and Its Components
#'
#'Allows to calculate the theoretically optimal bandwidth for estimating
#'the trend and the seasonality in an equidistant time series with short-range 
#'dependence using locally weighted regression, if the trend function 
#'and the exact ARMA dependence structure of the errors are known.
#'
#'@param m an expression that defines the trend function in terms of \code{x},
#'where \code{x} is the rescaled time on the interval \eqn{[0,1]}.
#'@param arma a list with the elements \code{ar}, \code{ma} and \code{sd_e};
#'\code{ar} is a numeric vector with the AR-coefficients, \code{ma} is a numeric 
#'vector with the MA-coefficients and \code{sd_e} is the innovation standard 
#'deviation.
#'@param p the order of polynomial to use locally for the trend estimation.
#'@param mu the smoothness parameter of the second-order kernel function used 
#'in the weighting process.
#'@param frequ the frequency of the theoretical time series (4 for quarterly and
#'12 for monthly time series).
#'@param n the number of observations.
#'@param cb the part of observations to drop at each boundary.
#'
#'@details
#'For simulation studies of the function \code{\link{deseats}} one may be 
#'interested in obtaining the theoretically optimal bandwidth for local 
#'regression first for a given theoretical process (from which realizations 
#'will be drawn in the simulation). This function assists in obtaining this 
#'theoretical bandwidth.
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
#'This function returns a list with various elements. See the documentation of 
#'\code{\link{deseats}} to understand, what each quantity signifies.
#'\describe{
#'\item{\code{b}}{This is the theoretical quantity \eqn{\beta_{(k)}}.}
#'\item{\code{hA}}{The theoretically asymptotically optimal global bandwidth for 
#'locally weighted regression applied to the theoretical time series under 
#'consideration.}
#'\item{\code{Imk}}{This is the theoretical quantity \eqn{I[m^{(k)}]}.}
#'\item{\code{RK}}{This is the theoretical quantity \eqn{R(K)}.}
#'\item{\code{RW}}{This is the theoretical quantity \eqn{R(W)}.}
#'\item{\code{sum_autocov}}{This is the theoretical quantity \eqn{2\pi c_f}.}
#'}
#'
#'@examples
#'\donttest{
#'arma <- list(ar = 0.8, sd_e = 0.01)
#'m_f <- expression(13.1 + 3.1 * x + (dnorm(x / 0.15 - 0.5 / 0.15) / 0.15) / 4)
#'n <- 500
#'p <- 1
#'mu <- 1
#'frequ <- 4
#'cb <- 0.05
#'
#'hA_calc(
#'  m = m_f,
#'  arma = arma, 
#'  p = p,
#'  mu = mu,
#'  frequ = frequ,
#'  n = n,
#'  cb = cb
#')
#'
#'t <- 1:n
#'xt <- t / n
#'mxt <- 13.1 + 3.1 * xt + dnorm(xt, mean = 0.5, sd = 0.15) / 4
#'
#'S2 <- rep(c(0, 1, 0, 0), length.out = n)
#'S3 <- rep(c(0, 0, 1, 0), length.out = n)
#'S4 <- rep(c(0, 0, 0, 1), length.out = n)
#'sxt <- -0.5 + 0.25 * S2 + 0.5 * S3 + 1.25 * S4
#'
#'set.seed(123)
#'et <- arima.sim(model = list(ar = 0.8), sd = 0.01, n = n)
#'yt <- ts(mxt + sxt + et, frequency = frequ)
#'plot(yt)
#'
#'est <- deseats(yt)
#'est@bwidth
#'est@sum_autocov
#'}
#'




hA_calc <- function(
  m,
  arma = list(ar = NULL, ma = NULL, sd_e = 1),
  p = c(1, 3),
  mu = c(0, 1, 2, 3),
  frequ = c(4, 12),
  n = 300,
  cb = 0.1
) {
  
  if (all(frequ == c(4, 12))) {
    frequ <- 4
  }
  if (all(p == c(1, 3))) {
    p <- 1
  }
  if (all(mu == c(0, 1, 2, 3))) {
    mu <- 1
  }
  
  sum_acov <- (arma$sd_e * ((1 + sum(arma$ma)) / (1 - sum(arma$ar))))^2
  
  k <- p + 1
  
  C <- c(0.5, 0.75, 15 / 16, 35 / 32)
  C <- C[[mu + 1]]
  
  Wu_s <- function(C, mu) {
    force(C)
    force(mu)
    function(u){C * (1 - u^2)^mu}
  }
  Wu <- Wu_s(C = C, mu = mu)
  
  if (p == 1) {
    Ku = Wu;
  } else if (p == 3) {
    if (mu == 0) {
      Ku <- function(u) {3.0 / 8.0 * (3.0 - 5.0 * u^2)}
    } else if (mu == 1) {
      Ku <- function(u) {15.0 / 32.0 * (3.0 - 10.0 * u^2 + 7.0 * u^4)}      
    } else if (mu == 2) {
      Ku <- function(u) {105.0 / 64.0 * (1.0 - 5.0 * u^2 + 7.0 * u^4 - 3.0 * u^6)}      
    } else if (mu == 3) {
      Ku <- function(u) {315.0 / 512.0 * (3.0 - 20.0 * u^2 + 42.0 * u^4 - 36.0 * u^6 + 11.0 * u^8)}
    }
  }
  
  mk <- stats::D(m, "x")
  
  for (i in 2:k) {
    mk <- stats::D(mk, "x")
  }
  
  mk_f <- function(x, mk = mk) {
    eval(mk)^2
  }
  
  Imk <- stats::integrate(mk_f, cb, 1 - cb, mk = mk, subdivisions = 100000L)[[1]]
  
  b_f <- function(u, Ku = Ku, k = k) {
    u^k * Ku(u)
  }
  
  b <- stats::integrate(b_f, -1, 1, Ku = Ku, k = k, subdivisions = 100000L)[[1]]
  
  K2_f <- function(u, Ku) {
    Ku(u)^2
  }
  RK <- stats::integrate(K2_f, -1, 1, Ku = Ku, subdivisions = 100000L)[[1]]
  
  W2_f <- function(u, Wu) {
    Wu(u)^2
  }
  RW <- stats::integrate(W2_f, -1, 1, Wu = Wu, subdivisions = 100000L)[[1]]
  
  h_c1 <- (factorial(k))^2 / (2 * k)
  h_c2 <- (sum_acov * ((1 - cb) - cb) * (RK + (frequ - 1) * RW)) / (Imk * b^2)
  
  list(
    RW = RW,
    RK = RK,
    sum_acov = sum_acov,
    b = b,
    Imk = Imk,
    hA = (h_c1 * h_c2 * (1 / n))^(1 / (2 * k + 1))
    
  )
  
}