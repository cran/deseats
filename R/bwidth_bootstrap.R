

#' Bootstrapping Confidence Intervals for Locally Weighted Regression Bandwidths
#' 
#' A stationary block bootstrap is applied to resample from a time 
#' series that was decomposed into a trend, a seasonal component and a 
#' remainder by means of data-driven local polynomial regression with 
#' automatically selected bandwidth. Bandwidth re-estimation from each 
#' bootstrapped sample results in confidence bounds for the 
#' bandwidth.
#' 
#' @param nonpar_model the object with the nonparametric trend and seasonality
#' estimation results returned by for example the function 
#' \code{\link{deseats}}.
#' @param blocklen a numerical vector of length one that indicates the average 
#' block length to be drawn from the detrended series; the default is 
#' \code{NULL}, which means \code{8} for quarterly and \code{24} for monthly 
#' data; selecting a suitable expected blocklength and checking the sensitivity 
#' of the blocklength are left for the user.
#' @param npaths a numeric vector of length one that indicates the number of
#' bootstrap paths; the default is \code{npaths = 1000}.
#' @param parallel a logical vector of length one that indicates whether or 
#' not to employ parallel programming for the resampling and the subsequently 
#' data-driven bandwidth estimations from the bootstrapped samples; the default 
#' is \code{patrallel = TRUE}.
#' @param num_cores a numeric vector of length one that indicates the number of
#' CPU cores to use for parallel programming, if \code{parallel = TRUE}; the 
#' default is \code{num_cores = future::availableCores() - 1}.
#' @param ... further arguments to pass to \code{\link{deseats}}.
#' 
#' @export
#' 
#' @details
#' Confidence bounds for the bandwidth in local polynomial regression 
#' for identifying the trend in a trend-stationary short-memory time 
#' series are obtained via a block bootstrap, which ensures that no 
#' specific model assumptions are required for the detrended series.
#' 
#' This function makes use of the \code{future} parallel programming 
#' framework to ensure exactly the same results regardless of whether
#' sequential or parallel programming, and then also regardless of
#' the number of workers, is employed.
#' 
#' @importFrom future availableCores plan multisession
#' @importFrom furrr future_map_dbl furrr_options
#' @importFrom purrr map
#' 
#' @return
#' A list with the following elements is returned.
#' \describe{
#' \item{\code{conf}}{A vector with named elements that gives the original 
#' bandwidth estimate as well as the bootstrapped bounds of the 95 and 
#' 99 percent confidence intervals of the bandwidth.}
#' \item{\code{bwidth_estimates}}{a vector with all the obtained bandwidths 
#' for the bootstrapped series.}
#' \item{\code{se_bwidth}}{the sample standard deviation of 
#' \code{bwidth_estimates}.}
#' }
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
#' xt <- log(EXPENDITURES)
#' est <- deseats(xt, set_options(order_poly = 3))
#' conf <- bwidth_confint(est, npaths = 200, num_cores = 2)
#' conf
#' }
#' 

bwidth_confint <- function(nonpar_model, blocklen = NULL, npaths = 1000, parallel = TRUE, num_cores = future::availableCores() - 1, ...) {
  
  stopifnot(
    "nonpar_model is missing with no default" = !missing(nonpar_model),
    'nonpar_model must be an object of class "deseats"' = inherits(nonpar_model, "deseats"),
    "blocklen must a be either NULL or a numeric vector of length one" = is.null(blocklen) || (is.numeric(blocklen) && length(blocklen) == 1),
    "npaths must be a numeric vector of length one" = is.numeric(npaths) && length(npaths) == 1,
    "parallel must be a single logical value" = is.logical(parallel) && length(parallel) == 1,
    "num_cores must be a numeric vector of length one" = is.numeric(num_cores) && length(num_cores) == 1
  )
  
  gxt <- fitted(nonpar_model)
  fr <- frequency(gxt)
  if (is.null(blocklen)) {
    blocklen <- fr * 2
  }
  gxt <- c(gxt)
  res <- c(residuals(nonpar_model))
  order_p <- nonpar_model@order_poly
  
  sm_opts <- set_options(
    order_poly = order_p,
    season = fr,
    kernel_fun = nonpar_model@kernel_fun,
    boundary_method = nonpar_model@boundary_method
  )
  
  if (parallel) {
    oldplan <- future::plan()
    future::plan(strategy = future::multisession(), workers = num_cores)
    on.exit({
      future::plan(oldplan)
    }, add = TRUE, after = TRUE)
  }
  
  p <- 1 / blocklen
  
  res2 <- c(res, res)
  n <- length(res)
  
  b0_all <- furrr::future_map_dbl(
    1:npaths,
    .f = function(.x, gxt, res2, p, n, sm_opts, ...) {
      Li <- rgeom(n, prob = p) + 1
      cLi <- cumsum(Li)
      nLi <- sum(cLi < n) + 1
      Li <- Li[1:nLi]
      Ni <- sample(1:n, size = nLi, replace = TRUE)
      
      index_sel <- unlist(
        purrr::map2(Ni, Li,
          function(.x, .y) {
            .x:(.x + .y - 1)
          }
        )
      )
      ts_new <- ts(gxt + head(res2[index_sel], n), frequency = fr)
      deseats(ts_new, smoothing_options = sm_opts, ...)@bwidth
      
    }, gxt = gxt, res2 = res2, p = p, n = n, sm_opts = sm_opts,
    ... = ...,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )
  
  out <- list(
    conf = c(
      "estimate" = nonpar_model@bwidth,
      quantile(b0_all, probs = c(0.005, 0.025, 0.975, 0.995))
    ),
    bwidth_estimates = b0_all,
    se_bwidth = stats::sd(b0_all)
  )
  
  out
  
}