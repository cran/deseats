# New S4 class for smoothing options

setClass("smoothing_options",
  slots = c(
    order_poly = "numeric",
    season = "numeric",
    kernel_fun = "character",
    bwidth = "numeric",
    boundary_method = "character"
  ),
  prototype = list(
    order_poly = 1,
    season = NA_real_,
    kernel_fun = "epanechnikov",
    bwidth = NA_real_,
    boundary_method = "extend"
  )
)

setValidity("smoothing_options", function(object) {
  if (length(object@order_poly) > 1 || !(object@order_poly %in% c(1, 3))) {
    "@order_poly must be of length 1 and set to either 1 or 3"
  } else if (length(object@season) > 1 || (!(is.na(object@season)) && !is.numeric(object@season))) {
    "@season must be of length 1 and either NA or a numeric value"
  } else if (length(object@kernel_fun) > 1 || !(object@kernel_fun %in% c("uniform", "epanechnikov", "bisquare", "triweight"))) {
    '@kernel_fun must be of length 1 and set to either "uniform", "epanechnikov", "bisquare" or "triweight"'
  } else if (length(object@bwidth) > 1 || !(is.na(object@bwidth) || (object@bwidth > 0 && object@bwidth < 0.5))) {
    "@bwidth must be of length 1 and must be either set to NA or to a numeric value between 0 and 0.5"
  } else if (length(object@boundary_method) > 1 || !(object@boundary_method %in% c("extend", "shorten"))) {
    "@boundary_method must be of length 1"
  } else {
    TRUE
  }
})

#' Specification of Smoothing Options
#' 
#' Set the smoothing specifications for locally weighted regression for
#' identifying the trend and the seasonality in an equidistant time series.
#' 
#'@importFrom Rcpp sourceCpp
#'@import methods
#' 
#'@param order_poly the order of the local polynomials used for estimating the
#' smooth nonparametric trend; the default is \code{1}.
#'@param season the frequency of observations per time unit, for example per 
#'year; set to \code{12} for monthly data and to \code{4} for quarterly data
#'and so on; the default is \code{NA_real_}, which leads to an automated 
#'frequency selection for time series objects in smoothing functions; if 
#'the argument is set to \code{NA_real_} and the observations used for smoothing 
#'are not formatted as time series objects, the frequency \code{1} will be 
#'used.
#'@param kernel_fun the weighting function to consider; supported are four
#'second-order kernel functions with compact support on \eqn{[-1, 1]}; enter
#'\code{"uniform"} for the uniform kernel, \code{"epanechnikov"} for the 
#'Epanechnikov kernel, \code{"bisquare"} for the bisquare kernel or 
#'\code{"triweight"} for the triweight kernel; the default is 
#'\code{"epanechnikov"}.
#'@param bwidth a numeric value that indicates the relative bandwidth to 
#'consider in the smoothing process; the default is \code{NA}, which then
#'triggers a data-driven selection of an globally optimal bandwidth when
#'the output of this function is passed to a smoothing function.
#'@param boundary_method a single character value; it indicates, what bandwidth 
#'method to use at boundary points; for \code{"extend"}, the default, the 
#'smoothing window around boundary points will be extended towards the center of 
#'the data; for \code{"shorten"}, the window width will keep decreasing at 
#'boundary points when approaching the very first and the very last observation.
#'
#'@export
#' 
#'@details
#'
#'An object of class \code{"smoothing_options"} is created that contains 
#'all required information to conduct a locally weighted regression for 
#'decomposing a seasonal time series. The information include the order 
#'of the trend polynomials, the frequency of the observed series, the 
#'second-order kernel function to use in the weighting process, the 
#'(relative) bandwidth to employ, and the boundary method for the bandwidth.
#'
#'@return
#'The function returns an S4 object with the following elements (access via 
#'\code{@}):
#'
#'\describe{
#'\item{order_poly}{identical to the input argument with that name; please see 
#'the description of that input argument.}
#'\item{season}{identical to the input argument with that name; please see 
#'the description of that input argument.}
#'\item{kernel_fun}{identical to the input argument with that name; please see 
#'the description of that input argument.}
#'\item{bwidth}{identical to the input argument with that name; please see 
#'the description of that input argument.}
#'\item{boundary_method}{identical to the input argument with that name; please 
#'see the description of that input argument.}
#'}
#' 
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}

set_options <- function(
    order_poly = 1,
    season = NA_real_,
    kernel_fun = "epanechnikov",
    bwidth = NA_real_,
    boundary_method = "extend") {
  
      methods::new("smoothing_options",
        order_poly = order_poly,
        season = season,
        kernel_fun = kernel_fun,
        bwidth = bwidth,
        boundary_method = boundary_method
      )
  
}


#'Retrieve or Set Smoothing Options
#'
#'Retrieve smoothing options from or set them in an object 
#'of class \code{"smoothing_options"}.
#'
#'@param object an object of class \code{"smoothing_options"}.
#'
#'@details
#'Various methods are provided to either retrieve smoothing option
#'settings from an object of class \code{"smoothing_options"} or 
#'to adjust them within such an object.
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
#'The methods without \code{<-} return the corresponding settings in the supplied 
#'object. The methods with \code{<-} set the corresponding option in the 
#'provided object.
#'
#'@examples
#'opts <- set_options()
#'opts
#'order_poly(opts)
#'order_poly(opts) <- 3
#'opts
#'
methods::setMethod("order_poly", "smoothing_options", function(object) object@order_poly)

#'
#'@param value the value to set the corresponding option to.
#'
#'@rdname order_poly-smoothing_options-method
#'
#'@export
methods::setMethod("order_poly<-", "smoothing_options", function(object, value) {
  object@order_poly <- value
  validObject(object)
  object
})

#'@rdname order_poly-smoothing_options-method
#'@export
methods::setMethod("season", "smoothing_options", function(object) object@season)

#'@rdname order_poly-smoothing_options-method
#'@export
methods::setMethod("season<-", "smoothing_options", function(object, value) {
  object@season <- value
  validObject(object)  
  object
})

#'@rdname order_poly-smoothing_options-method
#'@export
methods::setMethod("kernel_fun", "smoothing_options", function(object) object@kernel_fun)

#'@rdname order_poly-smoothing_options-method
#'@export
methods::setMethod("kernel_fun<-", "smoothing_options", function(object, value) {
  object@kernel_fun <- value
  validObject(object)  
  object
})

#'@rdname order_poly-smoothing_options-method
#'@export
methods::setMethod("bwidth", "smoothing_options", function(object) object@bwidth)

#'@rdname order_poly-smoothing_options-method
#'@export
methods::setMethod("bwidth<-", "smoothing_options", function(object, value) {
  object@bwidth <- value
  validObject(object)  
  object
})

#'@rdname order_poly-smoothing_options-method
#'@export
methods::setMethod("boundary_method", "smoothing_options", function(object) object@boundary_method)

#'@rdname order_poly-smoothing_options-method
#'@export
methods::setMethod("boundary_method<-", "smoothing_options", function(object, value) {
  object@boundary_method <- value
  validObject(object)  
  object
})

#------------------

#'Show Method for Smoothing Options
#'
#'Show smoothing settings in an object of class \code{"smoothing_options"}. 
#'
#'@param object an object of class \code{"smoothing_options"}.
#'
#'@details
#'This is a special printing method for objects object of class 
#'\code{"smoothing_options"}. Calling this method shows a nice looking 
#'overview of the saved smoothing settings.
#'
#'@return
#'This method returns \code{NULL}.
#'
#'@export
#'
#'@examples
#'opts <- set_options()
#'opts
#'
setMethod("show", "smoothing_options",
  function(object){
    .df <- data.frame(
      opt = c(
        "Order of local polynomials (trend):",
        "Frequency (seasonality):",
        "Kernel function:",
        "Bandwidth:",
        "Boundary method:"
      ),
      setting = c(
        object@order_poly,
        object@season,
        object@kernel_fun,
        object@bwidth,
        object@boundary_method
      )
    )
    colnames(.df) <- NULL
    cat(paste0("\n",
      "*******************************************************\n",
      "*                                                     *\n",
      "*                   Smoothing Options                 *\n",
      "*                                                     *\n",
      "*******************************************************\n\n"
    ))
    print.data.frame(.df, row.names = FALSE, right = FALSE)
    cat("\n")
  }
)
