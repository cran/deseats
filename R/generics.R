#'Smoothing Option Generics
#'
#'Various generics that can be used write methods to either retrieve or set 
#'smoothing options.
#'
#'@param object an object from which to either retrieve options or in which to set 
#'an option.
#'
#'@details
#'\code{order_poly}, \code{season}, \code{kernel_fun}, \code{bwidth}, and
#'\code{boundary_method} can be used to retrieve the order of polynomial,
#'the seasonal frequency, the kernel function setting, the bandwidth and
#'the boundary method from an object with smoothing options. The corresponding
#'generics beginning with \code{<-} are useful to set such options instead.
#'
#'The generics themselves are without a direct purpose. 
#'
#'@export
#'
#'@details
#'Generics that can be extended by methods to set or obtain settings (e.g.
#'smoothing options) in certain objects.
#'
#'@return
#'These generics do not return anything and are just the basis for more 
#'sophisticated methods.
#'
methods::setGeneric("order_poly", function(object) standardGeneric("order_poly"))
#'@rdname order_poly
#'
#'@param value the value to set the corresponding option to.
#'
#'@export
methods::setGeneric("order_poly<-", function(object, value) standardGeneric("order_poly<-"))

#'@rdname order_poly
#'@export
#'
#'@param ... without use; implemented for possible future compatibility only.
#'
methods::setGeneric("season", function(object, ...) standardGeneric("season"))
#'@rdname order_poly
#'@export
methods::setGeneric("season<-", function(object, value) standardGeneric("season<-"))

#'@rdname order_poly
#'@export
methods::setGeneric("kernel_fun", function(object) standardGeneric("kernel_fun"))
#'@rdname order_poly
#'@export
methods::setGeneric("kernel_fun<-", function(object, value) standardGeneric("kernel_fun<-"))

#'@rdname order_poly
#'@export
methods::setGeneric("bwidth", function(object) standardGeneric("bwidth"))
#'@rdname order_poly
#'@export
methods::setGeneric("bwidth<-", function(object, value) standardGeneric("bwidth<-"))

#'@rdname order_poly
#'@export
methods::setGeneric("boundary_method", function(object) standardGeneric("boundary_method"))
#'@rdname order_poly
#'@export
methods::setGeneric("boundary_method<-", function(object, value) standardGeneric("boundary_method<-"))

#------------------------------------------#

#'Obtain Estimated Components of a Time Series
#'
#'Obtain estimated components, such as the estimated trend, the seasonally 
#'adjusted series, and so on from an estimation object.
#'
#'@param object the estimation object.
#'@param ... currently without use; included for future compatibility.
#'
#'@export
#'
#'@details
#'Generics that can be extended by methods to obtain (estimated) time series
#'components from certain estimation objects.
#'
#'@return
#'These generics do not return anything and are just the basis for more 
#'sophisticated methods.
#'
methods::setGeneric("trend", function(object, ...) standardGeneric("trend"))

#'@rdname trend
#'
#'
#'
#'@export
methods::setGeneric("deseasonalize", function(object, ...) standardGeneric("deseasonalize"))

#'@rdname trend
#'@export
methods::setGeneric("detrend", function(object, ...) standardGeneric("detrend"))

#------------------------------------------#

#'Automatic Creation of Animations
#'
#'A generic that is the basis for methods that allow the user to create 
#'specific animations on-the-fly.
#'
#'@param object the input object.
#'@param ... currently of no use; included for future compatibility.
#'
#'@export
#'
#'@details
#'A generic that can be extended by methods to automatically create animations 
#'based on certain objects.
#'
#'@return
#'This generic does not return anything and is just the basis for more 
#'sophisticated methods.
#'
methods::setGeneric("animate", function(object, ...) standardGeneric("animate"))

#------------------------------------------#

#'Automatic Creation of Animations
#'
#'A generic that is the basis for methods that allow the user to exponentiate 
#'certain results quickly.
#'
#'@param object the input object.
#'@param ... currently of no use; included for future compatibility.
#'
#'@export
#'
#'@details
#'A generic that can be extended by methods to exponentiate the numeric 
#'information in certain objects.
#'
#'@return
#'This generic does not return anything and is just the basis for more 
#'sophisticated methods.
#'
methods::setGeneric("expo", function(object, ...) standardGeneric("expo"))