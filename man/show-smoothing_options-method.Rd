% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-smoothing_options.R
\name{show,smoothing_options-method}
\alias{show,smoothing_options-method}
\title{Show Method for Smoothing Options}
\usage{
\S4method{show}{smoothing_options}(object)
}
\arguments{
\item{object}{an object of class \code{"smoothing_options"}.}
}
\value{
This method returns \code{NULL}.
}
\description{
Show smoothing settings in an object of class \code{"smoothing_options"}.
}
\details{
This is a special printing method for objects object of class 
\code{"smoothing_options"}. Calling this method shows a nice looking 
overview of the saved smoothing settings.
}
\examples{
opts <- set_options()
opts

}
