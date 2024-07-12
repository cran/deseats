#'Time Series Object Conversion from \code{"zoo"} to \code{"ts"}
#'
#'Allows for the conversion of time series objects of class \code{"zoo"}
#'to time series objects of class \code{"ts"}. This is only suitable, if
#'the time series is observed in regular time intervals (monthly, quarterly,
#'yearly, etc.). The correct observation time points are then kept.
#'
#'@param xt a time series object of class \code{"zoo"} with equidistant 
#'observation time points (monthly, quarterly, yearly, etc.).
#'
#'@details
#'An equidistant time series object of class \code{"zoo"} is transformed to
#'class \code{"ts"}. This is particularly useful, since most functions of this
#'package work with objects of class \code{"ts"} only.
#'
#'@return
#'An object of class \code{"ts"} is returned.
#'
#'@export
#'
#'@importFrom zoo as.yearmon zoo coredata
#'
#'@importFrom stats as.ts
#'
#'@examples
#'# Create example zoo-object
#'tp <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-10-01"), by = "month")
#'xt <- zoo::zoo(1:10, order.by = tp)
#'xt
#'
#'# Transform into ts-object
#'yt <- zoo_to_ts(xt)
#'yt
#'

zoo_to_ts <- function(xt) {
  
  stopifnot('xt must be a time series object of class "zoo"' = (inherits(xt, "zoo")))
  
  d <- zoo::as.yearmon(time(xt))
  xt <- zoo::zoo(zoo::coredata(xt), order.by = d)
  stats::as.ts(xt)
}

#'Read in a Dataset Directly as an Object of Class \code{"ts"} or \code{"mts"}
#'
#'Allows the user to read in a data file directly as a \code{"ts"} or 
#'\code{"mts"} object, where a time point column in the data file is 
#'immediately used to set starting points and frequency of the time 
#'series automatically correctly. Works for equidistant observation time 
#'points, e.g. quarterly or monthly observations.
#'
#'@param file a data file name given as a string (including the file ending);
#'the file should have at least two columns: one time column and at least one or 
#'multiple columns for time series observations; alternatively, a data frame 
#'can be passed to this argument.
#'@param time_column a number that indicates which column in the dataset is 
#'the variable with the time points; by default, the first column is assumed to 
#'contain the information on time points.
#'@param sep the separation symbol between the dataset columns.
#'@param dec the decimal symbol in the dataset.
#'@param header \code{TRUE} or \code{FALSE}; does the dataset have a row 
#'with headers at the beginning?
#'@param time_format with the default \code{NULL}, standard date formats will 
#'be tried to transform the time points column into a date object; if the 
#'formatting of the time column is unusual, the formatting can be specified 
#'here as a string.
#'
#'@details
#'The data file is internally read into R as a \code{"zoo"} object and then 
#'transformed into a \code{"ts"} object using \code{\link{zoo_to_ts}}. This 
#'happens without the user noticing. The result is an immediate transformation 
#'of the input data into an object of class \code{"ts"} or \code{"mts"} for 
#'the user.
#'
#'@return
#'An object of class \code{"ts"} or \code{"mts"} is returned.
#'
#'@export
#'
#'@importFrom zoo zoo
#'
#'@importFrom stats as.ts
#'
#'@importFrom utils read.table
#'
#'@examples
#'\donttest{
#'### Create an example data file
#'a <- 1:12
#'b <- 21:32
#'tp <- seq(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month")
#'df <- data.frame(
#'  Time = tp,
#'  a = a,
#'  b = b
#')
#'
#'file <- file.path(tempdir(), "ExampleFile.csv")
#'
#'write.table(df, file = file, quote = FALSE, sep = ",",
#'  row.names = FALSE, col.names = TRUE)
#'  
#'### Use the function to read in the data
#'xt <- read_ts(file)
#'xt
#'}
#'

read_ts <- function(file, time_column = 1, sep = ",", dec = ".", header = TRUE, time_format = NULL) {
  
  stopifnot(
    "file must be a valid filename string (including the file ending) or a data frame" = ((is.character(file) && length(file) == 1) || inherits(file, "data.frame")),
    "time_column must be a numeric vector of length one" = (is.numeric(time_column) && length(time_column) == 1),
    "time_format must be either NULL or a character vector with data formats to try" = (is.null(time_format) || is.character(time_format))
  )
  
  if (is.character(file)) {
    df <- read.table(file = file, sep = sep, dec = dec, header = header)
  } else if (inherits(file, "data.frame")) {
    df <- file
  }
  
  if (is.null(time_format)) {
    tp <- as.Date(df[, time_column])
  } else if (is.character(time_format)) {
    tp <- as.Date(df[, time_column], tryFormats = time_format) 
  }

  df[, time_column] <- NULL
  mat <- as.matrix(df)
  xt <- zoo::zoo(mat, order.by = tp)
  zoo_to_ts(xt)
}
