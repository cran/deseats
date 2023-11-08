#'Shiny App for Decomposing Seasonal Time Series
#'
#'A shiny app is started that allows its user to run some of the decomposition
#'approaches of this package interactively.
#'
#'@details
#'Time series data can be uploaded to the app and then decomposed using 
#'some of the approaches implemented in this package. The decomposition 
#'is immediately visualized within the app, so that the user can assess the
#'suitability of the decomposition graphically. The decomposed data can then be 
#'saved in CSV format.
#'
#'@export
#'
#'@return
#'This function returns \code{NULL}.
#'
#'@author
#'\itemize{
#'\item Dominik Schulz (Research Assistant) (Department of Economics, Paderborn
#'University), \cr
#'Author and Package Creator
#'}
#'
runDecomposition <- function() {
  appDir <- system.file("shiny-apps", "lwr", package = "deseats")
  if (appDir == "") {
    stop('Could not find example directory. Try re-installing "deseats".', call. = FALSE)
  }

  .f1 <- zoo::zoo
  .f2 <- tools::vignetteInfo
  shiny::runApp(appDir, display.mode = "normal")
}