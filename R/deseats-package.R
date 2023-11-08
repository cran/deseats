#'Deseasonalize Time Series
#' 
#'A library of decomposition methods for equidistant time series with 
#'trend and seasonality.
#'
#'@details
#'\code{deseats} is an R package for the decomposition of equidistant time 
#'series with trend and seasonality. First and foremost, an own algorithm 
#'for bandwidth selection in locally weighted regression of such time series 
#'(with short-range dependence) is implemented that is based on both the 
#'algorithms by Feng (2013) and Feng et al. (2020). For comparison,
#'a simplified version of the BV4.1 (Berlin Procedure 4.1, Speth, 2004), is 
#'implemented as well that allows to implement the BV4.1 base model (trend 
#'component + seasonality component + irregular component) without any 
#'of the additional BV4.1 components (such as the calendar component). 
#'Permission to include the BV4.1 base model procedure was kindly provided by 
#'the Federal Statistical Office of Germany.
#'
#'@section Main Functions:
#'The main functions of the package are:
#'\describe{
#'\item{\code{\link{deseats}}:}{locally weighted regression with automatically 
#'selected bandwidth for decomposition,}
#'\item{\code{\link{BV4.1}}:}{BV4.1 base model for decomposition,}
#'\item{\code{\link{lm_decomp}}:}{ordinary least squares for decomposition,}
#'\item{\code{\link{llin_decomp}}:}{local linear regression for decomposition,}
#'\item{\code{\link{ma_decomp}}:}{moving averages for decomposition,}
#'\item{\code{\link{hamilton_filter}}:}{the time series filter by Hamilton.}
#'}
#'
#'@section Datasets:
#'The package includes a few datasets. Follow the corresponding links to the 
#'documentation of the datasets to find additional information including the 
#'sources.
#'\describe{
#'\item{\code{\link{CIVLABOR}}:}{civilian labor force level in the USA.}
#'\item{\code{\link{CONSUMPTION}}:}{real final consumption expenditure for Australia.}
#'\item{\code{\link{COVID}}:}{new COVID-19 cases in Germany.}
#'\item{\code{\link{DEATHS}}:}{recorded number of deaths in Germany.}
#'\item{\code{\link{ENERGY}}:}{production and distribution of electricity, gas, steam and air conditioning in Germany.}
#'\item{\code{\link{EXPENDITURES}}:}{consumption expenditures in the USA.}
#'\item{\code{\link{GDP}}:}{GDP of the USA.}
#'\item{\code{\link{HOUSES}}:}{new one family houses sold in the USA.}
#'\item{\code{\link{LIVEBIRTHS}}:}{recorded number of livebirths in Germany.}
#'\item{\code{\link{NOLABORFORCE}}:}{number of persons in the USA not belonging 
#'to the labor force.}
#'\item{\code{\link{RAINFALL}}:}{average amount of rain in Germany.}
#'\item{\code{\link{RETAIL}}:}{Retail sale volume in Germany.}
#'\item{\code{\link{SAVINGS}}:}{savings of private households in Germany.}
#'\item{\code{\link{SUNSHINE}}:}{average hours of sunshine in Germany.}
#'\item{\code{\link{TEMPERATURE}}:}{average temperature in Germany.}
#'}
#'
#'@section License:
#'The package is distributed under the General Public License v3
#'([GPL-3](https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3))).
#'
#'@references
#'\itemize{
#'\item{Feng, Y. (2013). An iterative plug-in algorithm for decomposing seasonal 
#'time series using the Berlin Method. Journal of Applied Statistics, 40(2): 
#'266-281. DOI: 10.1080/02664763.2012.740626.}
#'\item{Feng, Y., Gries. T, and Fritz, M. (2020). Data-driven local polynomial 
#'for the trend and its derivatives in economic time series. Journal of 
#'Nonparametric Statistics, 32(2): 510-533. DOI: 10.1080/10485252.2020.1759598.}
#'\item{Speth, H.-T. (2004). Komponentenzerlegung und Saisonbereinigung ökonomischer 
#'Zeitreihen mit dem Verfahren BV4.1. Methodenberichte 3. Statistisches 
#'Bundesamt. URL: https://www.destatis.de/DE/Methoden/Saisonbereinigung/BV41-methodenbericht-Heft3_2004.pdf?__blob=publicationFile.}
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
#'@importFrom stats as.formula frequency lm na.omit qnorm quantile
#'@importFrom stats rgeom time ts
#'@importFrom utils head tail
#'
#'@useDynLib deseats
#'@docType package
#'@name deseats-package
#'@aliases deseats-package
#'@useDynLib deseats
#'@importFrom Rcpp sourceCpp
NULL
