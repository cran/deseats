#' Quarterly Personal Consumption Expenditures in the USA
#'
#' A \code{ts} object that contains the quarterly observed personal
#' consumption expenditures in the USA in trillions of dollars from the
#' first quarter of 1947 to the last quarter of 2019. The object contains
#' 292 observations.
#' 
#'@source The data was obtained from the databank of the Federal Reserve Bank of 
#'St. Louis (accessed: 2022-07-15) and then transformed into a time series
#'object using R.
#'
#'\url{https://fred.stlouisfed.org/series/NA000349Q}
"EXPENDITURES"

#' Monthly Deaths in Germany
#'
#' A \code{ts} object that contains the monthly observed deaths in Germany
#' (in thousands of cases) from January 1990 to April 2022. The object 
#' contains 388 observations.
#' 
#'@source The data was obtained from the databank "Genesis" of the National 
#'Statistical Office of Germany (accessed: 2022-07-21) and then transformed into 
#'a time series object using R.
#'
#'\url{https://www-genesis.destatis.de/genesis/online?operation=abruftabelleBearbeiten&levelindex=2&levelid=1658407055248&auswahloperation=abruftabelleAuspraegungAuswaehlen&auswahlverzeichnis=ordnungsstruktur&auswahlziel=werteabruf&code=12613-0005&auswahltext=&werteabruf=Werteabruf#abreadcrumb}
"DEATHS"

#' Monthly Live Births in Germany
#'
#' A \code{ts} object that contains the monthly observed live births in Germany
#' (in thousands of cases) from January 1995 to March 2023. The object 
#' contains 339 observations.
#' 
#'@source The data was obtained from the databank "Genesis" of the National 
#'Statistical Office of Germany (accessed: 2023-06-15) and then transformed into 
#'a time series object using R.
#'
#'\url{https://www-genesis.destatis.de/genesis/online?operation=previous&levelindex=4&levelid=1686843345946&levelid=1686843308009&step=3#abreadcrumb}
"LIVEBIRTHS"

#' Quarterly Savings of Private Households in Germany
#'
#' A \code{ts} object that contains the quarterly observed savings of
#' private households in Germany
#' (in billions of EUR) from January 1991 to December 2019. The object 
#' contains 116 observations.
#' 
#'@source The data was obtained from the databank "Genesis" of the National 
#'Statistical Office of Germany (accessed: 2023-06-15) and then transformed into 
#'a time series object using R.
#'
#'\url{https://www-genesis.destatis.de/genesis/online?operation=abruftabelleBearbeiten&levelindex=1&levelid=1686857137066&auswahloperation=abruftabelleAuspraegungAuswaehlen&auswahlverzeichnis=ordnungsstruktur&auswahlziel=werteabruf&code=81000-0010&auswahltext=&werteabruf=Werteabruf#abreadcrumb}
"SAVINGS"

#' Monthly Civilian Labor Force Level in the USA
#'
#' A \code{ts} object that contains the monthly observed civilian labor force
#' level in the USA (in millions of persons) from January 1948 to December 2019. 
#' The object contains 864 observations.
#' 
#'@source The data was obtained from the databank of the Federal Reserve Bank of 
#'St. Louis (accessed: 2022-09-01) and then transformed into a time series
#'object using R.
#'
#'\url{https://fred.stlouisfed.org/series/LNU01000000}
"CIVLABOR"

#' Monthly Number of US Persons Not in the Labor Force
#'
#' A \code{ts} object that contains the monthly observed number of persons in
#' the USA that do not belong to the labor force (in millions of persons)
#' from January 1990 to December 2019. 
#' The object contains 360 observations.
#' 
#'@source The data was obtained from the databank of the Federal Reserve Bank of 
#'St. Louis (accessed: 2023-06-15) and then transformed into a time series
#'object using R.
#'
#'\url{https://fred.stlouisfed.org/series/LNU05000000}
"NOLABORFORCE"

#' Monthly Hours of Sunshine in Germany
#'
#' A \code{ts} object that contains the monthly observed hours of sunshine
#' in Germany from January 1951 to August 2023.
#' The object contains 872 observations.
#' 
#'@source The data was obtained from the databank of the 
#'German Weather Service (DWD) (accessed: 2023-09-25) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://www.dwd.de/DE/leistungen/zeitreihen/zeitreihen.html#buehneTop}
"SUNSHINE"

#' Monthly Average Temperature in Germany
#'
#' A \code{ts} object that contains the monthly observed average temperature
#' in Germany (in degrees Celsius) from January 1881 to August 2023.
#' The object contains 1712 observations.
#' 
#'@source The data was obtained from the databank of the 
#'German Weather Service (DWD) (accessed: 2023-09-25) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://www.dwd.de/DE/leistungen/zeitreihen/zeitreihen.html#buehneTop}
"TEMPERATURE"

#' Monthly Average Rainfall in Germany
#'
#' A \code{ts} object that contains the monthly observed average rainfall
#' in Germany (in mm) from January 1881 to August 2023.
#' The object contains 1712 observations.
#' 
#'@source The data was obtained from the databank of the 
#'German Weather Service (DWD) (accessed: 2023-09-25) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://www.dwd.de/DE/leistungen/zeitreihen/zeitreihen.html#buehneTop}
"RAINFALL"

#' Daily Confirmed New COVID-19 Cases in Germany 
#'
#' A \code{ts} object that contains the daily confirmed new COVID-19 cases
#' in Germany (in thousands of cases) from June 2021 to November 2021.
#' The object contains 183 observations. The time series object is created 
#' as a time series object with frequency 7, i.e. the time unit is in calendar 
#' weeks of the year 2021 and not in years.
#' 
#'@source The data was obtained from the COVID-19 Data Hub 
#'(accessed: 2023-09-25) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://covid19datahub.io/}
"COVID"

#' Quarterly US GDP
#'
#' A \code{ts} object that contains the quarterly US GDP
#' (in billions of USD) from the first quarter of 1947 to the last quarter in 
#' 2019. The object contains 292 observations.
#' 
#'@source The data was obtained from the databank of the Federal Reserve Bank 
#'of St. Louis
#'(accessed: 2023-09-25) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://fred.stlouisfed.org/series/NA000334Q}
"GDP"

#' Quarterly Real Final Consumption Expenditure for Australia
#'
#' A \code{ts} object that contains the quarterly real final consumption 
#' expenditure (in thousands of domestic currency) from the third quarter of 1959 
#' to the last quarter in  2019. The object contains 242 observations.
#' 
#'@source The data was obtained from the databank of the Federal Reserve Bank 
#'of St. Louis
#'(accessed: 2023-09-26) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://fred.stlouisfed.org/series/NCRNSAXDCAUQ}
"CONSUMPTION"

#' Monthly New One Family Houses Sold in the USA
#'
#' A \code{ts} object that contains the monthly new one family houses sold in 
#' the USA (in thousands of units) from January 1985 
#' to December 2005. The object contains 252 observations.
#' 
#'@source The data was obtained from the databank of the Federal Reserve Bank 
#'of St. Louis
#'(accessed: 2023-09-30) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://fred.stlouisfed.org/series/HSN1FNSA}
"HOUSES"

#' Monthly Total Production and Distribution of Electricity, Gas, Steam, and Air Conditioning for Germany
#'
#' A \code{ts} object that contains the monthly production and distribution 
#' of electricity gas steam, and air conditioning for Germany (Index 2015 = 100) 
#' from January 1991 
#' to June 2023. The object contains 390 observations.
#' 
#'@source The data was obtained from the databank of the Federal Reserve Bank 
#'of St. Louis
#'(accessed: 2023-10-11) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://fred.stlouisfed.org/series/DEUPREND401IXOBM}
"ENERGY"

#' Monthly Total Volume of Retail Trade in Germany 
#'
#' A \code{ts} object that contains the monthly total volume of retail trade 
#' in Germany (Index 2015 = 100)
#' from January 1991 
#' to December 2019. The object contains 348 observations.
#' 
#'@source The data was obtained from the databank of the Federal Reserve Bank 
#'of St. Louis
#'(accessed: 2023-10-11) and 
#'then transformed into a time series
#'object using R. 
#'
#'\url{https://fred.stlouisfed.org/series/DEUSLRTTO01IXOBM}
"RETAIL"