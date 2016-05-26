\name{AnalyzeTS-package}
\alias{AnalyzeTS-package}
\alias{AnalyzeTS}
\docType{package}
\title{
\packageTitle{AnalyzeTS}
}
\description{
\packageDescription{AnalyzeTS}
}
\details{
This package contains 15 functions which are used to analyze time series.

av.res function: The main function measures the accuracy of forecasted models.

Descriptives function: The main function descriptive statistics of a Time Series, a continous variable or continous variables in a data frame.

Dgroup function: The main function descriptive statistics in group for a continous variable.

forecastGARCH function: The main function extracts the forecast of next day of ARCH or GARCH models.

Frequencies function: The main function descriptive statistics for a desultory variable or desultory variables in a data frame.

fuzzy.ts1 function: The main function calculates fuzziness of time series with Chen, Singh, Heuristic and Chen-Hsu models.

fuzzy.ts2 function: The main function predicts time series by fuzziness method according to Abbasov-Manedova model.

fuzzy.ts3 function: The main function predicts time series by fuzziness method according to improve Abbasov-Manedova model.

grid.on function: The main function using to draw grid for line graph ( graph of time series) that is drawn by plot(), ts.plot or plot.ts() function.

PrintAIC function: The main function calculates and outputs AIC value for some models including ARMA, ARIMA, SARIMA, ARMAX, ARIMAX, SARIMAX, ARCH and GARCH.

SES function: The main function calculate simple exponential smoothing for a time series.

CMA function: The main function uses calculating  center moving average for a time series 

Abbasov.Cs2 function: The main function use to find a C valua that Abbasov-Mamedova model (model corresponding for fuzzy.ts2) will achieve the least MAE value when using the C value.

Abbasov.Cs3 function: The main function use to find a C valua that improve Abbasov-Mamedova model (model corresponding for fuzzy.ts3) will achieve the least MAE value when using the C value.

ChenHsu.bin function: The main function use to calculating bin point values, which devece divide fuzzy sets in Chen-Hsu model.
}
\author{
\packageAuthor{AnalyzeTS}

Maintainer: \packageMaintainer{AnalyzeTS}
}



\keyword{ package }

\examples{
library(AnalyzeTS)
}
