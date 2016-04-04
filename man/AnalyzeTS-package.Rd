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
This package contains 13 functions which are used to analyze time series.

av.res function: The main function measures the accuracy of forecasted models.

base.ts function: The main function calculates some basic norms for time series.

Descriptives function: The main function descriptive statistics of a Time Series, a continous variable or continous variables in a data frame.

Dgroup function: The main function descriptive statistics in group for a continous variable.

forecastGARCH function: The main function extracts the forecast of next day of ARCH or GARCH models.

Frequencies function: The main function descriptive statistics for a desultory variable or desultory variables in a data frame.

fuzzy.ts1 function: The main function calculates fuzziness of time series with Chen, Singh, Heuristic and Chen-Hsu models.

fuzzy.ts2 function: The main function predicts time series by fuzziness method according to Abbasov-Manedova model.

grid.on function: The main function using to draw grid for line graph ( graph of time series) that is drawn by plot(), ts.plot or plot.ts() function.

PrintAIC function: The main function calculates and outputs AIC value for some models including ARMA, ARIMA, SARIMA, ARMAX, ARIMAX, SARIMAX, ARCH and GARCH.

SES function: The main function calculate simple exponential smoothing for a time series.

CMA function: The main function uses calculating  center moving average for a time series 

}
\author{
\packageAuthor{AnalyzeTS}

Maintainer: \packageMaintainer{AnalyzeTS}
}

\references{
Nguyen Thi Diem My va Hong Viet Minh: Phan tich chuoi thoi gian voi su ho tro cua package AnalyzeTS.

Hong Viet Minh, Luan van tot nghiep dai hoc: Phan tich so lieu thong ke voi ngon ngu R.

Hong Viet Minh, Tran Thi Ngoc Han, Nguyen Thi Diem My, Mai Thi Hong Diem, Doan Hai Nghi: Phan tich chuoi thoi gian voi ngon ngu R su dung package AnalyzeTS.
}

\keyword{ package }
\seealso{
\code{\link{av.res}}, \code{\link{base.ts}}, \code{\link{Descriptives}},
\code{\link{Dgroup}}, \code{\link{forecastGARCH}}, \code{\link{Frequencies}},
\code{\link{fuzzy.ts1}}, \code{\link{fuzzy.ts2}}, \code{\link{grid.on}},
\code{\link{PrintAIC}}, \code{\link{SES}}, \code{\link{CMA}}
}
\examples{
library(AnalyzeTS)
}
