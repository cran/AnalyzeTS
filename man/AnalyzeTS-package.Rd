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
This package contains 17 functions which are used to analyze time series and 3 data time series.

av.res function: The main function measures the accuracy of forecasted models.

Descriptives function: The main function descriptive statistics of a Time Series, a continous variable or continous variables in a data frame.

Dgroup function: The main function descriptive statistics in group for a continous variable.

forecastGARCH function: The main function extracts the forecast of next day of ARCH or GARCH models.

Frequencies function: The main function descriptive statistics for a desultory variable or desultory variables in a data frame.

fuzzy.ts1 function: The main function calculates fuzziness of time series with Chen, Singh, Heuristic and Chen-Hsu models.

fuzzy.ts2 function: The main function predicts time series by fuzziness method according to Abbasov-Manedova model.

fuzzy.ts3 function: The main function predicts time series by fuzziness method according to Improve Abbasov-Manedova version 1 model.

grid.on function: The main function using to draw grid for line graph ( graph of time series) that is drawn by plot(), ts.plot or plot.ts() function.

PrintAIC function: The main function calculates and outputs AIC value for some models including ARMA, ARIMA, SARIMA, ARMAX, ARIMAX, SARIMAX, ARCH and GARCH.

SES function: The main function calculate simple exponential smoothing for a time series.

CMA function: The main function uses calculating  center moving average for a time series 

Abbasov.Cs2 function: The main function use to compare and sort Abbasov-Mamedova models according ME, MAE, MPE, MAPE, MSE RMSE for C values in Cs.

Abbasov.Cs3 function: The main function use to comparing and sort Improve Abbasov-Mamedova version 1 models according ME, MAE, MPE, MAPE, MSE RMSE for C values in Cs.

ChenHsu.bin function: The main function use to calculating bin point values, which devece divide fuzzy sets in Chen-Hsu model.

FindC2 function: The main function to find a C value, which is the best for Abbasov Mamedova model.

FindC3 function: The main function to find a C value, which is the best for Improve Abbasov Mamedova version 1 model.

population data time series: A time series of population from 1980 to 2001.

enrollment data time series: A time series of enrollment from 1971 to 1992.

sanility data time series: A time series of sanility from 2000 to 2015.
}
\author{
\packageAuthor{AnalyzeTS}

Maintainer: \packageMaintainer{AnalyzeTS}
}


\references{
Chen, S.M., 1996. Forecasting  enrollments  based  on  fuzzy  time  series. Fuzzy Sets  and Systems. 81: 311-319. 

Chen, S.M. and Hsu, C.C., 2004. A New method to forecast enrollments using fuzzy time series. International Journal of Applied Science and Engineering, 12: 234-244.

Huarng, H., 2001. Huarng models of fuzzy time series for forecasting. Fuzzy Sets and Systems. 123: 369-386.

Singh, S.R., 2008. A computational method of forecasting based on fuzzy time series.   Mathematics and Computers in Simulation. 79: 539-554

Abbasov, A.M. and Mamedova, M.H., 2003. Application of fuzzy time series to population forecasting, Proceedings of 8th Symposion on Information Technology in Urban and Spatial Planning, Vienna University of Technology, February 25-March1, 545-552.

Vo Van Tai,  Duong Ton Dam, Pham Minh Truc, Dang Kien Cuong, 2016. Forecasting crest of sanility at three main stations of Ca Mau province by fuzzy time series model.
}
\examples{
library(AnalyzeTS)
#Sing model
fuzzy.ts1(lh,n=5,type="Singh",plot=TRUE)

#Abbasov Mamedova model
data(population)
fuzzy.ts2(population,n=5,w=5,C=0.01,forecast=5,fty="f",plot=TRUE)

#Improve Abbasov Mamedova version 1 model
data(sanility)
fuzzy.ts3(sanility,n=5,w=5,C=0.01,forecast=5,fty="ts")
}

\keyword{ package }
