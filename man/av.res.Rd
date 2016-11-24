   \name{av.res}
\alias{av.res}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The criterion to evaluate forecasting model
}
\description{
Calculating to return answer which are 7 criterion to evaluate forecasting models, which are ME (Mean error), MAE (Mean absolute error), MPE (Mean percentage error), MAPE (Mean absolute percentage error), MSE (Mean squared error), RMSE (Root of mean square error), and U (Theil's U statistic).
}
\usage{
av.res(Y = NULL, F = NULL, E = NULL, r = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
  A data frame of univariate time series.
}

  \item{F}{
   A data frame of interpolat time series.
}
  \item{E}{
  A data frame of residual time series.
}
  \item{r}{
Display results returned to the specified number of decimal places (default 3). (See \code{\link{round2str}} for details of r paramicter.)
}
}
\details{
The Yt is 'observation series'. The Ft is 'Forecasting series'. The et is 'residual series'. The n is size of sample. The accuracies are calculated by theory:

ME = sum(et)/n 

MAE = sum(|et|)/n

MPE = sum((et/Yt)*100)/n

MAPE = sum((|et|/Yt)*100)/n

MSE = sum(et*et)/n

RMSE = sqrt(sum(et*et)/n)

U = RMSE of the forecast/RMSE of the naive forecast.

}
\value{
\item{ME}{Mean Error.}
\item{MAE}{Mean Absolute Error.}
\item{MPE}{Mean Percent Error (unit: \%).}
\item{MAPE}{Mean Absolute Percent Error (unit: \%).}
\item{MSE}{Mean Square Error.}
\item{RMSE}{Root of Mean Square Error.}
\item{U}{Number Theil U.}
\item{min.model}{The best model follow a criterion to evaluate forecasting model.}
}
\references{
\url{http://www.tailieu.tv/tai-lieu/bai-giang-quy-trinh-du-bao-
khao-sat-du-lieu-va-lua-chon-mo-hinh-22414/}
}
\author{
Nguyen Thi Diem My <myntdhg@gmail.com>

Hong Viet Minh <hongvietminh@gmail.com>
}
\note{
This function just receive data frame. You must translate your series to data frames before send to function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#----------The moving average models-------------------------
library(TTR)
data(enrollment)
sma5<-ts(SMA(enrollment,5),start=1971)
dsma5.5<-ts(SMA(SMA(enrollment)),start=1971)
cma5<-as.ts(CMA(enrollment,5))
ses.002<-as.ts(SES(enrollment,0.002))

#Translate series to data frame
actual<-data.frame(enrollment)
forecasted<-data.frame(sma5,dsma5.5,cma5,ses.002)

#Comparing forecasting models
av.res(Y=actual,F=forecasted,r=5)


#----------The liner and arima models-------------------------
#Loading data
#data(enrollment)

#Liner model
#t<-1:length(enrollment)
#lm.model<-lm(enrollment~t)

#Arima modle
#arima.model<-arima(enrollment,order=c(1,1,0))

#Translate residual series to data frame
#actual<-data.frame(enrollment)
#residual.models<-data.frame(lm=lm.model$resid,arima=arima.model$resid)

#Comparing forecasting models
#av.res(Y=actual,E=residual.models)


#----------The fuzzy time series models-------------------------
#Following example(fuzzy.ts1)
#Comparing fuzzy time series models for lh time series
#from example of fuzzy.ts1 function
#av.res(Y=data.frame(lh),F=data.frame(chen10,singh10,heuristic10,chenhsu6))

#Following example(Gfuzzy.ts1)
#Comparing fuzzy time series models for lh time series
#from example of Gfuzzy.ts1 function
#av.res(Y=data.frame(lh),F=KQ3)

#Following example(Gfuzzy.ts2)
#Comparing fuzzy time series models for enrollment time series
#from example of Gfuzzy.ts2 function
#av.res(Y=data.frame(enrollment),F=g.fuzzy1$interpolate)
}
