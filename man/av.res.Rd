   \name{av.res}
\alias{av.res}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The criterion to evaluate forecasting model
}
\description{
This function calculate to return answer which are 7 criterion to evaluate forecasting models. They are ME, MAE, MPE, MAPE, MSE, RMSE and U.
}
\usage{
av.res(Y = NULL, F = NULL, E = NULL, r = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
  Observation series.
}

  \item{F}{
  Forecasting series.
}
  \item{E}{
  Residual series.
}
  \item{r}{
   Rounds the answer to the specified number of decimal places (default 3).
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

U = sqrt(sum((Yt-Ft)*(Yt-Ft)))/sqrt((Yt-Y(t-1))*(Yt-Y(t-1)))
}
\value{
\item{ME}{Mean Error}
\item{MAE}{Mean Absolute Error}
\item{MPE}{Mean Percent Error (unit: \%)}
\item{MAPE}{Mean Absolute Percent Error (unit: \%)}
\item{MSE}{Mean Square Error}
\item{RMSE}{Root of Mean Square Error}
\item{U}{Number Theil U}
\item{min.model}{The best model follow a criterion to evaluate forecasting model}
}
\references{
\url{http://www.tailieu.tv/tai-lieu/bai-giang-quy-trinh-du-bao-khao-sat-du-lieu-va-lua-chon-mo-hinh-22414/}
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
#Calculate moving average
library(TTR)
data(ttrc)
sma.200<-as.ts(SMA(ttrc[,"Close"],200))
ema.200<-as.ts(EMA(ttrc[,"Close"],200))
dema.200<-as.ts(DEMA(ttrc[,"Close"],200))
wma.200<-as.ts(WMA(ttrc[,"Close"],200))
evwma.200<-as.ts(EVWMA(ttrc[,"Close"],ttrc[,"Volume"],200))
zlema.200<-as.ts(ZLEMA(ttrc[,"Close"],200))
vwap.200<-as.ts(VWAP(ttrc[,"Close"],ttrc[,"Volume"],200))

#Translate series to data frame
chuoigoc<-data.frame(ttrc[,"Close"])
dubao<-data.frame(sma.200,ema.200,dema.200,wma.200,evwma.200,zlema.200,vwap.200)

#Comparing forecasting models
av.res(Y=chuoigoc,F=dubao,r=5)
}

