\name{forecastGARCH}
\alias{forecastGARCH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecast GARCH Model
}
\description{
The function get two object from 'Arima' class and 'garch' class, and then calculate to return forecasting answer of mean and variance of next day.
}
\usage{
forecastGARCH(fitARMA, fitGARCH, r = 3, trace = FALSE, newxreg = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fitARMA}{
  A object from 'Arima' class.
}
  \item{fitGARCH}{
  A object from 'garch' class.
}
  \item{r}{
 Rounds the answer to the specified number of decimal places (default 3). (See \code{\link{round2str}} for details of r paramicter.)
}
  \item{trace}{
  Logical. Trace optimizer output?
}
  \item{newxreg}{
  A covariates value of next day for ARMAX-GARCH mdels.
}
}

\value{
\item{ARCH}{GARCH coefficients.}
\item{ARMA}{ARMA coefficients.}
\item{forecast}{
Forecasting answer:

Point: forecasting time.

res: forecasting residual.

res^2: res square.

SSL.forecast: forecating mean value.

VAR.forecast: forecasting variance value.
}
}

\author{
Mai Thi Hong Diem <maidiemks@gmail.com>

Hong Viet Minh <hongvietminh@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#Load data
library(TTR)
data(ttrc)

#Calculate SSL series
t<-ts(ttrc[,"Close"],start=1,frequency=5)
ln.t<-log(t)
r<-diff(ln.t)

#Find a ARIMA model
fit1<-arima(r,order=c(4,0,0))

#Find a GARCH model
res1<-resid(fit1)
library(tseries)
fit2<-garch(res1,order=c(2,1),trace=0)

#Forecasting
forecastGARCH(fit1,fit2,r=6,trace=TRUE)
forecastGARCH(fit1,fit2,r=6)
}


