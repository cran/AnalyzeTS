\name{PrintAIC}
\alias{PrintAIC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print AIC Values
}
\description{
Calculates and outputs AIC value for some models including ARMA, ARIMA, SARIMA, ARMAX, ARIMAX, SARIMAX, ARCH and GARCH. To classify and extracts the best model by AIC values.
}
\usage{
PrintAIC(DataTimeSeries, order = c(p, d = NULL, q = NULL), 
seas = list(order = c(P = NULL, D = NULL, Q = NULL), frequency = NULL), 
type = NULL,xreg=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataTimeSeries}{
Univariate time series.
}
  \item{order}{
If type="ARMA" (or ARMAX) then 'order' is a vector contain two positive integer which are order of ARMA model (or ARMAX model).
  
If type="ARIMA" (or ARIMAX) then 'order' is a vector contain three positive integer which are order of ARIMA model (or ARIMAX model).
  
If type="SARIMA" (or SARIMAX) then 'order' is a vector contain three positive integer which are order of ARIMA model (or ARIMAX model) of the non-seasonal part of the SARIMA model (or SARIMAX model).
  
If type="ARCH" then 'order' is a positive integer which are order of ARCH model.
  
If type="GARCH" then 'order' is a vector contain two positive integer which are order of GARCH model.
}
  \item{seas}{
  A list contain two part which are 'order' and 'frequency'.
  
  'order' part is a vector contain three positive integer which are order of ARIMA model of the seasonal part of the SARIMA model (or SARIMAX model).
  
  'frequency' part is frequency of observation series.
}
  \item{type}{
 A character string specifying the type of models, must be one of "ARMA", "ARIMA", "SARIMA", "ARCH", "GARCH", "ARMAX", "ARIMAX" or "SARIMAX".
 }

  \item{xreg}{
Optionally, a vector or matrix of external regressors, which must have the same number of rows as x.
}
}
\details{
The first, function identify type of models according to 'type' parameter. The next, test other parameters. All of parameters are reasonable, function will combine orders of models. And then, to calculate AIC value of each model. The last step, performing ranked
and extracting the best model.}
\value{
\item{mohinh}{Calculation results.}
\item{best}{The best model following AIC value.} 
}
\references{
Nguyen Thi Diem My va Hong Viet Minh, Phan tich chuoi thoi gian voi su ho tro cua package AnalyzeTS.

Hong Viet Minh, Luan van tot nghiep dai hoc: Phan tich so lieu thong ke voi ngon ngu R.
}
\author{
Tran Thi Ngoc Han <tranthingochan01011994@gmail.com>

Mai Thi Hong Diem <maidiemks@gmail.com>

Hong Viet Minh <hongvietminh@gmail.com>
}
\note{
You must be careful with 'order' and 'type' parameter of models.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#----A time series--------------------------------
sl<-ts(c(180,165,110,126,125,134,163,153,122,171,171,155
,175,248,99,187,173,147,184,108,171,195,192,163))

#----The ARMA models--------------------------------
PrintAIC(sl,order=c(1,4),type="ARMA")

#----The ARIMA models--------------------------------
PrintAIC(sl,order=c(1,1,4),type="ARIMA")

#----The SARIMA models--------------------------------
PrintAIC(sl,order=c(1,1,4),seas=list(order=c(0,0,1),
frequency=4),type="SARIMA")

#----The ARCH models--------------------------------
PrintAIC(sl,order=c(4),type="ARCH")

#----The GARCH models--------------------------------
PrintAIC(sl,order=c(1,4),type="GARCH")

#----The ARIMAX models--------------------------------
#A factor
date<-as.factor(c("Tue","Wed","Thu","Fri","Mon","Tue","Wed",
"Thu","Fri","Mon","Tue","Wed","Thu","Fri","Mon","Tue","Wed",
"Thu","Fri","Mon","Tue","Wed","Thu","Fri","Mon","Tue","Wed",
"Thu","Fri","Mon","Tue","Wed","Thu"))
#Observation series.
coffee<-c(5,6,8,4,3,7,6,0,3,2,3,4,9,1,3,8,7,8,2,3,8,6,4,
4,6,7,6,5,2,3,8,4,4)
coffee<-ts(coffee,start=c(1,2),frequency=5)
Mon<-1*(date=="Mon")
event<-data.frame(Mon)
PrintAIC(coffee,order=c(2,2),xreg=event,type="ARMAX")
}


