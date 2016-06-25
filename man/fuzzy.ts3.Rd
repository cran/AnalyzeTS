\name{fuzzy.ts3}
\alias{fuzzy.ts3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Improve Abbasov Mamedova version 1 model
}
\description{
Predicts time series by fuzziness method according to improve Abbasov-Manedova model.
}
\usage{
fuzzy.ts3(ts, n = 7, w = 7, D1 = 0, D2 = 0, C = NULL, forecast = 5, 
fty = c("ts", "f"), trace = FALSE, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{ts}{
  Observation series.
}
  \item{n}{
Number of fuzzy set.
}
  \item{w}{
'w' parameter.
}
  \item{D1}{
A adequate value.
}
  \item{D2}{
A adequate value.
}
  \item{C}{
A optional constant.
}

  \item{trace}{
  Let trace=TRUE to print all of calculation results out to creen.
  
  Let trace=FALSE (default) to only print forecasting series out to creen.
}
  \item{forecast}{
Number of points to forecast in future.
}
  \item{plot}{
 Let plot=TRUE to paint graph of obsevation series and fuzzy series.
  
 Let plot=FLASE (default) to do not paint graph.
}

\item{fty}{
fty="f", N(length(ts)) = N(length(ts)-1) + V(length(ts)+1).

fty="ts", N(length(ts)) = ts(length(ts)) + V(length(ts)+1).
}
}

\value{
\item{type}{Name of fuzzy model.}
\item{table1}{Information about changing fuzzy sets.}
\item{table2}{Observation series and changing series.}
\item{table3}{The change fuzzy of observation series.}
\item{table4}{Interpolate values.}
\item{table5}{Forecasting values.}
\item{table6}{The change fuzzy of forecasting series.}
\item{timeseries}{Forecasting timeseries.}
\item{accuracy}{Information about 6 accuracy of forecasting model.}
}
\references{
Vo Van Tai,  Duong Ton Dam, Pham Minh Truc, Dang Kien Cuong, 2016. Forecasting crest of sanility at three main stations of Ca Mau province by fuzzy time series model.
}
\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fuzzy.ts2}}
}
\examples{
data(sanility)
fuzzy.ts3(sanility,n=7,w=4,C=0.01,forecast=5,fty="f",plot=TRUE,trace=1)
fuzzy.ts3(sanility,n=5,w=5,C=0.01,forecast=5,fty="ts")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fuzzy.ts3 }% use one of  RShowDoc("KEYWORDS")
