\name{fuzzy.ts2}
\alias{fuzzy.ts2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Abbasov Mamedova model
}
\description{
Predicts time series by fuzziness method according to Abbasov-Manedova model.
}
\usage{
fuzzy.ts2(ts, n = 5, w = NULL, D1 = 0, D2 = 0, C = NULL, trace = FALSE,
forecast = NULL, plot = FALSE, fty = c("ts", "f"))
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
The 'w' parameter.}
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
fty="f", N(length(ts)+1) = N(length(ts)) + V(length(ts)+1).

fty="ts", N(length(ts)+1) = ts(length(ts)) + V(length(ts)+1).
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
\item{accuracy}{Information about 7 accuracy of forecasting model.}
}

\references{
Abbasov, A.M. and Mamedova, M.H., 2003. Application of fuzzy time series to population forecasting, Proceedings of 8th Symposion on Information Technology in Urban and Spatial Planning, Vienna University of Technology, February 25-March1, 545-552.
}
\author{
Doan Hai Nghi <Hainghi1426262609121094@gmail.com> 

Tran Thi Ngoc Han <tranthingochan01011994@gmail.com>

Hong Viet Minh <hongvietminh@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fuzzy.ts3}}
}
\examples{
data(population)
layout(1:2)
fuzzy.ts2(population,n=7,w=7,C=0.0001,forecast=11,fty="ts",trace=TRUE,plot=TRUE)
fuzzy.ts2(population,n=5,w=5,C=0.01,forecast=5,fty="f",plot=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ fuzzy.ts2 }% use one of  RShowDoc("KEYWORDS")