\name{fuzzy.ts2}
\alias{fuzzy.ts2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Abbasov Mamedova model and FVD model
}
\description{
Predicts time series by fuzziness method according to Abbasov-Manedova model or FVD model.
}
\usage{
fuzzy.ts2(ts, n = 7, w = 7, D1 = 0, D2 = 0, C = NULL, forecast = 5, 
trace = FALSE, plot = FALSE, type = "Abbasov-Mamedova")
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
  \item{type}{
Model is choosed to predicts time series by fuzziness, type = "Abbasov-Manedova" or type = "NFTS". }
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
\item{accuracy}{Information about the criterion to evaluate forecasting model.}
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

\examples{
data(enrollment)
layout(1:2)
fuzzy.ts2(enrollment,n=7,w=7,C=0.0001,forecast=11,trace=TRUE,plot=TRUE,type="NFTS")
fuzzy.ts2(enrollment,n=5,w=5,C=0.01,forecast=5,plot=TRUE,type="Abbasov-Mamedova")
}
