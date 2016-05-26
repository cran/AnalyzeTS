\name{fuzzy.ts3}
\alias{fuzzy.ts3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fuzzy Times Series 3
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
  Let trace=TRUE to print all of calculation results out to creen. Now, this calculationing are updating.
  
  Let trace=FALSE (default) to only print forecasting series out to creen.
}
  \item{forecast}{
Number of points to forecast in future.
}
  \item{plot}{
 Let plot=TRUE to paint graph of obsevation series and fuzzy series. Now, this calculationing are updating.
  
 Let plot=FLASE (default) to do not paint graph.
}

\item{fty}{
fty="f", forecasting timeseries of furetrue are counted from forecast timeseries of past.

fty="ts", forecasting timeseries of furetrue are counted from observation timeseries of past.
}
}

\value{
\item{timeseries}{Forecasting timeseries.}
\item{accuracy}{Information about 8 accuracy of forecasting model.}
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
fuzzy.ts3(lh,n=5,w=5,C=0.01,forecast=5,fty="f")
fuzzy.ts3(lh,n=5,w=5,C=0.01,forecast=5,fty="ts")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fuzzy.ts3 }% use one of  RShowDoc("KEYWORDS")
