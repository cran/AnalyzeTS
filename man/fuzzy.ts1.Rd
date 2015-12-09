  \name{fuzzy.ts1}
\alias{fuzzy.ts1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fuzzy Times Series 1
}
\description{
Calculates fuzziness of time series with Chen, Singh, Heuristic and Chen-Hsu.
}
\usage{
fuzzy.ts1(ts, n = 5, D1 = 0, D2 = 0, type = c("Chen", "Singh","Heuristic",
"Chen-Hsu"), bin = NULL, trace = FALSE, divide = NULL, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
  Observation series.
}
  \item{n}{
Number of fuzzy set.
}

  \item{D1}{
A adequate value.
}
  \item{D2}{
A adequate value.
}
  \item{type}{
Type of model.
}
  \item{bin}{
  Points to divide fuzzy stes for Chen-Hsu model. If bin=NULL (default) then function will divide fuzzy sets by 'divide' parameter.
}
  \item{trace}{
  Let trace=TRUE to print all of calculation results out to creen.
  
  Let trace=FALSE (default) to only print fuzzy series out to creen.
}
  \item{divide}{
  Method divide fuzzy set second.
  
  Let divide="distance" to divide old fuzzy sets into smaller fuzzy sets of equal about distance.
  
  Let divide="density" to divide old fuzzy sets into smaller fuzzy sets of equal about density.
}
  \item{plot}{
  Let plot=TRUE to paint graph of obsevation series and fuzzy series.
  
  Let plot=FLASE (default) to do not paint graph.
}
}

\value{
\item{type}{Name of fuzzy model.}
\item{table1}{Information about fuzzy sets.}
\item{table2}{
Information about fuzzy series of Chen, Sing, Heuristic and Chen-Hsu models (in bin!=NUL).

If type="Chen-Hsu" and bin=NULL then table2 supply information about fuzzy sets after they are second divided.
}
\item{table3}{Information about fuzzy series of Chen-Hsu model in bin = NULL}
\item{accuracy}{Information about 7 accuracy of forecasting model.}
}

\author{
Doan Hai Nghi <Hainghi1426262609121094@gmail.com> 

Tran Thi Ngoc Han <tranthingochan01011994@gmail.com>

Hong Viet Minh <hongvietminh@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
par(mfrow=c(2,2))
chen10<-fuzzy.ts1(lh,n=5,type="Chen",plot=TRUE)
singh10<-fuzzy.ts1(lh,n=5,type="Singh",plot=TRUE)
heuristic10<-fuzzy.ts1(lh,n=5,type="Heuristic",plot=TRUE)
chen.hsu10.d<-fuzzy.ts1(lh,n=5,type="Chen-Hsu",plot=TRUE,divide="distance")
chen.hsu10.f<-fuzzy.ts1(lh,n=5,type="Chen-Hsu",plot=TRUE,divide="density")

goc<-data.frame(lh)
mo<-data.frame(chen10,singh10,heuristic10,chen.hsu10.d,chen.hsu10.f)
av.res(Y=goc,F=mo)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fuzzy.ts1}

