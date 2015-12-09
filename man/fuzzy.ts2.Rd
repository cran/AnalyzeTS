   \name{fuzzy.ts2}
\alias{fuzzy.ts2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fuzzy Times Series 2
}
\description{
Predicts time series by fuzziness method according to Abbasov-Manedova model.
}
\usage{
fuzzy.ts2(ts, n = 5, w = NULL, D1 = 0, D2 = 0, C = NULL,
r = 4, trace = FALSE, forecast = NULL, plot = FALSE)
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
  \item{r}{
Rounds the answer to the specified number of decimal places (default 4).
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
}

\value{
\item{type}{Name of fuzzy model.}
\item{table1}{Information about changing fuzzy sets.}
\item{table2}{Observation series and changing series.}
\item{table3}{The change fuzzy of observation series.}
\item{table4}{Interpolate values.}
\item{table5}{Forecasting values.}
\item{table6}{The change fuzzy of forecasting series.}
\item{accuracy}{Information about 7 accuracy of forecasting model.}
}

\author{
Doan Hai Nghi <Hainghi1426262609121094@gmail.com> 

Tran Thi Ngoc Han <tranthingochan01011994@gmail.com>

Hong Viet Minh <hongvietminh@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

abbasov2<-fuzzy.ts2(lh,C=0.0001,n=5,w=2,forecast=10,plot=TRUE,trace=TRUE)
abbasov2

abbasov5<-fuzzy.ts2(lh,C=0.0001,n=5,w=5,forecast=10,trace=TRUE)
abbasov8<-fuzzy.ts2(lh,C=0.0001,n=5,w=8,forecast=10,trace=TRUE)
abbasov10<-fuzzy.ts2(lh,C=0.0001,n=5,w=10,forecast=10,trace=TRUE)
abbasov12<-fuzzy.ts2(lh,C=0.0001,n=5,w=12,forecast=10,trace=TRUE)

sosanh<-rbind(abbasov2$accuracy,abbasov5$accuracy,abbasov8$accuracy,
        abbasov10$accuracy,abbasov12$accuracy)
rownames(sosanh)<-c("abbasov2","abbasov5","abbasov8","abbasov10","abbasov12")
sosanh
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fuzzy.ts2}

