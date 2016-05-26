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
"Chen-Hsu"), bin = NULL, trace = FALSE, plot = FALSE)
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
  Point values use to divide fuzzy stes for Chen-Hsu model. If bin=NULL (default) then function just inform information about fuzzy sets.
}
  \item{trace}{
  Let trace=TRUE to print all of calculation results out to creen.
  
  Let trace=FALSE (default) to only print fuzzy series out to creen.
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
}
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

#useing ChenHsu.bin function to find divide point fuzzy set values.
a<-fuzzy.ts1(lh,type="Chen-Hsu",plot=1)
b<-ChenHsu.bin(a$table1,ni=c(1,2,1,1,1))
fuzzy.ts1(lh,type="Chen-Hsu",bin=b,plot=1,trace=1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{fuzzy.ts1}

