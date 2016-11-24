\name{fuzzy.ts2}
\alias{fuzzy.ts2}
\title{
Abbasov Mamedova model and FVD model
}
\description{
Calculating fuzziness  and forecast  time series by fuzziness method according to Abbasov - Manedova  (2010) and NFTS models.}
\usage{
fuzzy.ts2(ts, n = 7, w = 7, D1 = 0, D2 = 0, C = NULL, forecast = 5, 
r = 12, trace = FALSE, plot = FALSE, grid = FALSE, type = "Abbasov-Mamedova")
}
\arguments{
  \item{ts}{
Univariate time series.
}
  \item{n}{
Number of fuzzy set.
}
  \item{w}{
The w parameter.
}
  \item{D1, D2}{
Two proper positive numbers.
}
  \item{C}{
A optional constant.
}
  \item{forecast}{
Number of points to forecast in future.
}
  \item{r}{
Display results returned to the specified number of decimal places (default 12). (See \code{\link{round2str}} for details of r paramicter.)
}
  \item{trace}{
 Let trace=TRUE to print all of calculation results out to creen.
 Let trace=FALSE (default) to only print forecasting series out to creen.
 }
  \item{plot}{
 Let plot=TRUE to paint graph of obsevation series and fuzzy series.
 Let plot=FLASE (default) to do not paint graph.
 }
  \item{grid}{
If TRUE, a gray background grid is put on the graph.
}
  \item{type}{
Model is choosed to predicts time series by fuzziness, type = "Abbasov-Manedova" or type = "NFTS".
}
}

\value{
When trace = TRUE, results are returned as a list containing the following components.
\item{type}{The value of type.}
\item{table1}{Information about changing fuzzy sets consit four column: set is name of the fuzzy sets, low and up are upper and lower bounds of the fuzzy sets, and mid is middle values corresponding every fuzzy set.}
\item{table2}{Series - observation consit three column: point is time of observation, ts is the original series, and diff.ts is changing series from original series.}
\item{table3}{The change fuzzy of original series.}
\item{table4}{Series - interpolation consit three column: point is time of interpolation, interpolate is the series - interpolation, and diff.interpolate is changing series from series - interpolation.}
\item{table5}{Forecasting series consit three column: point is time of forecast, forecast is the forecasting series, and diff.forecast is changing series from forecasting series.}
\item{table6}{The change fuzzy of forecasting series.}
\item{accuracy}{Information about the criterion to evaluate forecasting model.}

When trace = FALSE, results are returned as a list containing two components.
\item{interpolate}{Series - interpolation.}
\item{forecast}{Forecasting series.}
}

\references{
Abbasov, A.M. and Mamedova, M.H., 2003. Application of fuzzy time series to population forecasting, Proceedings of 8th Symposion on Information Technology in Urban and Spatial Planning, Vienna University of Technology, February 25-March1, 545-552.
}
\author{
Doan Hai Nghi <Hainghi1426262609121094@gmail.com> 

Tran Thi Ngoc Han <tranthingochan01011994@gmail.com>

Hong Viet Minh <hongvietminh@gmail.com>
}
\seealso{
\code{\link{Gfuzzy.ts2}}
}
\examples{
#data(enrollment)
#layout(1:2)
#NF.mod<-fuzzy.ts2(enrollment,n=7,w=7,C=0.0001,forecast=11,trace=TRUE,plot=TRUE,type="NFTS")
#AM.mod<-fuzzy.ts2(enrollment,n=5,w=5,C=0.01,forecast=5,plot=TRUE,type="Abbasov-Mamedova")
#NF.mod
#AM.mod

#Finding the best C value by DOC function
#Abbasov-Mamedova model
#str.C1<-DOC(enrollment,n=7,w=7,D1=0,D2=0,CEF="MAPE",type="Abbasov-Mamedova")
#C1<-as.numeric(str.C1[1])
#fuzzy.ts2(enrollment,n=7,w=7,D1=0,D2=0,C=C1,forecast=5,type="Abbasov-Mamedova")

#NFTS model
#str.C2<-DOC(enrollment,n=7,w=7,D1=0,D2=0,CEF="MAPE",type="NFTS")
#C2<-as.numeric(str.C2[1])
#fuzzy.ts2(enrollment,n=7,w=7,D1=0,D2=0,C=C1,forecast=5,type="NFTS")

}