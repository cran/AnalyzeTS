\name{Gfuzzy.ts2}
\alias{Gfuzzy.ts2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Updating
}
\description{
Updating
}
\usage{
Gfuzzy.ts2(ts, n = 7, w = 7, D1 = 0, D2 = 0, C = list(C1 = NULL, C2 = NULL), 
forecast = 5, plot = FALSE, grid = FALSE, type = "Abbasov-Mamedova")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
Univariate time series. 
}
  \item{n}{
A numeric vector where each element is number of fuzzy set. 
}
  \item{w}{
A numeric vector where each element is w parameter. 
}
  \item{D1, D2}{
Two proper positive numbers. 
}
  \item{C}{
A list consiting 2 component C1 and C2 or a rusult object from GDOC function.
}
  \item{forecast}{
Number of points to forecast in future. 
}
  \item{plot}{
Let plot=TRUE to paint graph of obsevation series and fuzzy series. Let plot=FLASE (default) to do not paint graph.
}
  \item{grid}{
If TRUE, a gray background grid is put on the graph.
}
  \item{type}{
Model is choosed to predicts time series by fuzziness, type = "Abbasov-Manedova" or type = "NFTS" or both.
}
}
\details{
Gfuzzy.ts2 function consider length(n)*length(w)*length(type) models combining from three parameter n, w and type, and then using \code{\link{fuzzy.ts2}} function analyze for each submodel.
}
\value{
A list with three component.  
\item{information}{Explaining for lables in  annotation on the graph also as name of columns in interpolate and forecast components.}
\item{interpolate, forecast}{two data frame where each column is a time series interpolate and forecasted in future by fuzzy time series model corresponding.}
}
\references{
Abbasov, A.M. and Mamedova, M.H., 2003. Application of fuzzy time series to population forecasting, Proceedings of 8th Symposion on Information Technology in Urban and Spatial Planning, Vienna University of Technology, February 25-March1, 545-552.
}
\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}

\seealso{
Using \code{\link{Gfuzzy.ts1}} function in case only a  fuzzy time series model.
}
\examples{
#data(enrollment)
#g.C<-GDOC(enrollment,n=c(5,7,9),w=c(7,9),D1=0,D2=0,
#CEF="MSE",type=c("Abbasov-Mamedova","NFTS"))
#g.fuzzy1<-Gfuzzy.ts2(enrollment,n=c(5,7,9),w=c(7,9),D1=0,D2=0,C=g.C,forecast=5, 
#plot=1,grid=0,type=c("Abbasov-Mamedova","NFTS")) 
}
