\name{forecast.Greg.ts}
\alias{forecast.Greg.ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forcast group regression models Time-Series}
\description{
Forecast from models fitted by \code{\link{Greg.ts}}.}
\usage{
forecast.Greg.ts(object, model = "ALL", n.ahead = 5, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Resul returned from \code{\link{Greg.ts}} function.
}
  \item{model}{
Names of models want to forecast.
}
  \item{n.ahead}{
The number of steps ahead for which prediction is required.
}
  \item{plot}{
  Let plot=TRUE to paint graph of obsevation series and forecast series in future.
  
  Let plot=FLASE (default) to do not paint graph.}
}

\value{
A data frame where each column is a time series forecast in future from regression model corresponding.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Pham Minh Truc <trucm1813014@gstudent.ctu.edu.vn>
}

\seealso{
 \code{\link{Greg.ts}}
}
\examples{
data(enrollment)
mod<-Greg.ts(enrollment)
forecast.Greg.ts(mod)
}
