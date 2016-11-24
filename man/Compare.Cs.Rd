\name{Compare.Cs}
\alias{Compare.Cs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing and sort Abbasov-Mamedova models or NFTS models
}
\description{
Comparing and sort Abbasov-Mamedova models or NFTS models according a criterion to evaluate forecasting model (ME, MAE, MPE, MAPE, MSE or RMSE) for C values in Cs.
}
\usage{
Compare.Cs(ts, n = 7, w = 7, D1 = 0, D2 = 0, Cs = NULL, 
type = "Abbasov-Mamedova", complete = NULL)
}

%- maybe also 'usage' for other objects documented here.
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
  \item{D1,D2}{
Two proper positive numbers.
}
  \item{Cs}{
A vector contain C values.
}
  \item{type}{
Model is choosed to predicts time series by fuzziness, type = "Abbasov-Manedova" or type = "NFTS". 
}
  \item{complete}{
   A parameter help connecting from \code{\link{DOC}} and \code{\link{GDOC}} functions to \code{\link{Compare.Cs}} function. User are not allowed to use parameter 'complete'.
  }
}
\details{
Now, this documen are updating.
}

\value{
Table comparing and sort Abbasov-Mamedova models or FVD models.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
#Compering Abbasov-Mamedova models
Compare.Cs(lh,n=5,w=7,Cs=seq(0,1,0.01),type="Abbasov-Mamedova")

#Compering NFTS models
Compare.Cs(lh,n=5,w=7,Cs=seq(0,1,0.01),type="NFTS")
}
