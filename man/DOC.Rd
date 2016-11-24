\name{DOC}
\alias{DOC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finding the best C value
}
\description{
Finding the best C value for Abbasov Mamedova and NFTS models according to DOC algorithm.
}
\usage{
DOC(ts, n = 7, w = 7, D1 = 0, D2 = 0, error = 1e-06, k=500,r=13,
CEF = "MSE",type="Abbasov-Mamedova", show.complete = TRUE, keyword) 
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
    \item{error}{
Error of C value is finded by DOC algorithm, which compare the best C value really. Default error = 0.000001.}

 \item{k}{
In each iteration of the algorithm, k+1 (or k or k-1) values of C will be considered. The k must be a integer and greater than 499, default k = 500.
}
  \item{r}{
Display results returned to the specified number of decimal places (default 13). (See \code{\link{round2str}} for details of r paramicter.)
}
\item{CEF}{
One of the criterion to evaluate forecasting model, must be one of "ME","MAE" , "MPE", "MAPE", "MSE" (default), or "RMSE".
}
  \item{type}{
Model is choosed to predicts time series by fuzziness, type = "Abbasov-Manedova" (default) or type = "NFTS". 
}

  \item{show.complete}{
  If TRUE, a graph will appear showing the percentage completed.
  } 

  \item{keyword}{
  A keyword help connecting from \code{\link{GDOC}} function to \code{\link{Compare.Cs}} function. User are not allowed to use parameter 'keyword'.
  }

}


\value{
The best C value and CEF corresponding value.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}

\seealso{
\code{\link{GDOC}}
}

\examples{
#data(enrollment)
#DOC(enrollment,n=7,w=7,error=0.00001,CEF="MSE",type="Abbasov-Mamedova")
#DOC(enrollment,n=7,w=7,error=0.00001,CEF="MAPE",type="NFTS")
}

