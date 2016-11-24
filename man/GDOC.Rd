\name{GDOC}
\alias{GDOC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finding the best C values
}
\description{
Finding the best C values for more Abbasov Mamedova and NFTS models according to DOC algorithm at the same time.
}
\usage{
GDOC(ts, n = 7, w = 7, D1 = 0, D2 = 0, error = 1e-06, k = 500, r = 13, 
CEF = "MSE", type = "Abbasov-Mamedova", show.complete = TRUE)
}
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
  \item{D1,D2}{
Two proper positive numbers.
}
    \item{error}{
Error of C value is finded by DOC algorithm, which compare the best C value really. Default error = 0.000001.
}
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
A character vector where each element is choosing model to predicts time series by fuzziness, type = "Abbasov-Manedova" (default) or type = "NFTS" or both. 
}
  \item{show.complete}{
  If TRUE, a graph will appear showing the percentage completed.
  } 
}
\details{
GDOC function consider length(n)*length(w)*length(type) models combining from three parameter n, w and type, and then using \code{\link{DOC}} function finding the best C values forone by models.
}
\value{
A list contain two components where the first component is the best C values of Abbasov-Mamedova models and the second component is the best C values of NFTS models.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}
\seealso{
Using \code{\link{DOC}} function in case only a Abbasov-Mamedova or NFTS model.
}
\examples{
#For examples see example(Gfuzzy.ts2)
}

