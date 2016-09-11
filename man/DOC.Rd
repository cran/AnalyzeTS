\name{DOC}
\alias{DOC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the best C value for Abbasov Mamedova model or FVD model
}
\description{
Finding a C value, which is the best for Abbasov Mamedova model.
}
\usage{
DOC(ts, n = 7, w = 7, D1 = 0, D2 = 0, error = 1e-06, k=500,
CEF = "MSE",type="Abbasov-Mamedova") 
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
    \item{error}{
Error of C value is finded, which compare the best C value really. Default error = 0.000001}

 \item{k}{
In each iteration of the algorithm, k+1 (or k or k-1) values of C will be considered. The k must be a integer and greater than 499, default k = 500.
}

\item{CEF}{
One of the criterion to evaluate forecasting model, must be one of "ME","MAE" , "MPE", "MAPE", "MSE" (default), or "RMSE".
}


  \item{type}{
Model is choosed to predicts time series by fuzziness, type = "Abbasov-Manedova" or type = "NFTS". 
}
}
\details{
Now, this documen are updating.
}
\value{
The best C value and CEF corresponding value.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#data(enrollment)
#DOC(enrollment,n=7,w=7,error=0.00001,CEF="MSE",type="Abbasov-Mamedova")
#DOC(enrollment,n=7,w=7,error=0.00001,CEF="MAPE",type="NFTS")
}

