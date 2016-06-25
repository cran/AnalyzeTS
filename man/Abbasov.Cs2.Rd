\name{Abbasov.Cs2}
\alias{Abbasov.Cs2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing and sort Abbasov-Mamedova models
}
\description{
Comparing and sort Abbasov-Mamedova models according ME, MAE, MPE, MAPE, MSE RMSE for C values in Cs.
}
\usage{
Abbasov.Cs2(ts, n = 7, w = 7, D1 = 0, D2 = 0, Cs = NULL)
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
  \item{Cs}{
A vector contain C values.
}

}
\details{
Now, this documen are updating.
}

\value{
Table comparing and sort Abbasov-Mamedova models.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{FindC2}},\code{\link{FindC3}},\code{\link{Abbasov.Cs3}}
}
\examples{
Abbasov.Cs2(lh,n=5,w=7,Cs=seq(0,1,0.01))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Abbasov.Cs2 }% use one of  RShowDoc("KEYWORDS")
