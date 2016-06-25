\name{Abbasov.Cs3}
\alias{Abbasov.Cs3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparing and sort Improve Abbasov-Mamedova version 1 models
}
\description{
Comparing and sort Improve Abbasov-Mamedova version 1 models according ME, MAE, MPE, MAPE, MSE RMSE for C values in Cs.
}
\usage{
Abbasov.Cs3(ts, n = 7, w = 7, D1 = 0, D2 = 0, Cs = NULL, fty = c("ts", "f"))
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
 \item{fty}{
A character string specifying the focasting of Improve Abbasov Mamedova version 1 model, must be one of "ts" (default), or "f". You can specify just the initial letter.
}
 

}


\details{
fty="f", N(length(ts)) = N(length(ts)-1) + V(length(ts)+1).

fty="ts", N(length(ts)) = ts(length(ts)) + V(length(ts)+1).
}
\value{
Table comparing and sort Improve Abbasov-Mamedova version 1 models.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{FindC2}},\code{\link{FindC3}},\code{\link{Abbasov.Cs2}}
}
\examples{
Abbasov.Cs3(lh,n=5,w=7,Cs=seq(0,1,0.01),fty="ts")
Abbasov.Cs3(lh,n=5,w=7,Cs=seq(0,1,0.01),fty="f")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Abbasov.Cs3}% use one of  RShowDoc("KEYWORDS")
