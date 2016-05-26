\name{Abbasov.Cs2}
\alias{Abbasov.Cs2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The best C value for Abbasov model
}
\description{
Find a C valua that Abbasov-Mamedova model (model corresponding for fuzzy.ts2) will achieve the least MAE value when using the C value.
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

\value{
The best C and MAE values.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Abbasov.Cs3}}
}
\examples{
Abbasov.Cs2(lh,n=5,w=7,Cs=seq(0,1,0.01))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Abbasov.Cs2 }% use one of  RShowDoc("KEYWORDS")
