 \name{CMA}
\alias{CMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Center Moving Average
}
\description{
Calculate moving average by mothod of centered moving average centered.
}
\usage{
CMA(x, n = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Observation series.
}
  \item{n}{
Number of periods to average over.
}
}

\value{
Observation series after.
}

\author{
Doan Hai Nghi <Hainghi1426262609121094@gmail.com>
}
\note{
Before, name of this function is TTT.
}

\seealso{
\code{\link{SES}}
}
\examples{ 
CMA(lh,n=5)
CMA(lh,n=8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{CMA }
