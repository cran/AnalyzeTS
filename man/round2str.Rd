\name{round2str}
\alias{round2str}
\title{
rounded and converted to string of numbers}
\description{
Rounded and converted to string of numbers to the specified number of decimal places.
}
\usage{
round2str(x, r = 12)
}
\arguments{
  \item{x}{
A numeric, vector, matrix or data frame of numeric.
}
  \item{r}{
A integer indicating the number of decimal places.
}
}
\details{
Unlike \code{\link{round}} function, round2str function cross-section numbers to the specified number of decimal places and then converted result to string.
}
\author{
Hong Viet Minh <hongvietminh@gmail.com>
}

\examples{
a<-rnorm(3)
a
round2str(a,r=2)
round2str(a,r=5)
}

