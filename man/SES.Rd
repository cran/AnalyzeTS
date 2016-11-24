\name{SES}
\alias{SES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple Exponential Smoothing
}
\description{
Calculating simple exponential smoothing for a time series.
}
\usage{
SES(ts, alpha = 0.5, s0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
Univariate time series.
}
  \item{alpha}{
The smoothing parameter, 0 < alpha < 1 (default 0.5).
}
  \item{s0}{
 Original estimate of s0 value.
}
}
\details{
alpha = 2 / (n + 1) in n is moving periodic.
}
\value{
Observation series after.
}
\references{
\url{https://www.otexts.org/fpp/7/1}
}
\author{
Doan Hai Nghi <Hainghi1426262609121094@gmail.com> 

Hong Viet Minh <hongvietminh@gmail.com>
}
\note{
Before, name of this function is TM.
}

\seealso{
\code{\link{CMA}}
}
\examples{
SES(lh,alpha=0.5)
SES(lh,alpha=0.8)
}

