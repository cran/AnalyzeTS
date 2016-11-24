\name{pmax}
\alias{pmax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The lag maximum value
}
\description{
Choose number of lags for endogenous variable to be included for augmented Dickey-Fuller unit root test according Schwert (1989).
}
\usage{
pmax(ts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
Univariate time series.
}
}
\details{
p_max = [12*(T/4)^1/4]

where T is number of observations.

Note: [1.6] = 1
}
\value{
Number of lags for endogenous variable to be included for augmented Dickey-Fuller unit root test according Schwert (1989).
}

\author{
Hong Viet Minh
}


\examples{
data(enrollment)
p<-pmax(enrollment)
library(urca)
summary(ur.df(enrollment,type="trend",lag=p,selectlag="BIC"))
}
