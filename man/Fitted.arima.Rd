\name{Fitted.arima}
\alias{Fitted.arima}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fitted for ARIMA model}
\description{
Computes the fitted values of an ARIMA model.}
\usage{
Fitted.arima(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A fitted model from the arima function.
}
}

\value{
Fitted values.
}
\author{
Hong Viet Minh <hongvietminh@gmail.com>

Pham Minh Truc <trucm1813014@gstudent.ctu.edu.vn>
}

\seealso{
\code{\link{fitted.Arima}} in TSA package.
}
\examples{
mod<-arima(lh,order=c(0,1,1))
Fitted.arima(mod)
}
