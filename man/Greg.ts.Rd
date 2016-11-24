\name{Greg.ts}
\alias{Greg.ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Group regression models for Time-Series}
\description{
Building some regression models for time series.}
\usage{
Greg.ts(ts, p.max = 3, r = 4, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
Univariate time series.
}
  \item{p.max}{
Greatest level of polynomial models.}
  \item{r}{
Display results returned to the specified number of decimal places (default 3). (See \code{\link{round2str}} for details of r paramicter.)
}
  \item{plot}{
  Let plot=TRUE to paint graph of obsevation series and interpolate series.
  Let plot=FLASE (default) to do not paint graph.
  }
}
\details{
luy thua: Y = a*t^b

luong giac: Y = a + b*sin(t) + c*cos(t)

hyperbol: Y = a + b/t

CS cong: ...

CS nhan
}
\value{
Results are returned as a list containing the following components.
\item{Models}{Formula of regression models.}
\item{Interpolate, Error}{Two data frame where each column is a time series interpolate and error from regression model corresponding.}
}
\references{
Vo Van Tai, 2012. Du bao san luong lua Viet Nam bang cac mo hinh toan hoc, Tap chi Khoa hoc Dai hoc Can Tho. 23b125-134.
}
\author{
Hong Viet Minh <hongvietminh@gmail.com>

Pham Minh Truc <trucm1813014@gstudent.ctu.edu.vn>
}

\seealso{
The \code{\link{forecast.Greg.ts}} function using to predict for models fitted by Greg.ts function.
}
\examples{
data(enrollment)
Greg.ts(enrollment)
}

