\name{enrollment}
\alias{enrollment}
\docType{data}
\title{
Time Series of Enrollment
}
\description{
A time series of enrollment from 1971 to 1992.}

\usage{data("enrollment")}

\source{
Chen, S.M., 1996. Forecasting  enrollments  based  on  fuzzy  time  series. Fuzzy Sets  and Systems. 81: 311-319. 

Chen, S.M. and Hsu, C.C., 2004. A New method to forecast enrollments using fuzzy time series. International Journal of Applied Science and Engineering, 12: 234-244.

Huarng, H., 2001. Huarng models of fuzzy time series for forecasting. Fuzzy Sets and Systems. 123: 369-386.

Singh, S.R., 2008. A computational method of forecasting based on fuzzy time series.   Mathematics and Computers in Simulation. 79: 539-554

}

\examples{
data(enrollment)
ts.plot(enrollment,col="red")
grid.on()}
\keyword{datasets}
