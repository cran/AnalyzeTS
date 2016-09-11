\name{AnalyzeTS-package}
\alias{AnalyzeTS-package}
\alias{AnalyzeTS}
\docType{package}
\title{
\packageTitle{AnalyzeTS}
}
\description{
\packageDescription{AnalyzeTS}
}

\author{
\packageAuthor{AnalyzeTS}

Maintainer: \packageMaintainer{AnalyzeTS}
}


\references{
Chen, S.M., 1996. Forecasting  enrollments  based  on  fuzzy  time  series. Fuzzy Sets  and Systems. 81: 311-319. 

Chen, S.M. and Hsu, C.C., 2004. A New method to forecast enrollments using fuzzy time series. International Journal of Applied Science and Engineering, 12: 234-244.

Huarng, H., 2001. Huarng models of fuzzy time series for forecasting. Fuzzy Sets and Systems. 123: 369-386.

Singh, S.R., 2008. A computational method of forecasting based on fuzzy time series.   Mathematics and Computers in Simulation. 79: 539-554

Abbasov, A.M. and Mamedova, M.H., 2003. Application of fuzzy time series to population forecasting, Proceedings of 8th Symposion on Information Technology in Urban and Spatial Planning, Vienna University of Technology, February 25-March1, 545-552.

}
\examples{
library(AnalyzeTS)
data(enrollment)
#Sing model
fuzzy.ts1(lh,n=5,type="Singh",plot=TRUE)

#Abbasov Mamedova model
fuzzy.ts2(enrollment,n=5,w=5,C=0.01,forecast=5,plot=TRUE,type="Abbasov-Mamedova")

#NFTS model
fuzzy.ts2(enrollment,n=5,w=5,C=0.01,forecast=5,plot=TRUE,type="NFTS")
}
