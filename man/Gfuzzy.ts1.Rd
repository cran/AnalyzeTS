\name{Gfuzzy.ts1}
\alias{Gfuzzy.ts1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chen, Sing, Heuristic and Chen-Hsu models at the same time
}
\description{
Calculating fuzziness of time series with Chen (1996), Singh (2008), Heuristic (Huarng 2001) and Chen-Hsu (2004) models at the same time.
}
\usage{
Gfuzzy.ts1(ts, n = 5, D1 = 0, D2 = 0, type = "Chen", bin = NULL,
plot = FALSE, grid = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
Univariate time series.
}
  \item{n}{
A numeric vector where each element is number of fuzzy set.
}
  \item{D1,D2}{
Two proper positive numbers.
}
  \item{type}{
A character vector where each element is type of models.
}
  \item{bin}{
A list where each component is point-bin values use to divide fuzzy stes for Chen-Hsu models. If bin=NULL (default) then function just inform information about fuzzy sets compatible with each Chen-Hsu model.
  }
  \item{plot}{
  Let plot=TRUE to paint graph of obsevation series and fuzzy series.
  Let plot=FLASE (default) to do not paint graph.
}
\item{grid}{
If TRUE, a gray background grid is put on the graph.
}
}
\details{
Gfuzzy.ts1 function consider length(n)*length(type) models combining from two parameter n and type.
}
\value{
A data frame where each column is a time series fitted by fuzzy time series model corresponding.
}
\references{
Chen, S.M., 1996. Forecasting  enrollments  based  on  fuzzy  time  series. Fuzzy Sets  and Systems. 81: 311-319. 

Chen, S.M. and Hsu, C.C., 2004. A New method to forecast enrollments using fuzzy time series. International Journal of Applied Science and Engineering, 12: 234-244.

Huarng, H., 2001. Huarng models of fuzzy time series for forecasting. Fuzzy Sets and Systems. 123: 369-386.

Singh, S.R., 2008. A computational method of forecasting based on fuzzy time series.  Mathematics and Computers in Simulation. 79: 539-554
}
\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}

\seealso{
Using \code{\link{fuzzy.ts1}} function in case only a  fuzzy time series model.
}
\examples{
#Step 1: Analyze fuzzy time series actual series of n fuzzy set (n is
#number fuzzy set choosed in the first times, in our case n = 5, 7 and 9)
#to has information about fuzzy sets. 
#KQ1<-Gfuzzy.ts1(lh,n=c(5,7,9),type=c("Chen","Singh","Heuristic","Chen-Hsu"))


#Step 2: Finding bin-point values divide fuzzy sets second times.
#v1<-c(1,1,1,1,1)
#v2<-c(1,2,2,1,2,1,1)
#v3<-c(1,1,2,4,1,2,1,1,1)
#KQ2<-GChenHsu.bin(KQ1,n.subset=list(v1,v2,v3))


#Step 3 Analyze fuzzy time series by Chen-Hsu more times with new fuzzy
#sets from step 2.
#KQ3<-Gfuzzy.ts1(lh,n=c(5,7,9),type=c("Chen","Singh","Heuristic",
#"Chen-Hsu"),bin=KQ2,plot=1,grid=1)
#KQ3
}
