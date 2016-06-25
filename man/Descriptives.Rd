 \name{Descriptives}
\alias{Descriptives}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptives
}
\description{
This function calculate to return answer which are descriptive statistics values for a continuously variable or continuously variables in data frame.
}
\usage{
Descriptives(x, plot = FALSE, r = 2, answer = 1, statistic = "ALL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{
 A continuously variable or a data frame contain continuously variables.
}
  \item{plot}{
  Parameter 'Plot' are used by 2 form:
  
  Let plot=TRUE to paint description graph when x is time series.
  
  Let plot=list(a1,a2,...) (in a1,a2,...are descriptive statistic values as 'Mean', 'Max',...) to paint comparing graph between variables.
}
  \item{r}{
   Rounds the answer to the specified number of decimal places (default 2).
}
  \item{answer}{
  Form of answers are returned. Let answer=1 or answer=2 (default 1)
}
  \item{statistic}{
A list contain descriptive statistic values that user want R print screen (default ALL).
}
}
\details{
Statistic descriptive values are calculated by theory of base statistic.
}
\value{
\item{N}{Length sample}
\item{NaN}{Number NA values}
\item{Min}{Min value}
\item{1sq QU}{Value in 25\% of interval probabilities}
\item{Median}{Median value}
\item{Mean}{Mean value}
\item{3rd QU}{Value in 75\% of interval probabilities}
\item{Max}{Max value}
\item{VAR}{Variance value}
\item{SD}{Standard Deviation}
\item{SE}{Standard Deviation of the Estimated Means}
}
\references{
Theory of base statistic.
}
\author{
Mai Thi Hong Diem <maidiemks@gmail.com> 

Hong Viet Minh <hongvietminh@gmail.com>
}
\note{
You must not withdraw discrete variables from data frame. When you let a data frame in to this function which will auto withdraw discrete variables and calculate descriptive statistic to continuously variables.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Frequencies}}, \code{\link{Dgroup}}
}
\examples{
#Load data
library(MASS)
data(crabs)

#Calculate descriptive statistic to a continuously variable
Descriptives(crabs$FL)

#Calculate descriptive statistic to continuously variables
Descriptives(crabs)
Descriptives(crabs,answer=2)
Descriptives(crabs,answer=2,r=6)

#To just see some descriptive statistic variables
Descriptives(crabs,statistic=list("Min","Mean","Median","Max"))

#Combined paint graph to compare
Descriptives(crabs,plot=list("Mean","SD"))

#Descriptives for time series
Descriptives(lh,plot=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Descriptives}