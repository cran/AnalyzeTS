   \name{Frequencies}
\alias{Frequencies}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Frequencies
}
\description{
This function calculate to return answer which are descriptive statistics values for a discrete variable or discrete variables in data frame.
}
\usage{
Frequencies(x, plot = FALSE, r = 2, answer = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{x}{
 A discrete variable or a data frame contain discrete variable.
}
  \item{plot}{
  Let plot=TRUE to paint pie graph.
}
  \item{r}{
   Rounds the answer to the specified number of decimal places (default 2).
}
  \item{answer}{
  Form of answers are returned. Let answer=1 or answer=2 (default 1)
}
}
\details{
Statistic descriptive values are calculated by theory of base statistic.
}
\value{
\item{N}{Length sample}
\item{NaN}{Number NA values}
\item{(xi)N}{Length of xi}
\item{VAR}{Variance value}
\item{SD}{Standard Deviation}
\item{SE}{Standard Deviation of the Estimated Means}
}
\references{
Theory of base statistic.
}
\author{
Hong Viet Minh <hongvietminh@gmail.com>
}
\note{
You must not withdraw continuously variables from data frame. When you let a data frame in to this function which will auto withdraw continuously variables and calculate descriptive statistic to discrete variables.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Descriptives}}, \code{\link{Dgroup}}
}
\examples{
#Load data
library(MASS)
data(crabs)

#Descriptive for a discrete variable
Frequencies(crabs$sp,plot=TRUE)

#Descriptive for discrete variables
Frequencies(crabs,answer=2,plot=TRUE)
}

