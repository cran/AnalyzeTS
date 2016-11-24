\name{Dgroup}
\alias{Dgroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Discriptives Follow Groups
}
\description{
Descriptive statistics in group for a continous variable. Usual using to statistic a time series following dates in week or months in year.
}
\usage{
Dgroup(x, follow = NULL, r = 2, answer = 1, statistic = "ALL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A continous variable or a time series.
}
  \item{follow}{
A factor or a list factor which contain not too two factors.
}
  \item{r}{
 Rounds the answer to the specified number of decimal places (default 2).
}
  \item{answer}{
 Form of answers are returned. Let answer=1 or answer=2 (default 1).
}
  \item{statistic}{
 A list contain descriptive statistic values that user want R print screen (default ALL).
}
}
\details{
Data is divided into groups by follow and then every group are calculated by Descriptives function.
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
Hong Viet Minh <hongvietminh@gmail.com>
}
\note{
The function just maximum calculated for two factors.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Descriptives}},\code{\link{Frequencies}}
}
\examples{
#Factor date
date<-as.factor(c("Tue","Wed","Thu","Fri","Mon","Tue","Wed","Thu","Fri","Mon",
"Tue","Wed","Thu","Fri","Mon","Tue","Wed","Thu","Fri","Mon","Tue","Wed","Thu",
"Fri","Mon","Tue","Wed","Thu","Fri","Mon","Tue","Wed","Thu"))

#Factor hk
hk<-as.factor(c("hk1","hk2","hk1","hk3","hk3","hk1","hk1","hk1","hk2","hk2","hk2",
"hk1","hk2","hk1","hk1","hk1","hk2","hk1","hk1","hk1","hk1","hk2","hk1",
"hk1","hk1","hk1","hk3","hk1","hk3","hk3","hk2","hk3","hk1"))

#A continous variable
coffee<-c(5,6,8,4,3,7,6,0,3,2,3,4,9,1,3,8,7,8,2,3,8,6,4,4,6,7,6,5,2,3,8,4,4)

#Descriptive statistics in group
Dgroup(coffee,r=4,answer=2)
Dgroup(coffee,follow=list(date),r=4)
Dgroup(coffee,follow=date,r=4,answer=2)
Dgroup(coffee,follow=date,r=4,statistic=list("Mean","Max"))
Dgroup(coffee,r=4,follow=list(date,hk),answer=1)
Dgroup(coffee,r=4,follow=list(date,hk),answer=2)
Dgroup(coffee,r=4,follow=list(hk,date),answer=1)
Dgroup(coffee,r=4,follow=list(hk,date),answer=2)
}

