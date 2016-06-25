\name{FindC3}
\alias{FindC3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the best C value for Improve Abbasov Mamedova version 1 model
}
\description{
Finding a C value, which is the best for Improve Abbasov Mamedova version 1 models.
}
\usage{
FindC3(ts, n = 7, w = 7, D1 = 0, D2 = 0, fty = c("ts", "f"), error = 1e-04,
trace = 0, MinC.start = 0, MaxC.start = 1000, type = "MAE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{ts}{
  Observation series.
}
  \item{n}{
Number of fuzzy set.
}
  \item{w}{
'w' parameter.
}
  \item{D1}{
A adequate value.
}
  \item{D2}{
A adequate value.
}

 \item{fty}{
fty="f", N(length(ts)) = N(length(ts)-1) + V(length(ts)+1).

fty="ts", N(length(ts)) = ts(length(ts)) + V(length(ts)+1).
}

 \item{trace}{
  Let trace=TRUE to print all of calculation results out to creen.
  
  Let trace=FALSE (default) to only print C and MAE value.
}
 
  \item{error}{
Error of C value from FindC2 function compare the best C value of model. Default error = 0.0001}

  \item{MinC.start}{
Min value of C in step first. Default MinC = 0.
}
  \item{MaxC.start}{
Max value of C in step first. Default MaxC = 1000.
}
\item{type}{
A character string specifying the accuracy of Improve Abbasov Mamedova version 1 model, must be one of "ME","MAE" (default), "MPE", "MAPE", "MSE", or "RMSE". You can specify just the initial letter.
}
}

\details{
Now, this documen are updating.
}
\value{
The best C values.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{FindC2}},\code{\link{Abbasov.Cs3}},\code{\link{Abbasov.Cs2}}
}

\examples{
#FindC3(lh,n=7,w=7,D1=0,D2=0,fty="ts",MaxC=1000000,trace=0,error=0.0001)
#FindC3(lh,n=7,w=7,D1=0,D2=0,fty="f",MaxC=1000000,trace=1,error=0.0001)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ FindC3 }% use one of  RShowDoc("KEYWORDS")
