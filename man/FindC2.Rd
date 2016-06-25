\name{FindC2}
\alias{FindC2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the best C value for Abbasov Mamedova model
}
\description{
Finding a C value, which is the best for Abbasov Mamedova model.
}
\usage{
FindC2(ts, n = 7, w = 7, D1 = 0, D2 = 0, type = "MAE",
error = 1e-04, trace = 0, MinC.start = 0, MaxC.start = 1000)
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

 \item{trace}{
  Let trace=TRUE to print all of calculation results out to creen.
  
  Let trace=FALSE (default) to only print C and MAE value.
}
\item{type}{
A character string specifying the accuracy of Abbasov Mamedova model, must be one of "ME","MAE" (default), "MPE", "MAPE", "MSE", or "RMSE". You can specify just the initial letter.
}
 
    \item{error}{
Error of C value from FindC2 function compare the best C value of model. Default error = 0.0001}

  \item{MinC.start}{
Min value of C in step first. Default MinC = 0.
}
  \item{MaxC.start}{
Min value of C in step first. Default MaxC = 1000.
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
\code{\link{FindC3}},\code{\link{Abbasov.Cs3}},\code{\link{Abbasov.Cs2}}
}
\examples{
#FindC2(lh,n=7,w=7,D1=0,D2=0,trace=0,MaxC=1000000,error=0.00001)
#FindC2(lh,n=7,w=7,D1=0,D2=0,trace=1,MaxC=1000,error=0.00001)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ FindC3 }% use one of  RShowDoc("KEYWORDS")
