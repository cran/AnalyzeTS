\name{ChenHsu.bin}
\alias{ChenHsu.bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bin point valuesin Chen-Hsu model
}
\description{
Calculating bin point values, which devece divide fuzzy sets in Chen-Hsu model.}
\usage{
ChenHsu.bin(table, n.subset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{
Object table1 from answer of fuzzy.ts1 fuction.}
  \item{n.subset}{
A vector contain fuzzy subset number in every old fuzzy set.}
}

\value{
A vector contain bin point values.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
a<-fuzzy.ts1(lh,type="Chen-Hsu",plot=1)
b<-ChenHsu.bin(a$table1,n.subset=c(1,2,1,1,1))
fuzzy.ts1(lh,type="Chen-Hsu",bin=b,plot=1,trace=1)
}
