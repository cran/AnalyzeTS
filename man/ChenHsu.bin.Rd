\name{ChenHsu.bin}
\alias{ChenHsu.bin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Devide point-bin values in Chen-Hsu model
}
\description{
Calculating point-bin values, which devece divide fuzzy sets in Chen-Hsu model.
}
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
A vector contain point-bin values.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>
}


\seealso{
\code{\link{GChenHsu.bin}} for more models at the same time.
}
\examples{
#For examples see example(fuzzy.ts1)
}
