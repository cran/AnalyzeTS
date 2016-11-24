\name{GChenHsu.bin}
\alias{GChenHsu.bin}
\title{
Devide point-bin values in more Chen-Hsu models at the same time
}
\description{
Calculating point-bin values, which devece divide fuzzy sets in more Chen-Hsu models (the result returned form Gfuzzy.ts1(...,type=c(...,"Chen-Hsu"),bin=NULL) function).
}
\usage{
GChenHsu.bin(list, n.subset)
}
\arguments{
  \item{list}{
The information of fuzzy sets in Chen-Hsu models form Gfuzzy.ts1(...,type=c(...,"Chen-Hsu"),bin=NULL) function.
}
  \item{n.subset}{
A list where each component is a vector containing fuzzy subset number in every old fuzzy set. The each components of n.subset must be compatible with each componets of list.}
}
\details{
GChenHsu.bin function use \code{\link{ChenHsu.bin}} function to calculate point-bin values in each Chen-Hsu model compatible each components of list.
}
\value{
A list where each components is a vector contain point-bin values for Chen-Hsu models.
}

\author{
Hong Viet Minh <hongvietminh@gmail.com>

Vo Van Tai <vvtai@ctu.edu.vn>
}

\seealso{
Using \code{\link{ChenHsu.bin}} function in case only a Chen-Hsu model.
}
\examples{
#For examples see example(Gfuzzy.ts1)
}