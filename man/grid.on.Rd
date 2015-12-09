\name{grid.on}
\alias{grid.on}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grid on
}
\description{
Paint to line graph.
}
\usage{
grid.on(h = TRUE, v = TRUE, col = "gray", nv = 1, nh = 1, lty = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h}{
  Let h=FALSE to do not paint grid in horizontal.
}
  \item{v}{
  Let v=FALSE to do not paint grid in vertical.
}
  \item{col}{
  The color for grid lines.
}
  \item{nv}{
  The number grid lines between two point in x axis.
}
  \item{nh}{
  The number grid lines between two point in y axis.
}
  \item{lty}{
  The grid line type.
}
}

\value{
A grid is painted in graph.
}

\author{
Hong Vet Minh <hongvietminh@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
plot(lh)
grid.on(h=TRUE,v=FALSE)
plot(co2)
grid.on(col="red",nv=3)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{grid.on}
