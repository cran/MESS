\name{cmd}
\alias{cmd}
\title{Correlation matrix distance}
\description{
Computes the correlation matrix distance between two correlation matrices
}
\usage{
cmd(m1, m2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m1}{First correlation matrix
}
  \item{m2}{Second correlation matrix
}
}
\details{
Returns a value between 0 and 1. The correlation matrix distance
becomes zero if the correlation matrices are equal up to a scaling
factor and one if they differ to a maximum extent.
}
\value{
Returns the correlation matrix distance.
}
\references{
Herdin, M., and Czink, N., and Ozcelik, H., and Bonek, E. (2005). \emph{Correlation matrix distance, a meaningful measure for evaluation of non-stationary mimo channels}. IEEE VTC.
}
\author{
Claus Ekstrom \email{claus@rprimer.dk}
}
\examples{
m1 <- matrix(rep(1, 16), 4)
m2 <- matrix(c(1, 0, .5, .5, 0, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1), 4)
m3 <- matrix(c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), 4)
cmd(m1, m1)
cmd(m1, m2) 
cmd(m2, m3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }

