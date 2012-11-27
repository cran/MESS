\name{lower.tri.vector}
\alias{lower.tri.vector}
\title{Split Matrix by Clusters and Return Lower Triangular Parts as Vector}
\description{
Split a matrix into block diagonal sub matrices according to clusters and combine the lower triangular parts into a vector
}
\usage{
lower.tri.vector(x, cluster = rep(1, nrow(x)), diag = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a square matrix}
  \item{cluster}{numeric or factor. Is used to identify the sub-matrices of \code{x} from which the lower triangular parts are extracted. Defaults to the full matrix.}
  \item{diag}{logical. Should the diagonal be included?}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns a numeric vector containing the elements of the lower triangular sub matrices.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Claus Ekstrom \email{claus@ekstroem.dk}
}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{lower.tri}}
}
\examples{
m <- matrix(1:64, ncol=8)
cluster <- c(1, 1, 1, 1, 2, 2, 3, 3)
lower.tri.vector(m, cluster)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

