\name{ordered.clusters}
\alias{ordered.clusters}
\title{Check if unique elements of a vector appear in contiguous clusters}
\description{
\code{ordered.clusters} determines if identical elements of a vector appear in contiguous clusters, and returns \code{TRUE} if the do and \code{FALSE} otherwise.
}
\usage{
ordered.clusters(id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{a vector}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The function returns TRUE if the elements appear in contiguous clusters and FALSE otherwise}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Claus Ekstrom \email{claus@ekstroem.dk} with suggestions from Peter Dalgaard.
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{duplicated}}
}
\examples{
x <- c(1, 1, 1, 2, 2, 3, 4, 1, 5, 5, 5)
ordered.clusters(x)
ordered.clusters(sort(x))
ordered.clusters(x[order(x)])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
