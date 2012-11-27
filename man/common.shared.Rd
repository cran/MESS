\name{common.shared}
\alias{common.shared}
\alias{common.shared.pedigree}
\alias{common.shared.pedigreeList}
\title{Compute a common shared environment matrix}
\description{
Compute the common shared environment matrix for a set of related 
subjects. The function is generic, and can accept a pedigree, or
pedigreeList as the first argument.
}
\usage{
common.shared(id, ...)
}
\arguments{
  \item{id}{either a pedigree object or pedigreeList object
}
  \item{\dots}{Any number of optional arguments. Not used at the moment}
}
\details{
     When called with a pedigreeList, i.e., with multiple families, the
     routine will create a block-diagonal-symmetric `bdsmatrix' object.
     Since the [i,j] value of the result is 0 for any two unrelated
     individuals i and j and a `bdsmatix' utilizes sparse
     representation, the resulting object is often orders of magnitude
     smaller than an ordinary matrix.  When called with a single
     pedigree and ordinary matrix is returned.
}
\value{a matrix of shared environment coefficients}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Claus Ekstrom \email{claus@rprimer.dk}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{pedigree}, \code{kinship}, 
}
\examples{
library(kinship2)
test1 <- data.frame(id  =c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                    mom =c(0, 0, 0, 0, 2, 2, 4, 4, 6,  2,  0,  0, 12, 13),
                    dad =c(0, 0, 0, 0, 1, 1, 3, 3, 3,  7,  0,  0, 11, 10),
                    sex =c(0, 1, 0, 1, 0, 1, 0, 1, 0,  0,  0,  1,  1,  1))
tped <- with(test1, pedigree(id, dad, mom, sex))
common.shared(tped)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }

