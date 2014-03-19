\name{rmvtnorm.pedigree}
\alias{rmvtnorm.pedigree}
\title{Simulate residual multivariate Gaussian data from a polygenic model}
\description{
Simulates residual multivariate Gaussian response data from a pedigree where the additive genetic, 
dominance genetic, and shared environmental effects are taken into account.
}
\usage{
rmvtnorm.pedigree(n=1, pedigree, h2 = 0, c2 = 0, d2 = 0)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{numeric. The number of simulations to generate}
  \item{pedigree}{a \code{pedigree} object}
  \item{h2}{numeric. The heritability}
  \item{c2}{numeric. The environmentability}
  \item{d2}{numeric. The dominance deviance effect}
}
\details{
The three parameters should have a sum: h2+c2+d2 that is less than 1. The total variance is set to 1, and the mean is zero.
}
\value{Returns a matrix with the simulated values with n columns (one for each simulation) and each row matches the corresponding individual from the pedigree
}
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
library(mvtnorm)
mydata <- data.frame(id=1:5, 
                     dadid=c(NA, NA, 1, 1, 1), 
                     momid=c(NA, NA, 2, 2, 2), 
                     sex=c("male", "female", "male", "male", "male"), 
                     famid=c(1,1,1,1,1))
relation <- data.frame(id1=c(3), id2=c(4), famid=c(1), code=c(1))
ped <- pedigree(id=mydata$id, dadid=mydata$dadid, momid=mydata$momid, 
                sex=mydata$sex, relation=relation)
rmvtnorm.pedigree(2, ped, h2=.25)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }
