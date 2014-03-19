\name{segregate.genes}
\alias{segregate.genes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Segregate genes through a pedigree}
\description{
Segregate di-allelic genes down through the generations of a pedigree. It is assumed that the founders are independent and that 
the genes are in Hardy Weinberg equilibrium in the population.}
\usage{
segregate.genes(pedigree, maf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pedigree}{a \code{pedigree} object}
  \item{maf}{a vector of minor allele frequencies for each diallelic gene to segregate through the pedigree}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns a data frame. Each row matches the order of the individuals in the pedigree and 
each column corresponds to each of the segregated genes. The data frame contains values 0, 1, or 2 
corresponding to the number of copies of the minor allele frequency allele that person has.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Claus Ekstrom \email{claus@rprimer.dk}
}
\seealso{
\code{pedigree}, \code{kinship},
}
\examples{
library(kinship2)
mydata <- data.frame(id=1:5, 
                     dadid=c(NA, NA, 1, 1, 1), 
                     momid=c(NA, NA, 2, 2, 2), 
                     sex=c("male", "female", "male", "male", "male"), 
                     famid=c(1,1,1,1,1))
relation <- data.frame(id1=c(3), id2=c(4), famid=c(1), code=c(1))
ped <- pedigree(id=mydata$id, dadid=mydata$dadid, momid=mydata$momid, 
                sex=mydata$sex, relation=relation)
segregate.genes(ped, c(.1, .3, .5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }
