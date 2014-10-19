\name{superroot2}
\alias{superroot2}
\docType{data}
\title{Gene expression data from two-color dye-swap experiment}
\description{
  Gene expression levels from two-color dye-swap experiment on 6
  microarrays. Arrays 1 and 2 represent the first biological sample (ie,
  the first dye swap), 3 and 4 the second, and arrays 5 and 6 the third.
}
\usage{data(superroot2)}
\format{
  A data frame with 258000 observations on the following 5 variables.
  \describe{
    \item{\code{color}}{a factor with levels \code{green} \code{red}
      representing the dye used for the gene expression}
    \item{\code{array}}{a factor with levels \code{1} \code{2} \code{3}
      \code{4} \code{5} \code{6} corresponding to the 6 arrays}
    \item{\code{gene}}{a factor with 21500 levels representing the genes
    on the arrays}
    \item{\code{plant}}{a factor with levels \code{rnt} \code{wt} for
      the two types of plants: runts and wild type}
    \item{\code{signal}}{a numeric vector with the gene expression level
    (normalized but not log transformed)}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Data provided by Soren Bak <bak@life.ku.dk>. \cr
Added by Claus Ekstrom <ekstrom@life.ku.dk> 
}
\references{
Morant, M. et al. (2010). Metabolomic, Transcriptional, Hormonal and
Signaling Cross-Talk in Superroot2. \emph{Molecular Plant}. 3, p.192--211.  
}
\examples{
data(superroot2)
# Select one gene
g1 <- superroot2[superroot2$gene=="AT2G24000.1",]
model <- lm(log(signal) ~ plant + color + array, data=g1)
summary(model)
}
\keyword{datasets}
