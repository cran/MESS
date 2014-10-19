\name{power.mcnemar.test}
\alias{power.mcnemar.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Power Calculations for Exact and Asymptotic McNemar Test in a 2 by 2 table}
\description{
Compute power of test, or determine parameters to obtain target power for matched case-control studies.
}
\usage{
power.mcnemar.test(n = NULL, paid = NULL, psi = NULL, sig.level = 0.05, power = NULL, 
                   alternative = c("two.sided", "one.sided"), 
                   method = c("normal", "exact"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of observations (number of pairs)
}
  \item{paid}{The probability that a case patient is not exposed and that the corresponding control patient was exposed}
  \item{psi}{The odds ratio for exposure in case and control individuals}
  \item{sig.level}{Significance level (Type I error probability)}
  \item{power}{Power of test (1 minus Type II error probability)}
  \item{alternative}{One- or two-sided test}
  \item{method}{Power calculations based on exact or asymptotic test}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Object of class \code{power.htest}, a list of the arguments (including the computed one) augmented with method and note elements.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Duffy, S (1984). Asymptotic and Exact Power for the McNemar Test and its Analogue with R Controls per Case
}
\author{
Claus Ekstrom \email{claus@rprimer.dk}
}
\note{
  \code{uniroot} is used to solve power equation for unknowns, so
  you may see errors from it, notably about inability to bracket the
  root when invalid arguments are given.
}
\seealso{\code{\link{mcnemar.test}}}
\examples{
power.mcnemar.test(n=NULL, paid=.1, psi=2, power=.8, method="normal")
power.mcnemar.test(n=NULL, paid=.1, psi=2, power=.8)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
