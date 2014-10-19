\name{adaptive.weights}
\alias{adaptive.weights}
\title{Compute weights for use with adaptive lasso.}
\description{
Fast computation of weights needed for adaptive lasso based on Gaussian family data.
}
\usage{
adaptive.weights(x, y, nu = 1, weight.method = c("multivariate", "univariate"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{input matrix, of dimension nobs x nvars; each row is an observation vector.}
  \item{y}{response variable.}
  \item{nu}{non-negative tuning parameter
}
  \item{weight.method}{Should the weights be computed for multivariate regression model (only possible when the number of observations is larger than the number of parameters) or by individual marginal/"univariate" regression coefficients.
}
}
\details{
The weights returned are 1/abs(beta_hat)^nu where the beta-parameters are estimated from the corresponding linear model (either multivariate or univariate).
}
\value{
Returns a list with two elements:
 \item{weights }{the computed weights}
 \item{nu }{the value of nu used for the computations}
}
\references{
Xou, H (2006). The Adaptive Lasso and Its Oracle Properties. JASA, Vol. 101.
}
\author{
Claus Ekstrom \email{claus@rprimer.dk}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{glmnet}
}
\examples{
library(glmnet)
set.seed(1)
x <- matrix(rnorm(50000), nrow=50)
y <- rnorm(50, mean=x[,1])
weights <- adaptive.weights(x, y)
glmnet(x, y, penalty.factor=weights$weights)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
