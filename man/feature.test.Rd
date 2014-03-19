\name{feature.test}
\alias{feature.test}
\title{Inference for features identified by the Lasso}
\description{Performs randomization tests of features identified by the Lasso}
\usage{
feature.test(x, y, B = 100, type.measure = "mae", s = "lambda.min", keeplambda = FALSE,
             control = list(trace = FALSE, maxcores = 28), ...)
}
\arguments{
  \item{x}{input matrix, of dimension nobs x nvars; each row is an observation vector.}
  \item{y}{quantitative response variable of length nobs}
  \item{B}{The number of randomizations used in the computations}
  \item{type.measure}{loss to use for cross-validation. See \code{cv.glmnet} for more information}
  \item{s}{Value of the penalty parameter 'lambda' at which
          predictions are required. Default is the entire sequence used
          to create the model. See \code{coef.glmnet} for more information}
  \item{keeplambda}{Logical. If set to \code{TRUE} then the estimated lambda from cross validation from the original dataset is kept and used for evaluation in the subsequent randomization datasets. This reduces computation time substantially as it is not necessary to perform cross validation for each randomization. Defaults to \code{FALSE}}
  \item{control}{A list of options that control the algorithm. Currently \code{trace} is a logical and if set to \code{TRUE} then the function produces more output. \code{maxcores} sets the maximum number of cores to use with the \code{parallel} package}
  \item{\dots}{Other arguments passed to \code{glmnet}}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{ Returns a list of 3 variables:
  \item{variables }{A vector of the indices of the non-zero variables selected by \code{glmnet} sorted from (numerically) highest to lowest estimated coefficient.}
  \item{p }{A vector of corresponding p-values}
  \item{coef }{A vector of corresponding estimated coefficients}
}
\references{
Brink-Jensen, K and Ekstrom, CT 2014. \emph{Inference for feature selection using the Lasso with high-dimensional data}. \url{http://arxiv.org/abs/1403.4296}
}
\author{
Claus Ekstrom \email{ekstrom@sund.ku.dk} and Kasper Brink-Jensen \email{kbrink@life.ku.dk}
}

\seealso{
\code{glmnet}
}
\examples{

# Simulate some data
x <- matrix(rnorm(30*100), nrow=30)
y <- rnorm(30, mean=1*x[,1])

# Make inference for features
\dontrun{feature.test(x, y)}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ ~htests }
