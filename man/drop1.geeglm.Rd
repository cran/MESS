\name{drop1.geeglm}
\alias{drop1.geeglm}
\title{Drop All Possible Single Terms to a geeglm Model Using Wald Test}
\description{
Compute all the single terms in the scope argument that can dropped from the model, and compute a table of the corresponding Wald test statistics.
}
\usage{
\method{drop1}{geeglm}(object, scope, test = c("Wald", "none"), 
method = c("robust", "naive", "sandwich"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a fitted object of class geese.}
  \item{scope}{
%%     ~~Describe \code{scope} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{method}{Indicates which method is used for computing the standard error. \code{robust} is the default and corresponds to the modified sandwich estimator. \code{naive} is the classical naive cariance estimate. \code{sandwich} is an alias for \code{robust}.}
  \item{\dots}{other arguments. Not currently used}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
An object of class "anova" summarizing the differences in fit between the models.
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
\code{\link{drop1}}, \code{geeglm}, \code{geese}
}
\examples{
library(geepack)
data(ohio)
fit <- geeglm(resp ~ age + smoke + age:smoke, id=id, data=ohio,
             family=binomial, corstr="exch", scale.fix=TRUE)
drop1(fit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }

