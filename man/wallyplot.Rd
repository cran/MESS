\name{wallyplot}
\alias{wallyplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a Wally plot}
\description{
  Produces a 3x3 grid of residual- or qq-plots plots from a lm
  object. One of the nine subfigures is the true residual plot/qqplot while
  the remaining are plots that fulfill the assumptions of the linear model
}
\usage{
wallyplot(x, y = x, type = c("residualplot", "qqnorm"), 
          hide = TRUE, candy = TRUE, mar = c(4, 4, 0.1, 0.1) + 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of x values, or an lm object.}
  \item{y}{a numeric vector of y values of the same length as x.}
  \item{type}{either the character string "residualplot" (the default)
    or "qqnorm". This determines the type of plot that is produced.
}
\item{hide}{
  logical; if \code{TRUE} (the default) then the identity of the true
  residual plot is hidden until the user presses a key. If \code{FALSE}
  then the true residual plot is shown in the center.
}
\item{candy}{
  logical; if \code{TRUE} (the default) then local pointwise 95\% prediction
  intervals are added to the residual plots, and the identity line is
  added to the qqnorm plots.
}
\item{mar}{A numerical vector of the form `c(bottom, left, top, right)'
          which gives the number of lines of margin to be passed on to
	  the par function
}
}
\details{
  User who look at residual plots or qqnorm plots for the first times
  often feel they lack the experience to determine if the residual plot
  is okay or if the model assumptions are indeed violated. One way to
  convey "experience" is to plot a series of residual- or qq-plots and
  see if the user can pinpoint one of them that looks like an
  odd-one-out. If the proper residual plot does not stand out then the
  assumptions are not likely to be violated.
  
  The wallyplot produces a 3x3 grid of residual- or qq-plots plots from
  a lm object or from a set of pairs of x and y values. One of the nine
  subfigures is the true residual plot/qqplot while the remaining are
  plots that fulfill the assumptions of the linear model. After the user
  interactively hits a key the correct residual plot (correponding to
  the provided data) is shown.
}
%\value{
%  The plot produced is the what is returned
%}
\references{
www.rprimer.dk
}
\author{
Claus Ekstrom \email{claus@rprimer.dk}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
data(trees)
model <- lm(Volume ~ Height + Girth, data=trees)
wallyplot(model)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ iplot }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
