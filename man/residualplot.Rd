\name{residualplot}
\alias{residualplot}
\alias{residualplot.lm}
\alias{residualplot.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots a standardaized residual}
\description{
  Plots a standardized residual plot from an lm object and provides
  additional graphics to help evaluate the variance homogeneity and mean.
}
\usage{
residualplot(x, y, candy = TRUE, bandwidth = 0.3, 
             xlab = "Fitted values", ylab = "Std.res.", 
             col.sd="blue", col.alpha=0.3, ...)
\method{residualplot}{lm}(x, y, candy = TRUE, bandwidth = 0.3, 
        xlab = "Fitted values", ylab = "Stud.res.", 
        col.sd="blue", col.alpha=0.3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{lm object or a numeric vector}
  \item{y}{numeric vector for the y axis values}
  \item{candy}{logical. Should a lowess curve and local standard deviation of the residual be added to the plot. Defaults to \code{TRUE}}	
  \item{bandwidth}{The width of the window used to calculate the local
    smoothed version of the mean and the variance. Value should be
    between 0 and 1 and determines the percentage of the window width
    used}
  \item{xlab}{x axis label}
  \item{ylab}{y axis label}
  \item{col.sd}{color for the background residual deviation}
  \item{col.alpha}{number between 0 and 1 determining the transprency of the standard deviation plotting color}
  \item{...}{Other arguments passed to the plot function}
}
\details{
  Plots a standardized residual plot from an lm object and provides
  additional graphics to help evaluate the variance homogeneity and
  mean.

  The brown area is a smoothed estimate of 1.96*SD of the standardized
  residuals in a window around the predicted value. The brown area
  should largely be rectangular if the standardized residuals have more
  or less the same variance.

  The dashed line shows the smoothed mean of the standardized residuals
  and should generally follow the horizontal line through (0,0).
}
\value{
  Produces a standardized residual plot
}
\author{Claus Ekstrom <claus@rprimer.dk>}
%\note{
%}
\seealso{\code{\link{rstandard}}, \code{\link{predict}}}
\examples{
# Linear regression example
data(trees)
model <- lm(Volume ~ Girth + Height, data=trees)
residualplot(model)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
