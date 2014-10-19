\name{desctable}
\alias{desctable}
\alias{desctable.character}
\alias{desctable.factor}
\alias{desctable.default}
\alias{desctable.data.frame}
\alias{desctable.integer}
\alias{desctable.numeric}
\title{
Create a descriptive table of summaries
}
\description{
Creates a descriptive table of summary statistics
}
\usage{
desctable(object, group = NULL, proportions = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object for which a descriptive summary table is desired}
  \item{group}{A factor the same length as object (or the elements of object if that is a data frame) that is used to split up the summary for each level of group. If set to NULL (the default) then every observation is assumed to be in one group}
  \item{proportions}{Logical. Should proportions be listed for categorical data (defaults to FALSE)}
  \item{\dots}{any other arguments passed of functions
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with length the same as object with items
\item{output }{Output for use with the descriptive table}
\item{p }{p-value for testing equality across groups (set to NULL if groups=NULL)}
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

\seealso{
\code{\link{summary}}
}
\examples{
desctable(ToothGrowth, ToothGrowth$supp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
