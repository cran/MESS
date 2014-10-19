\name{bees}
\alias{bees}
\docType{data}
\title{
Bee data. Number of different types of bees caught.}
\description{
Number of different types of bees caught in plates of different
colours. There are four locations and within each location there are
three replicates consisting of three plates of the three different
colours (yellow, white and blue). Data are collected at 5 different
dates over the summer season. Only data from one date available until
data has been published.}
\usage{data(bees)}
\format{
  A data frame with 72 observations on the following 7 variables.
  \describe{
    \item{\code{Locality}}{a factor with levels \code{Havreholm} \code{Kragevig} \code{Saltrup} \code{Svaerdborg}. Four different localities in Denmark.}
    \item{\code{Replicate}}{a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{Color}}{a factor with levels \code{Blue} \code{White} \code{Yellow}. Colour of  plates}
    \item{\code{Time}}{a factor with levels \code{july1} \code{july14}
      \code{june17} \code{june3} \code{june6}. Data collected at
      different dates in summer season. Only one day is present in the
      current data frame until the full data has been released.}
    \item{\code{Type}}{a factor with levels \code{Bumblebees} \code{Solitary}. Type of bee.}
    \item{\code{Number}}{a numeric vector. The response variable with number of bees catched.}
    \item{\code{id}}{a numeric vector. The id of the clusters (each containg three plates).}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Data were kindly provided by Casper Ingerslev Henriksen, Department of
Agricultural Sciences, KU-LIFE.
Added by Torben Martinussen <tma@life.ku.dk>
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(bees)
model <- glm(Number ~ Locality + Type*Color,
             family=poisson, data=bees)
}
\keyword{datasets}
