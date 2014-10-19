\name{soccer}
\alias{soccer}
\docType{data}
\title{Danish national soccer players}
\description{Players on the Danish national soccer team. The dataset consists of all players who have been picked to play on the men's senior A-team, their position, date-of-birth, goals and matches.
}
\usage{data(soccer)}
\format{
  A data frame with 805 observations on the following 5 variables.
  \describe{
    \item{\code{name}}{a factor with names of the players}
    \item{\code{DoB}}{a Date. The date-of-birth of the player}
    \item{\code{position}}{a factor with levels \code{Forward} \code{Defender} \code{Midfielder} \code{Goalkeeper}}
    \item{\code{matches}}{a numeric vector. The number of A matches played by the player}
    \item{\code{goals}}{a numeric vector. The number of goals scored by the player in A matches}
  }
}
\source{
Data collected from the player database of DBU on March 21st, 2014. See \url{http://www.dbu.dk} for more information.
}
\examples{
data(soccer)

birthmonth <- as.numeric(format(soccer$DoB, "\%m"))
birthyear <- as.numeric(format(soccer$DoB, "\%Y"))

}
\keyword{datasets}
