\name{soccer}
\alias{soccer}
\docType{data}
\title{Danish national soccer players}
\description{Players on the Danish national soccer team. The dataset consists of all players who have played at least one match on the men's senior A-team, their position, date-of-birth, goals and matches.
}
\usage{data(soccer)}
\format{
  A data frame with 773 observations on the following 7 variables.
  \describe{
    \item{\code{name}}{a factor with names of the players}
    \item{\code{DoB}}{a Date. The date-of-birth of the player}
    \item{\code{position}}{a factor with levels \code{ } \code{Forward} \code{Defender} \code{Futsal} \code{Midfielder} \code{Goalkeeper}}
    \item{\code{A.matches}}{a numeric vector. The number of A matches played by the player}
    \item{\code{A.goals}}{a numeric vector. The number of goals scored by the player in A matches}
    \item{\code{matches}}{a numeric vector. The number of matches played by the player on national soccer team}
    \item{\code{goals}}{a numeric vector. The number of goals scored on the national soccer team - not just A matches}
  }
}
\source{
Data collected from the player database of DBU on July 29th, 2013. See \url{http://www.dbu.dk} for more information.
}
\examples{
data(soccer)

birthmonth <- as.numeric(format(soccer$DoB, "\%m"))
birthyear <- as.numeric(format(soccer$DoB, "\%Y"))

# Select players from the national team
soc <- soccer[soccer$A.matches>0,]
soc <- soc[! (soc$position \%in\% c("", "Futsal")),]
}
\keyword{datasets}
