\name{greenland}
\alias{greenland}
\docType{data}
\title{
Average yearly summer air temperature for Tasiilaq, Greenland
}
\description{
Average yearly summer (June, July, August) air temperature for Tasiilaq, Greenland
}
\usage{data(greenland)}
\format{
  A data frame with 51 observations on the following 2 variables.
  \describe{
    \item{\code{year}}{year}
    \item{\code{airtemp}}{average air temperature (degrees Celcius)}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
  Data provided by Sebastian Mernild.\cr
  Originally obtained from http://www.dmi.dk/dmi/index/gronland/vejrarkiv-gl.htm. \cr
  Added by Claus Ekstrom <ekstrom@life.ku.dk>  
}
\references{
  Aktuelt Naturvidenskab september 2010. \cr
  http://aktuelnaturvidenskab.dk/fileadmin/an/nr-4/an4_2010gletscher.pdf    
}
\examples{
data(greenland)
model <- lm(airtemp ~ year, data=greenland)
plot(greenland$year, greenland$airtemp, xlab="Year", ylab="Air temperature")
abline(model, col="red")
}
\keyword{datasets}
