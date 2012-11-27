wallyplot <- function(x, y=x,
                  type=c("residualplot", "qqnorm"),
                  hide=TRUE,
                  candy=TRUE, 
                  mar=c(4, 4, .1, .1)+.1) {
  
  # Allow for input of an lm object
  if (inherits(x, "lm")) {
    y <- rstandard(x)
    x <- predict(x)
  }
  else {
    if (missing(y))
      type <- "qqnorm"
  }

  if (!is.numeric(x) && !is.numeric(y)) {
    stop("Must have a pair of numeric vectors or an lm object as input")
  }

  # What type of picture is desired
  type <- match.arg(type)

  
  cc <- complete.cases(x, y)
  x <- x[cc]
  y <- y[cc]

  oldpar <- par(no.readonly = TRUE)
  par(mar=mar)
  
  resplot <- function(x, y, ylim, candy, bandwidth=.3) {
    if (candy)
      plot(x, y, xlab = "Fitted values", ylab = "Std.res.", ylim=ylim, pch=1+15*(abs(y)>1.96))
    else
      plot(x, y, xlab = "Fitted values", ylab = "Std.res.", ylim=ylim)
    
    if (candy) {
      uniqx <- sort(unique(x))
      if (length(uniqx) > 3) {
        lines(smooth.spline(x, y, df = 3), lty = 2, lwd = 2, col = "black")
      }

      window <- bandwidth * (max(x) - min(x))/2
      vary <- length(uniqx)
      for (i in 1:length(uniqx)) {
        vary[i] <- 1.96 * sd(y[abs(x - uniqx[i]) <= window])
      }
      vary[is.na(vary)] <- 0
      color <- rgb(0, 0, 100, 50, maxColorValue = 255)
      polygon(c(uniqx, rev(uniqx)), c(vary, -(rev(vary))), col = color, 
              border = NA)      
    }
    return(invisible(NULL))
  }
  
  pos <- c(1:4,9,5:8)
  if (hide)
    pos <- sample(pos)
  
  # layout(matrix(pos, 3, 3))
  par(mfrow=c(3,3))
  
  newy <- matrix(c(rnorm(length(x)*8), y), ncol=9)
  
  for (i in 1:length(pos)) {
    switch(type,
           residualplot = resplot(x, newy[,pos[i]], ylim=range(newy), candy=candy),
           qqnorm = qqnorm(newy[,pos[i]], ylim=range(newy), main=""))
    if (type=="qqnorm" & candy) {
      abline(a=0,b=1)
    }
  }
  
  if (hide) {
    readline("Hit <Enter> to show the original residual plot. ")
  }

  figpos <- order(pos)[9]
  par(mfg=c(figpos %/% 3.1 + 1, figpos - (figpos %/% 3.1)*3 ))
  box(col="red", lwd=2)

  par(oldpar)

  invisible(NULL)

}
