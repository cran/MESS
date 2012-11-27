QIC.geeglm <- function(object, ...) {

  #
  # The majority of this code was taken from the internet
  #

  if (! ("geeglm" %in% class(object)) ) {
    stop("QIC requires a geeglm object as input")
  }

  # Setup functions
  invert <- if ("MASS" %in% loadedNamespaces()) 
    MASS:::ginv
  else solve
 
  # Missing:
  # Check correct handling of link and family functions

  # Create function to make the computations
  computeqic <- function(object) {
    # Fitted and observed values for quasi likelihood
    mu <- object$fitted.values
    y  <- object$y

    # Quasi Likelihood for Poisson
    # quasi.R <- sum((y*log(mu.R)) - mu.R) # poisson()$dev.resids - scale and weights = 1

    type <- family(object)$family
    quasi <- switch(type,
                    poisson = sum((y*log(mu)) - mu),
                    gaussian = sum(((y - mu)^2)/-2),
                    binomial = sum(y*log(mu/(1 - mu)) + log(1 - mu)),
                    Gamma = sum(-y/(mu - log(mu))),
                    stop("Error: distribution not recognized"))  

    # Fit model with independence correlation structure
    model.indep <- update(object, corstr="identity")
  
    # Trace term (penalty for model complexity)
    AIinverse <- invert(model.indep$geese$vbeta.naiv) 
    Vr <- object$geese$vbeta
    trace <- sum(diag(AIinverse %*% Vr))
    params <- length(mu) # Mean parameters in the model

    # QIC
    QIC <- -2*(quasi - trace)
    QICu <- -2*(quasi - params)
    output <- c(QIC, QICu, quasi, trace, params)
    names(output) <- c("QIC", "QICu", "Quasi Lik", "CIC", "params")
    output 
  }
  

  if (length(list(...))) {
    # Make the computations
    results <- lapply(list(object, ...), computeqic)

    # Check same data size
    check <- sapply(list(object, ...), function(x) {
      n <- length(x$y)
    })

    if (any(check != check[1]))
      warning("models are not all fitted to the same number of observations")

    # Merge the results together in a data.matrix
    res <- do.call("rbind", results)

    # Set the row names corresponding to the models
    Call <- match.call()
    Call$k <- NULL
    row.names(res) <- as.character(Call[-1L])
    res
  }
  else {
    computeqic(object)
  } 
}


QIC <- function(object, ...) {
  UseMethod("QIC")
}
