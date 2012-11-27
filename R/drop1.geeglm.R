drop1.geeglm <- function(object, scope, test = c("Wald", "none"),
                         method=c("robust", "naive", "sandwich"), ...) {
  test <- match.arg(test)
  method <- match.arg(method)

  if (! ("geeglm" %in% class(object)) ) {
    stop("Presently wald.test only works for geeglm objects")
  }

  x <- model.matrix(object)
  n <- nrow(x)
  asgn <- attr(x, "assign")
  
  tl <- attr(object$terms, "term.labels")
  if (missing(scope)) {
    scope <- drop.scope(object)
  }
  else {
    if (!is.character(scope)) 
      scope <- attr(terms(update.formula(object, scope)), 
                    "term.labels")
    if (!all(match(scope, tl, 0L) > 0L)) 
      stop("scope is not a subset of term labels")
  }

  ndrop <- match(scope, tl)
  ns <- length(scope)
  
  score <- numeric(ns)
  dfs <- numeric(ns)
  pvals <- numeric(ns)
  
  for (i in 1L:ns) {
    ii <- seq_along(asgn)[asgn == ndrop[i]]
    param <- coef(object)[ii]
    varmat <- switch(method,
                     robust = object$geese$vbeta[ii,ii],
                     sandwich = object$geese$vbeta[ii,ii],
                     naive = object$geese$vbeta.naiv[ii,ii]
                     )
    score[i] <- as.numeric(param %*% solve(varmat) %*% param)
    dfs[i] <- length(ii)
    pvals[i] <- pchisq(score[i], df=dfs[i], lower.tail=FALSE)
  }

  aod <- data.frame(DF=dfs, Wald=score, row.names=scope)
  if (test=="Wald")
    aod[,c("Pr(>Chi)")] <- list(pvals)

  head <- c("Single term deletions", "\nModel:", deparse(formula(object)))
  class(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- head
  aod
}

#coef.geese <- function(object, ...) {
#  if (! class(object) == "geese") 
#    stop("Must be an object of class geese")
#  object$beta
#}

#vcov.geese <- function(object, method=c("robust", "naive", "sandwich"), ...) {
#  if (! class(object) == "geese") 
#    stop("Must be an object of class geese")

#  method <- match.arg(method)

#  res <- switch(method, robust=object$vbeta, naive=object$vbeta.naiv, sandwich=object$vbeta)

#  rownames(res) <- names(object$beta)
#  colnames(res) <- names(object$beta)
#  res
#}

