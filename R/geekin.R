geekin <- function(formula,
                   family=gaussian, 
                   data,
                   weights,
                   subset,
                   id,
                   na.action,
                   control,
                   varlist,
                   ...
                   ) {

  if (is.matrix(varlist))
    varlist <- list(varlist)
  if (!is.list(varlist))
    stop("varlist must be a list")

  
  Call <- match.call()
#  method <- match.arg(method)

  glmcall <- Call

  glmcall$id <- NULL
  glmcall$varlist <- NULL
  glmcall[[1]] <- as.name("glm")

#  print(glmcall)

  glmFit <- eval(glmcall, parent.frame())

 # print(formula(glmFit))

#  print(formula(paste(deparse(formula(glmFit)), substitute(id), sep="+", collapse="")))

#  print("IUOIUOIU")
#  glmcall$formula <- formula(paste(deparse(formula(glmFit)), substitute(id), sep="+", collapse=""))
#    print("OIUOIU")
#  glmFit2 <- eval(glmcall, parent.frame())
#
#  print("OIUOIU")
#  print(formula(glmFit2))


  # Extract the dataset that is relevant for our analysis
#  realdata <- model.frame(glmFit2)

#  print(head(realdata))
  
  # Extract clusters from formula
  # Can only accept ONE simple random effect
  modterm <- attr(terms(formula), "term.labels")
  clusters <- modterm[grep("\\|", attr(terms(formula), "term.labels"))]
  if (length(clusters) != 1) {
#    stop("must provide exactly one random effect that determines the clusters")
  }
  if (length(grep("^1 \\| ", clusters))!=1) {
#    stop("only accepts a simple random effect to identify the clusters")
  }

 # cat("Hertil")
 # print(clusters)

  # Extract the cluster name
#  id <- sub("1 \\| ", "", clusters)

#  id <- clusters
  id <- id

  # Eval det rigtige sted

  # Check subset på variansmatricerne
 
#  print(get(id))
#  print(id)
  
  # Check that data are in the correct order
  if (!ordered.clusters(id)) {
    stop("the clusters must appear as contiguous blocks")
  }

#  print("hej")
  # for each element k in the varlist
  varelements <- lapply(varlist, function(x) {
    lower.tri.vector(x, cluster=id, diag=FALSE)
  })

#  print(varelements)



  geecall <- Call

  geecall$varlist <- NULL
  geecall$zcor <- do.call("cbind", varelements)
  geecall$corstr <- "userdefined"
  geecall[[1]] <- as.name("geeglm")

#  print(geecall)

  geeFit <- eval(geecall, parent.frame())

  geeFit
#  geeglm(formula, family=family, id=id, data=data,
#         zcor=varelements)
}

         
  
