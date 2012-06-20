lower.tri.vector <- function(x, diag = FALSE, cluster=rep(1,nrow(x))) {

  if (!is.matrix(x) & nrow(x) != ncol(x))
    stop("Input must be a square matrix")

  # Simple check
  if (length(cluster) != nrow(x))
    stop("Length of cluster vector must match dimensions of matrix")

  # Remember to check new function on the ordering
  if (!ordered.clusters(cluster))
    stop("the cluster elements should be in contiguous blocks")
    
  unlist(lapply(unique(cluster), 
         function(id) { sel <- id==cluster ;
                        m <- x[sel,sel] ; 
                        as.vector(m[lower.tri(m, diag=diag)])
                      }
         )
  )
}
