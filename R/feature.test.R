feature.test <- function(x, y, B=100,
                         type.measure="mae", s="lambda.min",
                         keeplambda=FALSE,
                         control=list(trace=FALSE, maxcores=28), ...) {

    require(parallel)
    require(glmnet)
    
    # Parse the control options
    con <- list(trace=FALSE)
    con[names(control)] <- control
    control <- con

    sfixed <- ifelse(is.numeric(s), TRUE, FALSE)
    lambda <- NA

    # Starts by scaling both the X and the Y variable
    # This should save us a wee bit of speed later on
    x <- scale(x)
    y <- scale(y)

    # Analysis of the original data
    # We assume that the intercept is automatically in the model
    if (sfixed) {        
        o <- glmnet(x, y, standardize=FALSE, ...)
        lambda <- s
    } else {
        o <- cv.glmnet(x, y, standardize=FALSE, type.measure=type.measure, ...)
        lambda <- as.numeric(o[s])
    }

    o.select <- which(coef(o, s=lambda)[-1] != 0)
    o.sorted <- sort(abs((coef(o, s=lambda)[-1])[o.select]), decreasing=TRUE)
    # Fix the order
    o.select <- o.select[order(abs((coef(o, s=lambda)[-1])[o.select]), decreasing=TRUE)]

    result <- NA
    
    if (length(o.sorted)>0) {
        simnull <- mclapply(1:B, function(iii) {
            py <- sample(y)

            if (keeplambda) {
                to <- glmnet(x, py, standardize=FALSE, ...)
                to.select <- which(coef(to, s=lambda)[-1] != 0)
                to.sorted <- sort(abs((coef(to, s=lambda)[-1])[to.select]), decreasing=TRUE)

            } else {
                to <- cv.glmnet(x, py, standardize=FALSE, type.measure=type.measure, ...)
                lambda <- as.numeric(to[s])
                to.select <- which(coef(to, s=lambda)[-1] != 0)
                to.sorted <- sort(abs((coef(to, s=lambda)[-1])[to.select]), decreasing=TRUE)
            }
            to.sorted
        }, mc.cores = max(getOption("mc.cores", 2L), control$maxcores))
        
        # Number of variables identified by the original dataset
        nvar <- length(o.sorted)
        suppressWarnings(resmatrix <- do.call(rbind, lapply(simnull, `[`, seq_len(nvar))))
        resmatrix[is.na(resmatrix)] <- 0

        # Now compute the p-value for each variable
        
        final <- scale(resmatrix, scale=FALSE, center=o.sorted)
        final <- (final>=0)
        result <- colSums(final) / nrow(final)
    } else {
        o.sorted <- NA
        o.select <- NA
    }

    if (control$trace) {
    }
    
    list(variables = o.select,
         p=result,
         coef=o.sorted)
}


