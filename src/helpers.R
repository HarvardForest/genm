
### Creates a symmetric matrix comprised of 
### the sum of the upper and lower triangles.
### MKLau - 06July2016

symSum <- function(x='matrix',zero.diag=TRUE){
    if (zero.diag == TRUE){diag(x) <- 0}
    sum.lu <- t(x)[lower.tri(x)]  + x[lower.tri(x)]
    x[lower.tri(x)] <- sum.lu
    x <- t(x)
    x[lower.tri(x)] <- sum.lu
    if (isSymmetric(x)){x}else{
        warning('Output matrix is not symmetirc.')
    }
}

### 
altDiff <- function(x){x[2] - x[1]}

###