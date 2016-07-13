
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
NED.plot <- function(NED){plot(NED, xlab="x coordinate (m)", ylab="y coordinate (m)",
                               legend.lab="Altitude (masl)")}
p.points <- function(gspecies){points(gspecies, col="black", pch=20, cex=.30)}

###
m <- function(scd){(1-(scd/max(scd)))}

###



###