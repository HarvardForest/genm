complicated <- matrix(1:8, nrow=4, ncol=4)
diag(complicated) <- 0

m = complicated

m[upper.tri(m)] <- (complicated[upper.tri(complicated)] + complicated[lower.tri(complicated)])
m[lower.tri(m)] <- (t(complicated)[upper.tri(complicated)] + t(complicated)[lower.tri(complicated)])
m
