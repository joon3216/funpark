
dpmf <- function(x, pmf_vec, support_vec) {

    M <- length(pmf_vec)
    if (missing(support_vec)) {
        names(pmf_vec) <- 0:(M - 1)
    } else {
        names(pmf_vec) <- support_vec
    }
    sapply(
        x,
        function(d) {
            if (d %in% names(pmf_vec)) {
                unname(pmf_vec[as.character(d)])
            } else {
                0
            }
        }
    )
}

rpmf <- function(n, pmf, support, ...) {

    cdf <- c(0, cumsum(pmf(support, ...)))
    unif_01 <- runif(n)
    result <- numeric(length = n)
    for (k in 1:n) {
        for (j in 1:(length(cdf) - 1)) {
            if (I(unif_01[k] >= cdf[j] & unif_01[k] < cdf[j + 1])) {
                result[k] <- support[j]
            }
        }
    }
    result
}



