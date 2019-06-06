
dpmf <- function(x, pmf_vec, support_vec) {
    # x: a number, or a vector whose class is the same as support_vec.
    # pmf_vec: a numeric vector, where all elements fall into (0, 1) and 
    #          sum up to 1.
    # support_vec: a vector with the same length as pmf_vec where
    #              each entry of support_vec is the input that corresponds to
    #              the probability in pmf_vec; if missing, it is replaced 
    #              with 0:(length(pmf_vec) - 1).
    
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
    # n: an integer
    # pmf, support: the same as pmf_vec and support_vec in dpmf respectively
    # ...: additional arguments of pmf
    
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



