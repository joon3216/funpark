
#' dpmf
#'
#' Create a probbility mass function.
#'
#' @usage dpmf(x, pmf_vec, support_vec)
#' @param x A number, or a vector whose class is the same as \code{support_vec}
#' @param pmf_vec A numeric vector, where each element falls in [0, 1] and sums up to 1
#' @param support_vec A vector with the same length as \code{pmf_vec} where each entry of \code{support_vec} is the input that corresponds to the probability in \code{pmf_vec}; if missing, it is replaced with \code{0:(length(pmf_vec) - 1)}.
#' @author Junkyu Park
#' @seealso \code{\link{rpmf}}
#' @examples
#' # Example 1: dfruit
#' dfruit <- function(x){dpmf(x, c(.25, .4, .35), c('apple', 'orange', 'neither'))}
#' dfruit(c('apple', 'neither', 'pineapple'))
#' # Example 2: dbinom with size = 2 and prob = .5
#' dbinom2 <- function(x){dpmf(x, c(.25, .5, .25), 0:2)}
#' dbinom2(-2:3)
#' @export
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



#' rpmf
#'
#' Draw random samples from a probability mass function using the \href{https://en.wikipedia.org/wiki/Inverse_transform_sampling}{inverse transform sampling}.
#'
#' @usage rpmf(n, pmf, support, ...)
#' @param n An integer >= 1.
#' @param pmf A probability mass function which takes elements of an ambient space of pmf's support (i.e. an input that makes pmf return a probability) in its first argument. This pmf has a finite \code{support}, and must be able to take an array of elements and return an array of probabilities. Each output of this \code{pmf} must fall in [0, 1], and every conceivable output, including 0, must sum up to 1.
#' @param support A vector which consists of the elements of the support of \code{pmf}, i.e. \code{sum(pmf(support, ...)) == 1 && !(0 \%in\% pmf(support, ...))}
#' @param \dots An additional argument of \code{pmf}
#' @author Junkyu Park
#' @seealso \code{\link{dpmf}}
#' @examples
#' # Example: dfruit in dpmf example
#' # Generate 12 random samples from dfruit
#' rpmf(12, dfruit, c('apple', 'orange', 'neither'))
#' @export
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



