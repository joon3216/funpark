
#' csum_N
#'
#' Using the Discrete Fourier Transform, return the approximate pmf vector
#' of S = sum of iid X_i's, i = 1, ..., N, where N ~ Pois(lambda) and
#' N is independent of all X_i's.
#'
#' @usage csum_N(pmf, support, lambda, eps = 1e-05)
#' @param pmf A probability mass function of X_i which takes elements of
#'   an ambient space of pmf's support (i.e. an input that makes pmf return
#'   a probability) in its first argument. This pmf must have a finite
#'   \code{support}, and be able to take an array of elements and return
#'   an array of probabilities. Each output of this \code{pmf} must fall in
#'   [0, 1], and every conceivable output, including 0, must sum up to 1,
#'   i.e. if an object that is not an element of \code{support} is given,
#'   \code{pmf} must return 0.
#' @param support An integer vector from 0 to the largest element of the
#'   pmf's support. \code{support} (of X_i) consists of nonnegative integers.
#' @param lambda A number > 0.
#' @param eps A number in (0, 1); \code{1e-05} by default
#' @author Junkyu Park
#' @seealso
#'   \code{\link{dpmf}},
#'   \code{\link{rpmf}},
#'   \href{https://joon3216.github.io/research_materials/2018/pgf}{Evaluating a hard-to-evaluate pmf using pgf and DFT}
#' @examples
#' # Example 1: S_Y = sum of iid Y_i's, Y_i ~ dY
#' dY <- function(y) {
#'     sapply(
#'         y,
#'         function(d) {
#'             if (d %in% c(1, 4)) {
#'                 .25
#'             } else if (d == 2) {
#'                 .5
#'             } else {
#'                 0
#'             }
#'         }
#'     )
#' }
#' result_Y <- csum_N(dY, support = 0:4, lambda = 3)
#' # Example 2: S = sum of iid X_i's, X_i ~ dX
#' dX <- function(x) {
#'     sapply(
#'         x,
#'         function(d) {
#'             if (d == 0) {
#'                 .05
#'             } else if (d %in% c(1, 3, 4)) {
#'                 .1
#'             } else if (d == 2) {
#'                 .075
#'             } else if (d == 5) {
#'                 .575
#'             } else {
#'                 0
#'             }
#'         }
#'     )
#' }
#' result_X <- csum_N(dX, support = 0:5, lambda = 3)
#' @export
csum_N <- function(pmf, support, lambda, eps = 1e-05) {

    pmf_vec <- pmf(support)

    # Define the pgf of X_i
    g <- function(t) {sapply(t, function(d) {sum(d^support * pmf_vec)})}

    # Find M
    Ms <- function(t) {(-lambda * (1 - g(t)) - log(eps)) / log(t)}
    M <- ceiling(optimize(Ms, interval = c(1.001, 30000))$objective)

    # Append 0's
    pmf_vec <- c(pmf_vec, rep(0, M - length(pmf_vec)))

    # Apply DFT and inverse DFT
    gtks <- fft(pmf_vec)
    gS_gtks <- exp(-lambda * (1 - gtks))
    pS_tks <- Re(fft(gS_gtks, inv = T) / M)
    pS_tks
}



#' dpmf
#'
#' Create a probbility mass function.
#'
#' @usage dpmf(x, pmf_vec, support_vec)
#' @param x A number, or a vector whose class is the same as \code{support_vec}
#' @param pmf_vec A numeric vector, where each element falls in [0, 1] and
#'   sums up to 1
#' @param support_vec A vector with the same length as \code{pmf_vec} where
#'   each entry of \code{support_vec} is the input that corresponds to the
#'   probability in \code{pmf_vec}; if missing, it is replaced with
#'   \code{0:(length(pmf_vec) - 1)}.
#' @author Junkyu Park
#' @seealso
#'   \code{\link{rpmf}},
#'   \href{https://joon3216.github.io/research_materials/2018/pgf}{Evaluating a hard-to-evaluate pmf using pgf and DFT}
#' @examples
#' # Example 1: dfruit
#' dfruit <- function(x){
#'     unname(dpmf(x, c(.25, .4, .35), c('apple', 'orange', 'neither')))
#' }
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



#' histo
#'
#' Given one-dimensional data \code{dat} and bins \code{bins}, evaluate the
#' histogram function at \code{x}:
#' \deqn{\text{hist}(x) := \sum_{k = 1}^{m} \Big[ \frac{I(x \in B_k)}{n(u_k - u_{k - 1})} \sum_{i = 1}^{n} I(x_i \in B_k) \Big]}
#'
#' @usage histo(x, dat, bins)
#' @param x A number, or vector of numbers.
#' @param dat A vector of numbers.
#' @param bins A vector of numbers where \code{min(bins) <= min(dat)} and
#'   \code{max(bins) > max(dat)}.
#' @author Junkyu Park
#' @seealso
#'   \code{\link{csum_N}},
#'   \code{\link{dpmf}},
#'   \code{\link{rpmf}},
#'   \href{https://joon3216.github.io/research_materials/2018/pgf}{Evaluating a hard-to-evaluate pmf using pgf and DFT}
#' @examples
#' # Example: dX in csum_N
#' # Evaluate the histogram of 10000 samples of S = sum of iid X_i ~ dX
#' # at 0, 1, ..., M - 1 for some integer M, where the endpoints of bins are
#' # set to be seq(-.5, M - .5, by = 1).
#' result_X <- csum_N(dX, support = 0:5, lambda = 3)
#' M <- length(result_X)
#' samples_N <- rpmf(10000, dpmf, 0:(M - 1), result_X)
#' histo(0:(M - 1), samples_N, seq(-.5, M - .5, by = 1))
#' @export
histo <- function(x, dat, bins) {

    bins <- sort(bins)
    n <- length(dat)
    m <- length(bins) - 1
    counts_B_k <- numeric(length = m)
    for (i in 1:n) {
        bin_not_found_yet <- T
        k <- 1
        while (bin_not_found_yet) {
            u_k_1 <- bins[k]
            u_k <- bins[k + 1]
            if (I(dat[i] >= u_k_1 && dat[i] < u_k)) {
                counts_B_k[k] <- counts_B_k[k] + 1
                bin_not_found_yet <- F
            }
            k <- k + 1
        }
    }
    denoms <- n * diff(bins)
    sapply(
        x,
        function(d) {
            I_x_B_k <- numeric(length = m)
            for (k in 1:m) {
                u_k_1 <- bins[k]
                u_k <- bins[k + 1]
                if (I(d >= u_k_1 && d < u_k)) {
                    I_x_B_k[k] <- 1
                }
            }
            sum((I_x_B_k / denoms) * counts_B_k)
        }
    )
}



#' rpmf
#'
#' Draw random samples from a probability mass function using the \href{https://en.wikipedia.org/wiki/Inverse_transform_sampling}{inverse transform sampling}.
#'
#' @usage rpmf(n, pmf, support, ...)
#' @param n An integer >= 1.
#' @param pmf A probability mass function which takes elements of an ambient
#'   space of pmf's support (i.e. an input that makes pmf return a probability)
#'   in its first argument. This pmf has a finite \code{support}, and must be
#'   able to take an array of elements and return an array of probabilities.
#'   Each output of this \code{pmf} must fall in [0, 1], and every conceivable
#'   output, including 0, must sum up to 1, i.e. if an object that is not
#'   an element of \code{support} is given, \code{pmf} must return 0.
#' @param support A vector which consists of the elements of the support
#' of \code{pmf}, i.e. \code{sum(pmf(support, ...)) == 1 &&
#' !(0 \%in\% pmf(support, ...))}
#' @param \dots An additional argument of \code{pmf}
#' @author Junkyu Park
#' @seealso
#'   \code{\link{csum_N}},
#'   \code{\link{dpmf}},
#'   \href{https://joon3216.github.io/research_materials/2018/pgf}{Evaluating a hard-to-evaluate pmf using pgf and DFT}
#' @examples
#' # Example 1: dfruit in dpmf example
#' # Generate 12 random samples from dfruit
#' rpmf(12, dfruit, c('apple', 'orange', 'neither'))
#' # Example 2: dX in csum_N example
#' # Generate 20 samples of X_i ~ dX
#' rpmf(20, dX, 0:5)
#' # Example 3: S = sum of iid X_i's, i = 1, ..., N,
#' # where N ~ Pois(3) and N independent of all X_i ~ dX (in Example 2)
#' # Generate 12 samples of S
#' result_X <- csum_N(dX, support = 0:5, lambda = 3)
#' M <- length(result_X)
#' rpmf(12, dpmf, 0:(M - 1), result_X)
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

